## code to prepare `GBIF_merged` dataset goes here


## code to prepare `Treemendous.Trees` dataset goes here
packages = c("dplyr", "stringr",
             "tidyr", "purrr",
             "readr", "memoise", "multidplyr",
             "furrr", "fs")

N_WORKERS = 4

## Now load or install&load all
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

get_tree_genera_list <- function(paths){
  BGCI <- load_BGCI(paths)
  list_genera <- base::unique(BGCI$Genus)
  return(list_genera)
}

load_BGCI <- function(paths){
  fieldnames = c('TaxonName', 'Author')
  df <- readr::read_csv(paths[['bgci']], col_select = all_of(fieldnames)) %>%
    tidyr::separate('TaxonName', sep = ' ', into = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE) %>%
    tidyr::drop_na(Genus, Species) %>%
    dplyr::rename('BGCI_Authors' = 'Author')
  return(df)
}

load_GBIF <- function(paths){
  # reproduce examples for revisions
  message("Read raw data from disk...")
  list_of_genera <- get_tree_genera_list(paths)
  ## load dataset
  fieldnames <- c('specificEpithet', 'kingdom', 'phylum', 'class', 'order', 'family', 'genericName', 'taxonID', 'acceptedNameUsageID', 'taxonRank', 'taxonomicStatus', 'scientificNameAuthorship', 'infraspecificEpithet')
  GBIF <- read_delim(paths[['gbif']], delim = '\t', col_select = all_of(fieldnames)) %>%
    dplyr::rename('GBIF_Family' = 'family',
                  'Genus' = 'genericName',
                  'Species' = 'specificEpithet',
                  'GBIF_Infraspecific' = 'infraspecificEpithet',
                  'GBIF_ID' = 'taxonID',
                  'GBIF_accepted_ID' = 'acceptedNameUsageID',
                  'GBIF_Authors' = 'scientificNameAuthorship',
                  'GBIF_Status' = 'taxonomicStatus',
                  'GBIF_Rank' = 'taxonRank') %>%
    # dplyr::filter(kingdom == 'Plantae' &
    #                 phylum == 'Tracheophyta' &
    #                 class %in% c('Magnoliopsida', 'Pinopsida', 'Ginkgoopsida', 'Cycadopsida')) %>%
    tidyr::drop_na(c('Genus', 'Species')) %>%
    dplyr::mutate(GBIF_Rank = stringr::str_to_sentence(GBIF_Rank),
                  GBIF_Status = stringr::str_to_sentence(GBIF_Status)) %>%
    dplyr::filter(GBIF_Rank %in% c('Form', 'Species', 'Subspecies', 'Variety')) %>% # entries with following ranks are discared: unranked (1221) (no unranked are considered accepted)
    dplyr::mutate(kingdom = NULL,
                  phylum = NULL,
                  class = NULL,
                  order = NULL,
                  GBIF_accepted_ID = as.character(GBIF_accepted_ID),
                  GBIF_ID = as.character(GBIF_ID))

  #GBIF <- GBIF[1:100, ]

  # Check for potential Author conflicts
  # returns TRUE:
  #     entries with the same genus, species are resolved to multiple different genus, species combinations
  check_diverging_accepted_names_boolean <- function(ids, accepted_ids, bb_name = "GBIF"){
    if(all(is.na(accepted_ids))){
      return(NA)
    }

    # start with ids
    # if accepted_id not NA
    # replace corresponding id with accepted_id
    to_resolve_ids <- ids
    idx_with_nonNA <- which(!is.na(accepted_ids))
    to_resolve_ids[idx_with_nonNA] <- accepted_ids[idx_with_nonNA]
    resolved_ids <- unique(to_resolve_ids)
    resolved_df <- get(bb_name) %>%
      filter(get(paste0(bb_name, "_ID")) %in% resolved_ids) %>% distinct(Genus, Species, .keep_all = TRUE)  # don't return the grouping variables Genus Species within group_modify because of the following error: Error in `group_modify()`: ! The returned data frame cannot contain the original grouping variables: Genus, Species.
    if(nrow(resolved_df) > 1){ # more than one unique genus species combination that the names could be resolved to
      input_df <- get(bb_name) %>%
        filter(get(paste0(bb_name, "_ID")) %in% ids)

      input_df_rank_species <- input_df %>% filter(GBIF_Rank == 'Species')

      resolved_from_authorhsip_ambiguity <- resolved_df %>% semi_join(input_df_rank_species, by = c('GBIF_ID' = 'GBIF_accepted_ID')) %>%
        bind_rows(input_df_rank_species %>% filter(is.na(GBIF_accepted_ID))) %>%
        distinct(Genus, Species)

      ## if all input species with rank Species resolve to different unique Genus Species combinations
      # mark as potential authorship conflict
      ## else: mark as infraspecific conflict
      if(nrow(resolved_from_authorhsip_ambiguity) > 1){
        #print(input_df)
        return('authorship ambiguity')
      } else{
        return('infraspecific ambiguity')
      }
    } else{
      return(NA)
    }
  }

  message("Check for author and infraspecific ambiguity")
  #initialixe workers
  cluster <- new_cluster(N_WORKERS)
  # copy the required data and libraries to the nodes
  cluster_copy(cluster, "check_diverging_accepted_names_boolean")
  cluster_copy(cluster, 'GBIF')
  cluster_library(cluster, "dplyr")

  # find conflicts within Genus, Species combination
  # this takes long: run in parallel
  GBIF_conflicts <- GBIF %>% # TODO: change here again to full dataset in the end %>%
    group_by(Genus, Species) %>%
    filter(n()>1) %>%
    partition(cluster) %>%
    summarise('GBIF_Flag' = check_diverging_accepted_names_boolean(GBIF_ID, GBIF_accepted_ID, bb_name = 'GBIF')) %>%
    collect() %>%
    ungroup()

  # add flag to GBIF
  GBIF <- GBIF %>%
    dplyr::select(-any_of("GBIF_Flag")) %>% # delete column GBIF flag if already present
    dplyr::left_join(GBIF_conflicts) %>%
    dplyr::relocate(Genus, Species) %>%
    dplyr::arrange(Genus, Species)

  # get accepted in list of genera in backbone
  GBIF_Accepted <- GBIF %>%
    dplyr::filter(Genus %in% list_of_genera &
                    GBIF_Status %in% c('Accepted')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  # get all synonyms in list of genera in backbone
  GBIF_Synonyms <- GBIF %>%
    dplyr::filter(Genus %in% list_of_genera &
                    GBIF_Status %in% c('Synonym', 'Homotypic_Synonym'),
                  !is.na(GBIF_accepted_ID)) %>% # exclude all the synonyms which do not contain any information on the corresponding accepted species.
    dplyr::anti_join(GBIF_Accepted, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)
  assertthat::assert_that(!any(is.na(GBIF_Synonyms$GBIF_accepted_ID)))

  GBIF_Accepted_Synonyms <- dplyr::bind_rows(GBIF_Accepted, GBIF_Synonyms)
  assertthat::assert_that(nrow(dplyr::distinct(GBIF_Accepted_Synonyms, Genus, Species)) == nrow(GBIF_Accepted) + nrow(GBIF_Synonyms))

  GBIF_Outgoing_Edges <- GBIF %>%
    dplyr::semi_join(GBIF_Accepted_Synonyms, by = c('GBIF_ID' = 'GBIF_accepted_ID')) %>%
    dplyr::anti_join(GBIF_Accepted_Synonyms, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)
  assertthat::assert_that(all(is.na(GBIF_Outgoing_Edges$GBIF_accepted_ID)))

  GBIF_Incoming_Edges <- GBIF %>%
    dplyr::semi_join(GBIF_Accepted_Synonyms, by = c('GBIF_accepted_ID' = 'GBIF_ID')) %>%
    dplyr::anti_join(GBIF_Accepted_Synonyms, by = c('Genus', 'Species')) %>%
    dplyr::anti_join(GBIF_Outgoing_Edges, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)
  assertthat::assert_that(!any(is.na(GBIF_Incoming_Edges$GBIF_accepted_ID)))

  ## Solve issue where outgoing species are not of Rank Species: see comments in load_WFO for more details
  ## problem: the connected names might have been deleted while calling dplyr::distinct : therefore the GBIF_accepted_ID might not be present anymore
  ## instead of dropping the 26000 species, we could search for other valid GBIF_ID's with the same Genus, Species name
  GBIF_Synonyms_where_Accepted_not_found <- GBIF_Synonyms %>%
    dplyr::filter(!is.na(GBIF_accepted_ID)) %>% # redundant line
    dplyr::anti_join(GBIF_Accepted_Synonyms, by = c('GBIF_accepted_ID' = 'GBIF_ID')) %>%
    dplyr::anti_join(GBIF_Outgoing_Edges, by = c('GBIF_accepted_ID' = 'GBIF_ID'))


  ##################
  # Reconnect GBIF_accepted_ID, when it points to entry ID that got removed during distinct(Genus, Species)
  ##################
  message("Reconnecting synonym-accepted relation to the present Genus Species entries... ")
  find_new_connection <- function(accepted_id){
    connected_name <- GBIF %>% filter(GBIF_ID == accepted_id) %>% select(Genus, Species)
    connected_name_in_accepted <- GBIF_Accepted %>% bind_rows(GBIF_Outgoing_Edges) %>% semi_join(connected_name, by = c('Genus', 'Species'))
    new_accepted_id <- connected_name_in_accepted$GBIF_ID
    if(length(new_accepted_id) != 1){
      #browser()
      message(paste0(connected_name$Genus, ' ', connected_name$Species, " : could not reconnect to new accepted species.."))
      return(NA)
    }
    assertthat::assert_that(length(new_accepted_id) == 1)
    new_accepted_id
  }

  # reconnect to entry with the same Genus, Species combination which is still present in the database.
  plan(multisession, workers = N_WORKERS)
  GBIF_Synonyms_where_Accepted_not_found_reconnect <- GBIF_Synonyms_where_Accepted_not_found %>%
    mutate(GBIF_new_connection = future_pmap_chr(.l = list(GBIF_accepted_ID), .f = find_new_connection)) %>%
    mutate(GBIF_new_linkage = !is.na(GBIF_new_connection)) %>%
    mutate(GBIF_accepted_ID = coalesce(GBIF_new_connection, GBIF_accepted_ID)) %>%
    select(-c("GBIF_new_connection"))

  # helper function
  replace_subset <- function(df, df_subset, id_col_names = c()) {
    # work out which of the columns contain "new" data
    new_data_col_names <- colnames(df_subset)[which(!colnames(df_subset) %in% id_col_names)]
    # complete the df_subset with the extra columns from df
    df_sub_to_join <- df_subset %>%
      left_join(select(df, -all_of(new_data_col_names)), by = c(id_col_names))
    # join and bind rows
    df_out <- df %>%
      anti_join(df_sub_to_join, by = c(id_col_names)) %>%
      bind_rows(df_sub_to_join)
    return(df_out)
  }

  # with the new connections
  GBIF_Accepted_Synonyms_NEW <- replace_subset(GBIF_Accepted_Synonyms %>% mutate(GBIF_new_linkage = NA), GBIF_Synonyms_where_Accepted_not_found_reconnect, c("Genus", "Species"))

  GBIF_Synonyms_where_Accepted_not_found_NEW <- GBIF_Synonyms_where_Accepted_not_found_reconnect %>%
    dplyr::anti_join(GBIF_Accepted_Synonyms, by = c('GBIF_accepted_ID' = 'GBIF_ID')) %>%
    dplyr::anti_join(GBIF_Outgoing_Edges, by = c('GBIF_accepted_ID' = 'GBIF_ID'))

  GBIF_Accepted_Synonyms_without_Conflict <- GBIF_Accepted_Synonyms_NEW %>%
    dplyr::anti_join(GBIF_Synonyms_where_Accepted_not_found_NEW, by = c('Genus', 'Species'))

  GBIF_merged <- GBIF_Accepted_Synonyms_without_Conflict %>%
    dplyr::bind_rows(GBIF_Outgoing_Edges, GBIF_Incoming_Edges) %>%
    dplyr::distinct(Genus, Species, .keep_all = T) %>% # this line should not be necessary
    dplyr::arrange(Genus, Species)

  nrow_conflicting <- GBIF_merged %>% filter(!is.na(GBIF_accepted_ID)) %>% dplyr::anti_join(GBIF_merged, by = c('GBIF_accepted_ID' = 'GBIF_ID')) %>% nrow()
  if(nrow_conflicting > 0){
    warning(paste('The GBIF database is not self-contained, meaning that there are synonyms for which the accepted species is not in the database! In total, there are', nrow_conflicting, 'conflicting species.'))
  }
  return(GBIF_merged)
}


## !! if backbones are updated: remember to update Treemendous.Trees documentation in R/data.R as well!!
paths <- yaml::read_yaml("data-raw/paths.yml")

#get_tree_genera_list <- memoise::memoise(helper.get_tree_genera_list) ## remember output of tree genera list using memoise:  only needs to evaluate it once

GBIF <- load_GBIF(paths)

# save to disk
message("Saving to disk...")

if(fs::dir_exists(fs::path('data-raw', 'add_to_sysdata'))){
  saveRDS(GBIF, file = fs::path('data-raw', 'add_to_sysdata', 'GBIF.rds'))
} else{
  fs::dir_create(fs::path('data-raw', 'add_to_sysdata'))
  saveRDS(GBIF, file = fs::path('data-raw', 'add_to_sysdata', 'GBIF.rds'))
}
# usethis::use_data(GBIF, overwrite = TRUE, internal = TRUE) # we use create_sysdata.R instead
