## code to prepare `WFO_merged` dataset goes here

## code to prepare `Treemendous.Trees` dataset goes here
packages = c("dplyr", "stringr",
             "tidyr", "purrr",
             "readr", "memoise", "multidplyr",
             "furrr")

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

helper.get_tree_genera_list <- function(paths){
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

load_WFO <- function(paths){
  # reproduce examples for revisions
  message("Read raw data from disk...")
  list_of_genera <- get_tree_genera_list(paths)
  ## load dataset

  fieldnames <- c('family', 'genus', 'specificEpithet', 'infraspecificEpithet', 'taxonRank', 'taxonID', 'acceptedNameUsageID', 'taxonomicStatus', 'scientificNameAuthorship')
  WFO <- read_delim(paths[['wfo']], delim = '\t', col_select = all_of(fieldnames)) %>%
    dplyr::rename('WFO_Family' = 'family',
                  'Genus' = 'genus',
                  'Species' = 'specificEpithet',
                  'WFO_ID' = 'taxonID',
                  'WFO_accepted_ID' = 'acceptedNameUsageID',
                  'WFO_Authors' = 'scientificNameAuthorship',
                  'WFO_Status' = 'taxonomicStatus',
                  'WFO_Rank' = 'taxonRank',
                  'WFO_Infraspecific' = 'infraspecificEpithet') %>%
    dplyr::mutate(WFO_Rank = stringr::str_to_sentence(WFO_Rank),
                  WFO_Status = stringr::str_to_sentence(WFO_Status)) %>%
    dplyr::filter(WFO_Rank %in% c('Species', 'Variety', 'Subspecies', 'Form')) %>%
    tidyr::drop_na(c('Genus', 'Species'))

  #WFO <- WFO[1:100,]

  # # move everything after f. | var. | subsp. into WFO_Infraspecific
  # WFO <- WFO %>%
  #   dplyr::mutate(WFO_Infraspecific = stringr::str_extract(Species, "(.+)(\\sf\\.|var\\.|subsp\\.\\s)(.+)", group = 3)) %>%
  #   dplyr::mutate(WFO_Infraspecific = stringr::str_trim(WFO_Infraspecific)) %>% # trim leading and trailing spaces
  #   dplyr::mutate(WFO_Species = stringr::str_remove(Species, "\\s.*")) %>%
  #   dplyr::mutate(Species = NULL) %>%
  #   dplyr::rename('Species' = 'WFO_Species')

  # Check for potential Author ambiguity
  # returns TRUE:
  #     entries with the same genus, species are resolved to multiple different genus, species combinations
  check_diverging_accepted_names_boolean <- function(ids, accepted_ids, bb_name = "WFO"){
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

      input_df_rank_species <- input_df %>% filter(WFO_Rank == 'Species')

      resolved_from_authorhsip_ambiguity <- resolved_df %>% semi_join(input_df_rank_species, by = c('WFO_ID' = 'WFO_accepted_ID')) %>%
        bind_rows(input_df_rank_species %>% filter(is.na(WFO_accepted_ID))) %>%
        distinct(Genus, Species)

      ## if all input species are rank Species
      # mark as potential authorship ambiguity
      ## else: mark as infraspecific ambiguity
      if(sum(input_df[[paste0(bb_name, "_Rank")]] == 'Species') > 1){
        return('authorship ambiguity')
      } else{
        return('infraspecific ambiguity')
      }
    } else{
      return(NA)
    }
  }

  message("Check for author and infraspecific ambiguities...")
  #initialixe workers
  cluster <- new_cluster(N_WORKERS)
  # copy the required data and libraries to the nodes
  cluster_copy(cluster, "check_diverging_accepted_names_boolean")
  cluster_copy(cluster, 'WFO')
  cluster_library(cluster, "dplyr")

  # find conflicts within Genus, Species combination
  # this takes long: run in parallel
  WFO_conflicts <- WFO %>% # TODO: change here again to full dataset in the end %>%
    group_by(Genus, Species) %>%
    filter(n()>1) %>%
    partition(cluster) %>%
    summarise('WFO_Flag' = check_diverging_accepted_names_boolean(WFO_ID, WFO_accepted_ID, bb_name = 'WFO')) %>%
    collect() %>%
    ungroup()

  # add flag to WFO
  WFO <- WFO %>%
    dplyr::select(-any_of("WFO_Flag")) %>% # delete column WFO flag if already present
    dplyr::left_join(WFO_conflicts) %>%
    dplyr::relocate(Genus, Species) %>%
    dplyr::arrange(Genus, Species)

  # get accepted in list of genera in backbone
  WFO_Accepted <- WFO %>%
    dplyr::filter(Genus %in% list_of_genera &
                    WFO_Status %in% c('Accepted')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  # get all synonyms in list of genera in backbone
  WFO_Synonyms <- WFO %>%
    dplyr::filter(Genus %in% list_of_genera &
                    WFO_Status %in% c('Synonym', 'Homotypic_Synonym'),
                  !is.na(WFO_accepted_ID)) %>% # exclude all the synonyms which do not contain any information on the corresponding accepted species.
    dplyr::anti_join(WFO_Accepted, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)
  assertthat::assert_that(!any(is.na(WFO_Synonyms$WFO_accepted_ID)))

  WFO_Accepted_Synonyms <- dplyr::bind_rows(WFO_Accepted, WFO_Synonyms)
  assertthat::assert_that(nrow(dplyr::distinct(WFO_Accepted_Synonyms, Genus, Species)) == nrow(WFO_Accepted) + nrow(WFO_Synonyms))

  WFO_Outgoing_Edges <- WFO %>%
    dplyr::semi_join(WFO_Accepted_Synonyms, by = c('WFO_ID' = 'WFO_accepted_ID')) %>%
    dplyr::anti_join(WFO_Accepted_Synonyms, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)
  assertthat::assert_that(all(is.na(WFO_Outgoing_Edges$WFO_accepted_ID)))

  WFO_Incoming_Edges <- WFO %>%
    dplyr::semi_join(WFO_Accepted_Synonyms, by = c('WFO_accepted_ID' = 'WFO_ID')) %>%
    dplyr::anti_join(WFO_Accepted_Synonyms, by = c('Genus', 'Species')) %>%
    dplyr::anti_join(WFO_Outgoing_Edges, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)
  assertthat::assert_that(!any(is.na(WFO_Incoming_Edges$WFO_accepted_ID)))

  ## Solve issue where outgoing species are not of Rank Species: see comments in load_WFO for more details
  ## problem: the connected names might have been deleted while calling dplyr::distinct : therefore the WFO_accepted_ID might not be present anymore
  ## instead of dropping the 26000 species, we could search for other valid WFO_ID's with the same Genus, Species name
  WFO_Synonyms_where_Accepted_not_found <- WFO_Synonyms %>%
    dplyr::filter(!is.na(WFO_accepted_ID)) %>% # redundant line
    dplyr::anti_join(WFO_Accepted_Synonyms, by = c('WFO_accepted_ID' = 'WFO_ID')) %>%
    dplyr::anti_join(WFO_Outgoing_Edges, by = c('WFO_accepted_ID' = 'WFO_ID'))


  ##################
  # Reconnect WFO_accepted_ID, when it points to entry ID that got removed during distinct(Genus, Species)
  ##################
  message("Reconnecting synonym-accepted relation to the present Genus Species entries... ")
  find_new_connection <- function(accepted_id){
    connected_name <- WFO %>% filter(WFO_ID == accepted_id) %>% select(Genus, Species)
    connected_name_in_accepted <- WFO_Accepted %>% bind_rows(WFO_Outgoing_Edges) %>% semi_join(connected_name, by = c('Genus', 'Species'))
    new_accepted_id <- connected_name_in_accepted$WFO_ID
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
  WFO_Synonyms_where_Accepted_not_found_reconnect <- WFO_Synonyms_where_Accepted_not_found %>%
    mutate(WFO_new_connection = future_pmap_chr(.l = list(WFO_accepted_ID), .f = find_new_connection)) %>%
    mutate(WFO_new_linkage = !is.na(WFO_new_connection)) %>%
    mutate(WFO_accepted_ID = coalesce(WFO_new_connection, WFO_accepted_ID)) %>%
    select(-c("WFO_new_connection"))

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
  WFO_Accepted_Synonyms_NEW <- replace_subset(WFO_Accepted_Synonyms %>% mutate(WFO_new_linkage = NA), WFO_Synonyms_where_Accepted_not_found_reconnect, c("Genus", "Species"))

  WFO_Synonyms_where_Accepted_not_found_NEW <- WFO_Synonyms_where_Accepted_not_found_reconnect %>%
    dplyr::anti_join(WFO_Accepted_Synonyms, by = c('WFO_accepted_ID' = 'WFO_ID')) %>%
    dplyr::anti_join(WFO_Outgoing_Edges, by = c('WFO_accepted_ID' = 'WFO_ID'))

  WFO_Accepted_Synonyms_without_Conflict <- WFO_Accepted_Synonyms_NEW %>%
    dplyr::anti_join(WFO_Synonyms_where_Accepted_not_found_NEW, by = c('Genus', 'Species'))

  WFO_merged <- WFO_Accepted_Synonyms_without_Conflict %>%
    dplyr::bind_rows(WFO_Outgoing_Edges, WFO_Incoming_Edges) %>%
    dplyr::distinct(Genus, Species, .keep_all = T) %>% # this line should not be necessary
    dplyr::arrange(Genus, Species)

  nrow_conflicting <- WFO_merged %>% filter(!is.na(WFO_accepted_ID)) %>% dplyr::anti_join(WFO_merged, by = c('WFO_accepted_ID' = 'WFO_ID')) %>% nrow()
  if(nrow_conflicting > 0){
    warning(paste('The WFO database is not self-contained, meaning that there are synonyms for which the accepted species is not in the database! In total, there are', nrow_conflicting, 'conflicting species.'))
  }
  return(WFO_merged)
}


## !! if backbones are updated: remember to update Treemendous.Trees documentation in R/data.R as well!!
paths <- yaml::read_yaml("data-raw/paths.yml")

get_tree_genera_list <- memoise::memoise(helper.get_tree_genera_list) ## remember output of tree genera list using memoise:  only needs to evaluate it once


WFO <-  load_WFO(paths)

# save to disk
message("Saving to disk...")

if(fs::dir_exists(fs::path('data-raw', 'add_to_sysdata'))){
  saveRDS(WFO, file = fs::path('data-raw', 'add_to_sysdata', 'WFO.rds'))
} else{
  fs::dir_create(fs::path('data-raw', 'add_to_sysdata'))
  saveRDS(WFO, file = fs::path('data-raw', 'add_to_sysdata', 'WFO.rds'))
}

# usethis::use_data(WFO, overwrite = TRUE, internal = TRUE)





