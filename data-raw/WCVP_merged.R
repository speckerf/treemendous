## code to prepare `WCVP` dataset goes here

## code to prepare `Treemendous.Trees` dataset goes here
packages = c("dplyr", "stringr",
             "tidyr", "purrr",
             "readr", "memoise", "multidplyr",
             "furrr")

N_WORKERS = 2

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

load_WCVP <- function(paths){
  # reproduce examples for revisions
  message("Read raw data from disk...")
  list_of_genera <- get_tree_genera_list(paths)
  fieldnames <- c('family', 'genus', 'kew_id', 'accepted_kew_id', 'authors', 'rank', 'taxonomic_status', 'taxon_name')
  WCVP <- read_delim(paths[['wcvp']], delim = '|', col_select = dplyr::all_of(fieldnames)) %>%
    dplyr::rename('WCVP_Family' = 'family',
                  'Genus' = 'genus',
                  'Species' = 'taxon_name',
                  'WCVP_ID' = 'kew_id',
                  'WCVP_accepted_ID' = 'accepted_kew_id',
                  'WCVP_Authors' = 'authors',
                  'WCVP_Status' = 'taxonomic_status',
                  'WCVP_Rank' = 'rank') %>%
    dplyr::mutate(Species = stringr::str_remove(Species, pattern = "^([A-Z])\\w+\\s?")) %>% # get all except Genus from taxon_name. ^\S+\h*(.*$). ".*?\\s"
    dplyr::filter(WCVP_Rank != 'GENUS') %>%
    dplyr::mutate(WCVP_Rank = stringr::str_to_sentence(WCVP_Rank),
                  WCVP_new_linkage = NULL) %>%
    dplyr::filter(WCVP_Rank %in% c('Form', 'Species', 'Subspecies', 'Variety')) %>%
    tidyr::drop_na(c('Genus', 'Species'))
  #WCVP <- WCVP[1:100000,]

  # move everything after f. | var. | subsp. into WCVP_Infraspecific
  WCVP <- WCVP %>%
    dplyr::mutate(WCVP_Infraspecific = stringr::str_extract(Species, "(.+)(\\sf\\.|var\\.|subsp\\.\\s)(.+)", group = 3)) %>%
    dplyr::mutate(WCVP_Infraspecific = stringr::str_trim(WCVP_Infraspecific)) %>% # trim leading and trailing spaces
    dplyr::mutate(WCVP_Species = stringr::str_remove(Species, "\\s.*")) %>%
    dplyr::mutate(Species = NULL) %>%
    dplyr::rename('Species' = 'WCVP_Species')

  # Check for potential Author conflicts
  # returns TRUE:
  #     entries with the same genus, species are resolved to multiple different genus, species combinations
  check_diverging_accepted_names_boolean <- function(ids, accepted_ids, bb_name = "WCVP"){
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

      ## if all input species are rank Species
      # mark as potential authorship conflict
      ## else: mark as infraspecific conflict
      if(sum(input_df[[paste0(bb_name, "_Rank")]] == 'Species') > 1){
        return('authorship conflict')
      } else{
        return('infraspecific conflict')
      }
    } else{
      return(NA)
    }
  }

  message("Check for author and infraspecific conflicts...")
  #initialixe workers
  cluster <- new_cluster(N_WORKERS)
  # copy the required data and libraries to the nodes
  cluster_copy(cluster, "check_diverging_accepted_names_boolean")
  cluster_copy(cluster, 'WCVP')
  cluster_library(cluster, "dplyr")

  # find conflicts within Genus, Species combination
  # this takes long: run in parallel
  WCVP_conflicts <- WCVP %>% # TODO: change here again to full dataset in the end %>%
    group_by(Genus, Species) %>%
    filter(n()>1) %>%
    partition(cluster) %>%
    summarise('WCVP_Flag' = check_diverging_accepted_names_boolean(WCVP_ID, WCVP_accepted_ID, bb_name = 'WCVP')) %>%
    collect() %>%
    ungroup()

  # add flag to WCVP
  WCVP <- WCVP %>%
    dplyr::select(-any_of("WCVP_Flag")) %>% # delete column WCVP flag if already present
    dplyr::left_join(WCVP_conflicts) %>%
    dplyr::relocate(Genus, Species) %>%
    dplyr::arrange(Genus, Species)

  # get accepted in list of genera in backbone
  WCVP_Accepted <- WCVP %>%
    dplyr::filter(Genus %in% list_of_genera &
                    WCVP_Status %in% c('Accepted')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)

  # get all synonyms in list of genera in backbone
  WCVP_Synonyms <- WCVP %>%
    dplyr::filter(Genus %in% list_of_genera &
                    WCVP_Status %in% c('Synonym', 'Homotypic_Synonym'),
                  !is.na(WCVP_accepted_ID)) %>% # exclude all the synonyms which do not contain any information on the corresponding accepted species.
    dplyr::anti_join(WCVP_Accepted, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)
  assertthat::assert_that(!any(is.na(WCVP_Synonyms$WCVP_accepted_ID)))

  WCVP_Accepted_Synonyms <- dplyr::bind_rows(WCVP_Accepted, WCVP_Synonyms)
  assertthat::assert_that(nrow(dplyr::distinct(WCVP_Accepted_Synonyms, Genus, Species)) == nrow(WCVP_Accepted) + nrow(WCVP_Synonyms))

  WCVP_Outgoing_Edges <- WCVP %>%
    dplyr::semi_join(WCVP_Accepted_Synonyms, by = c('WCVP_ID' = 'WCVP_accepted_ID')) %>%
    dplyr::anti_join(WCVP_Accepted_Synonyms, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)
  assertthat::assert_that(all(is.na(WCVP_Outgoing_Edges$WCVP_accepted_ID)))

  WCVP_Incoming_Edges <- WCVP %>%
    dplyr::semi_join(WCVP_Accepted_Synonyms, by = c('WCVP_accepted_ID' = 'WCVP_ID')) %>%
    dplyr::anti_join(WCVP_Accepted_Synonyms, by = c('Genus', 'Species')) %>%
    dplyr::anti_join(WCVP_Outgoing_Edges, by = c('Genus', 'Species')) %>%
    dplyr::distinct(Genus, Species, .keep_all = TRUE)
  assertthat::assert_that(!any(is.na(WCVP_Incoming_Edges$WCVP_accepted_ID)))

  ## Solve issue where outgoing species are not of Rank Species: see comments in load_WFO for more details
  ## problem: the connected names might have been deleted while calling dplyr::distinct : therefore the WCVP_accepted_ID might not be present anymore
  ## instead of dropping the 26000 species, we could search for other valid WCVP_ID's with the same Genus, Species name
  WCVP_Synonyms_where_Accepted_not_found <- WCVP_Synonyms %>%
    dplyr::filter(!is.na(WCVP_accepted_ID)) %>% # redundant line
    dplyr::anti_join(WCVP_Accepted_Synonyms, by = c('WCVP_accepted_ID' = 'WCVP_ID')) %>%
    dplyr::anti_join(WCVP_Outgoing_Edges, by = c('WCVP_accepted_ID' = 'WCVP_ID'))


  ##################
  # Reconnect WCVP_accepted_ID, when it points to entry ID that got removed during distinct(Genus, Species)
  ##################
  message("Reconnecting synonym-accepted relation to the present Genus Species entries... ")
  find_new_connection <- function(accepted_id){
    connected_name <- WCVP %>% filter(WCVP_ID == accepted_id) %>% select(Genus, Species)
    connected_name_in_accepted <- WCVP_Accepted %>% bind_rows(WCVP_Outgoing_Edges) %>% semi_join(connected_name, by = c('Genus', 'Species'))
    new_accepted_id <- connected_name_in_accepted$WCVP_ID
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
  WCVP_Synonyms_where_Accepted_not_found_reconnect <- WCVP_Synonyms_where_Accepted_not_found %>%
    mutate(WCVP_new_connection = future_pmap_chr(.l = list(WCVP_accepted_ID), .f = find_new_connection)) %>%
    mutate(WCVP_new_linkage = !is.na(WCVP_new_connection)) %>%
    mutate(WCVP_accepted_ID = coalesce(WCVP_new_connection, WCVP_accepted_ID)) %>%
    select(-c("WCVP_new_connection"))

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
  WCVP_Accepted_Synonyms_NEW <- replace_subset(WCVP_Accepted_Synonyms %>% mutate(WCVP_new_linkage = NA), WCVP_Synonyms_where_Accepted_not_found_reconnect, c("Genus", "Species"))

  WCVP_Synonyms_where_Accepted_not_found_NEW <- WCVP_Synonyms_where_Accepted_not_found_reconnect %>%
    dplyr::anti_join(WCVP_Accepted_Synonyms, by = c('WCVP_accepted_ID' = 'WCVP_ID')) %>%
    dplyr::anti_join(WCVP_Outgoing_Edges, by = c('WCVP_accepted_ID' = 'WCVP_ID'))

  WCVP_Accepted_Synonyms_without_Conflict <- WCVP_Accepted_Synonyms_NEW %>%
    dplyr::anti_join(WCVP_Synonyms_where_Accepted_not_found_NEW, by = c('Genus', 'Species'))

  WCVP_merged <- WCVP_Accepted_Synonyms_without_Conflict %>%
    dplyr::bind_rows(WCVP_Outgoing_Edges, WCVP_Incoming_Edges) %>%
    dplyr::distinct(Genus, Species, .keep_all = T) %>% # this line should not be necessary
    dplyr::arrange(Genus, Species)

  nrow_conflicting <- WCVP_merged %>% filter(!is.na(WCVP_accepted_ID)) %>% dplyr::anti_join(WCVP_merged, by = c('WCVP_accepted_ID' = 'WCVP_ID')) %>% nrow()
  if(nrow_conflicting > 0){
    warning(paste('The WCVP database is not self-contained, meaning that there are synonyms for which the accepted species is not in the database! In total, there are', nrow_conflicting, 'conflicting species.'))
  }
  return(WCVP_merged)
}


## !! if backbones are updated: remember to update Treemendous.Trees documentation in R/data.R as well!!
paths <- yaml::read_yaml("data-raw/paths.yml")

get_tree_genera_list <- memoise::memoise(helper.get_tree_genera_list) ## remember output of tree genera list using memoise:  only needs to evaluate it once

WCVP <- load_WCVP(paths)

# save to disk
message("Saving to disk...")
usethis::use_data(WCVP, overwrite = TRUE)





