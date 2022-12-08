#' Fuzzy Match Species within Genus
#' @description
#' Tries to fuzzy match the species name to `Treemendous.Trees` within a genus. Uses `fuzzyjoin::stringdist()` to perform fuzzy matching.
#'
#' @param df `tibble` containing the species binomial split into the columns `Orig.Genus` and `Orig.Species`.
#' @param backbone specifies which backbone is used: needs to be a subset of `c('BGCI', 'WCVP', 'WFO', 'GBIF')` or `NULL` if the whole database should be used.
#' @param target_df is used if the user wants to provide a custom target dataset. The parameter is intended only for compatibility with the function translate_trees and should not be directly used.
#'
#' @return
#' Returns a `tibble` with the additional logical column `fuzzy_match_species_within_genus`, indicating whether the specific epithet was successfully fuzzy matched within the matched genus (`r TRUE`) or not (`r FALSE`).
#' @export
#'
#' @examples
#' iucn %>%
#'     dplyr::mutate(Orig.Genus = stringr::str_replace(Orig.Genus, '.{1}$', '')) %>%
#'     dplyr::mutate(Matched.Genus = Orig.Genus) %>%
#'     fuzzy_match_species_within_genus()
fuzzy_match_species_within_genus <- function(df, backbone = NULL, target_df = NULL){
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species', 'Matched.Genus') %in% colnames(df)))

  ## solve issue of empty input tibble, and needed to ensure compatibility with sequential_matching: because there the columns already exists for the second backbone
  if(nrow(df) == 0){
    if(!all(c('fuzzy_match_species_within_genus', 'fuzzy_species_dist') %in% colnames(df))){
      return(tibble::add_column(df, fuzzy_match_species_within_genus = NA, fuzzy_species_dist = NA))
    }
    else{
      return(df)
    }
  }

  ## solve issue in second iteration of sequential_matching: necessary to remove fuzzy_species_dist column: otherwise 2 columns are generated 'fuzzy_species_dist...1, fuzzy_species_dist...2'
  if('fuzzy_species_dist' %in% colnames(df)){
    df <- df %>% dplyr::mutate(fuzzy_species_dist = NULL)
  }

  res <- df %>%
    dplyr::group_by(Matched.Genus) %>%
    dplyr::group_split() %>% ## TODO: change to dplyr::group_map to be able to omit dplyr::group_split() stage
    map_dfr_progress(fuzzy_match_species_within_genus_helper, backbone, target_df)

  return(res)
}


fuzzy_match_species_within_genus_helper <- function(df, backbone, target_df){
  # subset database
  genus <- df %>% dplyr::distinct(Matched.Genus) %>% unlist()
  database_subset <- memoised_get_trees_of_genus(genus, backbone, target_df)

  # fuzzy match
  matched <- df %>%
    fuzzyjoin::stringdist_left_join(database_subset,
                                    by = c('Orig.Species' = 'Species'),
                                    distance_col = 'fuzzy_species_dist') %>%
    dplyr::mutate(Matched.Species = Species) %>%
    dplyr::select(-c('Species', 'Genus')) %>%
    dplyr::group_by(Orig.Genus, Orig.Species) %>%
    dplyr::filter(fuzzy_species_dist == min(fuzzy_species_dist)) %>%
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0, return(.x), return(dplyr::slice_head(.x,n=1))) ## In cases of multiple matches: we choose first match. Alternatively could use something more sophisticated here: like for instance choosing the one with more support (present in more databases)
    ) %>%
    dplyr::ungroup()

  unmatched <- fuzzyjoin::stringdist_anti_join(df, database_subset, by = c('Orig.Species' = 'Species'))

  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'fuzzy_match_species_within_genus') %>%
    dplyr::mutate(fuzzy_match_species_within_genus = (fuzzy_match_species_within_genus == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble
  return(combined)
}




