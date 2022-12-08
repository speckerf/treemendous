#' Direct Match Species within Genus
#'
#' @description
#' Tries to directly match the specific epithet within an already matched genus in `Treemendous.Trees`
#'
#' @param df `tibble` containing the species binomial split into the columns `Orig.Genus` and `Orig.Species`.
#' @param backbone specifies which backbone is used: needs to be a subset of `c('BGCI', 'WCVP', 'WFO', 'GBIF')` or `NULL` if the whole database should be used.
#' @param target_df is used if the user wants to provide a custom target dataset. The parameter is intended only for compatibility with the function translate_trees and should not be directly used.
#'
#' @return
#' Returns a `tibble` with the additional logical column `direct_match_species_within_genus`, indicating whether the specific epithet was successfully matched within the matched genus (`r TRUE`) or not (`r FALSE`).
#' @export
#'
#' @examples
#' iucn %>% dplyr::mutate(Matched.Genus = Orig.Genus) %>% direct_match_species_within_genus()
direct_match_species_within_genus <- function(df, backbone = NULL, target_df = NULL){

  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species', 'Matched.Genus') %in% colnames(df)))

  ## solve issue of empty input tibble, and needed to ensure compatilbility with sequential_matching: because there the columns already exists for the second backbone
  if(nrow(df) == 0){
    if(!all(c('direct_match_species_within_genus') %in% colnames(df))){
      return(tibble::add_column(df, direct_match_species_within_genus = NA))
    }
    else{
      return(df)
    }
  }

  res <- df %>%
    dplyr::group_by(Matched.Genus) %>%
    dplyr::group_split() %>%
    map_dfr_progress(direct_match_species_within_genus_helper, backbone, target_df)

  return(res)
}

direct_match_species_within_genus_helper <- function(df, backbone, target_df){
  # subset database
  genus <- df %>% dplyr::distinct(Matched.Genus) %>% unlist()
  #database_subset <- get_trees_by_genera(backbone)[[genus]] %>% dplyr::select(c('Genus', 'Species'))
  #database_subset <- get_trees_of_genus(genus, backbone) %>%
  #  dplyr::select(c('Genus', 'Species')) # optionally could use memoise to potentially speed up this function: would need to test what is faster
  database_subset <- memoised_get_trees_of_genus(genus, backbone, target_df)

  # match specific epithet within genus
  matched <- df %>%
    dplyr::semi_join(database_subset, by = c('Orig.Species' = 'Species')) %>%
    dplyr::mutate(Matched.Species = Orig.Species)
  unmatched <- df %>%
    dplyr::anti_join(database_subset, by = c('Orig.Species' = 'Species'))
  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'direct_match_species_within_genus') %>%
    dplyr::mutate(direct_match_species_within_genus = (direct_match_species_within_genus == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble
  return(combined)
}
