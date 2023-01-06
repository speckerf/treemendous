#' Suffix Match Species within Genus
#' @description
#' Tries to match the specific epithet by exchanging common suffixes within an already matched genus in `Treemendous.Trees`. The following suffixes are captured: `c("a", "i", "is", "um", "us", "ae")`
#'
#' @param df `tibble` containing the species binomial split into the columns `Orig.Genus` and `Orig.Species`.
#' @param backbone specifies which backbone is used: needs to be a subset of `c('BGCI', 'WCVP', 'WFO', 'GBIF')` or `NULL` if the whole database should be used.
#' @param target_df is used if the user wants to provide a custom target dataset. The parameter is intended only for compatibility with the function translate_trees and should not be directly used.
#'
#' @return
#' Returns a `tibble` with the additional logical column `suffix_match_species_within_genus`, indicating whether the specific epithet was successfully matched within the matched genus (`r TRUE`) or not (`r FALSE`).
#' @export
#'
#' @examples
#' # substitute endings c('um$|i$|is$|us$|ae$') with 'a' of specific epithet
#' iucn_modified<- iucn %>%
#'     dplyr::mutate(Orig.Species = stringr::str_replace(Orig.Species, 'um$|i$|is$|us$|ae$', 'a'))
#' iucn_modified %>%
#'     dplyr::mutate(Matched.Genus = Orig.Genus) %>%
#'     suffix_match_species_within_genus(backbone = c('BGCI', 'WFO'))
suffix_match_species_within_genus <- function(df, backbone = NULL, target_df = NULL){
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species', 'Matched.Genus') %in% colnames(df)))

  ## solve issue of empty input tibble, and needed to ensure compatilbility with sequential_matching: because there the columns already exists for the second backbone
  if(nrow(df) == 0){
    if(!all(c('suffix_match_species_within_genus') %in% colnames(df))){
      return(tibble::add_column(df, suffix_match_species_within_genus = NA))
    }
    else{
      return(df)
    }
  }

  res <- df %>%
    dplyr::group_by(Matched.Genus) %>%
    dplyr::group_split() %>%
    map_dfr_progress(suffix_match_species_within_genus_helper, backbone, target_df)

  return(res)
}


suffix_match_species_within_genus_helper <- function(df, backbone, target_df){
  # subset database
  genus <- df %>% dplyr::distinct(Matched.Genus) %>% unlist()
  database_subset <- memoised_get_trees_of_genus(genus, backbone, target_df)

  # ending match
  ## create word root column in both the database subset and user input
  #common_suffixes <- c("a", "i", "is", "um", "us", "ae", "oides", "escens")
  common_suffixes <- rev(c("a", "i", "is", "um", "us", "ae"))
  catch_suffixes <- paste0("(.*?)(", paste0(common_suffixes, collapse = "|"), ")$")
  df <- df %>%
    dplyr::mutate(Root = stringr::str_match(Orig.Species, catch_suffixes)[,2])
  database_subset <- database_subset %>%
    dplyr::mutate(Root = stringr::str_match(Species, catch_suffixes)[,2])

  ## matching based on root column
  matched <- df %>%
    dplyr::inner_join(database_subset, by = 'Root', na_matches = 'never') %>%
    dplyr::mutate(Matched.Species = Species) %>%
    dplyr::select(-c('Species', 'Genus', 'Root')) %>%
    dplyr::group_by(Orig.Genus, Orig.Species) %>%
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0, return(.x), return(dplyr::slice_head(.x,n=1)))
    ) %>%
    dplyr::ungroup()

  unmatched <- df %>%
    dplyr::anti_join(database_subset, by = c('Root'), na_matches = 'never') %>%
    dplyr::select(-c('Root'))

  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'suffix_match_species_within_genus') %>%
    dplyr::mutate(suffix_match_species_within_genus = (suffix_match_species_within_genus == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble
  return(combined)
}




