#' Match Genus name
#'
#' @description
#' #' Tries to match the genus name to `Treemendous.Trees`.
#'
#' @param df `tibble` containing the species binomial split into the columns `Orig.Genus` and `Orig.Species`.
#' @param backbone specifies which backbone is used: needs to be a subset of `c('BGCI', 'WCVP', 'WFO', 'GBIF')` or `NULL` if the whole database should be used.
#' @param target_df is used if the user wants to provide a custom target dataset. The parameter is intended only for compatibility with the function translate_trees and should not be directly used.
#'
#' @return
#' Returns a `tibble` with the additional logical column `genus_match`, indicating whether the genus was successfully matched (`r TRUE`) or not (`r FALSE`)
#' @export
#'
#' @examples
#' iucn %>% genus_match()
genus_match <- function(df, backbone = NULL, target_df = NULL){
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species') %in% colnames(df)))

  ## solve issue of empty input tibble, and needed to ensure compatilbility with sequential_matching: because there the columns already exists for the second backbone
  if(nrow(df) == 0){
    if(!all(c('genus_match') %in% colnames(df))){
      return(tibble::add_column(df, genus_match = NA))
    }
    else{
      return(df)
    }
  }

  matched <- df %>%
    dplyr::semi_join(get_db(backbone, target_df), by = c('Orig.Genus' = 'Genus')) %>%
    dplyr::mutate(Matched.Genus = Orig.Genus)
  unmatched <- df %>%
    dplyr::anti_join(get_db(backbone, target_df), by = c('Orig.Genus' = 'Genus'))
  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'genus_match') %>%
    dplyr::mutate(genus_match = (genus_match == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble

  return(combined)
}
