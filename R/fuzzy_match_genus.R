#' Fuzzy Match Genus Name
#' @description
#' Tries to fuzzy match the specific epithet within the same Genus to the Trees Database. Uses `fuzzyjoin::stringdist()` method to perform fuzzy matching.
#' @param df
#' tibble containing the species binomial split into two columns: 'Genus' & 'Species'
#'
#' @return
#' Returns a `tibble` with the same number of rows as the input `df` and with one additional Boolean column
#' _New.fuzzy_match_genus_ indicating whether the genus name was successfully fuzzy matched (`r TRUE`) or not (`r FALSE`)
#' @export
#'
#' @examples
#' fuzzy_match_genus(test4)
fuzzy_match_genus <- function(df){
  assertthat::assert_that(all(c('Genus', 'Species') %in% colnames(df)))

  Tree.Genera <- Trees.Full %>% dplyr::count(Genus)

  # fuzzy match
  matched <- fuzzyjoin::stringdist_semi_join(df, Tree.Genera, by = c('Genus'), max_dist = 1)
  unmatched <- fuzzyjoin::stringdist_anti_join(df, Tree.Genera, by = c('Genus'), max_dist = 1)
  assertthat::are_equal(dim(df), dim(matched)[1] + dim(unmatched)[1])

  res <-  dplyr::bind_rows(matched, unmatched, .id = 'fuzzy_match_genus') %>%
    dplyr::mutate(fuzzy_match_genus = (fuzzy_match_genus == 1)) %>% ## convert to Boolean
    dplyr::arrange(Genus, Species) %>%
    dplyr::relocate(c('Genus', 'Species')) ## Genus & Species column at the beginning of tibble

  return(res)
}
