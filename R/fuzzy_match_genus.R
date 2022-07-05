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
#' test4 %>% fuzzy_match_genus()
fuzzy_match_genus <- function(df){
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species') %in% colnames(df)))

  if(nrow(df) == 0){
    return(tibble::add_column(df, fuzzy_match_genus = NA, fuzzy_genus_dist = NA))
  }


  Tree.Genera <- Trees.Full %>% dplyr::distinct(Genus)

  # fuzzy match
  matched <- fuzzyjoin::stringdist_left_join(df, Tree.Genera, by = c('Orig.Genus' = 'Genus'), max_dist = 1, distance_col = 'fuzzy_genus_dist') %>%
    # save matched Genus name to New.Genus
    dplyr::mutate(Matched.Genus = Genus) %>%
    dplyr::select(-c('Genus')) %>%
    # in case of multiple matches: select the one with smallest distance (TODO: what exactly happens if two have the same minimal distance has to be investigated...)
    dplyr::group_by(Orig.Genus, Orig.Species) %>%
    dplyr::filter(fuzzy_genus_dist == min(fuzzy_genus_dist)) %>%
    #dplyr::slice_sample(n=1) %>% ### TODO!!! ISSUE here: need to fix how to choose if multiple genera have equal minimal fuzzy matching distance
    dplyr::ungroup()

  unmatched <- fuzzyjoin::stringdist_anti_join(df, Tree.Genera, by = c('Orig.Genus' = 'Genus'), max_dist = 1)
  assertthat::assert_that(dim(df)[1] == (dim(matched)[1] + dim(unmatched)[1]))

  res <-  dplyr::bind_rows(matched, unmatched, .id = 'fuzzy_match_genus') %>%
    dplyr::mutate(fuzzy_match_genus = (fuzzy_match_genus == 1)) %>% ## convert to Boolean
    dplyr::arrange(Orig.Genus, Orig.Species) %>%
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble

  return(res)
}
