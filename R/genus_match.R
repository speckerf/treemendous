#' Match Genus name
#'
#' @description
#' #' Tries to match the genus name to the Trees database. Does not perform any fuzzy matching nor is capturing any other spelling errors.
#' Expects a `tibble` input with species binomials split into the two columns 'Genus' and 'Species'.
#'
#' @param df
#' tibble containing the species binomial split into two columns: 'Genus' & 'Species'
#'
#' @return
#' Returns a `tibble` with the same number of rows as the input `df` and with one additional Boolean column
#' _New.genus_match_ indicating whether the genus was matched (`r TRUE`) or not (`r FALSE`)
#' @export
#'
#' @examples
#' genus_match(test1)
genus_match <- function(df){
  assertthat::assert_that(all(c('Genus', 'Species') %in% colnames(df)))

  if(nrow(df) == 0){
    return(tibble::add_column(df, genus_match = NA))
  }

  matched <- dplyr::semi_join(df, Trees.Reduced, by = c('Genus')) %>%
    dplyr::mutate(New.Genus = Genus)
  unmatched <- dplyr::anti_join(df, Trees.Reduced, by = c('Genus'))
  assertthat::are_equal(dim(df), dim(matched)[1] + dim(unmatched)[1])

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'genus_match') %>%
    dplyr::mutate(genus_match = (genus_match == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Genus', 'Species')) ## Genus & Species column at the beginning of tibble

  return(combined)
}
