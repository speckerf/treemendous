#' Match Genus name
#'
#' @description
#' #' Tries to match the genus name to the Trees database. Does not perform any fuzzy matching nor is capturing any other spelling errors.
#' Expects a `tibble` input with species binomials split into the two columns 'Genus' and 'Species'.
#'
#' @param df tibble containing the species binomial split into two columns: 'Genus' & 'Species'
#' @param backbone specifies which backbone is used: needs to be one of c('BGCI', 'WCVP', 'WFO', 'GBIF', 'FIA', 'PM') or NULL if the whole database should be used
#'
#' @return
#' Returns a `tibble` with the same number of rows as the input `df` and with one additional Boolean column
#' _Matched.genus_match_ indicating whether the genus was matched (`r TRUE`) or not (`r FALSE`)
#' @export
#'
#' @examples
#' test1 %>% genus_match()
genus_match <- function(df, backbone = NULL){
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species') %in% colnames(df)))

  if(nrow(df) == 0){
    return(tibble::add_column(df, genus_match = NA))
  }

  matched <- df %>%
    dplyr::semi_join(get_db(backbone), by = c('Orig.Genus' = 'Genus')) %>%
    dplyr::mutate(Matched.Genus = Orig.Genus)
  unmatched <- df %>%
    dplyr::anti_join(get_db(backbone), by = c('Orig.Genus' = 'Genus'))
  assertthat::assert_that(dim(df)[1] == (dim(matched)[1] + dim(unmatched)[1]))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'genus_match') %>%
    dplyr::mutate(genus_match = (genus_match == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble

  return(combined)
}
