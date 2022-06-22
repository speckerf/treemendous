#' Direct Match Species & Genus Binomial
#' @description
#' Tries to directly match Genus + Species Binomial to Trees database. Does not perform any fuzzy matching nor is capturing any other spelling errors.
#' Expects a `tibble` input with species binomials split into the two columns 'Genus' and 'Species'.
#' @param df
#' tibble containing the species binomial split into two columns: 'Genus' & 'Species'
#'
#' @return
#' Returns a `tibble` with the same number of rows as the input `df` and with one additional Boolean column
#' _New.Direct_Match_ indicating whether the binomial was directly matched (`r TRUE`) or not (`r FALSE`)
#' @export
#'
#' @examples
#' test1 %>% direct_match()
direct_match <- function(df){
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species') %in% colnames(df)))

  if(nrow(df) == 0){
    return(tibble::add_column(df, direct_match = NA))
  }

  matched <- dplyr::semi_join(df, Trees.Full, by = c('Orig.Genus' = 'Genus', 'Orig.Species' = 'Species')) %>%
    dplyr::mutate(Matched.Genus = Orig.Genus,
                  Matched.Species = Orig.Species)
  unmatched <- dplyr::anti_join(df, Trees.Full, c('Orig.Genus' = 'Genus', 'Orig.Species' = 'Species'))
  assertthat::assert_that(dim(df)[1] == (dim(matched)[1] + dim(unmatched)[1]))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'direct_match') %>%
    dplyr::mutate(direct_match = (direct_match == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble

  return(combined)
}

