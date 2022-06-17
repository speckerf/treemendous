#' Match Specific Epithet
#'
#' @description
#' #' Tries to match the specific epithet within a Genus to the Trees database. Does not perform any fuzzy matching nor is capturing any other spelling errors.
#' Expects a `tibble` input with species binomials split into the two columns 'Genus' and 'Species'.
#'
#' @param df
#' tibble containing the species binomial split into two columns: 'Genus' & 'Species'
#'
#' @return
#' Returns a `tibble` with the same number of rows as the input `df` and with one additional Boolean column
#' _New.species_within_genus_match_ indicating whether the genus was matched (`r TRUE`) or not (`r FALSE`)
#' @export
#'
#' @examples
#' test2 %>%
#'  dplyr::mutate(New.Genus = Genus,
#'                New.Species = as.character(NA)) %>%
#'  species_within_genus_match()
species_within_genus_match <- function(df){
  assertthat::assert_that(all(c('Genus', 'Species') %in% colnames(df)))

  ## solve issue of empty input tibble, return
  if(nrow(df) == 0){
    return(tibble::add_column(df, species_within_genus_match = NA))
  }

  res <- df %>%
    dplyr::group_by(New.Genus) %>%
    dplyr::group_split() %>%
    purrr::map(species_within_genus_match_helper) %>%
    dplyr::bind_rows()

  return(res)
}

species_within_genus_match_helper <- function(df){
  # subset database
  genus <- df %>% dplyr::distinct(New.Genus) %>% unlist()
  database_subset <- Trees.by.Genus[[genus]] %>% dplyr::select(c('Species', 'Genus'))

  # match specific epithet within genus
  matched <- dplyr::semi_join(df, database_subset, by = c('Species')) %>%
    dplyr::mutate(New.Species = Species)
  unmatched <- dplyr::anti_join(df, database_subset, by = c('Species'))
  assertthat::are_equal(dim(df), dim(matched)[1] + dim(unmatched)[1])

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'species_within_genus_match') %>%
    dplyr::mutate(species_within_genus_match = (species_within_genus_match == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Genus', 'Species')) ## Genus & Species column at the beginning of tibble
  return(combined)
}
