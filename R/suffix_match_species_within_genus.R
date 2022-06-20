#' Suffix Match Specific Epithet within Genus
#' @description
#' Tries to match specific epithet within the same Genus to the Trees Database by exchanging common suffixes. The list of common suffixes is: `c("a", "i", "is", "um", "us", "ae", "oides", "escens")`
#' @param df
#' tibble containing the species binomial split into two columns: 'Genus' & 'Species'
#'
#' @return
#' Returns a `tibble` with the same number of rows as the input `df` and with one additional Boolean column
#' _New.suffix_match_species_within_genus_ indicating whether the specific epithet was successfully suffix matched (`r TRUE`) or not (`r FALSE`)
#' @export
#'
#' @examples
#' test3 %>%
#'  dplyr::mutate(New.Genus = Genus,
#'                New.Species = as.character(NA)) %>%
#'  suffix_match_species_within_genus()
suffix_match_species_within_genus <- function(df){
  assertthat::assert_that(all(c('Genus', 'Species') %in% colnames(df)))

  ## solve issue of empty input tibble, return
  if(nrow(df) == 0){
    return(tibble::add_column(df, suffix_match_species_within_genus = NA))
  }

  res <- df %>%
    dplyr::group_by(New.Genus) %>%
    dplyr::group_split() %>%
    purrr::map(suffix_match_species_within_genus_helper) %>%
    dplyr::bind_rows()

  return(res)
}


suffix_match_species_within_genus_helper <- function(df){
  # subset database
  genus <- df %>% dplyr::distinct(New.Genus) %>% unlist()
  database_subset <- Trees.by.Genus[[genus]] %>% dplyr::select(c('Genus', 'Species'))

  # ending match
  ## create word root column in both the database subset and user input
  common_suffixes <- c("a", "i", "is", "um", "us", "ae", "oides", "escens")
  catch_suffixes <- paste0("(.*?)(", paste0(common_suffixes, collapse = "|"), ")$")
  df <- df %>%
    dplyr::mutate(Root = stringi::stri_match_first_regex(Species, catch_suffixes)[,2])
  database_subset <- database_subset %>%
    dplyr::mutate(Root = stringi::stri_match_first_regex(Species, catch_suffixes)[,2])

  unmatched_suffix <- df %>% dplyr::filter(is.na(Root)) %>% dplyr::select(-c('Root'))
  ## matching based on root column
  matched <- dplyr::inner_join(df, database_subset, by = 'Root', na_matches = 'never') %>%
    dplyr::mutate(New.Species = Species.y,
                  Species = Species.x,
                  Genus = Genus.x) %>%
    dplyr::select(-c('Species.y', 'Species.x', 'Genus.y', 'Genus.x', 'Root'))
    # what to do in case of multiple matches????

  unmatched <- dplyr::anti_join(df, database_subset, by = c('Root')) %>%
    dplyr::select(-c('Root'))

  assertthat::see_if(assertthat::are_equal(dim(df)[1], dim(matched)[1] + dim(unmatched)[1] + dim(unmatched_suffix)[1]))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, dplyr::bind_rows(unmatched_suffix, unmatched), .id = 'suffix_match_species_within_genus') %>%
    dplyr::mutate(suffix_match_species_within_genus = (suffix_match_species_within_genus == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Genus', 'Species')) ## Genus & Species column at the beginning of tibble
  return(combined)
}




