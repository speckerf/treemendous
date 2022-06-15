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
#' direct_match(test1)
#'
fuzzy_match_species_within_genus <- function(df){
  assertthat::assert_that(all(c('Genus', 'Species') %in% colnames(df)))


  list_unique_genera <- unique(df$Genus)
  tibble_per_genus <- lapply(list_unique_genera, helper_function, df)

  res <- dplyr::bind_rows(tibble_per_genus)

  return(res)
}


helper_function <- function(genus, df){
  # subset data
  df_subset <- df %>%
    dplyr::filter(Genus == genus)
  database_subset <- Trees.by.Genus[[genus]]

  # fuzzy match
  matched <- fuzzyjoin::stringdist_semi_join(df_subset, database_subset, by = c('Species'))
  unmatched <- fuzzyjoin::stringdist_anti_join(df_subset, database_subset, by = c('Species'))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'fuzzy_match_species_within_genus') %>%
    dplyr::mutate(fuzzy_match_species_within_genus = (fuzzy_match_species_within_genus == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Genus', 'Species')) ## Genus & Species column at the beginning of tibble
}

