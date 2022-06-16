#' Fuzzy Match Specific Epithet within Genus
#' @description
#' Tries to fuzzy match the specific epithet within the same Genus to the Trees Database. Uses `fuzzyjoin::stringdist()` method to perform fuzzy matching.
#' @param df
#' tibble containing the species binomial split into two columns: 'Genus' & 'Species'
#'
#' @return
#' Returns a `tibble` with the same number of rows as the input `df` and with one additional Boolean column
#' _New.fuzzy_match_species_within_genus_ indicating whether the specific epithet was successfully fuzzy matched (`r TRUE`) or not (`r FALSE`)
#' @export
#'
#' @examples
#' fuzzy_match_species_within_genus(test3)
fuzzy_match_species_within_genus <- function(df){
  assertthat::assert_that(all(c('Genus', 'Species') %in% colnames(df)))

  ## solve issue of empty input tibble, return
  if(nrow(df) == 0){
    return(tibble::add_column(df, fuzzy_match_species_within_genus = NA))
  }

  res <- df %>%
    split(.$Genus) %>%
    purrr::map2(names(.), fuzzy_match_species_within_genus_helper) %>%
    dplyr::bind_rows()

  return(res)
}


fuzzy_match_species_within_genus_helper <- function(df, genus){
  # subset database
  database_subset <- Trees.by.Genus[[genus]]

  ## introduce speed up if database_subset is too large (more than 1'000 for instance)
  if (dim(database_subset)[1] > 2000){
    print(paste0('number of comparisons for fuzzyjoin algorithm in genus ', genus, ' : ', dim(df)[1], ' x ',  dim(database_subset)[1], ' = ', dim(df)[1] * dim(database_subset)[1]))
  }

  # fuzzy match
  matched <- fuzzyjoin::stringdist_semi_join(df, database_subset, by = c('Species'))
  unmatched <- fuzzyjoin::stringdist_anti_join(df, database_subset, by = c('Species'))
  assertthat::are_equal(dim(df), dim(matched)[1] + dim(unmatched)[1])

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'fuzzy_match_species_within_genus') %>%
    dplyr::mutate(fuzzy_match_species_within_genus = (fuzzy_match_species_within_genus == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Genus', 'Species')) ## Genus & Species column at the beginning of tibble
  return(combined)
}




