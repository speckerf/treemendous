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
#' test3 %>%
#'  dplyr::mutate(New.Genus = Genus,
#'                New.Species = as.character(NA)) %>%
#'  fuzzy_match_species_within_genus()
fuzzy_match_species_within_genus <- function(df){
  assertthat::assert_that(all(c('Genus', 'Species') %in% colnames(df)))

  ## solve issue of empty input tibble, return
  if(nrow(df) == 0){
    return(tibble::add_column(df, fuzzy_match_species_within_genus = NA, fuzzy_species_dist = NA))
  }

  res <- df %>%
    dplyr::group_by(New.Genus) %>%
    dplyr::group_split() %>%
    purrr::map(fuzzy_match_species_within_genus_helper) %>%
    dplyr::bind_rows()

  return(res)
}


fuzzy_match_species_within_genus_helper <- function(df){
  # subset database
  genus <- df %>% dplyr::distinct(New.Genus) %>% unlist()
  database_subset <- Trees.by.Genus[[genus]] %>% dplyr::select(c('Genus', 'Species'))

  ## introduce speed up if database_subset is too large (more than 1'000 for instance)
  if (dim(database_subset)[1] > 2000){
    print(paste0('number of comparisons for fuzzyjoin algorithm in genus ', genus, ' : ', dim(df)[1], ' x ',  dim(database_subset)[1], ' = ', dim(df)[1] * dim(database_subset)[1]))
  }

  # fuzzy match
  matched <- fuzzyjoin::stringdist_left_join(df, database_subset, by = c('Species'), distance_col = 'fuzzy_species_dist') %>%
    dplyr::mutate(New.Species = Species.y,
                  Species = Species.x,
                  Genus = Genus.x) %>%
    dplyr::select(-c('Species.y', 'Species.x', 'Genus.y', 'Genus.x')) %>%
    # in case of multiple matches: select the one with smallest distance (TODO: what exactly happens if two have the same minimal distance has to be investigated...)
    dplyr::group_by(Genus, Species) %>%
    dplyr::filter(fuzzy_species_dist == min(fuzzy_species_dist)) %>%
    dplyr::ungroup()

  unmatched <- fuzzyjoin::stringdist_anti_join(df, database_subset, by = c('Species'))
  assertthat::are_equal(dim(df), dim(matched)[1] + dim(unmatched)[1])

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'fuzzy_match_species_within_genus') %>%
    dplyr::mutate(fuzzy_match_species_within_genus = (fuzzy_match_species_within_genus == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Genus', 'Species')) ## Genus & Species column at the beginning of tibble
  return(combined)
}




