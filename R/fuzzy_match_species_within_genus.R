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
#' test3 %>% dplyr::mutate(Matched.Genus = Orig.Genus) %>% fuzzy_match_species_within_genus()
fuzzy_match_species_within_genus <- function(df){
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species', 'Matched.Genus') %in% colnames(df)))

  ## solve issue of empty input tibble, return
  if(nrow(df) == 0){
    return(tibble::add_column(df, fuzzy_match_species_within_genus = NA, fuzzy_species_dist = NA))
  }

  res <- df %>%
    dplyr::group_by(Matched.Genus) %>%
    dplyr::group_split() %>%
    purrr::map(fuzzy_match_species_within_genus_helper) %>%
    dplyr::bind_rows()

  return(res)
}


fuzzy_match_species_within_genus_helper <- function(df){
  # subset database
  genus <- df %>% dplyr::distinct(Matched.Genus) %>% unlist()
  database_subset <- get_trees_by_genera()[[genus]] %>% dplyr::select(c('Genus', 'Species'))

  ## introduce speed up if database_subset is too large (more than 1'000 for instance)
  # TODO

  # fuzzy match
  matched <- fuzzyjoin::stringdist_left_join(df, database_subset, by = c('Orig.Species' = 'Species'), distance_col = 'fuzzy_species_dist') %>%
    dplyr::mutate(Matched.Species = Species) %>%
    dplyr::select(-c('Species', 'Genus')) %>%
    # in case of multiple matches: select the one with smallest distance (TODO: what exactly happens if two have the same minimal distance has to be investigated...)
    dplyr::group_by(Orig.Genus, Orig.Species) %>%
    dplyr::filter(fuzzy_species_dist == min(fuzzy_species_dist)) %>%
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0, return(.x), return(dplyr::slice_sample(.x,n=1))) # alternative option: ~ifelse(nrow(.x) == 0, return(.x), return(head(.x,1L)))
    ) %>%
    dplyr::ungroup()

  unmatched <- fuzzyjoin::stringdist_anti_join(df, database_subset, by = c('Orig.Species' = 'Species'))
  assertthat::assert_that(dim(df)[1] == (dim(matched)[1] + dim(unmatched)[1]))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'fuzzy_match_species_within_genus') %>%
    dplyr::mutate(fuzzy_match_species_within_genus = (fuzzy_match_species_within_genus == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble
  return(combined)
}




