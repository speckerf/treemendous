#' Match Specific Epithet Within Genus
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
#' _Matched.species_within_genus_match_ indicating whether the genus was matched (`r TRUE`) or not (`r FALSE`)
#' @export
#'
#' @examples
#' test2 %>% dplyr::mutate(Matched.Genus = Orig.Genus) %>% direct_match_within_genus()
direct_match_within_genus <- function(df, backbone = NULL){

  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species', 'Matched.Genus') %in% colnames(df)))

  ## solve issue of empty input tibble, and needed to ensure compatilbility with sequential_matching: because there the columns already exists for the second backbone
  if(nrow(df) == 0){
    if(!all(c('direct_match_within_genus') %in% colnames(df))){
      return(tibble::add_column(df, direct_match_within_genus = NA))
    }
    else{
      return(df)
    }
  }

  res <- df %>%
    dplyr::group_by(Matched.Genus) %>%
    dplyr::group_split() %>%
    map_dfr_progress(direct_match_within_genus_helper, backbone)

  return(res)
}

direct_match_within_genus_helper <- function(df, backbone){
  # subset database
  genus <- df %>% dplyr::distinct(Matched.Genus) %>% unlist()
  #database_subset <- get_trees_by_genera(backbone)[[genus]] %>% dplyr::select(c('Genus', 'Species'))
  #database_subset <- get_trees_of_genus(genus, backbone) %>%
  #  dplyr::select(c('Genus', 'Species')) # optionally could use memoise to potentially speed up this function: would need to test what is faster
  database_subset <- memoised_get_trees_of_genus(genus, backbone)

  # match specific epithet within genus
  matched <- df %>%
    dplyr::semi_join(database_subset, by = c('Orig.Species' = 'Species')) %>%
    dplyr::mutate(Matched.Species = Orig.Species)
  unmatched <- df %>%
    dplyr::anti_join(database_subset, by = c('Orig.Species' = 'Species'))
  assertthat::assert_that(dim(df)[1] == (dim(matched)[1] + dim(unmatched)[1]))

  # combine matched and unmatched and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <-  dplyr::bind_rows(matched, unmatched, .id = 'direct_match_within_genus') %>%
    dplyr::mutate(direct_match_within_genus = (direct_match_within_genus == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble
  return(combined)
}
