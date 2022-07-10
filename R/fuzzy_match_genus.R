#' Fuzzy Match Genus Name
#' @description
#' Tries to fuzzy match the specific epithet within the same Genus to the Trees Database. Uses `fuzzyjoin::stringdist()` method to perform fuzzy matching.
#' @param df
#' tibble containing the species binomial split into two columns: 'Genus' & 'Species'
#'
#' @return
#' Returns a `tibble` with the same number of rows as the input `df` and with one additional Boolean column
#' _Matched.fuzzy_match_genus_ indicating whether the genus name was successfully fuzzy matched (`r TRUE`) or not (`r FALSE`)
#' @export
#'
#' @examples
#' test4 %>% fuzzy_match_genus()
fuzzy_match_genus <- function(df, backbone = NULL){
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species') %in% colnames(df)))

  ## solve issue of empty input tibble, and needed to ensure compatilbility with sequential_matching: because there the columns already exists for the second backbone
  if(nrow(df) == 0){
    if(!all(c('fuzzy_match_genus', 'fuzzy_genus_dist') %in% colnames(df))){
      return(tibble::add_column(df, fuzzy_match_genus = NA, fuzzy_genus_dist = NA))
    }
    else{
      return(df)
    }
  }

  ## solve issue in second iteration of sequential_matching: necessary to remove fuzzy_species_dist column: otherwise 2 columns are generated 'fuzzy_species_dist...1, fuzzy_species_dist...2'
  if('fuzzy_genus_dist' %in% colnames(df)){
    df <- df %>% dplyr::mutate(fuzzy_genus_dist = NULL)
  } ## TODO: can potentially be removed again????


  Tree.Genera <- get_db(backbone) %>% dplyr::distinct(Genus)

  # fuzzy match
  matched <- df %>%
    fuzzyjoin::stringdist_left_join(Tree.Genera,
                                    by = c('Orig.Genus' = 'Genus'),
                                    max_dist = 1,
                                    distance_col = 'fuzzy_genus_dist') %>%
    # save matched Genus name to Matched.Genus
    dplyr::mutate(Matched.Genus = Genus) %>%
    dplyr::select(-c('Genus')) %>%
    # in case of multiple matches: select the one with smallest distance
      # if these are still multiples: select random one in dplyr::group_modify
    dplyr::group_by(Orig.Genus, Orig.Species) %>%
    dplyr::filter(fuzzy_genus_dist == min(fuzzy_genus_dist)) %>%
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0, return(.x), return(dplyr::slice_sample(.x,n=1))) # alternative option: ~ifelse(nrow(.x) == 0, return(.x), return(head(.x,1L)))
    ) %>%
    dplyr::ungroup()

  unmatched <- df %>% fuzzyjoin::stringdist_anti_join(Tree.Genera,
                                                      by = c('Orig.Genus' = 'Genus'),
                                                      max_dist = 1)

  assertthat::assert_that(dim(df)[1] == (dim(matched)[1] + dim(unmatched)[1]))

  res <-  dplyr::bind_rows(matched, unmatched, .id = 'fuzzy_match_genus') %>%
    dplyr::mutate(fuzzy_match_genus = (fuzzy_match_genus == 1)) %>% ## convert to Boolean
    dplyr::arrange(Orig.Genus, Orig.Species) %>%
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble

  return(res)
}


