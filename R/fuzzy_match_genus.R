#' Fuzzy Match Genus Name
#' @description
#' Tries to fuzzy match the genus name to `Treemendous.Trees`. Uses `fuzzyjoin::stringdist()` to perform fuzzy matching.
#'
#' @param df `tibble` containing the species binomial split into the columns `Orig.Genus` and `Orig.Species`.
#' @param backbone specifies which backbone is used: needs to be a subset of `c('BGCI', 'WCVP', 'WFO', 'GBIF')` or `NULL` if the whole database should be used.
#' @param target_df is used if the user wants to provide a custom target dataset. The parameter is intended only for compatibility with the function translate_trees and should not be directly used.
#'
#' @return
#' Returns a `tibble` with the additional logical column `fuzzy_match_genus`, indicating whether the genus was successfully matched (`r TRUE`) or not (`r FALSE`).
#' Further, the additional column `fuzzy_genus_dist` returns the distance for every match.
#' @export
#'
#' @examples
#' iucn %>%
#'     dplyr::mutate(Orig.Genus = stringr::str_replace(Orig.Genus, '.{1}$', '')) %>%
#'     fuzzy_match_genus()
fuzzy_match_genus <- function(df, backbone = NULL, target_df = NULL){
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


  Tree.Genera <- get_db(backbone, target_df) %>% dplyr::distinct(Genus)

  # fuzzy match
  matched_temp <- df %>%
    fuzzyjoin::stringdist_left_join(Tree.Genera,
                                    by = c('Orig.Genus' = 'Genus'),
                                    max_dist = 1,
                                    distance_col = 'fuzzy_genus_dist') %>%
    # save matched Genus name to Matched.Genus
    dplyr::mutate(Matched.Genus = Genus) %>%
    dplyr::select(-c('Genus')) %>%
    dplyr::group_by(Orig.Genus, Orig.Species) %>%
    dplyr::filter(fuzzy_genus_dist == min(fuzzy_genus_dist))

## If there are multiple matches for the same genus: raise warning and advise for manual checking
  if(matched_temp %>% dplyr::filter(dplyr::n() > 1) %>% nrow() > 0){
    message("Multiple fuzzy matches for genera with similar string distance:
            Please consider curating the ambiguous entries by hand and re-run the pipeline.
            The ambiguous matched genera were saved to 'treemendous_ambiguous_genera.csv' in the current working directory.
             The algorithm will choose the first genus to continue.")
            #Do you want save a list of the ambiguous matched genera current working directory in 'treemendous_ambiguous_genera.csv'?")
    ## Save ambiguous genera for manual curation:
    matched_temp %>%
      dplyr::filter(dplyr::n() > 1) %>%
      dplyr::select(Orig.Genus, Orig.Species, Matched.Genus) %>%
      readr::write_csv(file = 'treemendous_ambiguous_genera.csv') ##
    ## Alternative Idea: prompt the user to insert the correct name. Caution here however because this might cause trouble with unit testing
  }

 ## continue selecting first genus if more than one match
  matched <- matched_temp %>%
    dplyr::group_modify(
      ~ifelse(nrow(.x) == 0, return(.x), return(dplyr::slice_head(.x,n = 1))) ## In cases of multiple matches: we choose first match. Alternatively could use something more sophisticated here: like for instance choosing the one with more support (present in more databases)
    ) %>%
    dplyr::ungroup()

  unmatched <- df %>% fuzzyjoin::stringdist_anti_join(Tree.Genera,
                                                      by = c('Orig.Genus' = 'Genus'),
                                                      max_dist = 1)

  assertthat::assert_that(nrow(df) == (nrow(matched) + nrow(unmatched)))

  res <-  dplyr::bind_rows(matched, unmatched, .id = 'fuzzy_match_genus') %>%
    dplyr::mutate(fuzzy_match_genus = (fuzzy_match_genus == 1)) %>% ## convert to Boolean
    dplyr::arrange(Orig.Genus, Orig.Species) %>%
    dplyr::relocate(c('Orig.Genus', 'Orig.Species')) ## Genus & Species column at the beginning of tibble

  return(res)
}


