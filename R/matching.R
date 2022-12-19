#' Matches species names to `Treemendous.Trees`
#' @description
#' This function takes species names and matches these against the internal database `Treemendous.Trees`.
#' The function is a wrapper around the following functions:
#' - [direct_match()]
#' - [genus_match()]
#' - [fuzzy_match_genus()]
#' - [direct_match_species_within_genus()]
#' - [suffix_match_species_within_genus()]
#' - [fuzzy_match_species_within_genus()]
#'
#' @param df `tibble` containing the species binomial split into the columns `Genus` and `Species`. May contain additional columns, which will be ignored.
#' @param backbone specifies which backbone is used: needs to be a subset of `c('BGCI', 'WCVP', 'WFO', 'GBIF')` or `NULL` if the whole database should be used.
#' @param target_df is used if the user wants to provide a custom target dataset. The parameter is intended only for compatibility with the function translate_trees and should not be directly used.
#'
#' @return
#' Returns a `tibble`, with the matched names in `Matched.Genus` and `Matched.Species`.
#' Process information is added as individual columns for every function.
#' The original input columns `Genus` and `Species` are renamed to `Orig.Species` and `Orig.Genus`.
#'
#' @details
#' First, [direct_match()] is called, which matches a name, when the exact same name (genus and specific epithet) is present in the database.
#' If there was no direct match, [genus_match()] checks, whether the genus exists in the database.
#' If the genus was not present, [fuzzy_match_genus()] is called, which tries to inexactly match genus names using the package _fuzzyjoin_ based on an _optimal string alignment distance_ of one, as implemented in _stringdist_.
#' In addition to insertions, deletions and substitutions, the metric also considers transpositions (e.g. Quercus &rarr; Quecrus) as operations of distance one.
#' If more than one genus matched, the alphabetically first match is picked, but the user is informed and encouraged to curate the ambiguous entries by hand.
#' The maximal genus edit distance is set to one by design, because typos in genus names can be considered much rarer compared to the specific epithet and because genus names are usually quite short.
#'
#' After the genus name has been matched, three functions are called within a certain genus.
#' First, [direct_match_species_within_genus()] checks if the specific epithet is present in the matched genus.
#' If not, [suffix_match_species_within_genus()] tries to capture gender-specific endings or other common suffixes.
#' More specifically, the following suffixes are substituted `c("a", "i", "is", "um", "us", "ae")`.
#' Next, the remaining unmatched species names are fuzzy matched with a maximal _optimal string alignment distance_ of two.
#'
#' The function [matching()] returns a `tibble` with the new columns `Matched.Genus` and `Matched.Species` containing the matched names, or `NA` if there was no match.
#' Further, a logical column is added for every function called to allow the user to inspect which functions were for every name during the process.
#' When a process column shows `NA`, then this function was not called for the given name, because it was already matched with a preceding function.
#'
#' @export
#'
#' @examples
#' iucn %>% matching()
matching <- function(df, backbone = NULL, target_df = NULL){
  ##########
  # Input checks
  ##########

  ### Check backbone argument
  assertthat::assert_that(
    is.null(backbone) | all(backbone %in% c('GBIF', 'WFO', 'WCVP', 'BGCI', 'CUSTOM')),
    msg = "Invalid backbone argument. Must be either NULL or one of (a combination of) c('GBIF', 'WFO', 'WCVP', 'BGCI')"
  )

  ### Check input df for correct formatting / data issues
  df <- check_df_format(df)
  check_df_consistency(df)

  ### Add two Columns Matched.Genus & Matched.Species and fill with NA's
  if(!all(c('Matched.Genus', 'Matched.Species') %in% colnames(df))){
    df <- df %>% tibble::add_column(Matched.Genus = as.character(NA), Matched.Species = as.character(NA))
  }


  ##########
  # Input
  ##########
  # Node 1: direct_match()
  # Node 2: genus_match()
  # Node 3: fuzzy_match_genus()
  # Node 4: species_within_genus_match()
  # Node 5a: suffix_match_species_within_genus()
  # Node 5b: fuzzy_match_species_within_genus()

  ##########
  # Output A: matched
  # Output B: unmatched
  # Output: merged A & B
  ##########

  # Node 1: Direct Match
  Node_1_processed <- df %>%
    direct_match(backbone, target_df)
  Node_1_TRUE <- Node_1_processed %>%
    dplyr::filter(direct_match == TRUE)
  Node_1_FALSE <- Node_1_processed %>%
    dplyr::filter(direct_match == FALSE)
  assertthat::assert_that(nrow(df) == (nrow(Node_1_TRUE) + nrow(Node_1_FALSE)))

  # Node 2: Genus Match
  Node_2_processed <- Node_1_FALSE %>%
    genus_match(backbone, target_df)
  Node_2_TRUE <- Node_2_processed %>%
    dplyr::filter(genus_match == TRUE)
  Node_2_FALSE <- Node_2_processed %>%
    dplyr::filter(genus_match == FALSE)
  assertthat::assert_that(nrow(Node_2_processed) == (nrow(Node_2_TRUE) + nrow(Node_2_FALSE)))

  # Node 3: Fuzzy Match Genus
  Node_3_processed <- Node_2_FALSE %>%
    fuzzy_match_genus(backbone, target_df)
  Node_3_TRUE <- Node_3_processed %>%
    dplyr::filter(fuzzy_match_genus == TRUE)
  Node_3_FALSE <- Node_3_processed %>%
    dplyr::filter(fuzzy_match_genus == FALSE)
  assertthat::assert_that(nrow(Node_3_processed) == (nrow(Node_3_TRUE) + nrow(Node_3_FALSE)))

  # Node 4: Direct (Exact) Match Species within Genus
  Node_4_input <- Node_3_TRUE %>%
    dplyr::bind_rows(Node_2_TRUE)
  Node_4_processed <- Node_4_input %>%
    direct_match_species_within_genus(backbone, target_df)
  Node_4_TRUE <- Node_4_processed %>%
    dplyr::filter(direct_match_species_within_genus == TRUE)
  Node_4_FALSE <- Node_4_processed %>%
    dplyr::filter(direct_match_species_within_genus == FALSE)
  assertthat::assert_that(nrow(Node_4_processed) == (nrow(Node_4_TRUE) + nrow(Node_4_FALSE)))

  # Node 5a: Suffix Match Species within Genus
  Node_5a_processed <- Node_4_FALSE %>%
    suffix_match_species_within_genus(backbone, target_df)
  Node_5a_TRUE <- Node_5a_processed %>%
    dplyr::filter(suffix_match_species_within_genus == TRUE)
  Node_5a_FALSE <- Node_5a_processed %>%
    dplyr::filter(suffix_match_species_within_genus == FALSE)
  assertthat::assert_that(nrow(Node_4_FALSE) == (nrow(Node_5a_TRUE) + nrow(Node_5a_FALSE)))

  # Node 5b: Fuzzy Match Species within Genus
  Node_5b_input <- Node_5a_FALSE
  Node_5b_processed <- Node_5b_input %>%
    fuzzy_match_species_within_genus(backbone, target_df)
  Node_5b_TRUE <- Node_5b_processed %>%
    dplyr::filter(fuzzy_match_species_within_genus == TRUE)
  Node_5b_FALSE <- Node_5b_processed %>%
    dplyr::filter(fuzzy_match_species_within_genus == FALSE)
  assertthat::assert_that(nrow(Node_5b_input) == (nrow(Node_5b_TRUE) + nrow(Node_5b_FALSE)))

  # Output
  # Output A: matched
  matched <- dplyr::bind_rows(Node_1_TRUE, Node_4_TRUE, Node_5a_TRUE, Node_5b_TRUE) %>%
    dplyr::arrange(Orig.Genus, Orig.Species)
  # Set which backbone it matched to (only when only a single backbone was selected):
  if((length(backbone) == 1) & ('Matched.Backbone' %in% colnames(df))){
    matched$Matched.Backbone <- backbone
  }
  # Output B: unmatched
  unmatched <- dplyr::bind_rows(Node_3_FALSE, Node_5b_FALSE) %>%
    dplyr::arrange(Orig.Genus, Orig.Species)

  # Concatenate Output A and Output B
  res <- dplyr::bind_rows(matched, unmatched, .id='matched') %>%
    dplyr::mutate(matched = (matched == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Orig.Genus', 'Orig.Species', 'Matched.Genus', 'Matched.Species', 'matched', 'direct_match', 'genus_match', 'fuzzy_match_genus', 'direct_match_species_within_genus', 'suffix_match_species_within_genus',  'fuzzy_match_species_within_genus')) ## Genus & Species column at the beginning of tibble

  assertthat::assert_that(nrow(df) == nrow(res))

  return(res)
}
