#' Runs whole matching pipeline
#' @description
#' This function is the core of the `treemendous` package and a wrapper around the following functions:
#' - `direct_match()`
#' - `genus_match()`
#' - `fuzzy_match_genus()`
#' - `species_within_genus_match()`
#' - `suffix_match_species_within_genus()`
#' - `fuzzy_match_species_within_genus()`
#'
#' @param df
#' `tibble` containing the species binomial split into two columns: 'Genus' & 'Species'. May contain additional columns, which will be ignored and returned by the function.
#'
#' @return
#' Returns a `tibble`
#'
#' @export
#'
#' @examples
#' matching(test1)
matching <- function(df, backbone = NULL){

  ### Check if Orig.Genus, Orig.Species or Genus, Species columns exist
  assertthat::assert_that(all(c('Genus', 'Species') %in% colnames(df)) | all(c('Orig.Genus', 'Orig.Species') %in% colnames(df)))
  if(!all(c('Orig.Genus', 'Orig.Species') %in% colnames(df))){
    df <- df %>% dplyr::rename(Orig.Genus = Genus, Orig.Species = Species)
  }

  ### Add two Columns Matched.Genus & Matched.Species and fill with NA's
  if(!all(c('Matched.Genus', 'Matched.Species') %in% colnames(df))){
    df <- df %>% tibble::add_column(Matched.Genus = as.character(NA), Matched.Species = as.character(NA))
  }

  ### Add column Matched.Backbone if it does not yet exist and if a single backbone is specified
  if((length(backbone) == 1) & !('Matched.Backbone' %in% colnames(df))){
    df <- df %>% tibble::add_column(Matched.Backbone = as.character(NA))
  }

  ### Check backbones Input is valid
  assertthat::assert_that(is.null(backbone) | all(backbone %in% c('FIA', 'GBIF', 'WFO', 'WCVP', 'PM', 'BGCI')))


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
    direct_match(backbone)
  Node_1_TRUE <- Node_1_processed %>%
    dplyr::filter(direct_match == TRUE)
  Node_1_FALSE <- Node_1_processed %>%
    dplyr::filter(direct_match == FALSE)
  assertthat::assert_that(nrow(df) == (nrow(Node_1_TRUE) + nrow(Node_1_FALSE)))

  # Node 2: Genus Match
  Node_2_processed <- Node_1_FALSE %>%
    genus_match(backbone)
  Node_2_TRUE <- Node_2_processed %>%
    dplyr::filter(genus_match == TRUE)
  Node_2_FALSE <- Node_2_processed %>%
    dplyr::filter(genus_match == FALSE)
  assertthat::assert_that(nrow(Node_2_processed) == (nrow(Node_2_TRUE) + nrow(Node_2_FALSE)))

  # Node 3: Fuzzy Match Genus
  Node_3_processed <- Node_2_FALSE %>%
    fuzzy_match_genus(backbone)
  Node_3_TRUE <- Node_3_processed %>%
    dplyr::filter(fuzzy_match_genus == TRUE)
  Node_3_FALSE <- Node_3_processed %>%
    dplyr::filter(fuzzy_match_genus == FALSE)
  assertthat::assert_that(nrow(Node_3_processed) == (nrow(Node_3_TRUE) + nrow(Node_3_FALSE)))

  # Node 4: Direct (Exact) Match Species within Genus
  Node_4_processed <- Node_3_TRUE %>%
    direct_match_species_within_genus(backbone)
  Node_4_TRUE <- Node_4_processed %>%
    dplyr::filter(direct_match_species_within_genus == TRUE)
  Node_4_FALSE <- Node_4_processed %>%
    dplyr::filter(direct_match_species_within_genus == FALSE)
  assertthat::assert_that(nrow(Node_4_processed) == (nrow(Node_4_TRUE) + nrow(Node_4_FALSE)))

  # Node 5a: Suffix Match Species within Genus
  Node_5a_input <- dplyr::bind_rows(Node_2_TRUE, Node_4_FALSE)
  Node_5a_processed <- Node_5a_input %>%
    suffix_match_species_within_genus(backbone)
  Node_5a_TRUE <- Node_5a_processed %>%
    dplyr::filter(suffix_match_species_within_genus == TRUE)
  Node_5a_FALSE <- Node_5a_processed %>%
    dplyr::filter(suffix_match_species_within_genus == FALSE)
  assertthat::assert_that(nrow(Node_5a_input) == (nrow(Node_5a_TRUE) + nrow(Node_5a_FALSE)))

  # Node 5b: Fuzzy Match Species within Genus
  Node_5b_input <- Node_5a_FALSE
  Node_5b_processed <- Node_5b_input %>%
    fuzzy_match_species_within_genus(backbone)
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
