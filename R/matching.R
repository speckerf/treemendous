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
matching <- function(df){
  assertthat::assert_that(all(c('Genus', 'Species') %in% colnames(df)))

  ## Add two Columns New.Genus & New.Species and fill with NA's
  df <- df %>% tibble::add_column(New.Genus = as.character(NA), New.Species = as.character(NA))

  # Input

  # Node 1: direct_match()
  # Node 2: genus_match()
  # Node 3: fuzzy_match_genus()
  # Node 4: species_within_genus_match()
  # Node 5a: suffix_match_species_within_genus()
  # Node 5b: fuzzy_match_species_within_genus()

  # Output A: matched
  # Output B: matched

  # Node 1:
  Node_1_processed <- df %>%
    direct_match()
  Node_1_TRUE <- Node_1_processed %>%
    dplyr::filter(direct_match == TRUE)
  Node_1_FALSE <- Node_1_processed %>%
    dplyr::filter(direct_match == FALSE)

  # Node 2:
  Node_2_processed <- Node_1_FALSE %>%
    genus_match()
  Node_2_TRUE <- Node_2_processed %>%
    dplyr::filter(genus_match == TRUE)
  Node_2_FALSE <- Node_2_processed %>%
    dplyr::filter(genus_match == FALSE)

  # Node 3:
  Node_3_processed <- Node_2_FALSE %>%
    fuzzy_match_genus()
  Node_3_TRUE <- Node_3_processed %>%
    dplyr::filter(fuzzy_match_genus == TRUE)
  Node_3_FALSE <- Node_3_processed %>%
    dplyr::filter(fuzzy_match_genus == FALSE)

  # Node 4:
  Node_4_processed <- Node_3_TRUE %>%
    species_within_genus_match()
  Node_4_TRUE <- Node_4_processed %>%
    dplyr::filter(species_within_genus_match == TRUE)
  Node_4_FALSE <- Node_4_processed %>%
    dplyr::filter(species_within_genus_match == FALSE)

  # Node 5:
  Node_5a_input <- dplyr::bind_rows(Node_2_TRUE, Node_4_FALSE)
  Node_5a_processed <- Node_5a_input %>%
    suffix_match_species_within_genus()
  Node_5a_TRUE <- Node_5a_processed %>%
    dplyr::filter(suffix_match_species_within_genus == TRUE)
  Node_5a_FALSE <- Node_5a_processed %>%
    dplyr::filter(suffix_match_species_within_genus == FALSE)

  Node_5b_input <- Node_5a_FALSE
  Node_5b_processed <- Node_5b_input %>%
    fuzzy_match_species_within_genus()
  Node_5b_TRUE <- Node_5b_processed %>%
    dplyr::filter(fuzzy_match_species_within_genus == TRUE)
  Node_5b_FALSE <- Node_5b_processed %>%
    dplyr::filter(fuzzy_match_species_within_genus == FALSE)

  # Output
  # Output A: matched
  matched <- dplyr::bind_rows(Node_1_TRUE, Node_4_TRUE, Node_5a_TRUE, Node_5b_TRUE) %>%
    dplyr::arrange(Genus, Species)
  # Output B: unmatched
  unmatched <- dplyr::bind_rows(Node_3_FALSE, Node_5b_FALSE) %>%
    dplyr::arrange(Genus, Species)

  # Concatenate Output A and Output B
  res <- dplyr::bind_rows(matched, unmatched, .id='matched') %>%
    dplyr::mutate(matched = (matched == 1)) %>% ## convert to Boolean
    dplyr::relocate(c('Genus', 'Species', 'New.Genus', 'New.Species', 'matched', 'direct_match', 'genus_match', 'fuzzy_match_genus', 'species_within_genus_match', 'suffix_match_species_within_genus',  'fuzzy_match_species_within_genus')) ## Genus & Species column at the beginning of tibble

  return(res)
}
