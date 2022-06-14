#' Match User Species Binomials to database Trees.Full
#'
#' @description
#' `match_name()` tries to match every input species to a species in the Trees.Full database. First, we try to match the genus name followed by the species epithet.
#'
#' @param df
#'
#' @return
#' returns a data.frame with the original species binomials as wells as the matched species binomials from the Trees.Full database
#' @export
#'
#' @examples
#' # match to a subset of Trees.Full
#' match_name(test1)
#' # match to a modified subset of Trees.Full
#' match_name(test2)
match_name = function(df){
  # probably this section can be dropped, because it is always available in the packages environment
  if(!exists("Trees.Full")){
    data("Trees.Full")
  }
  df$Treemendous.Genus_Match <- match_genus(df$Genus)

  df <- match_species_within_genus(df)

  df$Treemendous.Direct_Match <- df$Treemendous.Genus_Match & df$Treemendous.Species_within_Genus_Match

  matched_df <- df %>%
    dplyr::filter((Treemendous.Direct_Match == TRUE))

  unmatched_df <- df %>%
    dplyr::filter((Treemendous.Direct_Match == FALSE))


  matched_df$Treemendous.Fuzzy <- rep(as.logical(0), nrow(matched_df))
  unmatched_df$Treemendous.Fuzzy <- fuzzy_match_specific_epithet(unmatched_df)


  return(dplyr::bind_rows(list(matched_df, unmatched_df)))
}

match_genus <-  function(list_genera){
  return(list_genera %in% Trees.Reduced$Genus)
}

match_species_within_genus_helper <-  function(genus, input){
  database_genus <- Trees.by.Genus[[genus]]
  input_genus <- input %>%
    dplyr::filter(Genus == genus) %>%
    dplyr::mutate(Treemendous.Species_within_Genus_Match = Species %in% database_genus$Species)
  return(input_genus)
}

match_species_within_genus <-  function(df){
  list_unique_genera <- unique(df$Genus)
  tibble_per_genus <- lapply(list_unique_genera, match_species_within_genus_helper, df)
  res <- dplyr::bind_rows(tibble_per_genus)
  return(res)
}

fuzzy_match_specific_epithet_helper <- function(genus, input){
  database_genus <- Trees.by.Genus[[genus]]
  input_genus <- input %>%
    dplyr::filter(Genus == genus)
  list_fuzzy_matches <- lapply(input_genus$Species, agrep, x = database_genus$Species, value = T, max.distance = 1)
  one_match <- lapply(list_fuzzy_matches, function(x) if (length(x) == 1){TRUE} else {FALSE})
  flattened_one_match <- unlist(one_match)
  return(flattened_one_match)
}

fuzzy_match_specific_epithet <-  function(df){
  list_unique_genera <- unique(df$Genus)
  tibble_per_genus <- lapply(list_unique_genera, fuzzy_match_specific_epithet_helper, df)
  res <- unlist(tibble_per_genus)
  return(res)
}


