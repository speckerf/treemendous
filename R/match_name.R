#' Direct match of species names
#'
#' @param df Data.Frame containing the two columns Species and Genus
#'
#' @return data.frame with rows that for Species names that were
#' @export
#'
#' @examples
direct_match = function(df){
  # probably this section can be dropped, because it is always available in the packages environment
  if(!exists("Trees.Full")){
    data("Trees.Full")
  }
  df$Treemendous.Genus_Match <- match_genus(df$Genus)

  df$Treemendous.Species_within_Genus_Match <- match_species_within_genus(df)
  return(df)
}

match_genus = function(list_genera){
  return(list_genera %in% Trees.Reduced$Genus)
}

match_species_within_genus_helper = function(genus, df){
  df_genus <- Trees.by.Genus[[genus]]
  df_new <- df %>%
    dplyr::filter(Genus == genus) %>%
    dplyr::mutate(Treemendous.Species_within_Genus_Match = Species %in% df_genus$Species)
  return(df_new)
}

match_species_within_genus = function(df){
  list_unique_genera <- unique(df$Genus)
  tibble_per_genus <- lapply(list_unique_genera, match_species_within_genus_helper, df)
  res <- dplyr::bind_rows(tibble_per_genus)
  return(res$Treemendous.Species_within_Genus_Match)
}



