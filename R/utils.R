helper.get_trees_by_genera <- function(backbone = NULL){
  return(get_db(backbone) %>% base::split(.$Genus))
}

get_trees_by_genera <- memoise::memoise(helper.get_trees_by_genera)

get_trees_of_genus <- function(genus, backbone = NULL){
  return(get_db(backbone) %>%
           dplyr::filter(Genus == genus) %>%
           dplyr::select(c('Genus', 'Species')))
}

memoised_get_trees_of_genus <- memoise::memoise(get_trees_of_genus)

get_db <- function(backbone = NULL){
  assertthat::assert_that(any(is.null(backbone), backbone %in% c('FIA', 'GBIF', 'WFO', 'WCVP', 'PM', 'BGCI')))
  if(is.null(backbone)){
    return(Trees.Full)
  }
  else {
    return(dplyr::filter(Trees.Full, get(backbone) == TRUE))
  }
}
