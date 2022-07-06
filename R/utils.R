helper.get_trees_by_genera <- function(){
  return(Trees.Full %>% base::split(.$Genus))
}

get_trees_by_genera <- memoise::memoise(helper.get_trees_by_genera)

get_db <- function(backbone = NULL){
  assertthat::assert_that(any(is.null(backbone), backbone %in% c('FIA', 'GBIF', 'WFO', 'WCVP', 'PM', 'BGCI')))
  if(is.null(backbone)){
    return(Trees.Full)
  }
  else {
    return(dplyr::filter(Trees.Full, get(backbone) == TRUE))
  }
}
