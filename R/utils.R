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
  #####
  # Utility function which returns the full or a backbone specific subset of Trees.Full
  # Backbone:
  #  - NULL: Full Trees.Full
  #  - single string %in% c('FIA', 'GBIF', 'WFO', 'WCVP', 'PM', 'BGCI'): returns specific backbone
  #  - vector of strings s %in% c('FIA', 'GBIF', 'WFO', 'WCVP', 'PM', 'BGCI'): returns every species present in at least one of the specified backbone
  #####
  assertthat::assert_that(
    any(
      is.null(backbone),
      all(backbone %in% c('FIA', 'GBIF', 'WFO', 'WCVP', 'PM', 'BGCI'))
    )
  )

  if(is.null(backbone)){
    return(Trees.Full)
  }
  else {
    if(length(backbone) == 1){
      return(dplyr::filter(Trees.Full, get(backbone) == TRUE))
    }
    else{
      return(Trees.Full %>%
               dplyr::filter(
                 dplyr::if_any(
                   .cols = dplyr::matches(stringr::str_c('^', backbone, '$')),
                   .fns = ~.x == TRUE)))
    }
  }
}


