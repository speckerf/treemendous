## returns Treemendous.Trees for specified backbones and filtered by a single genus
get_trees_of_genus <- function(genus, backbone = NULL){
  return(get_db(backbone) %>%
           dplyr::filter(Genus == genus) %>%
           dplyr::select(c('Genus', 'Species')))
}
## locally save output of get_trees_of_genus of called more than once for the same inputs.
memoised_get_trees_of_genus <- memoise::memoise(get_trees_of_genus)

get_db <- function(backbone = NULL){
  #####
  # Utility function which returns the full or a backbone specific subset of Treemendous.Trees
  # Param: backbone:
  #  - Default: `NULL`: Full Treemendous.Trees
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
    return(Treemendous.Trees)
  }
  else {
    if(length(backbone) == 1){
      return(dplyr::filter(Treemendous.Trees, get(backbone) == TRUE))
    }
    else{
      return(Treemendous.Trees %>%
               dplyr::filter(
                 dplyr::if_any(
                   .cols = dplyr::matches(stringr::str_c('^', backbone, '$')),
                   .fns = ~.x == TRUE)))
    }
  }
}


## analog to map_dfr, which additionally prints progress bars using the package progress
map_dfr_progress <- function(.x, .f, ..., .id = NULL) { ## credits to https://www.jamesatkins.net/posts/progress-bar-in-purrr-map-df/
  function_name <- stringr::str_remove(substitute(.f), '_helper')
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x),
                                   force = TRUE,
                                   format = paste(paste0(eval(...), collapse = ' '), ": ", function_name, "[:bar] :percent", collapse = ''))

  f <- function(...) {
    pb$tick()
    .f(...)
  }

  #future::plan(future::multicore, workers = 4)
  purrr::map_dfr(.x, f, ..., .id = .id)
}


## analog to map_dfr, which additionally prints progress bars using the package progress
map_progress <- function(.x, .f, ..., .id = NULL) { ## credits to https://www.jamesatkins.net/posts/progress-bar-in-purrr-map-df/
  #function_name <- stringr::str_remove(substitute(.f), '_helper')
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(total = length(.x),
                                   force = TRUE,
                                   format = paste(paste0(eval(...), collapse = ' '), ": enforce_matching: [:bar] :percent", collapse = ''))

  f <- function(...) {
    pb$tick()
    .f(...)
  }

  #future::plan(future::multicore, workers = 4)
  purrr::map(.x, f, ...)
}


### potential implementation of parallel purrr using furrr:
# parallel + progress https://furrr.futureverse.org/articles/progress.html
# parallel: https://byuistats.github.io/M335/parallel_furrr.html

# map_dfr_progress_parallel <- function(.x, .f, ..., .id = NULL) { ## credits to https://www.jamesatkins.net/posts/progress-bar-in-purrr-map-df/
#   function_name <- stringr::str_remove(substitute(.f), '_helper')
#   .f <- purrr::as_mapper(.f, ...)
#   pb <- progress::progress_bar$new(total = length(.x),
#                                    force = TRUE,
#                                    format = paste(paste0(eval(...), collapse = ' '), ": ", function_name, "[:bar] :percent", collapse = ''))
#
#   f <- function(...) {
#     pb$tick()
#     .f(...)
#   }
#
#   #future::plan(future::multicore, workers = 4)
#   purrr::map_dfr(.x, f, ..., .id = .id)
# }

#######
###  Get a testset with specified characteristics of length n (default 10) from specified backbones (default all)
#######
# Possible permutations mutations:
# 1: remove last character of specific epithet
# 2: remove last character of genus
# 3: remove last character of genus & specific epithet
# 4:

get_testset <- function(n = 10,
                        backbone = NULL,
                        mutation = 0){
  set.seed(333)
  df <- dplyr::sample_n(get_db(backbone), n) %>%
    dplyr::select(c('Genus', 'Species')) %>%
    dplyr::rename(Orig.Genus = Genus, Orig.Species = Species)
  if(mutation == 0){
    return(df)
  }
  else if(mutation == 1){
    dplyr::mutate(df, Orig.Species = stringr::str_replace(Orig.Species, '.{1}$', '')) %>%
      return()
  }
  else if(mutation == 2){
    dplyr::mutate(df, Orig.Genus = stringr::str_replace(Orig.Genus, '.{1}$', '')) %>%
      return()
  }
  else if(mutation == 3){
    dplyr::mutate(df,
                  Orig.Species = stringr::str_replace(Orig.Species, '.{1}$', ''),
                  Orig.Genus = stringr::str_replace(Orig.Genus, '.{1}$', '')) %>%
      return()
  }
}




create_undirected_synonym_graph <- function(){
  #message('Creating undirected graphs for all synonym-accepted relations in WFO, WCVP and GBIF using igraph()...')


  ## potentially optimize this code

  WCVP_edges <- get_db('WCVP') %>%
    dplyr::select(WCVP_ID, WCVP_accepted_ID) %>%
    dplyr::mutate(WCVP_accepted_ID = if_else(is.na(WCVP_accepted_ID), WCVP_ID, WCVP_accepted_ID)) %>%
    dplyr::relocate(WCVP_ID) %>%
    dplyr::inner_join(get_db() %>% dplyr::select(WCVP_ID, ID_merged), by = 'WCVP_ID') %>%
    dplyr::mutate(from = ID_merged) %>%
    dplyr::select(from, WCVP_accepted_ID) %>%
    dplyr::inner_join(get_db(), by = c('WCVP_accepted_ID' = 'WCVP_ID')) %>%
    dplyr::select(from, ID_merged) %>%
    dplyr::rename(to = ID_merged)

  WFO_edges <- get_db('WFO') %>%
    dplyr::select(WFO_ID, WFO_accepted_ID) %>%
    dplyr::mutate(WFO_accepted_ID = if_else(is.na(WFO_accepted_ID), WFO_ID, WFO_accepted_ID)) %>%
    dplyr::relocate(WFO_ID) %>%
    dplyr::inner_join(get_db() %>% dplyr::select(WFO_ID, ID_merged), by = 'WFO_ID') %>%
    dplyr::mutate(from = ID_merged) %>%
    dplyr::select(from, WFO_accepted_ID) %>%
    dplyr::inner_join(get_db(), by = c('WFO_accepted_ID' = 'WFO_ID')) %>%
    dplyr::select(from, ID_merged) %>%
    dplyr::rename(to = ID_merged)

  GBIF_edges <- get_db('GBIF') %>%
    dplyr::select(GBIF_ID, GBIF_accepted_ID) %>%
    dplyr::mutate(GBIF_accepted_ID = if_else(is.na(GBIF_accepted_ID), GBIF_ID, GBIF_accepted_ID)) %>%
    dplyr::relocate(GBIF_ID) %>%
    dplyr::inner_join(get_db() %>% dplyr::select(GBIF_ID, ID_merged), by = 'GBIF_ID') %>%
    dplyr::mutate(from = ID_merged) %>%
    dplyr::select(from, GBIF_accepted_ID) %>%
    dplyr::inner_join(get_db(), by = c('GBIF_accepted_ID' = 'GBIF_ID')) %>%
    dplyr::select(from, ID_merged) %>%
    dplyr::rename(to = ID_merged)

  edges <- dplyr::bind_rows(GBIF_edges, WCVP_edges, WFO_edges)
  edges_without_self <- edges %>% dplyr::filter(from != to) %>% dplyr::distinct()

  vertices_species <- tibble::tibble(ID_merged = unique(c(edges_without_self$from, edges_without_self$to))) %>%
    dplyr::left_join(get_db(), by = 'ID_merged') %>%
    dplyr::select(c('ID_merged', 'Genus', 'Species'))

  #g <- igraph::graph.data.frame(edges_without_self, directed = TRUE, vertices = vertices_species)
  g <- igraph::graph.data.frame(edges_without_self, directed = FALSE, vertices = vertices_species)
  g
}

