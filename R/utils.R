check_df_format <- function(df){
  # Errors
  ### Checks if tibble contains missing values
  assertthat::assert_that(
    assertthat::noNA(dplyr::select(df, c('Orig.Genus', 'Orig.Species'))),
    msg = sprintf(
      "Input tibble contains %s missing values. \n Please remove the conflicting rows with e.g. tidyr::drop_na(df). \n Alternatively, fill the missing values manually or using tidyr::replace_na()",
      sum(is.na(df))
      )
  )

  ### Check if Genus, Species binomials are unique
  assertthat::assert_that(
    nrow(df) == (nrow(dplyr::distinct(df, Orig.Genus, Orig.Species))),
    msg = "Species names of input are not unique. Prior to calling matching(), please remove duplicates with e.g. dplyr::distinct(df, Genus, Species)."
  )

  ### Check that Genus starts with uppercase character
  assertthat::assert_that(
    all(stringr::str_detect(df$Orig.Genus, '^[:upper:]')),
    msg = "Not all genera start with an uppercase letter: use e.g dplyr::mutate(Genus = stringr::str_to_title(Genus)) in your preprocessing"
  )

  ### Check that Genus only contains one uppercase character
  assertthat::assert_that(
    all(stringr::str_count(df$Orig.Genus, '[:upper:]') == 1),
    msg = "Some genera contain more than one uppercase letter: use e.g dplyr::mutate(Genus = stringr::str_to_title(Genus)) in your preprocessing"
  )

  ### Check that Species does not contain any uppercase character
  assertthat::assert_that(
    !any(stringr::str_detect(df$Orig.Species, '[:upper:]')),
    msg = "Some specific epithets contain uppercase letters: use for instance dplyr::mutate(Genus = stringr::str_to_title(Genus)) in your preprocessing"
  )

  ### Check that there are no hybrid characters in Genus names
  assertthat::assert_that(
    !any(stringr::str_detect(df$Orig.Genus, '\u00D7')),
    msg = "The special character denoting hybrid species \u00D7 (Unicode: \\u00D7) was found in some Genus names. To avoid unnecessary fuzzy matching, consider removing them using e.g. dplyr::mutate(Genus = stringr::str_replace_all(Genus, '\\u00D7'))"
  )

  # Warnings
  options(warn = 1) ## tell R to immediately display warning and not cache them and output them at the end
  ### Check for trailing spaces in Genus names
  if(any(stringr::str_detect(df$Orig.Genus, '[:space:]$'))){
    num_spaces <- sum(stringr::str_detect(df$Orig.Genus, '[:space:]$'))
    warning(sprintf("%s trailing space character(s) were detected in the genera names. To avoid unnecessary fuzzy matching, consider removing them using dplyr::mutate(Genus = stringr::str_trim(Genus, side = 'right'))", num_spaces))
  }

  ### Check for trailing spaces in Species names
  if(any(stringr::str_detect(df$Orig.Species, '[:space:]$'))){
    num_spaces <- sum(stringr::str_detect(df$Orig.Species, '[:space:]$'))
    warning(sprintf("%s trailing space character(s) were detected in the species names. To avoid unnecessary fuzzy matching, consider removing them using dplyr::mutate(Species = stringr::str_trim(Species, side = 'right'))", num_spaces))
  }

  ### Check for hybrid characters in Species names / UNICODE for hybrid 'x': 00D7
  if(any(stringr::str_detect(df$Orig.Species, '\u00D7'))){
    num_hybrid <- sum(stringr::str_detect(df$Orig.Species, '\u00D7'))
    warning(sprintf("The special character denoting hybrid species \u00D7 (Unicode: \\u00D7) was found in %s species names. To avoid unnecessary fuzzy matching, consider removing them using e.g. dplyr::mutate(Species = stringr::str_replace_all(Species, '\\u00D7'))", num_hybrid))
  }

}


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
  set.seed(111)
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
    dplyr::select(c('WCVP_ID', 'WCVP_accepted_ID')) %>%
    dplyr::mutate('WCVP_accepted_ID' = dplyr::if_else(is.na(WCVP_accepted_ID), WCVP_ID, WCVP_accepted_ID)) %>%
    dplyr::relocate('WCVP_ID') %>%
    dplyr::inner_join(get_db() %>% dplyr::select(c('WCVP_ID', 'ID_merged')), by = 'WCVP_ID') %>%
    dplyr::mutate('from' = ID_merged) %>%
    dplyr::select(c('from', 'WCVP_accepted_ID')) %>%
    dplyr::inner_join(get_db(), by = c('WCVP_accepted_ID' = 'WCVP_ID')) %>%
    dplyr::select(c('from', 'ID_merged')) %>%
    dplyr::rename('to' = ID_merged)

  WFO_edges <- get_db('WFO') %>%
    dplyr::select(c('WFO_ID', 'WFO_accepted_ID')) %>%
    dplyr::mutate('WFO_accepted_ID' = dplyr::if_else(is.na(WFO_accepted_ID), WFO_ID, WFO_accepted_ID)) %>%
    dplyr::relocate('WFO_ID') %>%
    dplyr::inner_join(get_db() %>% dplyr::select(c('WFO_ID', 'ID_merged')), by = 'WFO_ID') %>%
    dplyr::mutate('from' = ID_merged) %>%
    dplyr::select(c('from', 'WFO_accepted_ID')) %>%
    dplyr::inner_join(get_db(), by = c('WFO_accepted_ID' = 'WFO_ID')) %>%
    dplyr::select(c('from', 'ID_merged')) %>%
    dplyr::rename('to' = ID_merged)

  GBIF_edges <- get_db('GBIF') %>%
    dplyr::select(c('GBIF_ID', 'GBIF_accepted_ID')) %>%
    dplyr::mutate('GBIF_accepted_ID' = dplyr::if_else(is.na(GBIF_accepted_ID), GBIF_ID, GBIF_accepted_ID)) %>%
    dplyr::relocate('GBIF_ID') %>%
    dplyr::inner_join(get_db() %>% dplyr::select(c('GBIF_ID', 'ID_merged')), by = 'GBIF_ID') %>%
    dplyr::mutate('from' = ID_merged) %>%
    dplyr::select(c('from', 'GBIF_accepted_ID')) %>%
    dplyr::inner_join(get_db(), by = c('GBIF_accepted_ID' = 'GBIF_ID')) %>%
    dplyr::select(c('from', 'ID_merged')) %>%
    dplyr::rename('to' = ID_merged)

  edges <- dplyr::bind_rows(GBIF_edges, WCVP_edges, WFO_edges)
  edges_without_self <- edges %>% dplyr::filter(from != to) %>% dplyr::distinct()

  vertices_species <- tibble::tibble(ID_merged = unique(c(edges_without_self$from, edges_without_self$to))) %>%
    dplyr::left_join(get_db(), by = 'ID_merged') %>%
    dplyr::select(c('ID_merged', 'Genus', 'Species'))

  #g <- igraph::graph.data.frame(edges_without_self, directed = TRUE, vertices = vertices_species)
  g <- igraph::graph.data.frame(edges_without_self, directed = FALSE, vertices = vertices_species)
  g
}

