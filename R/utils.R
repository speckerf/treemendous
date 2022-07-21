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
    dplyr::mutate(df, Orig.Species = gsub('.{1}$', '', Orig.Species)) %>%
      return()
  }
  else if(mutation == 2){
    dplyr::mutate(df, Orig.Genus = gsub('.{1}$', '', Orig.Genus)) %>%
      return()
  }
  else if(mutation == 3){
    dplyr::mutate(df,
                  Orig.Species = gsub('.{1}$', '', Orig.Species),
                  Orig.Genus = gsub('.{1}$', '', Orig.Genus)) %>%
      return()
  }
}


