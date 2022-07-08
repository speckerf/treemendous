## deprecated
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
  # Param: backbone:
  #  - Default: `NULL`: Full Trees.Full
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

