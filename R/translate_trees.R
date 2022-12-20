#' Translate species names according to a custom target database.
#'
#' @param df `tibble` with species that the user wants to translate into the species names of target. Species binomial split into the columns `Genus` and `Species`
#' @param target `tibble` with a new custom target database. Species binomial split into the columns `Genus` and `Species`
#' @param max_iter parameter which is passed to `enforce_matching()` and controls the maximum depth for matches.
#'
#' @description
#' The function is essentially a wrapper around the functions [matching()] and [enforce_matching()].
#' Species names from `df` are first directly matched to `target` by calling `matching(df, backbone = 'CUSTOM', target_df = target)`.
#' Subsequently, the function calls `enforce_matching(df, backbone = 'CUSTOM', target_df = target)` to increase the number of translated species.
#'
#'
#' @return
#' Returns a `tibble` with the species names of the input `df` in `Orig.Genus`, `Orig.Species`, and the translated names in `Matched.Genus` and `Matched.Species`.
#' Process information from calling matching() and enforce_matching() is added to the output.
#' @export
#'
#' @examples
#' translate_trees(df = iucn, target = fia)
translate_trees <- function(df, target, max_iter = 3){

  ## check input formats
  df <- check_df_format(df)
  df <- check_df_consistency(df)
  check_df_format(target) %>%
    check_df_consistency()

  message("Translating species from input to target...")
  message("Calling: matching(df = df, backbone = 'CUSTOM', target_df = target)")

  df <- matching(df, backbone = 'CUSTOM', target_df = target)

  message("Calling: enforce_matching(df = df, backbone = 'CUSTOM', target_df = target)")

  output <- enforce_matching(df = df, backbone = 'CUSTOM', target_df = target, max_iter = max_iter)
  return(output)
}
