#' Translate species names according to a custom target database.
#'
#' @param df `tibble` with species that the user wants to translate into the species names of target. Species binomial split into the columns `Genus` and `Species`
#' @param target `tibble` with a new custom target database. Species binomial split into the columns `Genus` and `Species`
#'
#' @description
#' The function is essentially a wrapper around the functions [matching()] and [enforce_matching()].
#' Species names from `df` are first directly matched to `target` by calling `matching(df, backbone = 'CUSTOM', target_df = target)`.
#' Subsequently, the function calls `enforce_matching(df, backbone = 'CUSTOM', target_df = target)` to increase the number of translated species.
#'
#'
#' @return
#' Returns a `tibble` with the same corresponding translated names in `Matched.Genus` and `Matched.Species`.
#' Process information from calling matching() and enforce_matching() is added to the output.
#' @export
#'
#' @examples
#' set.seed(100)
#' target_df <- iucn %>% dplyr::sample_n(size = nrow(iucn) - 5) %>% dplyr::rename(Genus = Orig.Genus, Species = Orig.Species)
#' df <- iucn %>% dplyr::slice(1:100)
#' a <- translate_trees(df, target_df)
translate_trees <- function(df, target){

  ## check input formats
  df <- check_df_format(df)
  df <- check_df_consistency(df)
  check_df_format(target) %>%
    check_df_consistency()

  message("Translating species from input to target...")
  message("Calling: matching(df = df, backbone = 'CUSTOM', target_df = target)")

  df <- matching(df, backbone = 'CUSTOM', target_df = target)

  message("Calling: enforce_matching(df = df, backbone = 'CUSTOM', target_df = target)")

  output <- enforce_matching(df = df, backbone = 'CUSTOM', target_df = target)
  return(output)
}
