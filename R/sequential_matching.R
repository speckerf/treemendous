#' sequentially match all according to specific backbone ordering
#'
#' @param df
#' @param sequential_backbones
#'
#' @return
#' @export
#'
#' @examples
sequential_matching <- function(df, sequential_backbones = NULL){
  ### Check if Orig.Genus, Orig.Species or Genus, Species columns exist
  assertthat::assert_that(all(c('Genus', 'Species') %in% colnames(df)) | all(c('Orig.Genus', 'Orig.Species') %in% colnames(df)))

  assertthat::assert_that(
    any(
      is.null(sequential_backbones),
      all(sequential_backbones %in% c('FIA', 'GBIF', 'WFO', 'WCVP', 'PM', 'BGCI'))
    )
  )


  if(!all(c('Orig.Genus', 'Orig.Species') %in% colnames(df))){
    df <- df %>% dplyr::rename(Orig.Genus = Genus, Orig.Species = Species)
  }

  if(is.null(sequential_backbones)){
    warning("use `matching()` instead of `sequential_matching()` when you don't want to perform the matching according to a certain backbone ordering")
    return(df %>% matching(backbone = NULL))
  }
  else{
    message(paste("Matching sequentially against backbones", paste(sequential_backbones, collapse = ", "), ". If no explicit sequential backbone ordering is required, use matching() instead"))
  }


  df_temp_unmatched <- df
  for(current_backbone in sequential_backbones){
    message(paste("Matching against", current_backbone, "..."))
    ## dplyr::mutate(... = NULL) is required in order to get correct process information for matched species (matched not for the first database):
    df_temp_processed <- df_temp_unmatched %>%
      dplyr::mutate(Matched.Genus = NULL,
                    Matched.Species = NULL,
                    matched = NULL,
                    direct_match = NULL,
                    genus_match = NULL,
                    fuzzy_match_genus = NULL,
                    fuzzy_genus_dist = NULL,
                    species_within_genus_match = NULL,
                    suffix_match_species_within_genus = NULL,
                    fuzzy_match_species_within_genus = NULL,
                    fuzzy_species_dist = NULL) %>%
      matching(backbone = current_backbone)

    ## initialize results tibble if it doesn't exist yet.
    if(!exists('df_matched')){df_matched <- df_temp_unmatched[0,]}

    df_temp_matched <- df_temp_processed %>% dplyr::filter(matched == TRUE)
    df_temp_unmatched <- df_temp_processed %>% dplyr::filter(matched == FALSE) #%>%

    assertthat::assert_that(nrow(df_temp_processed) == (nrow(df_temp_matched) + nrow(df_temp_unmatched)))

    df_matched <- df_matched %>% dplyr::bind_rows(df_temp_matched)

    assertthat::assert_that(nrow(df) == (nrow(df_matched) + nrow(df_temp_unmatched)))
  }

  ## get original df of all unmatched species: re-apply matching with all backbones to get correct process information columns for unmatched species
  if(nrow(df_temp_unmatched) > 0 & length(sequential_backbones) > 1){
    if(all(c('Genus', 'Species') %in% colnames(df)) & !all(c('Orig.Genus', 'Orig.Species') %in% colnames(df))){
      df_unmatched <- df %>%
        dplyr::semi_join(df_temp_unmatched, by = c('Genus' = 'Orig.Genus', 'Species' = 'Orig.Species')) %>%
        matching(sequential_backbones)
    }
    else if(!all(c('Genus', 'Species') %in% colnames(df)) & all(c('Orig.Genus', 'Orig.Species') %in% colnames(df))){
      df_unmatched <- df %>%
        dplyr::semi_join(df_temp_unmatched, by = c('Orig.Genus', 'Orig.Species')) %>%
        matching(sequential_backbones)
    }
    else{
      df_unmatched <- df_temp_unmatched
    }
  }

  df_unmatched <- df_temp_unmatched
  df_result <- df_matched %>% dplyr::bind_rows(df_unmatched)
  return(df_result)
}
