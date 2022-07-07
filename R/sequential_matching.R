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


  df_current <- df
  for(current_backbone in sequential_backbones){

    df_temp <- df_current %>% matching(backbone = current_backbone)

    ## initialize results tibble if it doesn't exist yet.
    if(!exists('res_df')){res_df <- df_temp[0,]}

    df_temp_matched <- df_temp %>% dplyr::filter(matched == TRUE)
    df_temp_unmatched <- df_temp %>% dplyr::filter(matched == FALSE) #%>%
      # dplyr::mutate('Matched.Genus' = as.character(NA),
      #               'Matched.Species' = as.character(NA))
      ### TODO: solve problem which arises from the second iteration on: set back to NA several variables: process, Matched.Genus & Matched.Species
      # maybe genus was fuzzy matched for first backbone: but actually could be directly matched for second backbone: But Matched.Genus is already written
      # do not set to NA for the last iteration of backbones.

    assertthat::assert_that(nrow(df_temp) == (nrow(df_temp_matched) + nrow(df_temp_unmatched)))

    res_df <- res_df %>% dplyr::bind_rows(df_temp_matched)

    df_current <- df_temp_unmatched

    assertthat::assert_that(nrow(df) == (nrow(res_df) + nrow(df_current)))
  }

  res_df <- res_df %>% dplyr::bind_rows(df_temp_unmatched)
  return(res_df)
}
