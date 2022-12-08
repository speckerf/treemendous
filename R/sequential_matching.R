#' Sequentially Matches Species Names to `Treemendous.Trees`
#' @description
#' This function is a wrapper around `matching()`, which matches species names against the internal database `Treemendous.Trees` according to a specified backbone.
#' `matching()` is called for every individual backbone provided via the argument `sequential_backbones` in the order of appearance.
#'
#' @param df `tibble` containing the species binomial split into the columns `Genus` and `Species`. May contain additional columns, which will be ignored.
#' @param sequential_backbones specifies the backbone which are sequentially used: needs to be a subset of `c('BGCI', 'WCVP', 'WFO', 'GBIF', 'FIA', 'PM')`.
#'
#' @return
#' Returns a `tibble`, with the matched names in `Matched.Genus` and `Matched.Species`.
#' Process information is added as individual columns for every function.
#' The original input columns `Genus` and `Species` are renamed to `Orig.Species` and `Orig.Genus`.
#'
#' @export
#'
#' @examples
#' iucn %>% sequential_matching(sequential_backbones = c('WFO', 'BGCI'))
sequential_matching <- function(df, sequential_backbones){

  ##########
  # Input checks
  ##########

  ### Check sequential_backbones argument
  assertthat::assert_that(
      all(sequential_backbones %in% c('GBIF', 'WFO', 'WCVP', 'BGCI')),
      msg = "Invalid sequential_backbone argument. Must be a combination of c('GBIF', 'WFO', 'WCVP', 'BGCI')"
  )

  assertthat::assert_that(length(sequential_backbones) >= 2,
                          msg = "At least two backbones have to be specified, so e.g. sequential_matching(df, sequential_backbones = c('BGCI', 'WFO')). If you want to use a single backbone, please use the function matching() directly.")

  ### Check input df for correct formatting / data issues
  df <- check_df_format(df)
  check_df_consistency(df)

  ### Add column Matched.Backbone if it does not yet exist
  if(!('Matched.Backbone' %in% colnames(df))){
    df <- df %>% tibble::add_column(Matched.Backbone = as.character(NA))
  }


  ##########
  # Wrapping around matching()
  ##########
  message(paste("Matching sequentially against backbones", paste(sequential_backbones, collapse = ", "), ". If no explicit sequential backbone ordering is required, use `matching()` instead"))

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
                    direct_match_species_within_genus = NULL,
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

    if(nrow(df_temp_unmatched)==0){
      break
    }
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
