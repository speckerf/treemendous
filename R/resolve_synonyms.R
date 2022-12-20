#' Resolve Synonyms for Matched Species Names
#'
#' @description
#' This function is called after `matching()` and resolve synonyms based on the database `Trees.Full`.
#' Information on synonyms comes from the databases `WCVP`, `WFO` and `GBIF`. `WFO` is considered to be the primary backbone, `WFO` the secondary, and `GBIF` the tertiary.
#'
#' @param df : tibble containing the two columns `Matched.Genus` and `Matched.Species`, which need to be created by calling `matching()`.
#' @param backbones specifies the order in which synonyms are resolved: needs to be a subset of c('BGCI', 'WCVP', 'WFO', 'GBIF') or NULL if the default ordering c('BGCI', 'WFO', 'WCVP', 'FIA', 'PM', 'GBIF') should be used .
#'
#' @return tibble with two new columns: `Accepted.Genus` and `Accepted.Species`
#' @export
#'
#' @examples
#' backbones = c('BGCI', 'WFO')
#' iucn %>% matching(backbones) %>% resolve_synonyms(backbones)
resolve_synonyms <- function(df, backbones = NULL){

  assertthat::assert_that('matched' %in% colnames(df),
                          'Matched.Genus' %in% colnames(df),
                          'Matched.Species' %in% colnames(df),
                          msg = 'Species names have to be matched via matching() (or sequential_matching()) before synonyms can be resolved.')

  assertthat::assert_that(is.null(backbones) | all(backbones %in% c('GBIF', 'WFO', 'WCVP', 'BGCI')))

  message('resolve_synonyms()...')


  ## check if sequential_matching() or matching() was used in the first place
  sequential <- ifelse('Matched.Backbone' %in% colnames(df), TRUE, FALSE)

  # stash unused cols: use 'Orig.Genus', 'Orig.Species' as a key to join later again. --> potentially move this to a wrapper function for both matching and synonynm resolving
  if(sequential){
    df_input_meta <- df %>% dplyr::select(-c('Matched.Genus', 'Matched.Species', 'matched', 'Matched.Backbone'))
    df_informative <- df %>% dplyr::select(c('Orig.Genus', 'Orig.Species', 'Matched.Genus', 'Matched.Species', 'matched', 'Matched.Backbone'))
    informative_cols <- c('Orig.Genus', 'Orig.Species', 'Matched.Genus', 'Matched.Species', 'matched', 'Matched.Backbone')
  }
  else{
    df_input_meta <- df %>% dplyr::select(-c('Matched.Genus', 'Matched.Species', 'matched'))
    df_informative <- df %>% dplyr::select(c('Orig.Genus', 'Orig.Species', 'Matched.Genus', 'Matched.Species', 'matched'))
    informative_cols <- c('Orig.Genus', 'Orig.Species', 'Matched.Genus', 'Matched.Species', 'matched')
  }

  input_colnames <- colnames(df)
  unmatched <- df_informative %>% dplyr::filter(matched == FALSE)
  matched <- df_informative %>% dplyr::filter(matched == TRUE)
  assertthat::assert_that(nrow(df_informative) == nrow(unmatched) + nrow(matched))

  if (sequential){
    assertthat::assert_that(!is.null(backbones), all(matched$Matched.Backbone %in% backbones), msg = 'Species names were matched sequentially: therefore the user should provide the same ordering for resolving synonyms.')

    for (backbone in backbones){
      matched_by_backbone <- matched %>% dplyr::filter(Matched.Backbone == backbone)

      ## call helper function that resolve synonyms for a single backbone
      resolved_by_backbone <- helper_resolving(matched_by_backbone, backbone, informative_cols)

      if(!exists('resolved_species')){resolved_species <- resolved_by_backbone[0,]}
      resolved_species <- resolved_species %>% dplyr::bind_rows(resolved_by_backbone)
    }
  }

  else{
    custom_backbone_priorities <- c('BGCI', 'WFO', 'WCVP', 'GBIF')
    if(is.null(backbones)){
      backbone_order <- custom_backbone_priorities
    }
    else{
      backbone_order <- custom_backbone_priorities[custom_backbone_priorities %in% backbones]
    }
    for (backbone in backbone_order){
      matched_by_backbone <- matched %>%
        dplyr::semi_join(get_db(backbone), by = c('Matched.Genus' = 'Genus', 'Matched.Species' = 'Species'))

      ## remove matched_by_backbone from matched
      matched <- matched %>% dplyr::anti_join(matched_by_backbone, by = c('Orig.Genus', 'Orig.Species'))

      ## call helper function that resolve synonyms for a single backbone
      resolved_by_backbone <- helper_resolving(matched_by_backbone, backbone, informative_cols)

      if(!exists('resolved_species')){resolved_species <- resolved_by_backbone[0,]}
      resolved_species <- resolved_species %>% dplyr::bind_rows(resolved_by_backbone)
    }
    assertthat::assert_that(nrow(df) == nrow(matched) + nrow(unmatched) + nrow(resolved_species))
    ## captures the cases which were matched but not resolved because not the same backbones were provided for matching and resolve_synonyms
    if (nrow(matched) > 0 && nrow(df) != nrow(unmatched) + nrow(resolved_species)){
      warning("Not able to resolve all the matched names. This could be the case because you have not provided the same backbones to matching() and resolve_synonyms().")
      resolved_species <- dplyr::bind_rows(resolved_species, matched)
    }
  }

  res <- dplyr::bind_rows(unmatched, resolved_species) %>%
    dplyr::inner_join(df_input_meta, by = c('Orig.Genus', 'Orig.Species')) %>%
    dplyr::relocate('Orig.Genus', 'Orig.Species', 'Matched.Genus', 'Matched.Species', 'Accepted.Genus', 'Accepted.Species') %>%
    dplyr::arrange(Orig.Genus, Orig.Species)

  assertthat::assert_that(nrow(df) == nrow(res))
  return(res)
}




helper_resolving <- function(df, backbone, informative_cols){
  ######
  ## if one of these: directly label as accepted since no information on about synonyms is present
  if(backbone == 'BGCI'){
    resolved_by_backbone <- df %>%
      dplyr::mutate('Accepted.Genus' = Matched.Genus,
                    'Accepted.Species' = Matched.Species,
                    'Accepted.Backbone' = backbone)
  }
  ## if in WFO, WCVP, GBIF: proceed as following
  else{
    backbone_accepted_ID <- paste(backbone, '_accepted_ID', sep='')
    backbone_ID <- paste(backbone, '_ID', sep='')
    matched_in_db <- get_db(backbone) %>%
      dplyr::semi_join(df, by = c('Genus' = 'Matched.Genus', 'Species' = 'Matched.Species'))
    accepted_in_db <- matched_in_db %>% dplyr::filter(is.na(get(backbone_accepted_ID)))
    synonyms_in_db <- matched_in_db %>% dplyr::filter(!is.na(get(backbone_accepted_ID)))
    accepted_of_synonyms_in_db <- get_db(backbone) %>%
      dplyr::semi_join(synonyms_in_db,
                       by = c(stats::setNames(nm = backbone_ID, backbone_accepted_ID)), ## corresponds to: by = c(get(backbone_ID) = get(backbone_accepted_ID))
                       na_matches = 'never')

    assertthat::assert_that(nrow(matched_in_db) == nrow(accepted_in_db) + nrow(synonyms_in_db))

    ## proceed with accepted_in_db
    accepted_by_backbone <- df %>%
      dplyr::semi_join(accepted_in_db, by = c('Matched.Genus' = 'Genus', 'Matched.Species' = 'Species')) %>%
      dplyr::mutate('Accepted.Genus' = Matched.Genus,
                    'Accepted.Species' = Matched.Species,
                    'Accepted.Backbone' = backbone)

    ## proceed with synonyms_in_db
    synonyms_by_backbone <- df %>%
      dplyr::semi_join(synonyms_in_db, by = c('Matched.Genus' = 'Genus', 'Matched.Species' = 'Species')) %>%
      dplyr::left_join(synonyms_in_db, by = c('Matched.Genus' = 'Genus', 'Matched.Species' = 'Species')) %>%
      dplyr::select(dplyr::all_of(informative_cols), dplyr::all_of(backbone_accepted_ID)) %>%
      dplyr::left_join(accepted_of_synonyms_in_db, by = c(stats::setNames(nm = backbone_accepted_ID, backbone_ID))) %>%
      dplyr::rename('Accepted.Genus' = 'Genus',
                    'Accepted.Species' = 'Species') %>%
      dplyr::mutate('Accepted.Backbone' = backbone) %>%
      dplyr::select(dplyr::all_of(informative_cols), 'Accepted.Genus', 'Accepted.Species', 'Accepted.Backbone')

    resolved_by_backbone <- dplyr::bind_rows(accepted_by_backbone, synonyms_by_backbone)
  }
  ######
  assertthat::assert_that(nrow(df) == nrow(resolved_by_backbone))
  return(resolved_by_backbone)
}
