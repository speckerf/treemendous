backbone_synonym_resolving <- function(df, backbone = 'WFO'){
  assertthat::assert_that(all(c('Matched.Species', 'Matched.Genus') %in% colnames(df)))
  assertthat::assert_that(backbone %in% c('WFO', 'WCVP', 'GBIF'))

  ## get column names of backbone
  endings <- c('_accepted_ID', '_ID', '_Status', '')
  backbone_col_names <- purrr::map2_chr(rep(backbone, length(endings)), endings, paste, sep='')

  ## individual column names
  backbone_accepted_ID <- backbone_col_names[1]
  backbone_ID <- backbone_col_names[2]
  backbone_Status <- backbone_col_names[3]
  matched.backbone <- paste('Matched.',backbone_col_names[4], sep = '')
  accepted_by_backbone <- paste('Accepted.by.', backbone, sep = '')

  if(nrow(df) == 0){
    return(df %>% tibble::add_column(accepted_by_backbone))
  }

  ## select synonym information for matched species
  df_informative <- df %>% dplyr::inner_join(Trees.Full, by = c('Matched.Genus' = 'Genus', 'Matched.Species' = 'Species')) %>%
    select_columns(backbone) %>%
    dplyr::rename(!! matched.backbone := backbone)

  ## 2.present_in_backbone
  present_in_backbone <- df_informative %>% dplyr::filter(get(matched.backbone) == TRUE)
  temporarily_unresolved_species <- df_informative %>% dplyr::filter(get(matched.backbone) == FALSE)

  ## 2.accepted_in_backbone
  accepted_in_backbone <- present_in_backbone %>% dplyr::filter(is.na(get(backbone_accepted_ID))) %>%
    dplyr::mutate('Accepted.Genus' = Matched.Genus,
                  'Accepted.Species' = Matched.Species,
                  !! accepted_by_backbone := TRUE) %>% ## how to pass a string as variable name in dplyr::mutate --> https://stackoverflow.com/questions/59690702/pass-a-string-as-variable-name-in-dplyrmutate
    select_columns(backbone, mode = 3)
  input_resolve_synonyms_in_backbone <- present_in_backbone %>% dplyr::filter(!is.na(get(backbone_accepted_ID)))

  ## 2.resolve_synonyms_in_backbone
  resolved_synonyms_in_backbone <- input_resolve_synonyms_in_backbone %>%
    dplyr::inner_join(Trees.Full, by = c(setNames(nm = backbone_accepted_ID, backbone_ID)), na_matches = 'never') %>% ## see for why we use setNames(): https://stackoverflow.com/questions/28399065/dplyr-join-on-by-a-b-where-a-and-b-are-variables-containing-strings
    select_columns(backbone, mode = 2) %>%
    dplyr::rename('Accepted.Genus' = 'Genus',
                  'Accepted.Species' = 'Species')# %>%

  unresolved_synonyms_in_backbone <- input_resolve_synonyms_in_backbone %>%
    dplyr::anti_join(resolved_synonyms_in_backbone, by = c('Orig.Genus', 'Orig.Species')) %>%
    select_columns(backbone)

  ## output for next iteration
  temporarily_unresolved_species <- dplyr::bind_rows(temporarily_unresolved_species, unresolved_synonyms_in_backbone) %>%
    select_columns(backbone, mode = 3)
  df_accepted <- dplyr::bind_rows(accepted_in_backbone, resolved_synonyms_in_backbone)

  # combine df_accepted and temporarily_unresolved_species and add Boolean indicator: TRUE = matched, FALSE = unmatched
  combined <- dplyr::bind_rows(df_accepted, temporarily_unresolved_species, .id = accepted_by_backbone) %>%
    dplyr::mutate(!! accepted_by_backbone := (get(accepted_by_backbone) == 1)) %>% ## convert to Boolean
    select_columns(backbone, mode = 4)

  assertthat::assert_that(nrow(df) == nrow(combined))
  return(combined)
}


## helper function to select columns; moved here to keep the above main function more tidy. : however this function is mess as well... --> needs some restructuring
select_columns <- function(df, backbone, mode = 1){
  if(mode == 1){
    df_temp <- df %>% dplyr::select(dplyr::matches('Orig.'),
                                    dplyr::matches('Matched.'),
                                    dplyr::matches(paste('^', backbone, '$', sep = '')),
                                    dplyr::matches("Accepted.by.[GW][BFC][IOV]"),
                                    dplyr::matches("Accepted.by.[GW][BFC][IOV]"),
                                    dplyr::matches(paste(backbone, "_", sep = '')))
  }
  else if(mode == 2){
    df_temp <- df %>% dplyr::select('Genus', 'Species', dplyr::matches('Orig.'),
                                    dplyr::matches('Matched.'),
                                    dplyr::matches(paste('^', backbone, '$', sep = '')),
                                    dplyr::matches("Accepted.by.[GW][BFC][IOV]"),
                                    dplyr::matches("Accepted.by.[GW][BFC][IOV]"))
  }

  else if(mode == 3){
    df_temp <- df %>% dplyr::select(dplyr::matches('Orig.'),
                                    dplyr::matches('Matched.'),
                                    dplyr::matches('Accepted.Genus'),
                                    dplyr::matches('Accepted.Species'),
                                    dplyr::matches(paste('^', backbone, '$', sep = '')),
                                    dplyr::matches("Accepted.by.[GW][BFC][IOV]"),
                                    dplyr::matches("Accepted.by.[GW][BFC][IOV]"))

  }

  else if(mode == 4){
    df_temp <- df %>% dplyr::select(dplyr::matches('Orig.'),
                                    dplyr::matches('Matched.'),
                                    'Accepted.Genus', 'Accepted.Species',
                                    dplyr::matches("Accepted.by.[GW][BFC][IOV]"),
                                    dplyr::matches("Accepted.by.[GW][BFC][IOV]"))

  }


  return(df_temp)
}
