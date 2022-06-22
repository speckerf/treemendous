backbone_synonym_resolving <- function(df, backbone = 'WFO'){
  assertthat::assert_that(all(c('Matched.Species', 'Matched.Genus') %in% colnames(df)))
  assertthat::assert_that(backbone %in% c('WFO', 'WCVP', 'GBIF'))

  ## get column names of backbone
  endings <- c('_accepted_ID', '_ID', '_Status')
  backbone_col_names <- purrr::map2_chr(rep(backbone, 3), endings, paste, sep='')

  ## individual column names
  backbone_accepted_ID <- backbone_col_names[1]
  backbone_ID <- backbone_col_names[2]
  backbone_Status <- backbone_col_names[3]
  accepted_by_backbone <- paste('Accepted.by.', backbone, sep = '')
  varr <- 'Matched.Genus'


  ## select synonym information for matched species
  df_informative <- df %>% dplyr::inner_join(Trees.Full, by = c('Matched.Genus' = 'Genus', 'Matched.Species' = 'Species')) %>%
    dplyr::select(c('Orig.Genus', 'Orig.Species', 'Matched.Genus', 'Matched.Species', backbone_col_names))

  ## based on backbone: get accepted
  df_accepted <- df_informative %>%
    dplyr::inner_join(Trees.Full, by = c(setNames(nm = backbone_accepted_ID, backbone_ID)), na_matches = 'never') %>% ## see for why we use setNames(): https://stackoverflow.com/questions/28399065/dplyr-join-on-by-a-b-where-a-and-b-are-variables-containing-strings
    dplyr::select(Orig.Genus, Orig.Species, Matched.Genus, Matched.Species, Genus, Species, ID_merged) %>%
    dplyr::mutate(Accepted.Genus = Genus,
                  Accepted.Species = Species,
                  !! accepted_by_backbone := TRUE) %>% ## how to pass a string as variable name in dplyr::mutate --> https://stackoverflow.com/questions/59690702/pass-a-string-as-variable-name-in-dplyrmutate
    dplyr::select(-c(Genus, Species))

  return(df_accepted)
}
