iterate_synonym_resolving <- function(df, first_backbone = 'WFO', second_backbone = 'WCVP', third_backbone = 'GBIF'){
  assertthat::assert_that(all(c('Matched.Species', 'Matched.Genus') %in% colnames(df)))
  assertthat::assert_that(length(unique(c(first_backbone, second_backbone, third_backbone))) == 3)
  assertthat::assert_that(all(c(first_backbone, second_backbone, third_backbone) %in% c('WFO', 'WCVP', 'GBIF')))

  df_first <- backbone_synonym_resolving(df, backbone = first_backbone)

  accepted_first_backbone <- df_first %>% dplyr::filter(get(paste('Accepted.by.', first_backbone, sep = '')) == TRUE)
  unresolved_first_backbone <- df_first %>% dplyr::filter(get(paste('Accepted.by.', first_backbone, sep = '')) == FALSE)

  df_second <- backbone_synonym_resolving(unresolved_first_backbone, backbone = second_backbone)

  accepted_second_backbone <- df_second %>% dplyr::filter(get(paste('Accepted.by.', second_backbone, sep = '')) == TRUE)
  unresolved_second_backbone <- df_second %>% dplyr::filter(get(paste('Accepted.by.', second_backbone, sep = '')) == FALSE)

  df_third <- backbone_synonym_resolving(unresolved_second_backbone, backbone = third_backbone)

  accepted_third_backbone <- df_third %>% dplyr::filter(get(paste('Accepted.by.', third_backbone, sep = '')) == TRUE)
  unresolved_third_backbone <- df_third %>% dplyr::filter(get(paste('Accepted.by.', third_backbone, sep = '')) == FALSE)

  accepted <- dplyr::bind_rows(accepted_first_backbone, accepted_second_backbone, accepted_third_backbone)
  unresolved <- dplyr::bind_rows(unresolved_third_backbone)
  res <- dplyr::bind_rows(accepted, unresolved, .id = 'Accepted') %>%
    dplyr::mutate('Accepted' = ('Accepted' == 1)) %>%
    dplyr::relocate(c('Orig.Genus', 'Orig.Species', 'Matched.Genus', 'Matched.Species', 'Accepted.Genus', 'Accepted.Species'))

  assertthat::assert_that(nrow(df) == nrow(res))
  return(res)
}
