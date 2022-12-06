translate_trees <- function(new, target, translation_backbone = NULL){
  ## check input formats
  new <- check_df_format(new)
  new <- check_df_consistency(new)
  target_copy <- check_df_format(target)
  check_df_consistency(target_copy)
  #target <- check_df_format_translate(target)
  #target <- check_df_consistency_translate(target)

  ##
  translations <- matching(new, backbone = 'CUSTOM', target_df = target)

  translated <- translations %>% dplyr::filter(matched = TRUE)
  currently_untranslated <- translations %>% dplyr::filter(matched = FALSE)
  assertthat::assert_that(nrow(translations) == nrow(translated) + nrow(currently_untranslated))

  translated <- new %>% dplyr::semi_join(translated, by = c('Orig.Species', 'Orig.Genus'))
  currently_untranslated <- new %>% dplyr::semi_join(currently_untranslated, by = c('Orig.Species', 'Orig.Genus'))

  target_to_tree <- target %>% check_df_format() %>% matching(backbone = translation_backbone)
  currently_untranslated_to_tree <- currently_untranslated %>% matching(backbone = translation_backbone)


  currently_untranslated_to_tree
}
