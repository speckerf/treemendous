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
