#' Summarizes the output of the treemendous pipeline
#'
#' @param df : `tibble` being the output of matching()/sequential_matching()/enforce_matching() and optionally resolve_synonyms().
#'
#' @return Returns a `list` containing summary information about the matched species names and if provided also the resolved species names.
#' @export
#'
#' @examples
#' iucn %>% matching() %>% resolve_synonyms() %>% summarize_output()
summarize_output <- function(df){
  assertthat::assert_that(tibble::is_tibble(df))
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species', 'Matched.Genus', 'Matched.Species') %in% colnames(df)))
  both_steps <- ifelse(all(c('Accepted.Genus', 'Accepted.Species') %in% colnames(df)), TRUE, FALSE)
  used_enforce_matching <- ifelse(all(c('enforced_matched') %in% colnames(df)), TRUE, FALSE)
  if(both_steps){
    assertthat::assert_that(all(c('Accepted.Genus', 'Accepted.Species') %in% colnames(df)))
  }

  n_input <- nrow(df)
  n_matched <- sum(df$matched)
  n_matched_unique <- df %>%
    dplyr::filter(matched == TRUE) %>%
    dplyr::distinct(Matched.Genus, Matched.Species) %>%
    nrow()

  ## summarize matching() output
  # m_meta <- list()
  # m_meta$matched <- paste(n_matched, " / ", n_input, " were matched with ", n_matched_unique," distinct matched names.", sep = "")
  # m_meta$direct_match <- paste(sum(df$direct_match, na.rm = T), " / ", sum(!is.na(df$direct_match)), sep = "")
  # m_meta$genus_match <- paste(sum(df$genus_match, na.rm = T), " / ", sum(!is.na(df$genus_match)), sep = "")
  # m_meta$fuzzy_match_genus <- paste(sum(df$fuzzy_match_genus, na.rm = T), " / ", sum(!is.na(df$fuzzy_match_genus)), sep = "")
  # m_meta$direct_match_species_within_genus <- paste(sum(df$direct_match_species_within_genus, na.rm = T), " / ", sum(!is.na(df$direct_match_species_within_genus)), sep = "")
  # m_meta$suffix_match_species_within_genus <- paste(sum(df$suffix_match_species_within_genus, na.rm = T), " / ", sum(!is.na(df$suffix_match_species_within_genus)), sep = "")
  # m_meta$fuzzy_match_species_within_genus <- paste(sum(df$fuzzy_match_species_within_genus, na.rm = T), " / ", sum(!is.na(df$fuzzy_match_species_within_genus)), sep = "")
  # if('Matched.Backbone' %in% colnames(df)){
  #   m_meta$Matched.Backbone <- summary(as.factor(df$Matched.Backbone))
  # }

  matched_ <- paste('matched:', n_matched, "/", n_input, "were matched with", n_matched_unique,"distinct matched names.")
  direct_match_ <- paste('direct_match: ', sum(df$direct_match, na.rm = T), " / ", n_input, sep = "")
  if(used_enforce_matching){
    df_enforced <- df %>% dplyr::filter(!is.na(enforced_matched))
    df_matching <- df %>% dplyr::filter(is.na(enforced_matched) | enforced_matched == F)
  }
  else{
    df_matching <- df
  }
  no_direct_matches_ <- paste('indirectly matched: ', sum((df_matching$direct_match_species_within_genus | df_matching$suffix_match_species_within_genus | df_matching$fuzzy_match_species_within_genus), na.rm = T), " / ", n_input - sum(df_matching$direct_match, na.rm = T), sep = "")
  genus_match_ <- paste('    genus_match: ', sum(df_matching$genus_match, na.rm = T), " / ", sum(!is.na(df_matching$genus_match)), sep = "")
  fuzzy_match_genus_ <- paste("    fuzzy_match_genus: ", sum(df_matching$fuzzy_match_genus, na.rm = T), " / ", sum(!is.na(df_matching$fuzzy_match_genus)), sep = "")
  direct_match_species_within_genus_ <- paste('    direct_match_species_within_genus: ', sum(df_matching$direct_match_species_within_genus, na.rm = T), " / ", sum(!is.na(df_matching$direct_match_species_within_genus)), sep = "")
  suffix_match_species_within_genus_ <- paste('    suffix_match_species_within_genus: ', sum(df_matching$suffix_match_species_within_genus, na.rm = T), " / ", sum(!is.na(df_matching$suffix_match_species_within_genus)), sep = "")
  fuzzy_match_species_within_genus_ <- paste('    fuzzy_match_species_within_genus: ', sum(df_matching$fuzzy_match_species_within_genus, na.rm = T), " / ", sum(!is.na(df_matching$fuzzy_match_species_within_genus)), sep = "")
  if(used_enforce_matching) enforced_matching_ <- paste('number of species matched via enforce_matching(): ', sum(df_enforced$enforced_matched, na.rm = T), " / ", sum(!is.na(df_enforced$enforced_matched)),  sep = "")
  if('Matched.Backbone' %in% colnames(df)){
    Matched.Backbone_ <- summary(as.factor(df$Matched.Backbone))
  }
  else{
    Matched.Backbone_ <- NULL
  }

  if(used_enforce_matching) summary_matching <- c(matched_, direct_match_, no_direct_matches_, genus_match_, fuzzy_match_genus_, direct_match_species_within_genus_, suffix_match_species_within_genus_, fuzzy_match_species_within_genus_, enforced_matching_, Matched.Backbone_)
  else summary_matching <- c(matched_, direct_match_, no_direct_matches_, genus_match_, fuzzy_match_genus_, direct_match_species_within_genus_, suffix_match_species_within_genus_, fuzzy_match_species_within_genus_, Matched.Backbone_)


  ## summarize resolve_synonyms() output
  if(both_steps){
    n_resolved <- sum(!is.na(df$Accepted.Backbone))
    n_resolved_unique <- df %>%
      dplyr::filter(!is.na(Accepted.Backbone)) %>%
      dplyr::distinct(Accepted.Genus, Accepted.Species) %>%
      nrow()

    resolved_synonyms_ <- paste('resolved_synonyms: ', n_resolved, " / ", n_matched, " were resolved, with ", n_resolved_unique," being distinct species names.", sep = "")
    Accepted.Backbone_ <- summary(as.factor(df$Accepted.Backbone))

    summary_resolve <- c(resolved_synonyms_, Accepted.Backbone_)

  }

  if(exists('summary_resolve')){
    output <- list()
    output$matching <- summary_matching
    output$resolve_synonyms <- summary_resolve
    return(output)
  }
  return(summary_matching)
}
