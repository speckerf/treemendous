#' Summarizes the output of the treemendous pipeline
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
#' test1 %>% matching() %>% resolve_synonyms() %>% summarize_output()
summarize_output <- function(df){
  assertthat::assert_that(tibble::is_tibble(df))
  assertthat::assert_that(all(c('Orig.Genus', 'Orig.Species', 'Matched.Genus', 'Matched.Species') %in% colnames(df)))
  both_steps <- ifelse(all(c('Accepted.Genus', 'Accepted.Species') %in% colnames(df)), TRUE, FALSE)
  if(both_steps){
    assertthat::assert_that(all(c('Accepted.Genus', 'Accepted.Species') %in% colnames(df)))
  }

  n_input <- nrow(df)
  n_matched <- sum(df$matched)
  n_matched_unique <- df %>%
    dplyr::filter(matched == TRUE) %>%
    dplyr::distinct(Matched.Genus, Matched.Species) %>%
    nrow()

  m_meta <- list()
  m_meta$matched <- paste(n_matched, " / ", n_input, " with ", n_matched_unique," unique matched names", sep = "")
  m_meta$direct_match <- paste(sum(df$direct_match, na.rm = T), " / ", sum(!is.na(df$direct_match)), sep = "")
  m_meta$genus_match <- paste(sum(df$genus_match, na.rm = T), " / ", sum(!is.na(df$genus_match)), sep = "")
  m_meta$fuzzy_match_genus <- paste(sum(df$fuzzy_match_genus, na.rm = T), " / ", sum(!is.na(df$fuzzy_match_genus)), sep = "")
  m_meta$direct_match_species_within_genus <- paste(sum(df$direct_match_species_within_genus, na.rm = T), " / ", sum(!is.na(df$direct_match_species_within_genus)), sep = "")
  m_meta$suffix_match_species_within_genus <- paste(sum(df$suffix_match_species_within_genus, na.rm = T), " / ", sum(!is.na(df$suffix_match_species_within_genus)), sep = "")
  m_meta$fuzzy_match_species_within_genus <- paste(sum(df$fuzzy_match_species_within_genus, na.rm = T), " / ", sum(!is.na(df$fuzzy_match_species_within_genus)), sep = "")
  if('Matched.Backbone' %in% colnames(df)){
    m_meta$Matched.Backbone <- summary(as.factor(df$Matched.Backbone))
  }



  if(both_steps){
    n_resolved <- sum(!is.na(df$Accepted.Backbone))
    n_resolved_unique <- df %>%
      dplyr::filter(!is.na(Accepted.Backbone)) %>%
      dplyr::distinct(Accepted.Genus, Accepted.Species) %>%
      nrow()

    r_meta <- list()
    r_meta$resolved_synonyms <- paste(n_resolved, " / ", n_matched, " were resolved to ", n_resolved_unique," unique species names", sep = "")
    r_meta$Accepted.Backbone <- summary(as.factor(df$Accepted.Backbone))

  }

  if(exists('r_meta')){
    output <- list()
    output$matching <- m_meta
    output$resolve_synonyms <- r_meta
    return(output)
  }
  m_meta


}
