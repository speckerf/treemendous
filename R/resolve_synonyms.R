#' Resolve Synonyms for Matched Species Names
#'
#' @description
#' This function is called after `matching()` and resolve synonyms based on the database `Trees.Full`.
#' Information on synonyms comes from the databases `WCVP`, `WFO` and `GBIF`. `WFO` is considered to be the primary backbone, `WFO` the secondary, and `GBIF` the tertiary.
#'
#' @param df : tibble containing the two columns `New.Genus` and `New.Species`, which need to be created by calling `matching()`.
#'
#' @return tibble with two new columns: `Accepted.Genus` and `Accepted.Species`
#' @export
#'
#' @examples
#' test6 %>% matching() %>% resolve_synonyms()
resolve_synonyms <- function(df){
  assertthat::assert_that(all(c('New.Genus', 'New.Species') %in% colnames(df)))

  ## select synonym information for matched species
  df_informative <- df %>% dplyr::inner_join(Trees.Full, by = c('New.Genus' = 'Genus', 'New.Species' = 'Species')) %>%
    dplyr::select(c('New.Genus', 'New.Species', 'WCVP_accepted_ID', 'WFO_accepted_ID', 'GBIF_accepted_ID'))

  ## WCVP: get accepted
  df_accepted_wcvp <- df_informative %>%
    dplyr::inner_join(Trees.Full, by = c('WCVP_accepted_ID' = 'WCVP_ID'), na_matches = 'never') %>%
    dplyr::select(New.Genus, New.Species, Genus, Species, ID_merged) %>%
    dplyr::mutate(Accepted.Genus = Genus,
                  Accepted.Species = Species,
                  Accepted.by.WCVP = TRUE) %>%
    dplyr::select(-c(Genus, Species))

  ## WFO: get accepted
  df_accepted_wfo <- df_informative %>%
    dplyr::inner_join(Trees.Full, by = c('WFO_accepted_ID' = 'WFO_ID'), na_matches = 'never') %>%
    dplyr::select(New.Genus, New.Species, Genus, Species, ID_merged) %>%
    dplyr::mutate(Accepted.Genus = Genus,
                  Accepted.Species = Species,
                  Accepted.by.WFO = TRUE) %>%
    dplyr::select(-c(Genus, Species))

  ## GBIF: get accepted
  df_accepted_gbif <- df_informative %>%
    dplyr::inner_join(Trees.Full, by = c('GBIF_accepted_ID' = 'GBIF_ID'), na_matches = 'never') %>%
    dplyr::select(New.Genus, New.Species, Genus, Species, ID_merged) %>%
    dplyr::mutate(Accepted.Genus = Genus,
                  Accepted.Species = Species,
                  Accepted.by.GBIF = TRUE) %>%
    dplyr::select(-c(Genus, Species))

  res <- dplyr::full_join(df_accepted_wcvp, df_accepted_wfo) %>%
    dplyr::full_join(df_accepted_gbif) %>%
    dplyr::arrange(New.Genus, New.Species)

  return(res)
}
