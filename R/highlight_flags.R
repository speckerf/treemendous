#' Highlight relevant flags for caution upon resolving synonyms
#'
#' @param df `tibble` which is the output of `resolve_synonyms()` and therefor contains the columns `Accepted.Genus`, `Accepted.Species`, `Matched.Genus` and `Matched.Species`. May contain additional columns, which will be discarded.
#' @param backbone specifies for which backbone(s) the flags should be appended. Needs to be one of (or a combination of) `c('WCVP', 'WFO', 'GBIF')` or `NULL` if all should be used.
#'
#' @description
#' The user can call `highlight_flags()` from the output of `resolve_synonyms()` to investigate potential ambiguities when resolving synonyms to their accepted latin binomial names.
#' These ambiguities can be of varying importance for different use-cases and should be carefully assessed for each use-case.
#'
#'
#' @return
#' The function outputs a `tibble` that includes the original species names, the matched species names, the accepted species names, and the corresponding flags for the matched species.
#' It filters and returns only those entries where at least one flag was raised.
#' @details
#' The following flags have been introduced in order to clarify and alleviate potential ambiguities that were introduced upon compilation of `Treemendous.Trees`.
#' The flags always correspond to the latin binomial in the columns `c(Matched.Genus, Matched.Species)`.
#' The user is encouraged to check for their individual use-cases if she/he wants to exclude resolved names based on these flags. Below they are described in more detail.
#'
#' The first column appended has the suffix `_Flag` and can be either `authorship ambiguity`, `infraspecific ambiguity`, or `NA`.
#' - `authorship ambiguity`: For a given latin binomial,
#' there are multiple entries at taxonomic rank "Species" in the underlying backbone,
#' which would be resolved to different latin binomials due to different authorships.
#' For instance, in the `WCVP` backbone, two entries are present for the latin binomial _Acer flabellatum_ at rank "Species" ( _Acer flabellatum_ Greene, _Acer flabellatum_ Rehder).
#' The first is considered a synonym of _Acer macrophyllum_ Pursh, while the latter is considered a synonym of _Acer campbellii subsp. flabellatum_ (Rehder) A.E.Murray.
#' - `infraspecific ambiguity`: For a given latin binomial,
#' there are multiple entries at different taxonomic ranks (Species, Subspecies, Variety or Form) in the underlying backbone,
#' which would be resolved to different latin binomials.
#' For instance, in the `WCVP` backbone, four entries are present for the latin binomial _Nothofagus obliqua_, one at rank "Species" ( _Nothofagus obliqua_),
#' two at rank "Subspecies" ( _Nothofagus obliqua subsp. andina_ and _Nothofagus obliqua subsp. valdiviana_) and one at rank "Variety" ( _Nothofagus obliqua var. macrocarpa_).
#' While the former three would all be resolved to the latin binomial _Nothofagus obliqua_, the latter is considered to be a synonym of the accepted name _Nothofagus macrocarpa_.
#' `Resolve_synonyms()` will however always resolve it to _Nothofagus obliqua_, because this was the only accepted name out of all the four entries.
#' This flag should not be a problem if you are working with latin binomials from the beginning.
#' However, if your initial species names consists of trinomials (infraspecific species names), then this flag can help you identify ambiguous name resolving.
#' - `NA`: no ambiguity
#'
#' The second column with suffix `_new_linkage` is set to `TRUE` if the following scenario happened during the compilation of `Treemendous.Trees`:
#' If an entry in a given backbone is pointing (meaning is a synonym of) to another entry which has been removed (in favor of another entry with the same latin binomial),
#' then the linkage of the synonym-accepted relation was updated. Only considering latin binomials, this doesn't make any difference.
#' - For instance, `WCVP` considers the species _Betula kwangsiensis_ as a synonym of the accepted subspecies _Betula kweichowensis subsp. kweichowensis_.
#' This subspecies however shares its latin binomial with the accepted species _Betula kweichowensis_.
#' Therefore the species _Betula kwangsiensis_ had to be relinked to the species _Betula kweichowensis_ in order to resolve the latin binomials correctly.
#' - Another example would be: According to `WFO`, the species _Abies shastensis_ is a synonym of the accepted variety _Abies magnifica var. shastensis_.
#' However, because there is also an accepted entry at rank species _Abies magnifica_ (which is selected during the compilation of `Treemendous.Trees`),
#' the species _Abies shastensis_ has to be relinked to _Abies magnifica_.
#'
#'
#' @export
#'
#' @examples
#' iucn_resolved <- iucn %>% matching('WCVP') %>% resolve_synonyms('WCVP')
#' iucn_resolved %>% highlight_flags('WCVP')
highlight_flags <- function(df, backbone = NULL){
  message("highlight_flags: returns all matched latin binomials for which a flag was raised during database compilation. See ?highlight_flags for more details. ")

  # check inputs
  assertthat::assert_that(
    is.null(backbone) | all(backbone %in% c('GBIF', 'WFO', 'WCVP')),
    msg = "Invalid backbone argument. Must be either NULL or one of (a combination of) c('GBIF', 'WFO', 'WCVP')"
  )
  assertthat::assert_that(
    all(c('Matched.Genus', 'Matched.Species', 'Accepted.Genus', 'Accepted.Species') %in% colnames(df)),
    msg = "highlight_flags() is only expected to be called after resolve_synonyms(). No Accepted.Genus and Accepted.Species columns were found."
  )

  # only get Genus and Species names and discard processing information
  df_bare <- df %>% dplyr::select(dplyr::matches('Orig.'), dplyr::matches('Matched.'), dplyr::matches('Accepted.'))

  # select the columns of interest from the database ({backbone}_new_linkage and {backbone}_Flag)
  db_only_relevant_columns <- get_db() %>%  dplyr::select('Species', 'Genus', dplyr::matches(paste0(backbone, '_new_linkage')), dplyr::matches(paste0(backbone, '_Flag')))

  # join the bare input df with database containing the relevant flags and return it.
  df_bare %>%
    dplyr::left_join(db_only_relevant_columns, by = c('Matched.Genus' = 'Genus', 'Matched.Species' = 'Species')) %>%
    dplyr::filter(dplyr::if_any(dplyr::matches('_new_linkage|_Flag'), ~!is.na(.)))
}
