#' Title
#'
#' @param df asdf
#' @param backbone asdf
#' @param extended_synonym_search asdf
#' @param relaxed_fuzzy_matching asdf
#'
#' @return
#' @export
#'
#' @examples
#' iucn %>% matching('WFO') %>% enforce_matching('WFO')
enforce_matching <- function(df, backbone, extended_synonym_search = T, relaxed_fuzzy_matching = T){
  assertthat::assert_that(any(extended_synonym_search, relaxed_fuzzy_matching), msg = 'at least one of two modes has to be ways of enforcing matching has to be True')
  assertthat::assert_that(length(backbone) == 1)
  assertthat::assert_that(backbone %in% c('BGCI', 'WFO', 'WCVP', 'GBIF'))
  assertthat::assert_that(all(c('Matched.Genus', 'Matched.Species') %in% colnames(df)))
  assertthat::assert_that(tibble::is_tibble(df))

  ## mode 1: only based on synonyms
  ## mode 2: only based on relaxed fuzzy matching
  ## mode 3: first synonyms : second relaxed fuzzy matching
  if(extended_synonym_search){
    if(relaxed_fuzzy_matching){mode = 3}
    else{mode = 1}
  }
  else{mode = 2}

  ## split matched & unmatched
  matched <- df %>% dplyr::filter(matched == T)
  unmatched <- df %>% dplyr::filter(matched == F)
  assertthat::are_equal(nrow(df), nrow(unmatched) + nrow(matched))

  ## where to look for synonyms? other backbones than the specified ones
  bb <- c('WFO', 'WCVP', 'GBIF')
  #backbones_to_look_for_synonyms <- bb[!(bb %in% backbone)]

  ## see if unmatched could be matched to other backbones containing information on synonyms
  #output_matching <- unmatched %>% matching(backbones_to_look_for_synonyms)
  output_matching <- unmatched %>% matching(bb) %>%
    dplyr::select(Orig.Genus, Orig.Species,
                  Matched.Genus, Matched.Species,
                  matched)
  new_matched <- output_matching %>% dplyr::filter(matched == T)
  still_unmatched <- output_matching %>% dplyr::filter(matched == F)

  ## matched analogues in database
  new_matched_in_db <- get_db() %>%
    dplyr::semi_join(new_matched, by = c('Genus' = 'Matched.Genus', 'Species' = 'Matched.Species')) %>%
    dplyr::select(Genus, Species, dplyr::matches(bb), ID_merged)

  ## create directed graph
  g <- create_directed_synonym_graph()




  ## all nodes that are in g
  #unresolved_nodes_in_g <- new_matched_in_db$ID_merged[new_matched_in_db$ID_merged %in% igraph::get.vertex.attribute(g, 'name')]
  unresolved_species_in_g <- new_matched_in_db %>% dplyr::filter(new_matched_in_db$ID_merged %in% igraph::get.vertex.attribute(g, 'name'))
  for(path_distance in 1:3){
    neighbours_n <- igraph::ego(g, order = path_distance, as.character(unresolved_species_in_g$ID_merged)) ## as.character is very important! because this accesses ID_merged of the vertices (attribute names) instead of the ID given by igraph internally (which goes from 1:number_of_vertices(vertices))
    single_neighbour <- map_progress(neighbours_n, find_neighbour_from_backbone, backbone)
    enforced_matching_successfully <- unresolved_species_in_g %>%
      dplyr::filter(purrr::map(single_neighbour, nrow) == 1)
    enforced_matching_matches <- single_neighbour %>% dplyr::bind_rows()

    enforced_matches <- new_matched %>%
      dplyr::semi_join(enforced_matching_successfully,
                       by=c('Matched.Genus' = 'Genus', 'Matched.Species' = 'Species')) %>%
      dplyr::mutate(Matched.Genus = enforced_matching_matches$Genus,
                    Matched.Species = enforced_matching_matches$Species,
                    'enforced_matching_dist' = path_distance,
                    'enforced_matched' = TRUE)

    if(!exists('enforced_matches_all')){
      enforced_matches_all <- dplyr::sample_n(enforced_matches, size = 0)
    }
    enforced_matches_all <- enforced_matches_all %>% dplyr::bind_rows(enforced_matches)
    unresolved_species_in_g <- unresolved_species_in_g %>% dplyr::anti_join(enforced_matching_successfully,
                                                 by = c('Genus', 'Species'))

    if(nrow(unresolved_species_in_g) == 0){
      break
    }
    #enforced_matched <-
    # temp_matches_in_db <- new_matched_in_db %>%
    #   dplyr::select(Genus, Species, ID_merged) %>%
    #   dplyr::filter(ID_merged %in% id_found_neighbour)
    #  # dplyr::bind_rows()
    #browser()
  }


  new_matched_not_enforced_matched <- new_matched %>%
    dplyr::anti_join(enforced_matches_all,
                     by = c("Orig.Genus", "Orig.Species")) %>%
    dplyr::select('Orig.Genus', 'Orig.Species') %>%
    matching(backbone) %>%
    dplyr::mutate('enforced_matched' = FALSE)## rerun matching with original backbone to get correct process information
  new_matched_enforced_matched <- new_matched %>%
    dplyr::select(Orig.Genus, Orig.Species) %>%
    dplyr::right_join(enforced_matches_all,
                     by = c("Orig.Genus", "Orig.Species"))
  assertthat::assert_that(nrow(new_matched) == nrow(new_matched_enforced_matched) + nrow(new_matched_not_enforced_matched))

  new_matched_output <- dplyr::bind_rows(new_matched_enforced_matched,
                                         new_matched_not_enforced_matched)


  assertthat::assert_that(nrow(new_matched) == nrow(new_matched_in_db))

  all_processed <- still_unmatched %>%
    matching(backbone) %>%  ## rerun matching with original backbone to get correct process information
    dplyr::bind_rows(new_matched_output)

  ## join matched & unmatched
  res <- dplyr::bind_rows(matched, all_processed)
  #res <- res %>% dplyr::mutate(enforced_matched = !(is.na(enforced_matched) | isFALSE(enforced_matched)))
  return(res)
}


find_neighbour_from_backbone <- function(g_neighbour, target_backbone){
  neighbours_in_targetbb <- get_db() %>%
    filter(ID_merged %in% igraph::as_ids(g_neighbour)) %>%
    dplyr::filter(get(target_backbone) == TRUE)
  if(nrow(neighbours_in_targetbb) == 0){
    matching_neighbours_to_targetbb <- get_db() %>%
      filter(ID_merged %in% igraph::as_ids(g_neighbour)) %>%
      dplyr::select(Genus, Species) %>%
      matching(target_backbone) %>%
      dplyr::filter(matched == TRUE)
    matched_neighbours_to_targetbb <- get_db() %>%
      semi_join(matching_neighbours_to_targetbb, by=c('Genus' = 'Matched.Genus', 'Species' = 'Matched.Species'))
    assertthat::assert_that(nrow(matching_neighbours_to_targetbb) == nrow(matched_neighbours_to_targetbb))
  }
  if(nrow(neighbours_in_targetbb) == 1){
    return(neighbours_in_targetbb)
  }
  else if(nrow(neighbours_in_targetbb) > 1){
    return(dplyr::sample_n(neighbours_in_targetbb, size = 1)) ## alternatively could use something more sophisticated here: like for instance choosing the one with more support
  }
  if(nrow(neighbours_in_targetbb) == 0){
    if(nrow(matched_neighbours_to_targetbb) == 1){
      return(matched_neighbours_to_targetbb)
    }
    else if(nrow(matched_neighbours_to_targetbb) > 1){
      return(dplyr::sample_n(matched_neighbours_to_targetbb, size = 1))
    }
  }
  neighbours_in_targetbb ## return empty df if no hits
}
