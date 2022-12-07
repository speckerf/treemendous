#' Enforce Matching for Unmatched Species According to a Specified Backbone
#'
#' @description
#' `enforce_matching()` can be called after `matching()`.
#' The function tries to match all unmatched species, by making use of the synonym-accepted relations present in the backbones `WFO`, `WCVP` and `GBIF`.
#' A graph connecting all synonyms with accepted species is created and used to look for matches at increasing distance in this graph according to the desired backbone.
#'
#' @details
#' This function is useful when you want to increase the proportion of matched species against a single target backbone.
#' The package _igraph_ is used to create an undirected graph `g` connecting all synonyms with accepted species according the databases `WFO`, `WCVP` and `GBIF`.
#' From the output of `matching()`, all unmatched species are matched to all three backbones via `matching(c('WFO', 'WCVP', 'GBIF'))`.
#' If there is a match, then all neighbors in the graph `g` of the matched species are checked if they belong to the target backbone.
#' The neighbors are also allowed to not directly match, but match according to `matching(target_backbone)` (fuzzy matches, suffix matches).
#' This is repeated for neighbors in the graph `g` up to a distance of three, which is also returned as `enforced_matching_dist`.
#'
#'
#' @param df `tibble` which is the output of `matching()` or `sequential_matching()` and therefor contains the columns `Matched.Genus` and `Matched.Species`. May contain additional columns, which will be ignored.
#' @param backbone specifies which backbone is used: needs to be one of `c('BGCI', 'WCVP', 'WFO', 'GBIF')`.
#'
#' @return
#' A `tibble` with matched species in `Matched.Genus` and `Matched.Species`.
#' Along with the process information of `matching()`, the function returns the logical column `enforced_matched`, stating whether the species was successfully matched by `enforce_matching()`, and the distance in the neighborhood graph `g`.
#'
#' @export
#'
#' @examples
#' iucn %>% dplyr::sample_n(size = 10) %>% matching('WFO') %>% enforce_matching('WFO')
enforce_matching <- function(df, backbone, target_df = NULL){
  assertthat::assert_that(length(backbone) == 1)
  assertthat::assert_that(backbone %in% c('BGCI', 'WFO', 'WCVP', 'GBIF', 'CUSTOM'))
  assertthat::assert_that(all(c('Matched.Genus', 'Matched.Species') %in% colnames(df)),
                          msg = "No columns named 'Matched.Genus' and 'Matched.Species' found. \n Please make sure to use enforce_matching after having used matching() (or sequential_matching()). For instance, df %>% matching('WFO') %>% enforce_matching('WFO')")
  assertthat::assert_that(tibble::is_tibble(df))

  message('enforce_matching()...')
  ## split matched & unmatched
  matched <- df %>% dplyr::filter(matched == T)
  unmatched <- df %>% dplyr::filter(matched == F)
  assertthat::are_equal(nrow(df), nrow(unmatched) + nrow(matched))
  if(nrow(unmatched) == 0){
    return(matched)
  }

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

  ## create undirected graph
  g <- create_undirected_synonym_graph()

  ## all nodes that are in g
  unresolved_species_in_g <- new_matched_in_db %>% dplyr::filter(new_matched_in_db$ID_merged %in% igraph::get.vertex.attribute(g, 'name'))
  if(nrow(unresolved_species_in_g) == 0){
    return(df)
  }
  for(path_distance in 1:3){
    message(paste('iteration ', path_distance, '/', '3 ...', sep = ''))
    neighbours_n <- igraph::ego(g, order = path_distance, as.character(unresolved_species_in_g$ID_merged)) ## as.character is very important! because this accesses ID_merged of the vertices (attribute names) instead of the ID given by igraph internally (which goes from 1:number_of_vertices(vertices))
    single_neighbour <- map_progress(neighbours_n, find_neighbour_from_backbone, backbone, target_df) # add target_df
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
  }


  new_matched_not_enforced_matched <- new_matched %>%
    dplyr::anti_join(enforced_matches_all,
                     by = c("Orig.Genus", "Orig.Species")) %>%
    dplyr::select('Orig.Genus', 'Orig.Species') %>%
    matching(backbone, target_df) %>%
    dplyr::mutate('enforced_matched' = FALSE)## rerun matching with original backbone to get correct process information
  new_matched_enforced_matched <- new_matched %>%
    dplyr::select(Orig.Genus, Orig.Species) %>%
    dplyr::right_join(enforced_matches_all,
                     by = c("Orig.Genus", "Orig.Species"))
  assertthat::assert_that(nrow(new_matched) == nrow(new_matched_enforced_matched) + nrow(new_matched_not_enforced_matched))

  new_matched_output <- dplyr::bind_rows(new_matched_enforced_matched,
                                         new_matched_not_enforced_matched)

  assertthat::assert_that(nrow(new_matched) == nrow(new_matched_output))

  all_processed <- still_unmatched %>%
    matching(backbone, target_df) %>%  ## rerun matching with original backbone to get correct process information
    dplyr::bind_rows(new_matched_output)

  ## join matched & unmatched
  res <- dplyr::bind_rows(matched, all_processed)
  return(res)
}

find_neighbour_from_backbone <- function(g_neighbour, target_backbone, target_df){
  ## Check if the input species has any neighbor in the graph g that is in the target_backbone
  neighbours_in_targetbb <- get_db(target_df = target_df) %>%
    dplyr::filter(ID_merged %in% igraph::as_ids(g_neighbour)) %>%
    dplyr::filter(get(target_backbone) == TRUE)

  #### Depending on the number of matches, continue differently:
  # 1 neighbor: output this species
  # > 1 neighbor: simply chooses the first neighbor
  # 0 neighbors: see if the neighbours can potentially be fuzzy matched to the target database.
  ####

  if(nrow(neighbours_in_targetbb) == 1){
    return(neighbours_in_targetbb)
  }
  else if(nrow(neighbours_in_targetbb) > 1){
    ## please change here the way we treat the case of multiple hits
    ## AND FIND BETTER NAMES FOR matched_neighbours_to_targetbb versus neighbours_in_targetbb
    return(dplyr::sample_n(neighbours_in_targetbb, size = 1)) ## alternatively could use something more sophisticated here: like for instance choosing the one with more support
  }
  else { ## condition nrow(neighbours_in_targetbb) == 0)
    # match
    matching_neighbours_to_targetbb <- get_db() %>% # don't pass target_df here: because all neighbours are strictly in 'WFO', 'WCVP', 'GBIF'
      dplyr::filter(ID_merged %in% igraph::as_ids(g_neighbour)) %>%
      dplyr::select(Genus, Species) %>%
      matching(target_backbone, target_df) %>% # see if we can fuzzy match the neihgbours to our target_backbone (if 'CUSTOM': target_df will not be NULL)
      dplyr::filter(matched == TRUE)
    matched_neighbours_to_targetbb <- matching_neighbours_to_targetbb %>%
      dplyr::select(Matched.Genus, Matched.Species) %>%
      dplyr::left_join(get_db(target_df = target_df), # add target_df here: needed for translate_trees
                       by=c('Matched.Genus' = 'Genus', 'Matched.Species' = 'Species')) %>%
      dplyr::rename('Genus' = 'Matched.Genus', 'Species' = 'Matched.Species')
    assertthat::assert_that(nrow(matching_neighbours_to_targetbb) == nrow(matched_neighbours_to_targetbb)) #relax this assertion because two neighbours can be matched to the same species using matching(): changed on Nov 2nd, see if this has any conflicts.

    #### return these non-direct (fuzzy) matched neighbours
    # 1 matched: return this
    # > 1 matched: return first one
    # 0 matched: return empty df neighbours_in_targebb
    if(nrow(matched_neighbours_to_targetbb) == 1){
      return(matched_neighbours_to_targetbb)
    }
    else if(nrow(matched_neighbours_to_targetbb) > 1){
      ## please change here the way we treat the case of multiple hits
      return(dplyr::slice(matched_neighbours_to_targetbb, 1))
    }
    else{ ## nrow(matched_neighbours_to_targetbb) == 0
      assertthat::assert_that(nrow(neighbours_in_targetbb) == 0)
      return(neighbours_in_targetbb)
    }
  }
}
