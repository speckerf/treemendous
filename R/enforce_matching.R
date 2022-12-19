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
#' Vertices represent species names, and edges represent synonym-accepted relations between two species according to at least one backbone.
#' Additionally, two species names that can be matched via fuzzy-matching (maximum string-dist of two) are also connected with an edge.
#' To find these, each species name is matched against the whole database (excluding its own name).
#'
#' From the output of [matching()], all unmatched species are matched to all three backbones via `matching(c('WFO', 'WCVP', 'GBIF'))`.
#' The functions checks vertices that are at most `max_iter` (default = 3) edges apart in the graph `g`.
#' For multiple matches, the algorithm always selects the first match, i.e. the target vertex with lower `ID_matched` in `Treemendous.Trees` to ensure reproducibility.
#' By default, the function allows a maximum depth of three steps to search for an match in the target backbone, with the output field `enforced_matching_dist` denoting the depth of the match for each species (1, 2, or 3).
#' Filtering by this column allows the user to be more restrictive (depth $=1$), at the cost of incorrectly missing some matches, or be increasingly permissive with the matches (depth $=2$ or $3$), at the cost of potentially lumping species together.
#' Depending on the application, these different scenarios may be more or less preferable, and can be selected on a case-by-case basis.
#'
#'
#' @param df `tibble` which is the output of `matching()` or `sequential_matching()` and therefor contains the columns `Matched.Genus` and `Matched.Species`. May contain additional columns, which will be ignored.
#' @param backbone specifies which backbone is used: needs to be one of `c('BGCI', 'WCVP', 'WFO', 'GBIF')`.
#' @param target_df is used if the user wants to provide a custom target dataset. The parameter is intended only for compatibility with the function translate_trees and should not be directly used.
#' @param max_iter maximum distance (depth) in the graph for two species to be successfully enforce matched.
#'
#' @return
#' A `tibble` with matched species in `Matched.Genus` and `Matched.Species`.
#' Along with the process information of `matching()`, the function returns the logical column `enforced_matched`, stating whether the species was successfully matched by `enforce_matching()`, and the distance in the neighborhood graph `g`.
#'
#' @export
#'
#' @examples
#' output <- iucn %>% matching('BGCI') %>% enforce_matching('BGCI')
#' output %>% summarize_output()
enforce_matching <- function(df, backbone, target_df = NULL, max_iter = 3){
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

  ## backbones used to create synonym-accepted graph.
  bb <- c('WFO', 'WCVP', 'GBIF')

  ## see if unmatched could be matched to other backbones containing information on synonyms
  output_matching <- unmatched %>% matching(bb) %>%
    dplyr::select(Orig.Genus, Orig.Species,
                  Matched.Genus, Matched.Species,
                  matched) #%>% dplyr::left_join(get_db(backbone) %>% dplyr::select(Genus, Species, ID_merged), by = c('Matched.Species' = 'Species', 'Matched.Genus' = 'Genus'))
  new_matched <- output_matching %>% dplyr::filter(matched == T) %>% dplyr::select(-'matched')
  still_unmatched <- output_matching %>% dplyr::filter(matched == F) %>% dplyr::select(-'matched')

  ## matched analogues in database
  new_matched_in_db <- get_db() %>%
    dplyr::semi_join(new_matched, by = c('Genus' = 'Matched.Genus', 'Species' = 'Matched.Species')) %>%
    dplyr::select(Genus, Species, ID_merged)

  ## create undirected graph
  g <- create_undirected_synonym_graph()


  ## distance 1 adjancency matrix
  dist1 <- igraph::get.adjacency(g)
  diag(dist1) <- 0

  ids_matched <- as.character(new_matched_in_db$ID_merged)
  new_matched <- dplyr::bind_cols(new_matched, 'matched_id' = ids_matched)
  ids_matched_in_g <- ids_matched[ids_matched %in% rownames(dist1)]
  ids_matched_not_in_g <-ids_matched[!(ids_matched %in% rownames(dist1))]

  # only when called through translate_trees
  if(!is.null(target_df) & backbone == 'CUSTOM'){
    ## let's change how we get the target ID's... like this also inexact from the target to the database are considered
    target_matches_in_db <- matching(target_df, backbone = bb)
    ids_matched_targets <- target_matches_in_db %>%
      dplyr::select('Matched.Genus', 'Matched.Species') %>%
      dplyr::rename('Genus' = 'Matched.Genus', 'Species' = 'Matched.Species') %>%
      dplyr::left_join(get_db(bb), by = c('Genus', 'Species')) %>% # get ID's of matched targets
      dplyr::select(ID_merged) %>%
      dplyr::distinct() %>% .$ID_merged %>% as.character()
  }
  else{
    ids_matched_targets <- get_db(backbone, target_df) %>% .$ID_merged %>% as.character()
  }

  #ids_target <- get_db(backbone, target_df) %>% .$ID_merged %>% as.character()
  ids_matched_targets <- ids_matched_targets[!is.na(ids_matched_targets)] ## remove NA's that result target species outside the database.
  ## TODO: get closest species in g for target...
  ## Alternatively, increase the list of target_ids by appending the fuzzy matched ones that are actually in g.
  ids_matched_target_in_g <- ids_matched_targets[ids_matched_targets %in% rownames(dist1)]
  ids_matched_target_not_in_g <- ids_matched_targets[!(ids_matched_targets %in% rownames(dist1))]

  ids_to_be_processed <- ids_matched_in_g
  for(i in 1:max_iter){
    if(length(ids_to_be_processed) == 0){
      break
    }
    message(paste('iteration ', i, '/', max_iter, ' ...', sep = ''))
    ## raise the adjacency matrix to the i'th power
    if(i == 1){
      adjacency_matrix <- dist1
    }
    else{
      adjacency_matrix <- adjacency_matrix %*% dist1
    }
    ## caused errors: diag(adjacency_matrix) <- 0 # set to zero, so that no circles are considered
    assertthat::assert_that(Matrix::isSymmetric(adjacency_matrix), msg = "Error in creating adjacency matrix: resulting matriced should by symmetric because we are having an undirected graph.")

    ## get indices which can be connected to target_db
    if(length(ids_to_be_processed) == 1){
      ids_with_neighbour <- ids_to_be_processed[sum(adjacency_matrix[ids_to_be_processed, ids_matched_target_in_g]) >= 1]
    }
    else{
      ids_with_neighbour <- ids_to_be_processed[Matrix::rowSums(adjacency_matrix[ids_to_be_processed, ids_matched_target_in_g]) >= 1]
    }
    if(length(ids_with_neighbour) == 0){
      next
    }

    ## get index of corresponding neighbour
    # next line is the bottleneck for speed: how can we improve the way we extract the id's of the all neighbours which are in the target??
    all_neighbours_in_target <- lapply(ids_with_neighbour, FUN = function(x) names(adjacency_matrix[x,ids_matched_target_in_g][adjacency_matrix[x, ids_matched_target_in_g] >= 1]))
    ## if multiple matches: select first neighbour in target
    if(length(all_neighbours_in_target) > 0){
      one_neighbour_in_target <- sapply(all_neighbours_in_target, FUN = function(x) ifelse(length(x) >= 1, x[1], x))
    }

    ## save pairs of ID's
    if(exists('matched_and_neighbour')){
      new <- tibble::tibble('matched_id' = ids_with_neighbour, 'neighbour_in_targetbb' = one_neighbour_in_target, 'enforced_matching_dist' = i, 'matched' = TRUE, 'enforced_matched' = TRUE)
      matched_and_neighbour <- matched_and_neighbour %>% dplyr::bind_rows(new)
    }
    else{
      matched_and_neighbour <- tibble::tibble('matched_id' = ids_with_neighbour, 'neighbour_in_targetbb' = one_neighbour_in_target, 'enforced_matching_dist' = i, 'matched' = TRUE, 'enforced_matched' = TRUE)
    }


    ## don't consider successfully enforce matched species for further enforce matching
    ids_to_be_processed <- ids_to_be_processed[! ids_to_be_processed %in% matched_and_neighbour$matched_id]
  }

  if(!exists('matched_and_neighbour')){
    ## this creates empty output tibble with the correct column data types! Important if matched_and_neighbour is empty
    matched_and_neighbour <- tibble::tibble('matched_id' = '123', 'neighbour_in_targetbb' = '123', 'enforced_matching_dist' = i, 'matched' = TRUE, 'enforced_matched' = TRUE) %>%
      dplyr::sample_n(size = 0)
  }

  enforce_matched <- matched_and_neighbour %>%
    dplyr::left_join(dplyr::select(new_matched, c('Orig.Genus', 'Orig.Species', 'matched_id')), by = 'matched_id') %>%
    dplyr::mutate(neighbour_in_targetbb = as.integer(neighbour_in_targetbb)) %>%
    dplyr::left_join(
      get_db(bb) %>% dplyr::select(c('Genus', 'Species', 'ID_merged')),
      by = c('neighbour_in_targetbb' = 'ID_merged'), na_matches = 'never') %>%
    dplyr::rename('Matched.Genus' = 'Genus', 'Matched.Species' = 'Species') %>%
    dplyr::select(-c('matched_id', 'neighbour_in_targetbb')) %>%
    dplyr::relocate(c('Orig.Genus', 'Orig.Species', 'Matched.Genus', 'Matched.Species'))

  # only when called through translate_trees
  if(!is.null(target_df) & backbone == 'CUSTOM'){
    ## additional step: check if resulting matched names are in target: if not, then we matched to something that fuzzy matches to a target species, and we need to switch again the names with the target names
    enforce_matched_right_target <- enforce_matched %>% dplyr::semi_join(target_df, by = c('Matched.Genus' = 'Genus', 'Matched.Species' = 'Species'))
    enforce_matched_fuzzymatched_target <- enforce_matched %>% dplyr::anti_join(target_df, by = c('Matched.Genus' = 'Genus', 'Matched.Species' = 'Species'))
    enforce_matched_fuzzymatched_target_backtransformed <- enforce_matched_fuzzymatched_target %>%
      dplyr::left_join(
        dplyr::rename(
          dplyr::select(target_matches_in_db, c('Orig.Genus', 'Orig.Species', 'Matched.Genus', 'Matched.Species')),
          'Orig.Target.Genus' = 'Orig.Genus', 'Orig.Target.Species' = 'Orig.Species'
        ), by = c("Matched.Genus", "Matched.Species")
      ) %>%
      dplyr::select(-c('Matched.Genus', 'Matched.Species')) %>%
      dplyr::rename('Matched.Genus' = 'Orig.Target.Genus', 'Matched.Species' = 'Orig.Target.Species')
    enforce_matched <- dplyr::bind_rows(enforce_matched_right_target, enforce_matched_fuzzymatched_target_backtransformed)
  }

  successfull <- enforce_matched %>% dplyr::filter(matched == TRUE)
  non_successfull <- unmatched %>% dplyr::anti_join(dplyr::bind_rows(successfull, still_unmatched), by = c('Orig.Genus', 'Orig.Species'))

  ## rerun all unsuccessfully matched species to get correct process information
  all_unmatched <- dplyr::bind_rows(still_unmatched, non_successfull) %>% dplyr::mutate('enforced_matched' = FALSE) %>% matching(backbone, target_df)
  all_matched <- dplyr::bind_rows(matched, successfull)

  res <- dplyr::bind_rows(all_unmatched, all_matched)
  assertthat::assert_that(nrow(res) == nrow(df), msg = "Number of input species must agree with number of output species.")

  res
}
