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
#' Edges represent species names, and vertices represent synonym-accepted relations between two species according to at least one backbone.
#' From the output of `matching()`, all unmatched species are matched to all three backbones via `matching(c('WFO', 'WCVP', 'GBIF'))`.
#' If there is a match, then all neighbors in the graph `g` of the matched species are checked if they belong to the target backbone.
#' The neighbors are also allowed to not directly match, but match according to `matching(target_backbone)` (fuzzy matches, suffix matches).
#' This is repeated for neighbors in the graph `g` up to a distance of three, which is also returned as `enforced_matching_dist` for successful matches. .
#'
#'
#' @param df `tibble` which is the output of `matching()` or `sequential_matching()` and therefor contains the columns `Matched.Genus` and `Matched.Species`. May contain additional columns, which will be ignored.
#' @param backbone specifies which backbone is used: needs to be one of `c('BGCI', 'WCVP', 'WFO', 'GBIF')`.
#' @param target_df is used if the user wants to provide a custom target dataset. The parameter is intended only for compatibility with the function translate_trees and should not be directly used.
#'
#' @return
#' A `tibble` with matched species in `Matched.Genus` and `Matched.Species`.
#' Along with the process information of `matching()`, the function returns the logical column `enforced_matched`, stating whether the species was successfully matched by `enforce_matching()`, and the distance in the neighborhood graph `g`.
#'
#' @export
#'
#' @examples
#' set.seed(321)
#' output <- iucn %>% matching('WCVP') %>% enforce_matching('WCVP')
#' output
#' output %>% summarize_output()
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
    dplyr::select(Genus, Species, dplyr::matches(bb), ID_merged)

  ## create undirected graph
  g <- create_undirected_synonym_graph()


  ## distance 1 adjancency matrix
  dist1 <- igraph::get.adjacency(g)
  diag(dist1) <- 0

  ids_matched <- as.character(new_matched_in_db$ID_merged)
  new_matched <- dplyr::bind_cols(new_matched, 'matched_id' = ids_matched)
  ids_matched_in_g <- ids_matched[ids_matched %in% rownames(dist1)]
  ids_matched_not_in_g <-ids_matched[!(ids_matched %in% rownames(dist1))]
  ids_target <- get_db(backbone, target_df) %>% .$ID_merged %>% as.character()
  ids_target_in_g <- ids_target[ids_target %in% rownames(dist1)]
  ids_target_not_in_g <- ids_target[!(ids_target %in% rownames(dist1))]
  max_iter = 3

  ids_to_be_processed <- ids_matched_in_g
  for(i in 1:max_iter){
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
      ids_with_neighbour <- ids_to_be_processed[sum(adjacency_matrix[ids_to_be_processed, ids_target_in_g]) >= 1]
    }
    else{
      ids_with_neighbour <- ids_to_be_processed[Matrix::rowSums(adjacency_matrix[ids_to_be_processed, ids_target_in_g]) >= 1]
    }
    if(length(ids_with_neighbour) == 0){
      next
    }

    ## get index of corresponding neighbour
    all_neighbours_in_target <- lapply(ids_with_neighbour, FUN = function(x) names(adjacency_matrix[x,ids_target_in_g][adjacency_matrix[x, ids_target_in_g] >= 1]))
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

  ## gives the input species in Orig.Genus, Orig.Species and the enforced matched species (if successful) in Matched.Genus, Matched.Species and NA if not successfully enforce matched.
  enforce_matched <- new_matched %>%
    dplyr::full_join(matched_and_neighbour, by = 'matched_id') %>%
    dplyr::mutate(neighbour_in_targetbb = as.integer(neighbour_in_targetbb)) %>%
    dplyr::left_join(get_db(backbone, target_df) %>% dplyr::select(c('Genus', 'Species', 'ID_merged')), by = c('neighbour_in_targetbb' = 'ID_merged')) %>%
    dplyr::select(-c('Matched.Genus', 'Matched.Species', 'matched_id', 'neighbour_in_targetbb')) %>%
    dplyr::rename('Matched.Genus' = 'Genus', 'Matched.Species' = 'Species') %>%
    dplyr::relocate(c('Orig.Genus', 'Orig.Species', 'Matched.Genus', 'Matched.Species'))

  non_successfull <- enforce_matched %>% dplyr::filter(is.na(matched)) %>% dplyr::select(-c('matched', 'enforced_matching_dist'))
  successfull <- enforce_matched %>% dplyr::filter(matched == TRUE)
  assertthat::assert_that(nrow(non_successfull) + nrow(successfull) == nrow(enforce_matched))


  ## rerun all unsuccessfully matched species to get correct process information
  all_unmatched <- dplyr::bind_rows(still_unmatched, non_successfull) %>% dplyr::mutate('enforced_matched' = FALSE) %>% matching(backbone, target_df)
  all_matched <- dplyr::bind_rows(matched, successfull)

  res <- dplyr::bind_rows(all_unmatched, all_matched)
  assertthat::assert_that(nrow(res) == nrow(df), msg = "Number of input species must agree with number of output species.")
  res
}
