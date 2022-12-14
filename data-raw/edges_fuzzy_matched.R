## code to prepare `edges_fuzzy_matched` dataset goes here
get_edges_fuzzy_matched <- function(){
  .fuzzy_graph_helper <- function(genus, species, database_subset){
    single_species <- tibble::tibble(Genus = genus, Species = species)
    database_subset_withoutself <- database_subset %>% dplyr::anti_join(single_species, by = c("Genus", "Species"))
    assertthat::assert_that(nrow(database_subset_withoutself) + nrow(single_species) == nrow(database_subset))
    single_species %>% fuzzyjoin::stringdist_left_join(database_subset_withoutself,
                                                       by = c('Species'),
                                                       distance_col = 'fuzzy_species_dist')
  }

  all_genera <- Treemendous.Trees$Genus %>% unique()
  all_connections <- tibble::tibble('Genus.x' = 'a', 'Species.x' = 'a', 'Genus.y' = 'a', 'Species.y' = 'a', 'fuzzy_species_dist' = 1) %>% dplyr::sample_n(size = 0)
  for(genus in all_genera){
    message(paste0("Processing genus: ", genus))
    treemendous_subset <- memoised_get_trees_of_genus(genus)

    fuzzy <- mapply(FUN = .fuzzy_graph_helper, treemendous_subset$Genus, treemendous_subset$Species, MoreArgs = list(treemendous_subset), SIMPLIFY = F)
    fuzzy_tibble <- dplyr::bind_rows(fuzzy)
    all_connections <- dplyr::bind_rows(all_connections, fuzzy_tibble)
    # if(genus == 'Abbottia'){
    #   break
    # }
  }
  connections <- all_connections %>% tidyr::drop_na()
  connections_ids <- connections %>%
    dplyr::left_join(dplyr::select(Treemendous.Trees, c('Genus', 'Species', 'ID_merged')),
                     by = c('Genus.x' = 'Genus', 'Species.x' = 'Species')) %>%
    dplyr::left_join(dplyr::select(Treemendous.Trees, c('Genus', 'Species', 'ID_merged')),
                     by = c('Genus.y' = 'Genus', 'Species.y' = 'Species')) %>%
    dplyr::select(c('ID_merged.x', 'ID_merged.y')) %>%
    dplyr::rename('from' = 'ID_merged.x', 'to' = 'ID_merged.y')
  connections_ids

}

edges_fuzzy_matched <- get_edges_fuzzy_matched()

usethis::use_data(edges_fuzzy_matched, overwrite = TRUE, internal = TRUE)
