## code to prepare `edges_fuzzy_matched` dataset goes here
devtools::load_all()

library(doParallel)

#Leave one core to avoid overload your computer
cluster <- makeForkCluster(4)
registerDoParallel(cluster)

# delete log file if already exists
log_file_path <- fs::path('data-raw', 'enfore_fuyyy_matched.log')
if(fs::file_exists(log_file_path)){
  fs::file_delete(log_file_path)
}

# delete files with edges if already exist
for(file in list.files('data-raw/edges_in_genus', pattern = '.csv', full.names = TRUE)){
  fs::file_delete(file)
}



library(foreach)
.fuzzy_graph_helper <- function(genus, species, database_subset){
  single_species <- tibble::tibble(Genus = genus, Species = species)
  database_subset_withoutself <- database_subset %>% dplyr::anti_join(single_species, by = c("Genus", "Species"))
  assertthat::assert_that(nrow(database_subset_withoutself) + nrow(single_species) == nrow(database_subset))
  single_species %>% fuzzyjoin::stringdist_left_join(database_subset_withoutself,
                                                     by = c('Species'),
                                                     distance_col = 'fuzzy_species_dist')
}

all_genera <- Treemendous.Trees$Genus %>% unique()
# all_genera <- all_genera[1:20] # for debugging
all_connections <- tibble::tibble('Genus.x' = 'a', 'Species.x' = 'a', 'Genus.y' = 'a', 'Species.y' = 'a', 'fuzzy_species_dist' = 1) %>% dplyr::sample_n(size = 0)

# execute every genus in parallel
all_connections <- foreach(i = 1:length(all_genera), .combine=rbind) %dopar% {
  genus <- all_genera[i]
  message(paste0("Processing genus ", i , ":", length(all_genera), " --- ", genus))

  line=paste0("Processing genus ", i , ":", length(all_genera), ", ", genus)
  write(line,file=log_file_path,append=TRUE)



  treemendous_subset <- memoised_get_trees_of_genus(genus)

  fuzzy <- mapply(FUN = .fuzzy_graph_helper, treemendous_subset$Genus, treemendous_subset$Species, MoreArgs = list(treemendous_subset), SIMPLIFY = F)
  fuzzy_tibble <- dplyr::bind_rows(fuzzy) %>% tidyr::drop_na()
  #all_connections <- dplyr::bind_rows(all_connections, fuzzy_tibble)
  if(nrow(fuzzy_tibble) > 0){
    write.csv(fuzzy_tibble,file=paste0('data-raw/edges_in_genus/edges_', i, '.csv'), row.names=FALSE)
  }
}

#Stop cluster
stopCluster(cluster)


## load again all the tibble and rbind the
require(plyr)
connections <- ldply(fs::path('data-raw', 'edges_in_genus', list.files(path = 'data-raw/edges_in_genus')), read.csv, header=TRUE) %>%
  tidyr::drop_na() %>% dplyr::filter(fuzzy_species_dist == 1)

connections_ids <- connections %>%
  dplyr::left_join(dplyr::select(Treemendous.Trees, c('Genus', 'Species', 'ID_merged')),
                   by = c('Genus.x' = 'Genus', 'Species.x' = 'Species')) %>%
  dplyr::left_join(dplyr::select(Treemendous.Trees, c('Genus', 'Species', 'ID_merged')),
                   by = c('Genus.y' = 'Genus', 'Species.y' = 'Species')) %>%
  dplyr::select(c('ID_merged.x', 'ID_merged.y')) %>%
  dplyr::rename('from' = 'ID_merged.x', 'to' = 'ID_merged.y')

edges_fuzzy_matched <- connections_ids %>% tidyr::drop_na() # somehow missing ids are present, why? only keep non-missing rows

if(fs::dir_exists(fs::path('data-raw', 'add_to_sysdata'))){
  saveRDS(edges_fuzzy_matched, file = fs::path('data-raw', 'add_to_sysdata', 'edges_fuzzy_matched.rds'))
}

# usethis::use_data(edges_fuzzy_matched, overwrite = TRUE, internal = TRUE)




# connections <- all_connections %>% tidyr::drop_na()
# connections_ids <- connections %>%
#   dplyr::left_join(dplyr::select(Treemendous.Trees, c('Genus', 'Species', 'ID_merged')),
#                    by = c('Genus.x' = 'Genus', 'Species.x' = 'Species')) %>%
#   dplyr::left_join(dplyr::select(Treemendous.Trees, c('Genus', 'Species', 'ID_merged')),
#                    by = c('Genus.y' = 'Genus', 'Species.y' = 'Species')) %>%
#   dplyr::select(c('ID_merged.x', 'ID_merged.y')) %>%
#   dplyr::rename('from' = 'ID_merged.x', 'to' = 'ID_merged.y')
# edges_fuzzy_matched <- connections_ids
#
# usethis::use_data(edges_fuzzy_matched, overwrite = TRUE, internal = TRUE)

# get_edges_fuzzy_matched <- function(){
#   .fuzzy_graph_helper <- function(genus, species, database_subset){
#     single_species <- tibble::tibble(Genus = genus, Species = species)
#     database_subset_withoutself <- database_subset %>% dplyr::anti_join(single_species, by = c("Genus", "Species"))
#     assertthat::assert_that(nrow(database_subset_withoutself) + nrow(single_species) == nrow(database_subset))
#     single_species %>% fuzzyjoin::stringdist_left_join(database_subset_withoutself,
#                                                        by = c('Species'),
#                                                        distance_col = 'fuzzy_species_dist')
#   }
#
#   all_genera <- Treemendous.Trees$Genus %>% unique()
#   all_connections <- tibble::tibble('Genus.x' = 'a', 'Species.x' = 'a', 'Genus.y' = 'a', 'Species.y' = 'a', 'fuzzy_species_dist' = 1) %>% dplyr::sample_n(size = 0)
#
#   all_connections <- foreach(i = 1:length(all_genera), .combine=cbind) %dopar% {
#     genus <- all_genera[i]
#     message(paste0("Processing genus: ", genus))
#     treemendous_subset <- memoised_get_trees_of_genus(genus)
#
#     fuzzy <- mapply(FUN = .fuzzy_graph_helper, treemendous_subset$Genus, treemendous_subset$Species, MoreArgs = list(treemendous_subset), SIMPLIFY = F)
#     fuzzy_tibble <- dplyr::bind_rows(fuzzy)
#     all_connections <- dplyr::bind_rows(all_connections, fuzzy_tibble)
#     # if(genus == 'Abbottia'){
#     #   break
#     # }
#   }
#   connections <- all_connections %>% tidyr::drop_na()
#   connections_ids <- connections %>%
#     dplyr::left_join(dplyr::select(Treemendous.Trees, c('Genus', 'Species', 'ID_merged')),
#                      by = c('Genus.x' = 'Genus', 'Species.x' = 'Species')) %>%
#     dplyr::left_join(dplyr::select(Treemendous.Trees, c('Genus', 'Species', 'ID_merged')),
#                      by = c('Genus.y' = 'Genus', 'Species.y' = 'Species')) %>%
#     dplyr::select(c('ID_merged.x', 'ID_merged.y')) %>%
#     dplyr::rename('from' = 'ID_merged.x', 'to' = 'ID_merged.y')
#   connections_ids
#
# }
#
# edges_fuzzy_matched <- get_edges_fuzzy_matched()
