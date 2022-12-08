test_that("correct one character fuzzy match", {
  df <- get_testset(backbone = 'BGCI', mutation = 1) %>%
    genus_match() %>% fuzzy_match_species_within_genus()
  expect_true(all(df$Matched.Species %in% get_testset(backbone = 'BGCI', mutation = 0)$Orig.Species))
  expect_true(all(df$fuzzy_species_dist == 1))
})

test_that("transposition of adjacent characters: expect distance one based on optimal string alignment distance (osa): see stringdist", {
  #set.seed(100)
  #df <- Trees.Full %>% dplyr::select(Genus, Species) %>% dplyr::rename(Orig.Genus = Genus, Orig.Species = Species) %>% dplyr::sample_n(10)
  genera <- c("Deguelia", "Koanophyllon", "Maerua", "Dortmanna", "Photinia", "Salvia", "Dolicholus", "Mimosa", "Freycinetia", "Photinia")
  species <- c("chrysophylla","plicatum", "schinzii", "fervens", "chingshuiensis", "brachystachys", "pringlei", "weddelliana", "impavida", "zhijiangensis")
  df <- tibble::tibble('Orig.Genus' = genera, 'Orig.Species' = species)
  ## check if these species still present in database: else test examples has to be changed
  assertthat::assert_that(nrow(df) == nrow(df %>% dplyr::semi_join(get_db(), by = c('Orig.Genus' = 'Genus', 'Orig.Species' = 'Species'))))

  ## introduce transposition errors
  transposed_species <- c("chrysohpylla","pliactum", "schinizi", "fevrens", "chingshiuensis", "brachystachsy", "rpinglei", "weddleliana", "ipmavida", "zhiijangensis")
  transposed_df <- tibble::tibble(Orig.Genus = df$Orig.Genus, Orig.Species = transposed_species) %>%
    genus_match() %>% fuzzy_match_species_within_genus()
  expect_true(all(transposed_df$Matched.Species %in% df$Orig.Species))
  expect_true(all(transposed_df$fuzzy_species_dist == 1))
})

test_that("correct two character fuzzy match", {
  #set.seed(100)
  #df <- Trees.Full %>% dplyr::select(Genus, Species) %>% dplyr::rename(Orig.Genus = Genus, Orig.Species = Species) %>% dplyr::sample_n(10)
  genera <- c("Deguelia", "Koanophyllon", "Maerua", "Dortmanna", "Photinia", "Salvia", "Dolicholus", "Mimosa", "Freycinetia", "Photinia")
  species <- c("chrysophylla","plicatum", "schinzii", "fervens", "chingshuiensis", "brachystachys", "pringlei", "weddelliana", "impavida", "zhijiangensis")
  df <- tibble::tibble('Orig.Genus' = genera, 'Orig.Species' = species)

  transposed_species <- c("chryssophyla","pcatum", "shinzi", "fervennes", "chiingshuienis", "braschystacchys", "inglei", "wedeliana", "impavi", "ijiangensis")
  transposed_df <- tibble::tibble(Orig.Genus = df$Orig.Genus, Orig.Species = transposed_species) %>%
    genus_match() %>% fuzzy_match_species_within_genus()
  expect_true(all(transposed_df$Matched.Species %in% df$Orig.Species))
  expect_true(all(transposed_df$fuzzy_species_dist == 2))
})
