test_that("correct one character fuzzy match", {
  res <- test2 %>% genus_match() %>% fuzzy_match_species_within_genus()
  expect_true(all(res$Matched.Species %in% test1$Orig.Species))
  expect_true(all(res$fuzzy_species_dist == 1))
})

test_that("transposition of adjacent characters: expect distance one based on optimal string alignment distance (osa): see stringdist", {
  set.seed(100)
  df <- Trees.Full %>% select(Genus, Species) %>% rename(Orig.Genus = Genus, Orig.Species = Species) %>% sample_n(10)
  transposed_species <- c('argryophylla', 'ledremannii', 'heliantheomides', 'chainum', 'samabc',
                          'labiflora', 'idspermum', 'acdia', 'parivfolia', 'philippinenssi')
  transposed_df <- tibble::tibble(Orig.Genus = df$Orig.Genus, Orig.Species = transposed_species) %>%
    genus_match() %>% fuzzy_match_species_within_genus()
  expect_true(all(transposed_df$Matched.Species %in% df$Orig.Species))
  expect_true(all(transposed_df$fuzzy_species_dist == 1))
})

test_that("correct two character fuzzy match", {
  set.seed(100)
  df <- Trees.Full %>% select(Genus, Species) %>% rename(Orig.Genus = Genus, Orig.Species = Species) %>% sample_n(10)
  transposed_species <- c('argirophyla', 'ledermani', 'heliantemoidas', 'chyannum', 'sbac',
                          'alpiiflora', 'disspermium', 'acciida', 'barvyfolia', 'philipinnensis')
  transposed_df <- tibble::tibble(Orig.Genus = df$Orig.Genus, Orig.Species = transposed_species) %>%
    genus_match() %>% fuzzy_match_species_within_genus()
  expect_true(all(transposed_df$Matched.Species %in% df$Orig.Species))
  expect_true(all(transposed_df$fuzzy_species_dist == 2))
})
