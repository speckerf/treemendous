
test_that("correct matches for test6 dataset", {
  df <- get_testset(mutation = 0) %>% matching()
  expect_true(all(df$direct_match))
  df <- get_testset(mutation = 1) %>% matching()
  expect_false(any(df$direct_match))
  expect_true(all(df$genus_match))
  expect_true(all(df$suffix_match_species_within_genus | df$fuzzy_match_species_within_genus))
})

test_that("test random characters", {
  set.seed(111)
  random <- tibble::tibble(Genus = stringi::stri_rand_strings(10, length = 6, '[a-z]'),
                        Species = stringi::stri_rand_strings(10, length = 8, '[a-z]'))
  matched_random <- random %>% dplyr::mutate(Genus = stringr::str_to_title(Genus)) %>% matching()
  expect_false(any(matched_random$matched) | any(matched_random$direct_match) | any(matched_random$genus_match) | any(matched_random$fuzzy_match_genus))
  expect_true(all(is.na(matched_random)[,c('Matched.Genus', 'Matched.Species', 'direct_match_species_within_genus', 'suffix_match_species_within_genus', 'fuzzy_match_species_within_genus', 'fuzzy_genus_dist', 'fuzzy_species_dist')]))
})

test_that("test empty dataframe Genus, Species", {
  res <- Trees.Full %>% dplyr::sample_n(0) %>% dplyr::select(Genus, Species) %>% matching()
  expect_true(nrow(res) == 0)
  expect_true(all(c("Matched.Genus", 'Matched.Species') %in% colnames(res)))
  res <- Trees.Full %>% dplyr::sample_n(0) %>% dplyr::select(Genus, Species) %>% dplyr::rename(Orig.Genus = Genus, Orig.Species = Species) %>% matching()
  expect_true(nrow(res) == 0)
  expect_true(all(c("Matched.Genus", 'Matched.Species') %in% colnames(res)))
})
