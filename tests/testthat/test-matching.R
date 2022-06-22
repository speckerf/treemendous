
test_that("correct matches for test6 dataset", {
  correct_names <- c('Juglans allardiana', 'Conyza cayennensis', 'Leandra grandifolia', 'Chionanthus verruculatus', 'Tephrosia mariana', 'Stachytarpheta caudata', 'Cedrela brunellioides', 'Cereus mallisonii', 'Citrus kinokuni', 'Myrciaria maschalantha')
  correct_dataset <- correct_names %>% tibble::tibble() %>%
    dplyr::mutate(Genus = sapply(strsplit(correct_names, split = " "), "[[", 1),
                  Species = sapply(strsplit(correct_names, split = " "), "[[", 2)) %>%
    dplyr::select(-c("."))
  matched_test6 <- test6 %>% matching()
  expect_true(all(matched_test6$Matched.Genus %in% correct_dataset$Genus))
  expect_true(all(matched_test6$Matched.Species %in% correct_dataset$Species))
  expect_true(all(matched_test6$matched))
  expect_equal(sum(matched_test6$direct_match), 2)
  expect_equal(sum(matched_test6$genus_match, na.rm = TRUE), 4)
  expect_equal(sum(matched_test6$fuzzy_match_genus, na.rm = TRUE), 4)
  expect_equal(sum(matched_test6$species_within_genus_match, na.rm = TRUE), 2)
  expect_equal(sum(matched_test6$suffix_match_species_within_genus, na.rm = TRUE), 2)
  expect_equal(sum(matched_test6$fuzzy_match_species_within_genus, na.rm = TRUE), 4)
  expect_equal(sum(matched_test6$fuzzy_genus_dist, na.rm = TRUE), 4)
  expect_equal(sum(matched_test6$fuzzy_species_dist, na.rm = TRUE), 6)
})

test_that("test random characters", {
  set.seed(111)
  random <- tibble::tibble(Genus = stringi::stri_rand_strings(10, length = 6, '[a-z]'),
                        Species = stringi::stri_rand_strings(10, length = 8, '[a-z]'))
  matched_random <- random %>% matching()
  expect_false(any(matched_random$matched) | any(matched_random$direct_match) | any(matched_random$genus_match) | any(matched_random$fuzzy_match_genus))
  expect_true(all(is.na(matched_random)[,c('Matched.Genus', 'Matched.Species', 'species_within_genus_match', 'suffix_match_species_within_genus', 'fuzzy_match_species_within_genus', 'fuzzy_genus_dist', 'fuzzy_species_dist')]))
})
