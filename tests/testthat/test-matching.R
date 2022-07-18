
test_that("correct matches for test6 dataset", {
  #correct_names <- c('Juglans allardiana', 'Conyza cayennensis', 'Leandra grandifolia', 'Chionanthus verruculatus', 'Tephrosia mariana', 'Stachytarpheta caudata', 'Cedrela brunellioides', 'Cereus mallisonii', 'Citrus kinokuni', 'Myrciaria maschalantha')
  #correct_dataset <- correct_names %>% tibble::tibble() %>%
    #tidyr::separate(".", into = c('Orig.Genus', 'Orig.Species'), sep = ' ')
  matched_test6 <- test6 %>% matching()
  #expect_true(all(matched_test6$Matched.Genus %in% test1$Orig.Genus)) # not working at the moment: net reconsideration: test fails when for instance Genus Ileo / Ilea would exist and we have input Ile
  #expect_true(all(matched_test6$Matched.Species %in% test1$Orig.Species)) # if genus match was wrong, we were seeking in the wrong genus for species names
  expect_equal(sum(matched_test6$direct_match), 10)
  expect_equal(sum(matched_test6$genus_match, na.rm = TRUE), 10)
  expect_equal(sum(matched_test6$fuzzy_match_genus, na.rm = TRUE), 20)
})

test_that("test random characters", {
  set.seed(111)
  random <- tibble::tibble(Genus = stringi::stri_rand_strings(10, length = 6, '[a-z]'),
                        Species = stringi::stri_rand_strings(10, length = 8, '[a-z]'))
  matched_random <- random %>% dplyr::mutate(Genus = stringr::str_to_title(Genus)) %>% matching()
  expect_false(any(matched_random$matched) | any(matched_random$direct_match) | any(matched_random$genus_match) | any(matched_random$fuzzy_match_genus))
  expect_true(all(is.na(matched_random)[,c('Matched.Genus', 'Matched.Species', 'direct_match_within_genus', 'suffix_match_species_within_genus', 'fuzzy_match_species_within_genus', 'fuzzy_genus_dist', 'fuzzy_species_dist')]))
})

test_that("test empty dataframe Genus, Species", {
  res <- Trees.Full %>% dplyr::sample_n(0) %>% dplyr::select(Genus, Species) %>% matching()
  expect_true(nrow(res) == 0)
  res <- Trees.Full %>% dplyr::sample_n(0) %>% dplyr::select(Genus, Species) %>% dplyr::rename(Orig.Genus = Genus, Orig.Species = Species) %>% matching()
  expect_true(nrow(res) == 0)
  expect_true(all(c("Matched.Genus", 'Matched.Species') %in% colnames(res)))
})
