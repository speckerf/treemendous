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
  random <- tibble::tibble(Genus = sapply(vector("list", 10), FUN = function(x) paste(sample(letters, size = 6, replace = TRUE), collapse = '')),
                        Species = sapply(vector("list", 10), FUN = function(x) paste(sample(letters, size = 8, replace = TRUE), collapse = '')))
  matched_random <- random %>% dplyr::mutate(Genus = stringr::str_to_title(Genus)) %>% matching()
  expect_false(any(matched_random$matched) | any(matched_random$direct_match) | any(matched_random$genus_match) | any(matched_random$fuzzy_match_genus))
  expect_true(all(is.na(matched_random)[,c('Matched.Genus', 'Matched.Species', 'direct_match_species_within_genus', 'suffix_match_species_within_genus', 'fuzzy_match_species_within_genus', 'fuzzy_genus_dist', 'fuzzy_species_dist')]))
})

test_that("test empty dataframe Genus, Species", {
  res <- get_db() %>% dplyr::sample_n(0) %>% dplyr::select(Genus, Species) %>% matching()
  expect_true(nrow(res) == 0)
  expect_true(all(c("Matched.Genus", 'Matched.Species') %in% colnames(res)))
  res <- get_db() %>% dplyr::sample_n(0) %>% dplyr::select(Genus, Species) %>% dplyr::rename(Orig.Genus = Genus, Orig.Species = Species) %>% matching()
  expect_true(nrow(res) == 0)
  expect_true(all(c("Matched.Genus", 'Matched.Species') %in% colnames(res)))
})

test_that("test inconsistent input types", {
  ###
  # These cases should cause errors
  ###

  ## NA in input
  input <- tibble::tibble(Genus = c('Fagus'), Species = c(NA))
  expect_error(input %>% matching())
  input <- tibble::tibble(Genus = c(NA), Species = c('sylvatica'))
  expect_error(input %>% matching())
  ## duplicates
  input <- tibble::tibble(Genus = rep('Fagus', 2), Species = rep('sylvatica', 2))
  expect_error(input %>% matching())
  ## Genus first letter upper case
  input <- tibble::tibble(Genus = c('fagus'), Species = c('sylvatica'))
  expect_error(input %>% matching())
  ## Only one uppercase letter in Genus
  input <- tibble::tibble(Genus = c('Fagus-Pinus'), Species = c('sylvatica'))
  expect_error(input %>% matching())
  input <- tibble::tibble(Genus = c('FAGUS'), Species = c('sylvatica'))
  expect_error(input %>% matching())
  input <- tibble::tibble(Genus = c('FAgus'), Species = c('sylvatica'))
  expect_error(input %>% matching())
  ## No uppercase letter in Species
  input <- tibble::tibble(Genus = c('Fagus'), Species = c('Sylvatica'))
  expect_error(input %>% matching())
  input <- tibble::tibble(Genus = c('Fagus'), Species = c('sylvaTica'))
  expect_error(input %>% matching())
  input <- tibble::tibble(Genus = c('Fagus'), Species = c('SYLVATICA'))
  expect_error(input %>% matching())
  ## no hybrid characters in Genus name
  input <- tibble::tibble(Genus = c('Fagus\u00D7pinus'), Species = c('sylvatica'))
  expect_error(input %>% matching())
  input <- tibble::tibble(Genus = c('\u00D7Fagus'), Species = c('sylvatica'))
  expect_error(input %>% matching())
  input <- tibble::tibble(Genus = c('Fagus\u00D7'), Species = c('sylvatica'))
  expect_error(input %>% matching())

  ###
  # These cases should trigger warnings
  ###
  ## trailing spaces
  input <- tibble::tibble(Genus = c('Fagus'), Species = c('sylvatica '))
  expect_warning(input %>% matching())
  input <- tibble::tibble(Genus = c('Fagus '), Species = c('sylvatica'))
  expect_warning(input %>% matching())
  ## leading spaces
  input <- tibble::tibble(Genus = c('Fagus'), Species = c(' sylvatica'))
  expect_warning(input %>% matching())
  input <- tibble::tibble(Genus = c(' Fagus'), Species = c('sylvatica'))
  expect_warning(input %>% matching())
  ## hybrid characters in species names
  input <- tibble::tibble(Genus = c('Fagus'), Species = c('sylva\u00D7pendula'))
  expect_warning(input %>% matching())

  ###
  # These should trigger messages
  ###
  ## input is data.frame and not tibble::tibble
  input <- data.frame(Genus = c('Fagus'), Species = c('sylvatica'))
  expect_message(input %>% matching())

})

