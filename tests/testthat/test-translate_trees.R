test_that("test with large target db", {
  target_test <- get_testset(backbone = 'BGCI', n = 50000) %>%
    dplyr::rename(Genus = Orig.Genus, Species = Orig.Species)
  input_test <- get_testset(n = 3) %>%
    dplyr::rename(Genus = Orig.Genus, Species = Orig.Species)
  output <- translate_trees(input_test, target_test)
  expect_true(nrow(output) == 3)
})

test_that("test empty df", {
  target_test <- get_testset(backbone = 'BGCI', n = 50000) %>%
    dplyr::rename(Genus = Orig.Genus, Species = Orig.Species)
  input_test <- get_testset(n = 0) %>%
    dplyr::rename(Genus = Orig.Genus, Species = Orig.Species)
  output <- translate_trees(input_test, target_test)
  expect_true(nrow(output) == 0)
})

test_that("test large target df and large input", {
  target_test <- get_testset(backbone = 'BGCI', n = 50000) %>%
    dplyr::rename(Genus = Orig.Genus, Species = Orig.Species)
  input_test <- get_testset(backbone = 'WFO', n = 1000) %>%
    dplyr::rename(Genus = Orig.Genus, Species = Orig.Species)
  output <- translate_trees(input_test, target_test)
  expect_true(nrow(output) == 1000)
})


