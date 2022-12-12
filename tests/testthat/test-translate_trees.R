test_that("test with large target db", {
  target_test <- get_testset(backbone = 'BGCI', n = 5000) %>%
    dplyr::rename(Genus = Orig.Genus, Species = Orig.Species)
  input_test <- get_testset(n = 3) %>%
    dplyr::rename(Genus = Orig.Genus, Species = Orig.Species)
  output <- translate_trees(input_test, target_test)
  expect_true(nrow(output) == 3)
})

test_that("test empty df", {
  target_test <- get_testset(backbone = 'BGCI', n = 10) %>%
    dplyr::rename(Genus = Orig.Genus, Species = Orig.Species)
  input_test <- get_testset(n = 0) %>%
    dplyr::rename(Genus = Orig.Genus, Species = Orig.Species)
  output <- translate_trees(input_test, target_test)
  expect_true(nrow(output) == 0)
})

test_that("test single non-existent species", {
  target_test <- get_testset(backbone = 'BGCI', n = 10) %>%
    dplyr::rename(Genus = Orig.Genus, Species = Orig.Species)
  input_test <- tibble::tibble(Genus = 'Acer', Species = 'asdfghjkyxcvbnm')
  output <- translate_trees(input_test, target_test)
  expect_true(nrow(output) == 1)
  expect_false(any(output$matched, output$enforced_matched))
})

test_that("test known species and their distances in the graph",{
  target_test <- get_db('BGCI') %>% dplyr::select(Genus, Species) # all BGCI speices
  input_test <- tibble::tibble(Genus = c('Parinari', 'Atuna', 'Parinari', 'Maranthes'),
                               Species = c('laurina', 'racemosa', 'racemosa', 'corymbosa'))
  output <- translate_trees(input_test, target_test)
  expect_true(nrow(output) == 4)
  expect_true(all(c(NA, 1, 2, 3) %in% output$enforced_matching_dist))
  expect_equal(sum(output$enforced_matched, na.rm = T), 3)
})

test_that("test target df outside Treemendous", {
  # 1: species that is in Treemendous
  # 2: species that is in target
  # 3-5: fuzzy match to target
  # 6-9: should be enforced matched (see test above)
  target_test <- tibble::tibble(Genus = c('Fagus', 'Rainer', 'Gibtsnicht', 'Gibtsnicht', 'Foobar', 'Maranthes'), Species = c('sylvatica', 'zufall', 'gabsnie', 'wirdsniegeben', 'foobar', 'corymbosa'))
  input_test <- tibble::tibble(Genus = c('Fagus', 'Rainer', 'Gibtsnicht', 'Gibtsnichtx', 'Foobara', 'Parinari', 'Atuna', 'Parinari', 'Maranthes'), Species = c('sylvatica', 'zufall', 'gabsniex', 'wirdsniegeben', 'foobara', 'laurina', 'racemosa', 'racemosa', 'corymbosa'))
  output <- translate_trees(input_test, target_test)
  expect_true(nrow(output) == 9)
  expect_true(all(output$Matched.Species %in% c('sylvatica', 'zufall', 'gabsnie', 'wirdsniegeben', 'foobar', 'corymbosa')))
  expect_true(all(c(NA, 1, 2, 3) %in% output$enforced_matching_dist))
  expect_equal(sum(output$enforced_matched, na.rm = T), 3)
})

test_that("test fia target, input not able to be matched to target", {
  target_test <- fia
  input_test <- tibble::tibble(Genus = 'Acer', Species = 'acuminatum')
  output <- translate_trees(input_test, target_test)
  expect_true(nrow(output) == 1)
  expect_false(any(output$matched, output$enforced_matched))
})


