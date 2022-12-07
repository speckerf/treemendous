test_that("correct output columns", {
  backbones <- c('WFO', 'WCVP', 'GBIF', 'BGCI')
  df <- get_testset() %>% sequential_matching(backbones)
  expect_true(all(df$matched))
  expect_true(all(df$Matched.Backbone %in% backbones))
})

test_that("empty df input", {
  backbones <- c('WFO', 'WCVP', 'GBIF', 'BGCI')
  df_empty <- get_testset() %>% dplyr::sample_n(0) %>% sequential_matching(backbones)
  expect_true(nrow(df_empty) == 0)
  expect_true(all(c('Matched.Backbone', 'Matched.Genus', 'Matched.Species') %in% colnames(df_empty)))
})
