test_that("correct output columns", {
  backbones <- c('WFO', 'WCVP', 'GBIF', 'BGCI', 'PM', 'FIA')
  df <- get_testset() %>% sequential_matching(backbones)
  expect_true(all(df$matched))
})
