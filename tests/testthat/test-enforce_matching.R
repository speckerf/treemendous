test_that("empty df", {
  df <- get_testset(n=0) %>% matching('WFO') %>% enforce_matching('WFO')
  expect_true(nrow(df) == 0)
  expect_true(tibble::is_tibble(df))
})

test_that("test all different backbones", {
  backbones <- c('BGCI', 'WFO', 'WCVP', 'GBIF')
  for(bb in backbones){
    df <- get_testset(n=3) %>% matching(bb) %>% enforce_matching(bb)
    expect_true(nrow(df) == 3)
    expect_true(tibble::is_tibble(df))
  }
})

test_that("different backbones in matching and enforce_matching", {
  df <- get_testset(n=5) %>% matching('BGCI') %>% enforce_matching('WFO')
  expect_true(nrow(df) == 5)
  expect_true(tibble::is_tibble(df))
})


