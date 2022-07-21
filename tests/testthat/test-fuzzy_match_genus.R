test_that("all fuzzy matches", {
  df1 <- get_testset(mutation = 2) %>% fuzzy_match_genus()
  df2 <- get_testset(mutation = 3) %>% fuzzy_match_genus()
  dfs <- list(df1, df2)
  for(df in dfs){
    expect_true("Matched.Genus" %in% colnames(df1))
    expect_true(all(df$fuzzy_match_genus))
    expect_true(all(df$fuzzy_genus_dist <= 1)) ## should be equal 1: but due to a coincidence removeing  the last character led to another genus name
  }
})
