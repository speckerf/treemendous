test_that("all fuzzy matches", {
  df <- test4 %>% fuzzy_match_genus()
  expect_true("Matched.Genus" %in% colnames(df))
  expect_true(all(df$fuzzy_match_genus))
  expect_true(all(df$fuzzy_genus_dist == 1))
})
