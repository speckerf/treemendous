test_that("one step: returns character", {
  res <- fia %>% dplyr::sample_n(10) %>% matching() %>% summarize_output()
  expect_type(res, "character")
})

test_that("two step: returns list", {
  res <- fia %>% dplyr::sample_n(10) %>% matching() %>% resolve_synonyms() %>% summarize_output()
  expect_type(res, "list")
})

