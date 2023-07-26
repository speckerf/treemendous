test_that("test empty dataframe Genus, Species", {
  res <- get_db() %>% dplyr::sample_n(0) %>% dplyr::select(Genus, Species) %>% matching() %>% resolve_synonyms() %>% highlight_flags()
  expect_true(nrow(res) == 0)
  expect_true(all(c("Matched.Genus", 'Matched.Species', 'Accepted.Genus', 'Accepted.Species') %in% colnames(res)))

  for(bb in c('WFO', 'GBIF', 'WCVP')){
    res <- get_db(bb) %>% dplyr::sample_n(0) %>% dplyr::select(Genus, Species) %>% matching(bb) %>% resolve_synonyms(bb) %>% highlight_flags(bb)
    expect_true(nrow(res) == 0)
    expect_true(all(c("Matched.Genus", 'Matched.Species', 'Accepted.Genus', 'Accepted.Species') %in% colnames(res)))
  }
})

test_that("test all backbones", {
  res <- tibble::tibble(Genus = 'Abies', Species = 'shastensis') %>% matching() %>% resolve_synonyms() %>% highlight_flags()
  expect_true(nrow(res) == 1)
  expect_true(all(c("Matched.Genus", 'Matched.Species', 'Accepted.Genus', 'Accepted.Species') %in% colnames(res)))

  for(bb in c('WFO', 'GBIF', 'WCVP', NULL)){
    # this species should raise a flag for every backbone.
    res <- tibble::tibble(Genus = 'Ilex', Species = 'subrotundifolia') %>% matching(bb) %>% resolve_synonyms(bb) %>% highlight_flags(bb)
    expect_true(nrow(res) == 1)
    expect_true(all(c("Matched.Genus", 'Matched.Species', 'Accepted.Genus', 'Accepted.Species') %in% colnames(res)))
  }
})
