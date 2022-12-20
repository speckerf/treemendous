test_that("test empty dataframe Genus, Species", {
  res <- get_db() %>% dplyr::sample_n(0) %>% dplyr::select(Genus, Species) %>% matching() %>% resolve_synonyms()
  expect_true(nrow(res) == 0)
  expect_true(all(c("Matched.Genus", 'Matched.Species', 'Accepted.Genus', 'Accepted.Species') %in% colnames(res)))
  res <- get_db() %>% dplyr::sample_n(0) %>% dplyr::select(Genus, Species) %>% dplyr::rename(Orig.Genus = Genus, Orig.Species = Species) %>% matching() %>% resolve_synonyms()
  expect_true(nrow(res) == 0)
  expect_true(all(c("Matched.Genus", 'Matched.Species', 'Accepted.Genus', 'Accepted.Species') %in% colnames(res)))
})

test_that("test different matching and resolving backbones", {
  backbones <- c('BGCI', 'WFO', 'WCVP', 'GBIF')
  set.seed(111)
  df <- get_db() %>% dplyr::sample_n(10) %>% dplyr::select(Genus, Species) %>% matching('WFO')
  expect_warning(df %>% resolve_synonyms('GBIF'))
})

test_that("test different individual backbones", {
  backbones <- c('BGCI', 'WFO', 'WCVP', 'GBIF')
  set.seed(111)
  for(bb in backbones){
    res <- get_db() %>% dplyr::sample_n(1) %>% dplyr::select(Genus, Species) %>% matching(bb) %>% resolve_synonyms(bb)
    expect_true(nrow(res) == 1)
    expect_true(all(c("Matched.Genus", 'Matched.Species', 'Accepted.Genus', 'Accepted.Species') %in% colnames(res)))
  }
})

test_that("test multiple backbones", {
  backbones <- c('BGCI', 'WFO', 'WCVP', 'GBIF')
  for(i in 1:3){
    bbs <- backbones[sample(c(TRUE, FALSE), size = 4, replace = TRUE)]
    if(length(bbs) == 0){
      bbs = c('WFO', 'WCVP')
    }
    res <- get_db() %>% dplyr::sample_n(1) %>% dplyr::select(Genus, Species) %>% matching(bbs) %>% resolve_synonyms(bbs)
    expect_true(nrow(res) == 1)
    expect_true(all(c("Matched.Genus", 'Matched.Species', 'Accepted.Genus', 'Accepted.Species') %in% colnames(res)))
  }
})

