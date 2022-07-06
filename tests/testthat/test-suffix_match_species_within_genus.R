test_that("all suffix ending matches", {
  test_dat <- tibble::tibble(Orig.Genus = rep('Abarema', 4), Orig.Species = c('angulatum', 'abbottiae', 'arboroides', 'pseudotamarindi'))
  df <- test_dat %>% direct_match() %>% genus_match() %>% species_within_genus_match() %>% suffix_match_species_within_genus()
  expect_true(all(df$suffix_match_species_within_genus))
})
