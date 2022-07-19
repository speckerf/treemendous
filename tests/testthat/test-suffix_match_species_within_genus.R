test_that("all suffix ending matches", {
  #test_dat <- tibble::tibble(Orig.Genus = rep('Abarema', 4), Orig.Species = c('angulatum', 'abbottiae', 'arboroides', 'pseudotamarindi'))
  test_dat <- tibble::tibble(Orig.Genus = rep('Abarema', 3), Orig.Species = c('angulatum', 'abbottiae', 'pseudotamarindi'))
  df <- test_dat %>% direct_match() %>% genus_match() %>% direct_match_species_within_genus() %>% suffix_match_species_within_genus()
  expect_true(all(df$suffix_match_species_within_genus))
})
