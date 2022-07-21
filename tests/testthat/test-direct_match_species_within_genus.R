test_that("all species within genus names matched in test", {
 df <- get_testset(mutation = 2) %>%
   direct_match() %>% genus_match() %>% fuzzy_match_genus() %>% direct_match_species_within_genus()
 expect_true(all(df$direct_match_species_within_genus))
 expect_true(all(df$Matched.Species %in% df$Orig.Species))
})
