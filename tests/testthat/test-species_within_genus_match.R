test_that("all species within genus names matched in test", {
 df <- test4 %>% direct_match() %>% genus_match() %>% fuzzy_match_genus() %>% species_within_genus_match()
 expect_true(all(df$species_within_genus_match))
}
)
