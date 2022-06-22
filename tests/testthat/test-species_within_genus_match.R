test_that("all species within genus names matched in test", {
 test4_reduced <- test4 %>% dplyr::slice(1:10)
 df <- test4_reduced %>% direct_match() %>% genus_match() %>% fuzzy_match_genus() %>% species_within_genus_match()
 expect_true(all(df$species_within_genus_match))
 expect_true(all(df$Matched.Species %in% test4_reduced$Orig.Species))
}
)
