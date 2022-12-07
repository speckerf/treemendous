# test_that("empty input", {
#   target_test <- get_testset(n = 50000) %>%
#     dplyr::rename(Genus = Orig.Genus, Species = Orig.Species)
#   input_test <- get_testset(n = 5) %>%
#     dplyr::rename(Genus = Orig.Genus, Species = Orig.Species)
#   output <- translate_trees(input_test, target_test)
#   expect_true(nrow(output) == 5)
# })
