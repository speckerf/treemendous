test_that("all direct matches", {
  expect_true(all(genus_match(get_testset(mutation = 0))$genus_match))
  expect_true(all(genus_match(get_testset(mutation = 1))$genus_match))
})

test_that("no direct matches", {
  expect_false(any(genus_match(get_testset(mutation = 2))$genus_match))
  expect_false(any(genus_match(get_testset(mutation = 3))$genus_match))
})

