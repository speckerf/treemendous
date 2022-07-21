test_that("all direct matches", {
  expect_true(all(direct_match(get_testset(mutation = 0))$direct_match))
})

test_that("no direct matches", {
  expect_false(any(direct_match(get_testset(mutation = 1))$direct_match))
  expect_false(any(direct_match(get_testset(mutation = 2))$direct_match))
})
