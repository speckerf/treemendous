test_that("all direct matches", {
  expect_true(all(direct_match(test1)$direct_match))
})

test_that("no direct matches", {
  expect_false(any(direct_match(test2)$direct_match))
})

test_that("some direct matches", {
  df <- direct_match(test3)
  expect_equal(df$direct_match, rep(c(TRUE, FALSE), each = 5))
  expect_true(df$direct_match[df$Orig.Species == 'moragana'])
  expect_false(df$direct_match[df$Orig.Species == 'moragan'])
})
