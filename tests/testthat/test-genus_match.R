test_that("all direct matches", {
  expect_true(all(genus_match(test1)$genus_match))
  expect_true(all(genus_match(test2)$genus_match))
  expect_true(all(genus_match(test3)$genus_match))
})

test_that("no direct matches", {
  expect_false(any(genus_match(test4)$genus_match))
})

