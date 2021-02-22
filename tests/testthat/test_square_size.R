test_that("calcule square size and show error if needed", {
  expect_true(sum(tricot::square_size(31,24) == c(height = 0.323, width = 0.417)) == 2)
  expect_error(tricot::square_size("text"))
})
