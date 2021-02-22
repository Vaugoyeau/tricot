test_that("calcule grid size and show error if needed", {
  expect_true(sum(tricot::grid_size(5, 5, tricot::square_size(35,26)) == c(grid.height = 18, grid.width = 13)) == 2)
  expect_error(tricot::square_size("text"))
})
