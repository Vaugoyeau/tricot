test_that("load image and show error if needed", {
  expect_true(class(tricot::image_load("img/PeintureLaetitia.jpg")) == "magick-image")
  expect_error(tricot::image_load("text"))
})
