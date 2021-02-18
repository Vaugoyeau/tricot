test_that("load image and show error if needed", {
  expect_true(class(image_load("img/PeintureLaetitia.jpg")) == "magick-image")
  expect_error(image_load("text"))
})
