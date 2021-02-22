test_that(
  "calcule knitting image", 
  {
    expect_true(
      sum(
        class(
          tricot::knitting_image(
            tricot::image_load("img/Pingouin.png"), 
            c("grid.height" = 31, "grid.width" = 24)
          )
        ) == "ggplot"
      ) > 0)
    expect_error(tricot::knitting_image("text"))
  })

