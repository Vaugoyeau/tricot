test_that(
  "calcule knitting image", 
  {
    expect_true(
      sum(
        class(
          knitting_image(
            image_load("img/Pingouin.png"), 
            c("grid.height" = 31, "grid.width" = 24)
          )
        ) == "ggplot"
      ) > 0)
    expect_error(knitting_image("text"))
  })

