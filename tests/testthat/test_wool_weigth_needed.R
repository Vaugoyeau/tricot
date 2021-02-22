test_that(
  "calcule knitting image", 
  {
    expect_true(
      sum(
        class(
          tricot::wool_weigth_needed(
            tricot::knitting_image(
              tricot::image_load(
                "img/Pingouin.png"
              ), 
              tricot::grid_size(
                30, 
                30, 
                tricot::square_size(
                  35,
                  26
                )
              )
            ),
            35,
            26,
            100
          )
        ) == "datatables"
      ) > 0)
    expect_error(tricot::wool_weigth_needed("text"))
  })

