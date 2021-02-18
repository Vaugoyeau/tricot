test_that(
  "calcule knitting image", 
  {
    expect_true(
      sum(
        class(
          wool_weigth_needed(
            knitting_image(
              image_load(
                "img/Pingouin.png"
              ), 
              grid_size(
                30, 
                30, 
                square_size(
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
    expect_error(wool_weigth_needed("text"))
  })

