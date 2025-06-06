
test_that("Grid draws 2x2 overlay", {
    foo <- create_test_2x2_plots()
    p_sticky <- foo$p_sticky
    p_salience <- foo$p_salience
    p_free <- foo$p_free
    p_directed <- foo$p_directed

    result <- ThinkingGrid::thinkgrid_quadrant_plot(p_sticky, p_salience, p_free, p_directed)
    A <- result[[1]]
    B <- result[[2]]
    
    grid::grid.newpage()  # Clear the graphics device
    grid::grid.draw(B)
    grid::grid.draw(A)
    obj <- grid::grid.grab(wrap.grobs = TRUE)
    vdiffr::expect_doppelganger("plot_2x2", obj)
})


test_that("Grid overlays images", {
    foo <- create_test_2x2_plots()
    p_sticky <- foo$p_sticky
    p_salience <- foo$p_salience
    p_free <- foo$p_free
    p_directed <- foo$p_directed

    img_path <- system.file("test_data", "rabbiduck.png", package = "ThinkingGrid")    
    rabbi <- png::readPNG(img_path)
    rabbigrob <- grid::rasterGrob(rabbi)
    
    result2 <- ThinkingGrid::thinkgrid_quadrant_plot(p_sticky, p_salience, p_free, rabbigrob)

    C <- result2[[1]]
    D <- result2[[2]]

    grid::grid.newpage()  # Clear the graphics device
    grid::grid.draw(D)
    grid::grid.draw(C)

    obj <- grid::grid.grab(wrap.grobs = TRUE)    
    vdiffr::expect_doppelganger("plot_2x2_with_images", obj)    
})

test_that("Image can be first", {
    foo <- create_test_2x2_plots()
    p_sticky <- foo$p_sticky
    p_salience <- foo$p_salience
    p_free <- foo$p_free
    p_directed <- foo$p_directed
    
    img_path <- system.file("test_data", "rabbiduck.png", package = "ThinkingGrid")    
    rabbi <- png::readPNG(img_path)
    rabbigrob <- grid::rasterGrob(rabbi)

    result2 <- ThinkingGrid::thinkgrid_quadrant_plot(rabbigrob, p_salience, p_free, p_directed)

    C <- result2[[1]]
    D <- result2[[2]]

    grid::grid.newpage()  # Clear the graphics device
    grid::grid.draw(D)
    grid::grid.draw(C)
    
    obj <- grid::grid.grab(wrap.grobs = TRUE)    
    vdiffr::expect_doppelganger("plot_2x2_with_image_first", obj)
})

test_that("All plots can be images.", {
    foo <- create_test_2x2_plots()
    p_sticky <- foo$p_sticky
    p_salience <- foo$p_salience
    p_free <- foo$p_free
    p_directed <- foo$p_directed
    
    img_path <- system.file("test_data", "rabbiduck.png", package = "ThinkingGrid")
    rabbi <- png::readPNG(img_path)
    rabbigrob <- grid::rasterGrob(rabbi)

    result2 <- ThinkingGrid::thinkgrid_quadrant_plot(rabbigrob, rabbigrob, rabbigrob, rabbigrob)

    C <- result2[[1]]
    D <- result2[[2]]

    grid::grid.newpage()  # Clear the graphics device
    grid::grid.draw(D)
    grid::grid.draw(C)
    
    obj <- grid::grid.grab(wrap.grobs = TRUE)    
    vdiffr::expect_doppelganger("plot_2x2_with_all_images", obj)
})
