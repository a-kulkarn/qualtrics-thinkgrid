
test_that("Grid draws 2x2 overlay", {
    foo <- ThinkingGrid:::create_test_2x2_plots()
    p_sticky <- foo$p_sticky
    p_salience <- foo$p_salience
    p_free <- foo$p_free
    p_directed <- foo$p_directed

    obj <- ThinkingGrid::thinkgrid_quadrant_plot(p_sticky, p_salience, p_free, p_directed)    
    vdiffr::expect_doppelganger("plot_2x2", obj)
})


test_that("Grid overlays images", {
    foo <- ThinkingGrid:::create_test_2x2_plots()
    p_sticky <- foo$p_sticky
    p_salience <- foo$p_salience
    p_free <- foo$p_free
    p_directed <- foo$p_directed

    img_path <- system.file("test_data", "rabbiduck.png", package = "ThinkingGrid")    
    rabbi <- png::readPNG(img_path)
    rabbigrob <- grid::rasterGrob(rabbi)
    
    obj <- ThinkingGrid::thinkgrid_quadrant_plot(p_sticky, p_salience, p_free, rabbigrob)
    vdiffr::expect_doppelganger("plot_2x2_with_image", obj)
    
})

test_that("Image can be first", {
    foo <- ThinkingGrid:::create_test_2x2_plots()
    p_sticky <- foo$p_sticky
    p_salience <- foo$p_salience
    p_free <- foo$p_free
    p_directed <- foo$p_directed
    
    img_path <- system.file("test_data", "rabbiduck.png", package = "ThinkingGrid")    
    rabbi <- png::readPNG(img_path)
    rabbigrob <- grid::rasterGrob(rabbi)

    obj <- ThinkingGrid::thinkgrid_quadrant_plot(rabbigrob, p_salience, p_free, p_directed)
    vdiffr::expect_doppelganger("plot_2x2_with_image_first", obj)
})

test_that("All plots can be images.", {
    foo <- ThinkingGrid:::create_test_2x2_plots()
    p_sticky <- foo$p_sticky
    p_salience <- foo$p_salience
    p_free <- foo$p_free
    p_directed <- foo$p_directed
    
    img_path <- system.file("test_data", "rabbiduck.png", package = "ThinkingGrid")
    rabbi <- png::readPNG(img_path)
    rabbigrob <- grid::rasterGrob(rabbi)

    obj <- ThinkingGrid::thinkgrid_quadrant_plot(rabbigrob, rabbigrob, rabbigrob, rabbigrob)
    vdiffr::expect_doppelganger("plot_2x2_with_all_images", obj)
})
