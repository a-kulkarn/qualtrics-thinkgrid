## Test that basic functionality works.

setup_file <- system.file("test_data", "testQuestion.csv", package = "ThinkingGrid")
data_file <- system.file("test_data", "testQuestionOutput.csv", package = "ThinkingGrid")
expected_result <- system.file("test_data", "testExpectedResult.csv", package = "ThinkingGrid")

set.seed(42)
dc <- sample(1:6, 100, replace = TRUE)
ac <- sample(1:6, 100, replace = TRUE)
id <- rep(1:5, times = 20)
condition <- rep(c("a", "a", "a", "b", "b"), 20)

mock_tg_frame <- data.frame(
    Deliberate.Constraints = dc,
    Automatic.Constraints = ac,
    id = id,
    condition = condition
)

skip_if_no_pandas <- function() {
  have_pandas <- reticulate::py_module_available("pandas")
  if (!have_pandas)
    testthat::skip("pandas not available for testing")
}

test_that("generate_survey does not crash", {
    expect_equal(
        generate_survey(
            setup_file,
            "output_file",
            question_text = TRUE
        ),
        0
    )
})

test_that("read_qualtrics_data does not crash", {
    expect_equal(read_qualtrics_data(data_file, setup_file),
                 read.csv(expected_result, header = TRUE))
})

test_that("test pandas availability", {
    skip_if_no_pandas()
    expect_type(
        reticulate::import("pandas"),
        "environment"
    )
})


## ------------------------------------------------------------------------

test_that("plot_tg produces correct plot", {
    res <- plot_tg(mock_tg_frame, proportion_type = "overall", type = "cells")
    vdiffr::expect_doppelganger("plot_tg_overall_cells", res$plot)
})

test_that("plot_tg produces correct plot for quadrants", {
    res <- plot_tg(mock_tg_frame, proportion_type = "overall", type = "quadrants")
    vdiffr::expect_doppelganger("plot_tg_overall_quadrants", res$plot)
})

test_that("plot_tg produces correct plot for horizontal", {
    res <- plot_tg(mock_tg_frame, proportion_type = "overall", type = "horizontal")
    vdiffr::expect_doppelganger("plot_tg_overall_horizontal", res$plot)
})

test_that("plot_tg produces correct plot for vertical", {
    res <- plot_tg(mock_tg_frame, proportion_type = "overall", type = "vertical")
    vdiffr::expect_doppelganger("plot_tg_overall_vertical", res$plot)
})

test_that("plot_tg produces correct plot for constraints", {
    res <- plot_tg(mock_tg_frame, proportion_type = "overall", type = "constraints")
    vdiffr::expect_doppelganger("plot_tg_overall_constraints", res$plot)
})

test_that("plot_tg produces correct plot for depth", {
    res <- plot_tg(mock_tg_frame, proportion_type = "overall", type = "depth")
    vdiffr::expect_doppelganger("plot_tg_overall_depth", res$plot)
})

## ------------------------------------------------------------------------

#Tests for condition plots without comparison_type (use "separate")
test_that("plot_tg produces correct plot for condition (separate) with type = cells", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "cells", condition_column = "condition")
  vdiffr::expect_doppelganger("plot_tg_separate_cells", res$plot)
})

test_that("plot_tg produces correct plot for condition (separate) with type = quadrants", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "quadrants", condition_column = "condition")
  vdiffr::expect_doppelganger("plot_tg_separate_quadrants", res$plot)
})

test_that("plot_tg produces correct plot for condition (separate) with type = horizontal", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "horizontal", condition_column = "condition")
  vdiffr::expect_doppelganger("plot_tg_separate_horizontal", res$plot)
})

test_that("plot_tg produces correct plot for condition (separate) with type = vertical", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "vertical", condition_column = "condition")
  vdiffr::expect_doppelganger("plot_tg_separate_vertical", res$plot)
})

test_that("plot_tg produces correct plot for condition (separate) with type = constraints", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "constraints", condition_column = "condition")
  vdiffr::expect_doppelganger("plot_tg_separate_constraints", res$plot)
})

test_that("plot_tg produces correct plot for condition (separate) with type = depth", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "depth", condition_column = "condition")
  vdiffr::expect_doppelganger("plot_tg_separate_depth", res$plot)
})

## ------------------------------------------------------------------------
## Tests for condition plots with comparison_type = "difference"

test_that("plot_tg produces correct plot for condition (difference) with type = cells", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "cells", condition_column = "condition", comparison_type = "difference")
  vdiffr::expect_doppelganger("plot_tg_difference_cells", res$plot)
})

test_that("plot_tg produces correct plot for condition (difference) with type = quadrants", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "quadrants", condition_column = "condition", comparison_type = "difference")
  vdiffr::expect_doppelganger("plot_tg_difference_quadrants", res$plot)
})

test_that("plot_tg produces correct plot for condition (difference) with type = horizontal", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "horizontal", condition_column = "condition", comparison_type = "difference")
  vdiffr::expect_doppelganger("plot_tg_difference_horizontal", res$plot)
})

test_that("plot_tg produces correct plot for condition (difference) with type = vertical", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "vertical", condition_column = "condition", comparison_type = "difference")
  vdiffr::expect_doppelganger("plot_tg_difference_vertical", res$plot)
})

test_that("plot_tg produces correct plot for condition (difference) with type = constraints", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "constraints", condition_column = "condition", comparison_type = "difference")
  vdiffr::expect_doppelganger("plot_tg_difference_constraints", res$plot)
})

test_that("plot_tg produces correct plot for condition (difference) with type = depth", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "depth", condition_column = "condition", comparison_type = "difference")
  vdiffr::expect_doppelganger("plot_tg_difference_depth", res$plot)
})

## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

test_that("create_tg_animation is working with type = cells", {
    expect_type(
        create_tg_animation(mock_tg_frame, condition_column = "id", proportion_type = "overall", type = "cells", filename = "gifs/cells.gif"),
        "list"
    )
})

test_that("create_tg_animation is working with type = quadrants", {
    expect_type(
        create_tg_animation(mock_tg_frame, condition_column = "id", proportion_type = "overall", type = "quadrants", filename = "gifs/quadrants.gif"),
        "list"
    )
})

test_that("create_tg_animation is working with type = horizontal", {
    expect_type(
        create_tg_animation(mock_tg_frame, condition_column = "id", proportion_type = "overall", type = "horizontal", filename = "gifs/horizontal.gif"),
        "list"
    )
})

test_that("create_tg_animation is working with type = vertical", {
    expect_type(
        create_tg_animation(mock_tg_frame, condition_column = "id", proportion_type = "overall", type = "vertical", filename = "gifs/vertical.gif"),
        "list"
    )
})

test_that("create_tg_animation is working with type = constraints", {
    expect_type(
        create_tg_animation(mock_tg_frame, condition_column = "id", proportion_type = "overall", type = "constraints", filename = "gifs/constraints.gif"),
        "list"
    )
})

test_that("create_tg_animation is working with type = depth", {
    expect_type(
        create_tg_animation(mock_tg_frame, condition_column = "id", proportion_type = "overall", type = "depth", filename = "gifs/depth.gif"),
        "list"
    )
})
