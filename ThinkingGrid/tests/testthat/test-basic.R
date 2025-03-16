## Test that basic functionality works.

setup_file <- system.file("test_data", "testQuestion.csv", package = "ThinkingGrid")
data_file <- system.file("test_data", "testQuestionOutput.csv", package = "ThinkingGrid")
expected_result <- system.file("test_data", "testExpectedResult.csv", package = "ThinkingGrid")

dc <- sample(1:6, 100, replace = TRUE)
ac <- sample(1:6, 100, replace = TRUE)
id <- rep(1:5, times = 20)
condition <- rep(c("a", "a", "a", "b", "b"), 20)

skip_if_no_pandas <- function() {
  have_pandas <- reticulate::py_module_available("pandas")
  if (!have_pandas)
    skip("pandas not available for testing")
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


test_that("plot_tg is working", {
    expect_type(
        plot_tg(dc, ac, proportion_type = "overall", type = "cells"),
        "list"
    )
})

test_that("plot_tg is working", {
    expect_type(
        create_tg_animation(dc, ac, condition_col = id, proportion_type = "overall", type = "cells"),
        "list"
    )
})