test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

## # import the ThinkingGrid script
## # source("ThinkingGrid.R")

setup_file <- system.file("test_data", "testQuestion.csv", package = "ThinkingGrid")
data_file <- system.file("test_data", "testQuestionOutput.csv", package = "ThinkingGrid")
expected_result <- system.file("test_data", "testExpectedResult.csv", package = "ThinkingGrid")

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

