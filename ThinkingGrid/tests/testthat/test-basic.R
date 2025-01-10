test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

## # import the ThinkingGrid script
## # source("ThinkingGrid.R")

setup_file <- system.file("test_data", "testQuestion.csv", package = "ThinkingGrid")
data_file <- "test_qualtrics_output/testQuestionOutput.csv"

test_that("generate_survey does not crash", {
    expect_equal(
        generate_survey(
            setup_file,
            "../outputQSF/output_file",
            question_text = TRUE
        ),
        0
    )
})

## test_that("read_qualtrics_data does not crash", {
##     expect_no_error(read_qualtrics_data(
##         "test_qualtrics_output/testQuestionOutput.csv",
##         system.file("test_data", "testQuestion.csv", package = "ThinkingGrid")
##     ))
## })

