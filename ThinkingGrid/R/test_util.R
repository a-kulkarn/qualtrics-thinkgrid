
load_dev_test_data <- function() {
    setup_file <- system.file("test_data", "testQuestion.csv", package = "ThinkingGrid")
    data_file <- system.file("test_data", "testQuestionOutput.csv", package = "ThinkingGrid")
    expected_result <- system.file("test_data", "testExpectedResult.csv", package = "ThinkingGrid")

    dc <- sample(1:6, 100, replace = TRUE)
    ac <- sample(1:6, 100, replace = TRUE)
    id <- rep(1:5, times = 20)
    condition <- rep(c("a", "a", "a", "b", "b"), 20)

    return(read_qualtrics_data(data_file, setup_file))
}
