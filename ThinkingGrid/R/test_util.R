
load_dev_test_data <- function() {
    setup_file <- system.file("extdata", "sample_setup_file.csv", package = "ThinkingGrid")
    data_file <- system.file("extdata", "sample_qualtrics_output.csv", package = "ThinkingGrid")
    expected_result <- system.file("extdata", "sample_qualtrics_extracted.csv", package = "ThinkingGrid")

    dc <- sample(1:6, 100, replace = TRUE)
    ac <- sample(1:6, 100, replace = TRUE)
    id <- rep(1:5, times = 20)
    condition <- rep(c("a", "a", "a", "b", "b"), 20)

    return(read_qualtrics_data(data_file, setup_file))
}
