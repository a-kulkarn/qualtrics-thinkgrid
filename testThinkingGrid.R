# import the ThinkingGrid script
# source("ThinkingGrid.R")
setup_file <- "test_setup_file/testQuestion.csv"
data_file <- "test_qualtrics_output/testQuestionOutput.csv"
generate_survey(setup_file, "qsf_output_files/vishal_test", question_text = TRUE)
output_table <- read_qualtrics_data(data_file, setup_file)
