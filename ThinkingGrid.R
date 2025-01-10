library(reticulate)

# run terminal command in R
# py_path <- system("python -c 'import os, sys; print(os.path.dirname(sys.executable))'", intern = TRUE)
# # check if default python installation exists
# if (py_path == ""){
#     py_path <- system("which python", intern = TRUE)
# }
# use_python(paste0(py_path, "/python"))
# check if pandas is installed in the python environment
py_pandas <- system("python -c 'import pandas as pd; print(pd.__version__)'", intern = TRUE)

generate_survey <- function(survey_setup_file, output_file_name="output_survey", question_text = TRUE){
    # impport generate_survey.py
    source_python("generate_survey.py")
    # from generate survey call generate_survey function
    generate_survey(survey_setup_file, output_file_name, question_text)
    return(0)
}

read_qualtrics_data <- function(data_file, setup_file){
    source_python("read_qualtrics_data.py")
    res = read_qualtrics_data(data_file, setup_file)
    return(res)
}

# setup_file <- "test_setup_file/testQuestion.csv"
# data_file <- "test_qualtrics_output/testQuestionOutput.csv"
# Qdata <- read_qualtrics_data(data_file, setup_file)