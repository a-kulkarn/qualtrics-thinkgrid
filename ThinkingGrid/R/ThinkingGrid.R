library(reticulate)

# run terminal command in R
# py_path <- system("python -c 'import os, sys; print(os.path.dirname(sys.executable))'", intern = TRUE)
# # check if default python installation exists
# if (py_path == ""){
#     py_path <- system("which python", intern = TRUE)
# }
# use_python(paste0(py_path, "/python"))
# check if pandas is installed in the python environment
# py_pandas <- system("python -c 'import pandas as pd; print(pd.__version__)'", intern = TRUE)

py_script_path <- function(script_name){
    return(system.file("python", script_name, package="ThinkingGrid"))
}

py_module_path <- function(){
    return(system.file("python", package="ThinkingGrid"))
}


#' @export
generate_survey <- function(survey_setup_file,
                            output_file_name="output_survey",
                            question_text = TRUE) {
    mod <- reticulate::import_from_path("generate_survey", path = py_module_path())
    mod$generate_survey(survey_setup_file, output_file_name, question_text)
    return(0)
}

#' @export
read_qualtrics_data <- function(data_file, setup_file){
    mod <- reticulate::import_from_path("read_qualtrics_data", path = py_module_path())
    res <- reticulate::py_to_r(mod$read_qualtrics_data(data_file, setup_file))

    ## Fixup attributes for comparison's sake.
    attr(res, "pandas.index") <- NULL
    return(res)
}

#' @export
test_func <- function(){
    print("Hello World")
}

# setup_file <- "test_setup_file/testQuestion.csv"
# data_file <- "test_qualtrics_output/testQuestionOutput.csv"
# Qdata <- read_qualtrics_data(data_file, setup_file)
