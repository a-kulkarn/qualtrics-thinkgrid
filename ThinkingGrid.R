library(reticulate)

use_python("/usr/bin/python3")
# check if pandas is installed and save the result to a variable
pandas_installed <- py_module_available("pandas")
# if not installed install it
if (!pandas_installed) {
  py_install("pandas")
}

# import pandas
pd <- import("pandas")

# create function called 'generate_survey'
generate_survey <- function(x, y, z) {
    # input variables:
    ## survey_setup_file
    ## output_file_name
    ## question_text

    # returns nothing
    # creates a qsf of the name output_file_name
    return(0)
}

read_qualtrics_data <- function(x, y){
    # input variables:
    ## data_file: qualtrics output
    ## setup_file: survey setup file
    ## note to self: change these variable names to match with generate survey
    return(0)
}