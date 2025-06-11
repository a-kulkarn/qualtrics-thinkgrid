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

pandas <- NULL
skimage <- NULL
matplotlib <- NULL

.onLoad <- function(libname, pkgname) {
    reticulate::use_virtualenv("r-thinkgrid", required = FALSE)
}

#' Illustration of install_thinkgrid function
#'
#' Creates a virtual environment and installs the required python packages
#'
#' @param envname {character, optional} Name of the virtual environment to be created. Default is "r-thinkgrid".
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' install_thinkgrid()
#' install_thinkgrid("new_environment_name")
#' }
#'
#' @export
install_thinkgrid <- function(envname = "r-thinkgrid") {
    if(reticulate::virtualenv_exists(envname = envname) == FALSE) {
        reticulate::virtualenv_create(
                        envname,
                        packages = c("numpy", "pandas", "scikit-image", "matplotlib")
                    )
        print(paste("Created virtual environment", envname))
    } else {
        print(paste("Virtual environment", envname, "already exists"))
    }

    ## Install dependencies.
    reticulate::py_install("pandas", envname = envname)
    reticulate::py_install("scikit-image", envname = envname)
    reticulate::py_install("matplotlib", envname = envname)

    ## Inform.
    writeLines(
        paste(
            "",
            "ThinkingGrid installed successfully! to activate, run",
            "    library(ThinkingGrid)",
            sep = "\n\n"
        )
    )
}

#' Illustration of check_python_available function
#'
#' Checks if python is available. Installs python if appropriate flag is set.
#'
#' @param install_if_NA {logical, optional} If TRUE, installs python 3.13 and required packages. Default is FALSE.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' check_python_available()
#' check_python_available(install_if_NA = TRUE)
#' }
#'
#' @export
check_python_available <- function(install_if_NA = FALSE){
    if(!is.null(reticulate::py_discover_config()$python)){
        py_ver <- reticulate::py_discover_config()$version
        print(paste("Python version", py_ver, "is available"))
    } else {
        print("Python is not available")
        if(install_if_NA == TRUE){
            reticulate::install_python(version = "3.13:latest",)
            install_thinkgrid("r-reticulate")
        }
    }
}

#' Illustration of generate_survey function
#'
#' @param survey_setup_file {character, required} Path to a csv file containing the survey setup. This file MUST have a column called "id". Each row in this column should be unique. Indivudial thinking grids will be created for each row in this column. The other column is called "question". This column contains the question text. Quotes around question text is not required. If the question text is not provided, the function will use default text. Please note that these columns are case sensitive.
#' 
#' File setup for csv file without question text. For this to work the question_text parameter should be set to FALSE. A placeholder text ("Insert text here.") will be used in this case.: 
#' 
#' id
#' 
#' ThinkingGrid1
#' 
#' ThinkingGrid2
#' 
#' ThinkingGrid3
#' 
#' File setup for csv file with question text:
#' 
#' id,question
#' ThinkingGrid1,Report your thoughts on the thinking grid 1
#' ThinkingGrid2,Report your thoughts on the thinking grid 2
#' ThinkingGrid3,Report your thoughts on the thinking grid 3
#' 
#' The above file will create 3 thinking grids with the corresponding questions.
#' 
#' @param output_file_name {character, optional} Name of the output qsf file. Default is "output_survey". The extension qsf will be added automatically. If the desired file name is "my_qsf_output.qsf, the function should be called as generate_survey("path/to/setup/survey_setup_file.csv", "path/to/output/my_qsf_output"). Default name is "output_survey.qsf".
#' If a file with the same name exists, it will be overwritten. 
#'
#' 
#' @param question_text {logical, optional} If TRUE, the function will use the question text provided in the csv file. If FALSE, the function will use a default question text ("Insert text here."). Default is TRUE.
#' 
#' @return None
#'
#' @examples
#' # Generate survey from sample setup file
#' setup_file <- system.file("extdata", "sample_setup_file.csv", package = "ThinkingGrid")
#' if (file.exists(setup_file)) {
#'   generate_survey(setup_file, "_temp_output_")
#'   # Clean up
#'   file.remove("_temp_output_.qsf")
#' }
#' 
#' @export
generate_survey <- function(survey_setup_file,
                            output_file_name="output_survey",
                            question_text = TRUE) {
    mod <- reticulate::import_from_path("generate_survey", path = py_module_path())
    mod$generate_survey(survey_setup_file, output_file_name, question_text)
    return(0)
}


#' Illustration of read_qualtrics_data function
#' 
#' @param data_file {character, needed} The path to where the Qualtrics output is located. This should be a csv file that needs to be provided to this function UNEDITED. 
#' 
#' @param setup_file {character, needed} setup_file used to generate the survey. This file should be the same file that was used to generate the survey. This file should have the same format as the survey_setup_file used in the generate_survey function.
#' 
#' @return data frame containing the Qualtrics data. Columns include "uid", "Probe.Identifier", "Deliberate.Constraints", "Automatic.Constraints"
#' 
#' uid: Unique identifier for each participant. This correspons to the row number in the Qualtrics output file.
#' 
#' Probe.Identifier: Identifier for the probe. This is the same as the "id" column in the setup_file.
#' 
#' Deliberate.Constraints: The deliberate constraints provided by the participant. (X-axis on the thinking grid)
#' 
#' Automatic.Constraints: The automatic constraints provided by the participant. (Y-axis on the thinking grid)
#' 
#' @examples
#' # Read Qualtrics survey data
#' setup_file <- system.file("extdata", "sample_setup_file.csv", package = "ThinkingGrid")
#' data_file <- system.file("extdata", "sample_qualtrics_output.csv", package = "ThinkingGrid")
#' if (file.exists(setup_file) && file.exists(data_file)) {
#'   survey_data <- read_qualtrics_data(data_file, setup_file)
#' }
#' 
#' @export
read_qualtrics_data <- function(data_file, setup_file){
    mod <- reticulate::import_from_path("read_qualtrics_data", path = py_module_path())
    res <- reticulate::py_to_r(mod$read_qualtrics_data(data_file, setup_file))

    ## Fixup attributes for comparison's sake.
    attr(res, "pandas.index") <- NULL
    return(res)
}

#' Illustration of test_func function
#' #' A simple function to test the package.
#' @export
test_func <- function(){
    print("Hello World")
}


#' Illustration of extract_quadrant_depths function
#' 
#' @param data_file {character, needed} Path to csv or excel file containing the data.
#' 
#' @param dc_column {character, optional} Name of the column containing deliberate constraints. Default is "Deliberate.Constraints".
#' 
#' @param ac_column {character, optional} Name of the column containing automatic constraints. Default is "Automatic.Constraints".
#' 
#' @return data frame containing the quadrant depths. Columns include "Free.Depth", "Directed.Depth", "AffDir.Depth", and "Sticky.Depth".
#' 
#' @details 
#' The function calculates the quadrant depths based on the deliberate and automatic constraints provided in the data file. The quadrant depths are calculated using the taxicab norm. Only one depth will be populated per observation, depending on the quadrant the observation falls into. The remaining three depths will be set to 0.
#' 
#' @examples
#' # Calculate quadrant depths from survey data
#' data_file <- system.file("extdata", "sample_data.csv", package = "ThinkingGrid")
#' if (file.exists(data_file)) {
#'   depth_results <- extract_quadrant_depths(data_file, dc_column = "dc", ac_column = "ac")
#' }
#' 
#' @export
extract_quadrant_depths <- function(data_file, dc_column = "Deliberate.Constraints", ac_column = "Automatic.Constraints") {
    mod <- reticulate::import_from_path("extract_quadrant_depths", path = py_module_path())
    res <- reticulate::py_to_r(mod$extract_quadrant_depths(data_file, dc_column, ac_column))
    
    ## Fixup attributes for comparison's sake.
    attr(res, "pandas.index") <- NULL
    return(res)
}

# setup_file <- "test_setup_file/testQuestion.csv"
# data_file <- "test_qualtrics_output/testQuestionOutput.csv"
# Qdata <- read_qualtrics_data(data_file, setup_file)
