################################################################################
## Python env management.

py_script_path <- function(script_name){
    return(system.file("python", script_name, package="ThinkingGrid"))
}

py_module_path <- function(){
    return(system.file("python", package="ThinkingGrid"))
}

## Module-level variables for delay-loaded Python modules
numpy <- NULL
pandas <- NULL
skimage <- NULL
matplotlib <- NULL
generate_survey_mod <- NULL
read_qualtrics_data_mod <- NULL

.onLoad <- function(libname, pkgname) {
    reticulate::py_require("matplotlib")
    reticulate::py_require("scikit-image")
    reticulate::py_require("numpy")    
    reticulate::py_require("pandas")

    ## Import Python modules with delay_load = TRUE
    numpy <<- reticulate::import("numpy", delay_load = TRUE)
    pandas <<- reticulate::import("pandas", delay_load = TRUE)
    skimage <<- reticulate::import("skimage", delay_load = TRUE)
    matplotlib <<- reticulate::import("matplotlib", delay_load = TRUE)

    # Import custom Python modules from package
    py_path <- system.file("python", package = pkgname)
    if (nzchar(py_path)) {
        generate_survey_mod <<- reticulate::import_from_path(
            "generate_survey",
            path = py_path,
            delay_load = TRUE
        )
        read_qualtrics_data_mod <<- reticulate::import_from_path(
            "read_qualtrics_data",
            path = py_path,
            delay_load = TRUE
        )
    }
}


################################################################################
## Survey functions.

#' Creates a Qualtrics importable survey file from a CSV file of questions.
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
#' \dontrun{
#' # Generate survey from sample setup file (requires Python)
#' setup_file <- system.file("extdata", "sample_setup_file.csv", package = "ThinkingGrid")
#' if (file.exists(setup_file)) {
#'   generate_survey(setup_file, "_temp_output_")
#'   # Clean up
#'   file.remove("_temp_output_-0.qsf")
#' }
#' }
#' 
#' @export
generate_survey <- function(survey_setup_file,
                            output_file_name="output_survey",
                            question_text = TRUE) {
    # Check if Python module is available
    if (is.null(generate_survey_mod)) {
        stop("Python modules not loaded. This may occur if the package is not properly installed. ",
             "Try reinstalling the package or running install_thinkgrid().")
    }

    generate_survey_mod$generate_survey(survey_setup_file, output_file_name, question_text)
    return(0)
}


#' Parses Qualtrics survey output into a dataframe. 
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
#' \dontrun{
#' # Read Qualtrics survey data (requires Python)
#' setup_file <- system.file("extdata", "sample_setup_file.csv", package = "ThinkingGrid")
#' data_file <- system.file("extdata", "sample_qualtrics_output.csv", package = "ThinkingGrid")
#' if (file.exists(setup_file) && file.exists(data_file)) {
#'   survey_data <- read_qualtrics_data(data_file, setup_file)
#' }
#' }
#' 
#' @export
read_qualtrics_data <- function(data_file, setup_file){
    # Check if Python module is available
    if (is.null(read_qualtrics_data_mod)) {
        stop("Python modules not loaded. This may occur if the package is not properly installed. ",
             "Try reinstalling the package or running install_thinkgrid().")
    }

    res <- reticulate::py_to_r(read_qualtrics_data_mod$read_qualtrics_data(data_file, setup_file))

    ## Fixup attributes for comparison's sake.
    attr(res, "pandas.index") <- NULL
    return(res)
}

################################################################################
## Depth calculations.

get_quadrant_6x6 <- function(i, j) {
    (i < 4) && (j < 4) && return(1)
    (i < 4) && (j > 3) && return(2)
    (i > 3) && (j < 4) && return(3)    
    return(4)
}


depth_6x6 <- function(x, y) {
    round(abs(x - 3.5) + abs(y - 3.5))
}

#' Adds taxicab metric calculation columns to a dataframe.
#' 
#' @param data {data.frame, needed} Data frame containing columns for deliberate constraints and automatic constraints.
#' 
#' @param dc {character, optional} Name of the column containing deliberate constraints. Default is "Deliberate.Constraints".
#' 
#' @param ac {character, optional} Name of the column containing automatic constraints. Default is "Automatic.Constraints".
#' 
#' @return data frame containing the quadrant depths. Columns include "sticky", "hybrid", "free", "directed", "total_depth", and "quadrant". The value of quadrant is 1-4, corresponding to top-left, top-right, bottom-left, bottom-right.
#' 
#' @details 
#' The function calculates the quadrant depths based on the deliberate and automatic constraints provided in the data file. The quadrant depths are calculated using the taxicab norm. Only one depth will be populated per observation, depending on the quadrant the observation falls into. The remaining three depths will be set to 0.
#' 
#' @examples
#' # Calculate quadrant depths from survey data
#' data_file <- system.file("extdata", "sample_data.csv", package = "ThinkingGrid")
#' if (file.exists(data_file)) {
#'   data_file <- read.csv(data_file)
#'   depth_results <- add_depths(data_file, dc = "dc", ac = "ac")
#' }
#' 
#' @export
add_depths <- function(data, dc = "Deliberate.Constraints", ac = "Automatic.Constraints") {
    X <- data[, c(dc, ac)]
    d <- apply(X, 1, function(row) depth_6x6(row[1], row[2]))
    q <- apply(X, 1, function(row) get_quadrant_6x6(row[1], row[2]))

    Y <- data.frame(data)
    Y["total_depth"] <- d
    Y["quadrant"] <- q
    Y["sticky"] <- (q == 2) * d
    Y["hybrid"] <- (q == 4) * d    
    Y["free"] <- (q == 1) * d
    Y["directed"] <- (q == 3) * d

    
    return(Y)
}
