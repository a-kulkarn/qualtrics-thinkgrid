devtools::install_github("a-kulkarn/qualtrics-thinkgrid",
                         subdir="ThinkingGrid",
                         INSTALL_opts = "--install-tests")

## Run install.
## reticulate::install_python(version = "3.13:latest")
ThinkingGrid::install_thinkgrid()

## Install optional dependencies for the tests.
install.packages(c("png", "vdiffr", "lme4", "knitr", "sjPlot"))

## Run the tests.
testthat::test_package("ThinkingGrid")
