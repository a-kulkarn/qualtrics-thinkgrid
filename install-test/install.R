library(devtools)

devtools::install_github("a-kulkarn/qualtrics-thinkgrid",
                         subdir="ThinkingGrid",
                         INSTALL_opts = "--install-tests")

## Run install.
## library(ThinkingGrid)
## reticulate::install_python(version = "3.13:latest")
ThinkingGrid::install_thinkgrid()
library(ThinkingGrid)

## Run the tests.
library(testthat)
testthat::test_package("ThinkingGrid")
