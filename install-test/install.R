library(devtools)
## install.packages("reticulate")
## library(reticulate)

token = Sys.getenv("GITHUB_PAT")
devtools::install_github("a-kulkarn/qualtrics-thinkgrid",
                         subdir="ThinkingGrid",
                         auth_token = token,
                         INSTALL_opts = "--install-tests")

## Run install.
library(ThinkingGrid)
reticulate::install_python()
ThinkingGrid::install_thinkgrid()
library(ThinkingGrid)

## Run the tests.
library(testthat)
testthat::test_package("ThinkingGrid")
