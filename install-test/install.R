library(devtools)
token = Sys.getenv("GITHUB_PAT")
devtools::install_github("a-kulkarn/qualtrics-thinkgrid",
                         subdir="ThinkingGrid",
                         auth_token = token,
                         INSTALL_opts = "--install-tests")

## Run the tests.
library(ThinkingGrid)
library(testthat)
testthat::test_package("ThinkingGrid")

