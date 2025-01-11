library(devtools)
token = Sys.getenv("GITHUB_PAT")
devtools::install_github("a-kulkarn/qualtrics-thinkgrid", subdir="ThinkingGrid",
                         auth_token = token)
