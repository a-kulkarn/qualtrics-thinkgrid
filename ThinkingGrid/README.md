# ThinkingGrid <img src="man/figures/logo.jpg" align="right" />

Package that provides users functions to create qualtrics, extract and analyze data, and visualize effects related to the Thinking Grid (add Irving cite here). 

## Installation

### Latest development build

To install the ThinkingGrid package, run the following code in your R environment. 

```r
if (!require("devtools")) {
    install.packages("devtools", dependencies = TRUE)}

devtools::install_github("a-kulkarn/qualtrics-thinkgrid", subdir="ThinkingGrid", auth_token = token, INSTALL_opts = "--install-tests")
```
## Usage

The ThinkingGrid package uses Python in the back-end to run some functions. For complete functionality, please run the following code before using the package for the first time.

```r
ThinkingGrid::install_thinkgrid()
```

For description of each function run the code below.

```r
?package_name()

for example:

?install_thinkgrid()

```

## Citation

If you use this package please cite the manuscript below.

put cite here.