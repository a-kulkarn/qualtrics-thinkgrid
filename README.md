# ThinkingGrid <img src="ThinkingGrid/man/figures/logo.jpg" align="right" />

An **R** package for analyzing data from "Thinking Grid" style experiments.
This package provides functions to create Qualtrics surveys, extract and
analyze data, and visualize effects related to the Thinking Grid.

## Installation

### Step 0. Install the package using R.

#### From CRAN (Recommended)
Open an *R* session, and run the following:
```r
install.packages("ThinkingGrid")
```

#### Development Version (GitHub)
To install the development version:
```r
devtools::install_github("a-kulkarn/qualtrics-thinkgrid", subdir = "ThinkingGrid")
```

### Step 1. Install python dependencies.
This package uses [reticulate](https://rstudio.github.io/reticulate/) to pass data
from a python backend into **R**. 

#### (a). (Optional) Installing python.
If you do not have Python, run
```
library(ThinkingGrid)
reticulate::install_python(version = "3.13:latest")
```

_WARNING:_ This package requires Python 3.11 or higher.

#### (b). Setting up the virtual environment.
If you do not care about which python virtual environment you are using, run
```
ThinkingGrid::install_thinkgrid()
library(ThinkingGrid)
```

On the other hand, if for some reason you need everything to run in a common virtual
environment, run instead:
```
ThinkingGrid::install_thinkgrid(YOUR_ENV_NAME_HERE)
library(ThinkingGrid)
```
We make no guarantees about package conflicts with the existing environment.

## Usage

The exposed functions in this package are listed by:
```
getNamespaceExports("ThinkingGrid")
```

Currently, the package provides functions to:
- Create a Qualtrics survey for a Thinking Grid experiment ```generate_survey()```
- Extract data from a Qualtrics survey ```read_qualtrics_data()```
- Add depths into each of the four quadrants of the Thinking Grid ```add_depths()```
- Visualize graphs in their respective quadrants of the Thinking Grid ```thinkgrid_quadrant_plot()```
- Visualize proportion of responses across various cross-sections of the Thinking Grid ```plot_tg()```
- Create a GIF showing how proportions change across the Thinking Grid over time (or conditions)```create_tg_animation()```

Vignettes are available to help you get started with the package. You can access them by running:
```r
browseVignettes("ThinkingGrid")
```

The package has the following vignettes:
- **generate_survey_read_data**: A walkthrough of how to generate a Qualtrics survey and read the data back into R.
- **plot_2x2**: A walkthrough of how to visualize the Thinking Grid data in a 2x2 plot and how to extract depths from the quadrants.
- **plot_tg**: A walkthrough of how to visualize proportions of responses across the Thinking Grid and also how to create an animation of the proportions changing over time or conditions.

One can access the inline documentation in the usual way. For example:
```r
?install_thinkgrid
```


## Troubleshooting

### I tried installing the developer version of this package, but got this error message:
```
> devtools::install_github("a-kulkarn/qualtrics-thinkgrid", subdir = "ThinkingGrid")

Error: Failed to install 'unknown package' from GitHub:
  HTTP error 401.
  Bad credentials

  Rate limit remaining: X/60
  Rate limit reset at: SOME_TIMESTAMP

```

We have observed that a stale value for `GITHUB_PAT` can cause this problem. Removing
this variable from your environment sometimes fixes the issue. Please refer to
troubleshooting for `install_github`.
