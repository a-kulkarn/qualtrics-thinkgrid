# ThinkingGrid <img src="ThinkingGrid/man/figures/logo.jpg" align="right" />

An **R** package for analyzing data from "Thinking Grid" style experiments.
Package that provides users functions to create qualtrics, extract and
analyze data, and visualize effects related to the Thinking Grid (add
Irving cite here).

## Installation

### Step -2. Create the Personal Access Token (PAT)

**NOTICE:** Once the repo goes public, we won't have to worry about the tokens

Follow the instructions at the link below to create a *classic* PAT.

https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens#creating-a-personal-access-token-classic

You'll want to grant _repo_ permissions to the token.

### Step -1. Install the package using R.

Open an *R* session, and run the following:
```
GITHUB_PAT <- YOUR_GENERATED_PAT_HERE
devtools::install_github("a-kulkarn/qualtrics-thinkgrid",
                         subdir = "ThinkingGrid",
                         auth_token = GITHUB_PAT,
                         INSTALL_opts = "--install-tests")
```
If all went well, you should have successfully downloaded the package source from the repo. 

### Step 0. Install the package using R. (Public)

_TODO: Not implemented while repo is still private._

Open an *R* session, and run the following:
```
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

_WARNING:_ Our package does not work with python <=3.10. We can look into the issue on request.

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

As of July 18, 2024, the package has the following vignettes:
- **generate_survey_read_data**: A walkthrough of how to generate a Qualtric survey and read the data back into R.
- **plot_2x2**: A walkthrough of how to visualize the Thinking Grid data in a 2x2 plot and how to extract depths from the quadrants.
- **plot_tg**: A walkthrough of how to visualize proportions of responses across the Thinking Grid and also how to create an animation of the proportions changing over time or conditions.

One can access the inline documentation in the usual way. For example:
```r
?install_thinkgrid
```

## Citation

If you use this package please cite the manuscript below.

**TODO: put citation info here.**
