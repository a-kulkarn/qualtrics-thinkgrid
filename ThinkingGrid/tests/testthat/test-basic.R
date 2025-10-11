if (capabilities("cairo")) {
  options(bitmapType = "cairo")
}

# Set consistent ggplot2 theme with explicit parameters
if (requireNamespace("ggplot2", quietly = TRUE)) {
  ggplot2::theme_set(ggplot2::theme_grey(
    base_size = 11, 
    base_family = "",
    base_line_size = 0.5,
    base_rect_size = 0.5
  ))
}

# Set consistent graphics device parameters
old_options <- options(
  # Graphics device settings
  device = "png",
  # Font and text settings  
  family = "",
  # Ensure reproducible random number generation
  digits = 7
)

# Ensure locale is set consistently
old_locale <- Sys.getlocale("LC_NUMERIC")
Sys.setlocale("LC_NUMERIC", "C")

setup_file <- system.file("extdata", "sample_setup_file.csv", package = "ThinkingGrid")
data_file <- system.file("extdata", "sample_qualtrics_output.csv", package = "ThinkingGrid")
expected_result <- system.file("extdata", "sample_qualtrics_extracted.csv", package = "ThinkingGrid")

set.seed(42)
dc <- sample(1:6, 100, replace = TRUE)
ac <- sample(1:6, 100, replace = TRUE)
id <- rep(1:5, times = 20)
condition <- rep(c("a", "a", "a", "b", "b"), 20)

mock_tg_frame <- data.frame(
    Deliberate.Constraints = dc,
    Automatic.Constraints = ac,
    id = id,
    condition = condition
)

skip_if_no_python_deps <- function() {
  # Check if Python is available
  have_python <- reticulate::py_available(initialize = FALSE)
  if (!have_python) {
    testthat::skip("Python not available for testing")
  }

  # Check all required Python packages
  required_packages <- c("pandas", "numpy", "matplotlib", "skimage")
  missing_packages <- character(0)

  for (pkg in required_packages) {
    if (!reticulate::py_module_available(pkg)) {
      missing_packages <- c(missing_packages, pkg)
    }
  }

  if (length(missing_packages) > 0) {
    testthat::skip(paste("Required Python packages not available:",
                        paste(missing_packages, collapse = ", ")))
  }
}

test_that("generate_survey does not crash", {
    # During R CMD check, we skip the actual function call to avoid file creation
    # but still test that the function can be called without errors in normal testing
    if (nzchar(Sys.getenv("R_CHECK_TIMINGS")) || nzchar(Sys.getenv("_R_CHECK_PACKAGE_NAME_"))) {
        skip("Skipping generate_survey test during R CMD check to avoid file creation")
    }
    
    # Test the function with default parameters - just check it doesn't crash
    expect_equal(
        generate_survey(
            setup_file,
            question_text = TRUE
        ),
        0
    )
    
    # Clean up any QSF files that might have been created
    qsf_files <- list.files(pattern = "\\.qsf$", full.names = TRUE)
    if (length(qsf_files) > 0) {
        file.remove(qsf_files)
    }
})

test_that("read_qualtrics_data does not crash", {
    skip_if_no_python_deps()
    expect_equal(read_qualtrics_data(data_file, setup_file),
                 read.csv(expected_result, header = TRUE))
})

test_that("test pandas availability", {
    skip_if_no_python_deps()
    expect_type(
        reticulate::import("pandas"),
        "environment"
    )
})


## ------------------------------------------------------------------------

test_that("plot_tg produces correct plot", {
    res <- plot_tg(mock_tg_frame, proportion_type = "overall", type = "cells")
    vdiffr::expect_doppelganger("plot_tg_overall_cells", res$plot)
})

test_that("plot_tg produces correct plot for quadrants", {
    res <- plot_tg(mock_tg_frame, proportion_type = "overall", type = "quadrants")
    vdiffr::expect_doppelganger("plot_tg_overall_quadrants", res$plot)
})

test_that("plot_tg produces correct plot for horizontal", {
    res <- plot_tg(mock_tg_frame, proportion_type = "overall", type = "horizontal")
    vdiffr::expect_doppelganger("plot_tg_overall_horizontal", res$plot)
})

test_that("plot_tg produces correct plot for vertical", {
    res <- plot_tg(mock_tg_frame, proportion_type = "overall", type = "vertical")
    vdiffr::expect_doppelganger("plot_tg_overall_vertical", res$plot)
})

test_that("plot_tg produces correct plot for constraints", {
    res <- plot_tg(mock_tg_frame, proportion_type = "overall", type = "constraints")
    vdiffr::expect_doppelganger("plot_tg_overall_constraints", res$plot)
})

test_that("plot_tg produces correct plot for depth", {
    res <- plot_tg(mock_tg_frame, proportion_type = "overall", type = "depth")
    vdiffr::expect_doppelganger("plot_tg_overall_depth", res$plot)
})

## ------------------------------------------------------------------------

#Tests for condition plots without comparison_type (use "separate")
test_that("plot_tg produces correct plot for condition (separate) with type = cells", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "cells", condition_column = "condition")
  vdiffr::expect_doppelganger("plot_tg_separate_cells", res$plot)
})

test_that("plot_tg produces correct plot for condition (separate) with type = quadrants", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "quadrants", condition_column = "condition")
  vdiffr::expect_doppelganger("plot_tg_separate_quadrants", res$plot)
})

test_that("plot_tg produces correct plot for condition (separate) with type = horizontal", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "horizontal", condition_column = "condition")
  vdiffr::expect_doppelganger("plot_tg_separate_horizontal", res$plot)
})

test_that("plot_tg produces correct plot for condition (separate) with type = vertical", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "vertical", condition_column = "condition")
  vdiffr::expect_doppelganger("plot_tg_separate_vertical", res$plot)
})

test_that("plot_tg produces correct plot for condition (separate) with type = constraints", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "constraints", condition_column = "condition")
  vdiffr::expect_doppelganger("plot_tg_separate_constraints", res$plot)
})

test_that("plot_tg produces correct plot for condition (separate) with type = depth", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "depth", condition_column = "condition")
  vdiffr::expect_doppelganger("plot_tg_separate_depth", res$plot)
})

## ------------------------------------------------------------------------
## Tests for condition plots with comparison_type = "difference"

test_that("plot_tg produces correct plot for condition (difference) with type = cells", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "cells", condition_column = "condition", comparison_type = "difference")
  vdiffr::expect_doppelganger("plot_tg_difference_cells", res$plot)
})

test_that("plot_tg produces correct plot for condition (difference) with type = quadrants", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "quadrants", condition_column = "condition", comparison_type = "difference")
  vdiffr::expect_doppelganger("plot_tg_difference_quadrants", res$plot)
})

test_that("plot_tg produces correct plot for condition (difference) with type = horizontal", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "horizontal", condition_column = "condition", comparison_type = "difference")
  vdiffr::expect_doppelganger("plot_tg_difference_horizontal", res$plot)
})

test_that("plot_tg produces correct plot for condition (difference) with type = vertical", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "vertical", condition_column = "condition", comparison_type = "difference")
  vdiffr::expect_doppelganger("plot_tg_difference_vertical", res$plot)
})

test_that("plot_tg produces correct plot for condition (difference) with type = constraints", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "constraints", condition_column = "condition", comparison_type = "difference")
  vdiffr::expect_doppelganger("plot_tg_difference_constraints", res$plot)
})

test_that("plot_tg produces correct plot for condition (difference) with type = depth", {
  res <- plot_tg(mock_tg_frame, proportion_type = "condition", type = "depth", condition_column = "condition", comparison_type = "difference")
  vdiffr::expect_doppelganger("plot_tg_difference_depth", res$plot)
})

## ------------------------------------------------------------------------
## ------------------------------------------------------------------------

test_that("create_tg_animation is working with type = cells", {
    temp_gif <- tempfile(fileext = ".gif")
    expect_type(
        create_tg_animation(mock_tg_frame, condition_column = "id", proportion_type = "overall", type = "cells", filename = temp_gif),
        "list"
    )
    # Cleanup
    if (file.exists(temp_gif)) file.remove(temp_gif)
})

test_that("create_tg_animation is working with type = quadrants", {
    temp_gif <- tempfile(fileext = ".gif")
    expect_type(
        create_tg_animation(mock_tg_frame, condition_column = "id", proportion_type = "overall", type = "quadrants", filename = temp_gif),
        "list"
    )
    # Cleanup
    if (file.exists(temp_gif)) file.remove(temp_gif)
})

test_that("create_tg_animation is working with type = horizontal", {
    temp_gif <- tempfile(fileext = ".gif")
    expect_type(
        create_tg_animation(mock_tg_frame, condition_column = "id", proportion_type = "overall", type = "horizontal", filename = temp_gif),
        "list"
    )
    # Cleanup
    if (file.exists(temp_gif)) file.remove(temp_gif)
})

test_that("create_tg_animation is working with type = vertical", {
    temp_gif <- tempfile(fileext = ".gif")
    expect_type(
        create_tg_animation(mock_tg_frame, condition_column = "id", proportion_type = "overall", type = "vertical", filename = temp_gif),
        "list"
    )
    # Cleanup
    if (file.exists(temp_gif)) file.remove(temp_gif)
})

test_that("create_tg_animation is working with type = constraints", {
    temp_gif <- tempfile(fileext = ".gif")
    expect_type(
        create_tg_animation(mock_tg_frame, condition_column = "id", proportion_type = "overall", type = "constraints", filename = temp_gif),
        "list"
    )
    # Cleanup
    if (file.exists(temp_gif)) file.remove(temp_gif)
})

test_that("create_tg_animation is working with type = depth", {
    temp_gif <- tempfile(fileext = ".gif")
    expect_type(
        create_tg_animation(mock_tg_frame, condition_column = "id", proportion_type = "overall", type = "depth", filename = temp_gif),
        "list"
    )
    # Cleanup
    if (file.exists(temp_gif)) file.remove(temp_gif)
})

# Global cleanup - remove any QSF files that might have been left behind
# This is a safety net in case individual test cleanup fails
qsf_files <- list.files(pattern = "\\.qsf$", full.names = TRUE, recursive = TRUE)
if (length(qsf_files) > 0) {
  file.remove(qsf_files)
}
