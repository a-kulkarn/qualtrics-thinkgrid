# ThinkingGrid 0.1.0

## New Features

* Initial release of ThinkingGrid package
* `generate_survey()` - Create Qualtrics importable survey files from CSV setup files
* `read_qualtrics_data()` - Parse Qualtrics survey output into data frames
* `plot_tg()` - Create various Thinking Grid visualizations (cells, quadrants, horizontal, vertical, constraints, depth)
* `create_tg_animation()` - Generate animated GIF visualizations across conditions
* `add_depths()` - Calculate quadrant depths using taxicab metric
* `thinkgrid_quadrant_plot()` - Create 2x2 quadrant plots for statistical models

## Visualization Types

* **Cells**: Individual cell heatmap (6x6 grid)
* **Quadrants**: Four-quadrant summary plots
* **Horizontal**: Horizontal bands showing stickiness levels
* **Vertical**: Vertical bands showing directedness levels  
* **Constraints**: Diagonal constraint bands
* **Depth**: Distance from grid center in each quadrant

## Dependencies

* Python integration via reticulate for survey generation and data processing
* ggplot2 ecosystem for visualizations
* Support for enhanced color scaling for small values
* Comprehensive testing with testthat and vdiffr for visual regression testing
