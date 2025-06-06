check_install_package <- function(package_name) {
    if (!requireNamespace(package_name, quietly = TRUE)) {
        install_package <- readline(paste(package_name, "is required for this function. Would you like to install it? (y/n): "))
        if (tolower(install_package) == "y") {
            install.packages(package_name)
        } else {
            stop(paste(package_name, "is required for this function. Please install it before proceeding. Use command install.packages('", package_name, "') to install the package.", sep = ""))
            return(NULL)
        }
    }
}


# Helper function to create grid and calculate proportions
create_grid <- function(dc, ac, condition_filter = NULL, condition_col = NULL) {
    # Initialize grid
    grid <- matrix(0, nrow = 6, ncol = 6)

    # Apply condition filter if specified
    if (!is.null(condition_filter) && !is.null(condition_col)) {
        subset_indices <- which(condition_col == condition_filter)
        dc_subset <- dc[subset_indices]
        ac_subset <- ac[subset_indices]
    } else {
        dc_subset <- dc
        ac_subset <- ac
    }

    # Count occurrences
    for (i in 1:length(dc_subset)) {
        if (dc_subset[i] >= 1 && dc_subset[i] <= 6 && 
            ac_subset[i] >= 1 && ac_subset[i] <= 6) {
            grid[ac_subset[i], dc_subset[i]] <- grid[ac_subset[i], dc_subset[i]] + 1
        }
    }

    # Calculate proportions as percentages
    total <- sum(grid)
    if (total > 0) {
        prop_grid <- grid / total * 100  # Convert to percentage
    } else {
        prop_grid <- grid
    }

    return(prop_grid)
}

check_dataframe <- function(df, dc_col, ac_col, check_condition, condition_col = NULL, subset_condition = NULL) {
    if (!is.data.frame(df)) {
        stop("survey_results must be a data frame")
    }

    if (!dc_col %in% names(df) || !ac_col %in% names(df)) {
        stop(paste("Data frame must contain columns:", dc_col, "and", ac_col))
    }

    if (check_condition && is.null(condition_col)) {
        stop("condition_column must be provided when proportion_type is 'condition'")
    }

    if (check_condition && !condition_col %in% names(df)) {
        stop(paste("Data frame must contain column:", condition_col))
    }

    if (!is.numeric(df[[dc_col]]) || !is.numeric(df[[ac_col]])) {
        stop("Deliberate.Constraints and Automatic.Constraints must be numeric vectors")
    }

    # Handle data subsetting if condition is provided
    original_nrow <- nrow(df)
    if (!is.null(subset_condition) && nchar(trimws(subset_condition)) > 0) {
        message(paste("Applying subset condition:", subset_condition))
        
        # Extract column names mentioned in the condition
        # This regex finds words that could be column names
        potential_columns <- regmatches(subset_condition, gregexpr("\\b[a-zA-Z_][a-zA-Z0-9_\\.]*\\b", subset_condition))[[1]]
        
        # Filter to only actual column names from the dataframe
        referenced_columns <- intersect(potential_columns, names(df))
        
        if (length(referenced_columns) == 0) {
            warning("No valid column names detected in subset_condition. Proceeding without subsetting.")
        } else {
            # Check if all referenced columns exist
            missing_cols <- setdiff(referenced_columns, names(df))
            if (length(missing_cols) > 0) {
                stop(paste("The following columns referenced in subset_condition do not exist in the data frame:", 
                          paste(missing_cols, collapse = ", ")))
            }
            
            # Validate the condition syntax by trying to parse it
            tryCatch({
                # Create a test environment with the column names
                test_env <- new.env()
                for (col in referenced_columns) {
                    assign(col, df[[col]][1:min(5, nrow(df))], envir = test_env)  # Use first few rows for testing
                }
                
                # Try to evaluate the condition in the test environment
                eval(parse(text = subset_condition), envir = test_env)
                
            }, error = function(e) {
                stop(paste("Invalid subset_condition syntax:", e$message, 
                          "\nPlease check your condition string for proper R syntax."))
            })
            
            # Apply the subset condition
            tryCatch({
                # Create environment with actual data
                subset_env <- new.env()
                for (col in names(df)) {
                    assign(col, df[[col]], envir = subset_env)
                }
                
                # Evaluate the condition
                condition_result <- eval(parse(text = subset_condition), envir = subset_env)
                
                # Check if result is logical
                if (!is.logical(condition_result)) {
                    stop("subset_condition must evaluate to a logical vector (TRUE/FALSE values)")
                }
                
                # Check if result has the right length
                if (length(condition_result) != nrow(df)) {
                    stop(paste("subset_condition result length (", length(condition_result), 
                              ") does not match number of rows in data frame (", nrow(df), ")"))
                }
                
                # Apply the subset
                df <- df[condition_result & !is.na(condition_result), ]
                
                # Check if any rows remain
                if (nrow(df) == 0) {
                    stop("No rows remain after applying subset_condition. Please check your filtering criteria.")
                }
                
                message(paste("Subset applied successfully.", nrow(df), "rows remaining out of", original_nrow, "original rows."))
                
            }, error = function(e) {
                if (grepl("No rows remain", e$message)) {
                    stop(e$message)  # Re-throw our custom error
                } else {
                    stop(paste("Error applying subset_condition:", e$message))
                }
            })
        }
    }

    # drop any NA values
    if (any(is.na(df[[dc_col]])) || any(is.na(df[[ac_col]]))) {
        dropped_rows <- sum(is.na(df[[dc_col]]) | is.na(df[[ac_col]]))
        warning(paste("Dropping", dropped_rows, "rows with NA values in", dc_col, "or", ac_col))
        df <- df[!is.na(df[[dc_col]]) & !is.na(df[[ac_col]]), ]
    }

    # Final check if any rows remain after NA removal
    if (nrow(df) == 0) {
        stop("No rows remain after removing NA values. Please check your data.")
    }

    dc <- df[[dc_col]]
    ac <- df[[ac_col]]
    if (check_condition) {
        condition_col <- df[[condition_col]]
        if (length(condition_col) != length(dc)) {
            stop("condition_col must have the same length as Deliberate.Constraints and Automatic.Constraints")
        }
    } else {
        condition_col <- NULL
    }

    return(list(dc = dc, ac = ac, condition_col = condition_col, filtered_df = df))

}

#' Illustration of plot_tg function
#' 
#' @param survey_results {data.frame, required} Data frame containing survey results with columns "Deliberate.Constraints" and "Automatic.Constraints".
#' 
#' @param proportion_type {character, optional} Type of proportion to calculate. Options are "overall" (default) or "condition". If "condition", a condition column must be provided using the condition_col parameter.
#' 
#' @param type {character, optional} Type of plot to create. Options are "cells" (default), "quadrants", "horizontal", "vertical", "constraints", or "depth". Create a grid of proportions based on the type specified.
#' 
#' @param colorer {function, optional} Function to color the grid cells. Default is NULL. Use create_custom_colorer() to create one with specific colors and scaling.
#' @param palette {character, optional} Name of diverging palette from colorspace package. Default is "RdYlBu". Other options include "RdBu", "PiYG", "BrBG", "PuOr", "RdGy".
#' @param zero_color {character, optional} Color for zero values. Default is "#FFFFFF" (white) for clear distinction.
#' @param x_label {character, optional} Label for the x-axis. Default is "Directedness".
#' @param y_label {character, optional} Label for the y-axis. Default is "Stickiness".
#' 
#' @param condition_col {character, optional} Required if proportion_type is "condition". Can be passed as a vector. If part of the data frame, pass it as data$condition_col.
#' 
#' @param comparison_type {character, optional} Type of comparison to make when proportion_type is "condition". Options are "separate" (default) or "difference". If number of conditions is exactly 2, the plots will be shown side by side. Otherwise, the plots will be shown separately. The "difference" option requires exactly 2 conditions.
#' 
#' @param min_legend {numeric, optional} Minimum value for the legend. Default is NULL. If NULL, the minimum value will be calculated from the data.
#' 
#' @param max_legend {numeric, optional} Maximum value for the legend. Default is NULL. If NULL, the maximum value will be calculated from the data.
#' 
#' @param gradient_scaling {character, optional} Type of scaling to apply to color gradient mapping. Options are "linear" (default) or "enhanced". "enhanced" makes small differences more visible in the color gradient.
#' @param enhanced_threshold_pct {numeric, optional} Percentage of maximum value to use as threshold for enhanced scaling (default: 50). Values below this percentage get expanded.
#' @param enhanced_expansion_factor {numeric, optional} Factor by which to expand the lower range in enhanced scaling (default: 1.5). Higher values give more distinction to small values.
#' @examples
#' plot_tg(relevant_data)
#' plot_tg(relevant_data, proportion_type = "condition", condition_col = relevant_data$condition_col)
#' plot_tg(relevant_data, type = "cells", colorer = my_color_function)
#' plot_tg(relevant_data, type = "quadrants", palette = "RdBu", x_label = "X-axis label", y_label = "Y-axis label", proportion_type = "condition", condition_col = relevant_data$condition_col, comparison_type = "separate")
#' 
#' @return A list containing $plot and $prop_data. $plot is a ggplot object, and $prop_data is a data frame with the proportions.
#' 
#' @export
plot_tg <- function(survey_results,
                    proportion_type = "overall",
                    type = "depth",
                    colorer = NULL,
                    palette = "RdYlBu",
                    zero_color = "#FFFFFF",
                    gradient_scaling = "linear",
                    enhanced_threshold_pct = 50,
                    enhanced_expansion_factor = 1.5,
                    x_label = "Directedness",
                    y_label = "Stickiness",
                    dc_column = "Deliberate.Constraints",
                    ac_column = "Automatic.Constraints",
                    condition_column = NULL,
                    comparison_type = "separate",
                    max_legend = NULL,
                    min_legend = NULL,
                    plot_title = NULL,
                    legend_title = NULL,
                    plot_subtitle = NULL,
                    subset_condition = NULL) {
    
    condition_required = FALSE
    if (proportion_type == "condition") {
        condition_required = TRUE
    }
    
    temp <- check_dataframe(survey_results, dc_column, ac_column, condition_required, condition_column, subset_condition)
    dc <- temp$dc
    ac <- temp$ac
    condition_col <- temp$condition_col
    
    # Check required packages
    check_install_package("RColorBrewer")
    check_install_package("ggplot2")
    
    ## Options handling.
    ## ------------------------------------
    
    ## Validate proportion_type
    if (!(proportion_type %in% c("overall", "condition"))) {
        stop("Invalid proportion_type.")
    }
    
    ## Validate comparison_type when condition is present
    if (proportion_type == "condition" && !(comparison_type %in% c("separate", "difference"))) {
        stop("Invalid comparison_type. Please use 'separate' or 'difference'.")
    }
    
    # Validate plot type
    valid_types <- c("quadrants", "horizontal", "vertical", "constraints", "depth", "cells")
    if (!(type %in% valid_types)) {
        stop("type must be one of ", paste(valid_types, collapse = ", "))
    }
    
    # Process data
    # ------------
    
    # Calculate proportions based on proportion_type
    if (proportion_type == "overall") {
        # Single grid for overall proportions
        prop_grid <- create_grid(dc, ac)
        condition_grids <- NULL
        
        # Validate plot_subtitle is not a list for overall plot
        if (!is.null(plot_subtitle) && is.list(plot_subtitle)) {
            stop("For proportion_type='overall', plot_subtitle should be a single string, not a list.")
        }
        
    } else {  # condition-based analysis
        # Get unique conditions
        unique_conditions <- unique(condition_col)
        
        if (length(unique_conditions) < 2) {
            stop("At least 2 unique conditions are needed for condition analysis.")
            prop_grid <- create_grid(dc, ac)
            proportion_type <- "overall"
            condition_grids <- NULL
        } else {
            # Calculate proportion grids for each condition
            condition_grids <- lapply(unique_conditions, function(cond) {
                create_grid(dc, ac, cond, condition_col)
            })
            names(condition_grids) <- unique_conditions
            prop_grid <- NULL  # Not used for condition analysis
            
            # Validate plot_subtitle when using conditions
            if (!is.null(plot_subtitle)) {
                if (comparison_type == "separate") {
                    # For separate plots, validate list length matches number of conditions
                    if (!is.character(plot_subtitle)) {
                        stop("For proportion_type='condition' and comparison_type='separate', plot_subtitle should be a character of strings, one for each condition.")
                    }
                    
                    if (length(plot_subtitle) != length(unique_conditions)) {
                        stop(paste("Number of subtitles (", length(plot_subtitle), 
                               ") does not match number of conditions (", 
                               length(unique_conditions), "). Please provide exactly one subtitle per condition."))
                    }
                } else if (comparison_type == "difference") {
                    # For difference plot, we need a single subtitle
                    if (is.list(plot_subtitle)) {
                        stop("For comparison_type='difference', plot_subtitle should be a single string, not a list.")
                    }
                }
            }
        }
    }
    
    # Create default colorer if none provided
    if (is.null(colorer)) {
        colorer <- create_custom_colorer(
            palette = palette,
            zero_color = zero_color,
            gradient_scaling = gradient_scaling,
            enhanced_threshold_pct = enhanced_threshold_pct,
            enhanced_expansion_factor = enhanced_expansion_factor
        )
    }
    
    # Generate visualization
    # ---------------------
    
    # Define function mapping
    plot_functions <- list(
        "cells" = create_cells_plot,
        "quadrants" = create_quadrants_plot,
        "horizontal" = create_horizontal_plot,
        "vertical" = create_vertical_plot,
        "constraints" = create_constraints_plot,
        "depth" = create_depth_plot
    )
    
    # Check if the plot function exists
    if (is.null(plot_functions[[type]])) {
        stop(paste("Plot type", type, "not implemented"))
    }
    
    # Call the appropriate plot function
    plot_fn <- plot_functions[[type]]
    
    # All plot types now support condition-based visualization
    return(plot_fn(prop_grid, proportion_type, colorer, x_label, y_label,
                 condition_grids, comparison_type, max_legend, min_legend,
                 plot_title, legend_title, plot_subtitle))
}

#' Illustration of create_tg_animation function
#' 
#' @param survey_results {data.frame, required} Data frame containing survey results with columns "Deliberate.Constraints" and "Automatic.Constraints".
#' 
#' @param condition_col {character, required} Column name or vector containing the conditions for each observation. If part of the data frame, pass it as data$condition_col. If condition_col values are numeric, they will be sorted in ascending order. If they are character/factor, they will be displayed in random order unless sorted_conditions is provided.
#' 
#' @param type {character, optional} Type of plot to create. Options are "cells" (default), "quadrants", "horizontal", "vertical", "constraints", or "depth". Create a grid of proportions based on the type specified.
#' 
#' @param proportion_type {character, optional} Type of proportion to calculate. Currently only "overall" is supported.
#' 
#' @param colorer {function, optional} Function to color the grid cells. Default is NULL.
#' @param palette {character, optional} Name of diverging palette from colorspace package. Default is "RdYlBu".
#' @param zero_color {character, optional} Color for zero values. Default is "#FFFFFF".
#' 
#' @param x_label {character, optional} Label for the x-axis. Default is "Directedness".
#' 
#' @param y_label {character, optional} Label for the y-axis. Default is "Stickiness".
#' 
#' @param min_legend {numeric, optional} Minimum value for the legend. Default is NULL. If NULL, the minimum value will be calculated from the data.
#' 
#' @param max_legend {numeric, optional} Maximum value for the legend. Default is NULL. If NULL, the maximum value will be calculated from the data.
#' 
#' @param filename {character, optional} Name of the output GIF file. Default is "tg_animation.gif".
#' 
#' @param duration {numeric, optional} Duration of each frame in the GIF in seconds. Default is 1 second.
#' 
#' @param width {numeric, optional} Width of the GIF in pixels. Default is 800 pixels.
#' 
#' @param height {numeric, optional} Height of the GIF in pixels. Default is 800 pixels.
#' 
#' @param sorted_conditions {character, optional} A vector of conditions in the desired order. If provided, the function will use this order for the animation. If not provided, the function will sort numeric conditions in ascending order and display character/factor conditions in random order. This vector must contain all unique values from the condition_col.
#' 
#' @examples
#' create_tg_animation(relevant_data, condition_col = relevant_data$condition_col)
#' 
#' create_tg_animation(relevant_data, condition_col = relevant_data$condition_col, type = "constraints", filename = "my_animation.gif")
#' 
#' create_tg_animation(relevant_data, condition_col = relevant_data$condition_col, type = "cells", sorted_conditions = c("Condition1", "Condition2", "Condition3"))
#' 
#' @return A GIF file containing the animation of the Thinking Grid.
#' 
#' @export
create_tg_animation <- function(survey_results,
                                dc_column = "Deliberate.Constraints",
                                ac_column = "Automatic.Constraints",
                                condition_column = NULL,
                                type = "depth",
                                proportion_type = "overall",
                                colorer = NULL,
                                palette = "RdYlBu",
                                zero_color = "#FFFFFF",
                                gradient_scaling = "linear",
                                enhanced_threshold_pct = 50,
                                enhanced_expansion_factor = 1.5,
                                x_label = "Directedness",
                                y_label = "Stickiness",
                                max_legend = NULL,
                                min_legend = NULL,
                                plot_title = NULL,
                                legend_title = NULL,
                                plot_subtitle = NULL,
                                filename = "tg_animation.gif",
                                duration = 1,
                                width = 800,
                                height = 800,
                                sorted_conditions = NULL,
                                subset_condition = NULL) {
  
  check_install_package("RColorBrewer")
  check_install_package("ggplot2")
  check_install_package("gifski")
  
  temp <- check_dataframe(survey_results, dc_column, ac_column, TRUE, condition_column, subset_condition)
  dc <- temp$dc
  ac <- temp$ac
  condition_col <- temp$condition_col
  
  # Create default colorer if none provided  
  if (is.null(colorer)) {
      colorer <- create_custom_colorer(
          palette = palette,
          zero_color = zero_color,
          gradient_scaling = gradient_scaling,
          enhanced_threshold_pct = enhanced_threshold_pct,
          enhanced_expansion_factor = enhanced_expansion_factor
      )
  }
  
  # Convert condition_col to factor
  df <- data.frame(dc = dc, ac = ac, condition = condition_col)
  
  # Get unique conditions
  unique_conditions <- unique(df$condition)
  
  # Check if sorted_conditions is provided and valid
  if(!is.null(sorted_conditions)) {
    # Check if all values in sorted_conditions are present in condition_col
    missing_values <- setdiff(sorted_conditions, unique_conditions)
    if(length(missing_values) > 0) {
      stop(paste("The following values in sorted_conditions are not present in condition_col:", 
                 paste(missing_values, collapse = ", ")))
    }
    
    # Check if all values in condition_col are present in sorted_conditions
    extra_values <- setdiff(unique_conditions, sorted_conditions)
    if(length(extra_values) > 0) {
      stop(paste("The following values in condition_col are not present in sorted_conditions:", 
                 paste(extra_values, collapse = ", ")))
    }
    
    # Use the provided order
    ordered_conditions <- sorted_conditions
  } else {
    # No custom sorting provided, sort based on data type
    if(is.numeric(unique_conditions)) {
      # For numeric conditions, sort in ascending order
      ordered_conditions <- sort(unique_conditions)
    } else {
      # For character/factor conditions, use a random order if no sorting specified
      ordered_conditions <- sample(unique_conditions)
      message("Character conditions are being displayed in random order. Provide sorted_conditions parameter for custom ordering.")
    }
  }

  # Define helper functions to calculate proportions based on plot type
  # (These correspond to the functions in plot_types.R)
  
  # Calculate proportion values for this subset using the existing create_grid function
  # from the parent scope, avoiding code duplication
  
  # Function to calculate quadrant proportions
  calculate_quadrant_props <- function(grid) {
    # Initialize matrix for 2x2 quadrants
    quadrant_grid <- matrix(0, nrow = 2, ncol = 2) 
    
    # Define quadrants (bottom-left, bottom-right, top-left, top-right)
    # Bottom-left
    quadrant_grid[1,1] <- sum(grid[1:3, 1:3])  # Bottom-left
    # Bottom-right
    quadrant_grid[1,2] <- sum(grid[1:3, 4:6])  # Bottom-right
    # Top-left
    quadrant_grid[2,1] <- sum(grid[4:6, 1:3])  # Top-left
    # Top-right
    quadrant_grid[2,2] <- sum(grid[4:6, 4:6])  # Top-right
    
    return(quadrant_grid)
  }
  
  # Function to calculate horizontal proportions
  calculate_horizontal_props <- function(grid) {
    # Sum across each row (ac value)
    row_sums <- rowSums(grid)
    return(row_sums)
  }
  
  # Function to calculate vertical proportions
  calculate_vertical_props <- function(grid) {
    # Sum across each column (dc value)
    col_sums <- colSums(grid)
    return(col_sums)
  }
  
  # Function to calculate constraint proportions
  calculate_constraint_props <- function(grid) {
    # Initialize vector for constraint proportions
    constraint_props <- numeric(11)  # For constraints 2-12
    
    # Constraint 2 (ac=1, dc=1)
    constraint_props[1] <- grid[1, 1]
    # Constraint 3 (ac=1,dc=2 and ac=2,dc=1)
    constraint_props[2] <- grid[1, 2] + grid[2, 1]
    # Constraint 4 (ac=1,dc=3 and ac=2,dc=2 and ac=3,dc=1)
    constraint_props[3] <- grid[1, 3] + grid[2, 2] + grid[3, 1]
    # Constraint 5 (ac=1,dc=4 and ac=2,dc=3 and ac=3,dc=2 and ac=4,dc=1)
    constraint_props[4] <- grid[1, 4] + grid[2, 3] + grid[3, 2] + grid[4, 1]
    # Constraint 6 (ac=1,dc=5 and ac=2,dc=4 and ac=3,dc=3 and ac=4,dc=2 and ac=5,dc=1)
    constraint_props[5] <- grid[1, 5] + grid[2, 4] + grid[3, 3] + grid[4, 2] + grid[5, 1]
    # Constraint 7 (ac=1,dc=6 and ac=2,dc=5 and ac=3,dc=4 and ac=4,dc=3 and ac=5,dc=2 and ac=6,dc=1)
    constraint_props[6] <- grid[1, 6] + grid[2, 5] + grid[3, 4] + grid[4, 3] + grid[5, 2] + grid[6, 1]
    # Constraint 8 (ac=2,dc=6 and ac=3,dc=5 and ac=4,dc=4 and ac=5,dc=3 and ac=6,dc=2)
    constraint_props[7] <- grid[2, 6] + grid[3, 5] + grid[4, 4] + grid[5, 3] + grid[6, 2]
    # Constraint 9 (ac=3,dc=6 and ac=4,dc=5 and ac=5,dc=4 and ac=6,dc=3)
    constraint_props[8] <- grid[3, 6] + grid[4, 5] + grid[5, 4] + grid[6, 3]
    # Constraint 10 (ac=4,dc=6 and ac=5,dc=5 and ac=6,dc=4)
    constraint_props[9] <- grid[4, 6] + grid[5, 5] + grid[6, 4] 
      # Constraint 11 (ac=5,dc=6 and ac=6,dc=5)
    constraint_props[10] <- grid[5, 6] + grid[6, 5]
    
    # Constraint 12 (ac=6,dc=6)
    constraint_props[11] <- grid[6, 6]
    
    return(constraint_props)
  }
  
  # Function to calculate depth proportions
  calculate_depth_props <- function(grid) { 
    # Initialize matrix to store depth proportions for each quadrant (4 quadrants, 5 depths)
    depth_props <- matrix(0, nrow = 4, ncol = 5)
    
    # Quadrant 1 (bottom-left, dc 1-3, ac 1-3)
    depth_props[1, 1] <- grid[3, 3]  # Depth 1 - cell (3,3)
    depth_props[1, 2] <- grid[3, 2] + grid[2, 3]  # Depth 2 - cells (3,2) and (2,3)
    depth_props[1, 3] <- grid[3, 1] + grid[2, 2] + grid[1, 3]  # Depth 3 - cells (3,1), (2,2), (1,3)
    depth_props[1, 4] <- grid[2, 1] + grid[1, 2]  # Depth 4 - cells (2,1) and (1,2)
    depth_props[1, 5] <- grid[1, 1]  # Depth 5 - cell (1,1)
    
    # Quadrant 2 (bottom-right, dc 4-6, ac 1-3)
    depth_props[2, 1] <- grid[3, 4]  # Depth 1
    depth_props[2, 2] <- grid[3, 5] + grid[2, 4]  # Depth 2
    depth_props[2, 3] <- grid[3, 6] + grid[2, 5] + grid[1, 4]  # Depth 3
    depth_props[2, 4] <- grid[2, 6] + grid[1, 5]  # Depth 4
    depth_props[2, 5] <- grid[1, 6]  # Depth 5
    
    # Quadrant 3 (top-left, dc 1-3, ac 4-6)
    depth_props[3, 1] <- grid[4, 3]  # Depth 1
    depth_props[3, 2] <- grid[4, 2] + grid[5, 3]  # Depth 2
    depth_props[3, 3] <- grid[4, 1] + grid[5, 2] + grid[6, 3]  # Depth 3
    depth_props[3, 4] <- grid[5, 1] + grid[6, 2]  # Depth 4
    depth_props[3, 5] <- grid[6, 1]  # Depth 5
     # Quadrant 4 (top-right, dc 4-6, ac 4-6)
    depth_props[4, 1] <- grid[4, 4]  # Depth 1
    depth_props[4, 2] <- grid[4, 5] + grid[5, 4]  # Depth 2
    depth_props[4, 3] <- grid[4, 6] + grid[5, 5] + grid[6, 4]  # Depth 3
    depth_props[4, 4] <- grid[5, 6] + grid[6, 5]  # Depth 4
    depth_props[4, 5] <- grid[6, 6]  # Depth 5
    
    return(depth_props)
  }
  
  # Select the appropriate calculation function based on plot type
  calc_function <- switch(type,
                         "cells" = function(grid) grid,
                         "quadrants" = calculate_quadrant_props,
                         "horizontal" = calculate_horizontal_props,
                         "vertical" = calculate_vertical_props,
                         "constraints" = calculate_constraint_props,
                         "depth" = calculate_depth_props)
  
  # If legend limits are not provided, calculate them from all conditions
  if(is.null(max_legend) || is.null(min_legend)) {
    # Calculate proportion values for each condition to determine global min/max
    all_proportions <- list()
    
    for(cond in ordered_conditions) {
      # Subset data for this condition
      df_subset <- df[df$condition == cond, ]
      # Create basic proportion grid
      base_grid <- create_grid(df_subset$dc, df_subset$ac)
      # Apply the type-specific calculation
      type_specific_props <- calc_function(base_grid)
      # Store non-zero proportions
      all_proportions[[as.character(cond)]] <- type_specific_props[type_specific_props > 0]
    }
    
    # Combine all proportions and find global min/max
    all_values <- unlist(all_proportions)
    
    if(is.null(max_legend) && length(all_values) > 0) {
      max_legend <- max(all_values)
    }
    
    if(is.null(min_legend) && length(all_values) > 0) {
      min_legend <- 0
    }
  }
  
  # Generate individual plots for each condition
  plot_list <- list()
  for(i in seq_along(ordered_conditions)) {
    cond <- ordered_conditions[i]
    # Subset data for this condition
    df_subset <- df[df$condition == cond, ]

    df_subset_relabelled <- data.frame(
        Deliberate.Constraints = df_subset$dc,
        Automatic.Constraints = df_subset$ac
    )
    
    # Determine subtitle for this condition
    current_subtitle <- NULL
    if (!is.null(plot_subtitle)) {
      if (length(plot_subtitle) == 1) {
        # Use the same subtitle for all conditions
        current_subtitle <- plot_subtitle
        message(paste("Condition '", cond, "' uses subtitle: '", current_subtitle, "'", sep=""))
      } else {
        # Use condition-specific subtitle (by index, not name)
        current_subtitle <- plot_subtitle[i]
        message(paste("Condition '", cond, "' uses subtitle: '", current_subtitle, "'", sep=""))
      }
    }
      
    # Create plot using plot_tg function
    p <- plot_tg(df_subset_relabelled, 
                proportion_type = "overall", 
                type = type,
                colorer = colorer,  # Use the configured colorer
                x_label = x_label,
                y_label = y_label,
                max_legend = max_legend,
                min_legend = min_legend,
                plot_title = plot_title,
                legend_title = legend_title,
                plot_subtitle = current_subtitle)
    
    # Only add the "Condition: X" title if no subtitle is provided
    if (is.null(current_subtitle)) {
      # Create a combined title with user's plot_title and condition information
      combined_title <- if (!is.null(plot_title)) {
        paste0(plot_title, "\nCondition: ", cond)
      } else {
        paste("Condition:", cond)
      }
      
      # Set the title only if we're not using user-provided subtitles
      p$plot <- p$plot + 
        ggplot2::ggtitle(combined_title) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5))
    } else if (!is.null(plot_title)) {
      # If we have a subtitle and a plot_title, just set the plot_title
      p$plot <- p$plot + 
        ggplot2::ggtitle(plot_title) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5))
    }
    
    # Add to list
    plot_list[[as.character(cond)]] <- p$plot
  }
  
  # Create a gif from the plots
  if(length(plot_list) > 1) {
    # Create temporary directory for frames
    temp_dir <- tempfile()
    dir.create(temp_dir)
    
    # Save each plot as an image
    file_list <- c()
    for(i in seq_along(plot_list)) {
      cond <- names(plot_list)[i]
      file_path <- file.path(temp_dir, paste0("frame_", sprintf("%03d", i), ".png"))
      ggplot2::ggsave(file_path, plot_list[[i]], width = width/100, height = height/100)
      file_list <- c(file_list, file_path)
    }
    
    # Create gif using gifski
    gifski::gifski(file_list, 
                  gif_file = filename, 
                  width = width, 
                  height = height, 
                  delay = duration)
    
    # Clean up temporary files
    file.remove(file_list)
    unlink(temp_dir)
    
    return(invisible(plot_list))
  } else {
    message("Only one condition found, no animation created")
    return(plot_list[[1]])
  }
}
