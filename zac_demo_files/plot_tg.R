# Source the polygon coordinate functions
source("zac_demo_files/get_polygons.R")
source("zac_demo_files/plot_types.R")  # Source file with plot type functions

check_install_package <- function(package_name) {
    if (!requireNamespace(package_name, quietly = TRUE)) {
        install_package <- readline(paste(package_name, "is required for this function. Would you like to install it? (y/n): "))
        if (tolower(install_package) == "y") {
            install.packages(package_name)
        } else {
            warning(paste(package_name, "is required for this function. Please install it before proceeding. Use command install.packages('", package_name, "') to install the package.", sep = ""))
            return(NULL)
        }
    }
}

plot_tg <- function(dc, ac, proportion_type = "overall", type = "cells", color_palette = "Greens", x_label = "Directedness", y_label = "Stickiness", condition_col = NULL, comparison_type = "separate", pos_palette = "Greens", neg_palette = "Reds", max_legend = NULL, min_legend = NULL) {
    
    # Check required packages
    check_install_package("RColorBrewer")
    check_install_package("ggplot2")
    # Validate inputs
    # ---------------
    # Check if ac and dc are either both vectors or both matrices
    if (is.vector(dc) && is.vector(ac)) {
        if (length(dc) != length(ac)) {
            stop("dc and ac must have the same length")
        }
        # Check condition if proportion_type is "condition"
        if (proportion_type == "condition" && is.null(condition_col)) {
            stop("condition_col parameter must be provided when proportion_type is 'condition'")
        }
        if (proportion_type == "condition" && length(condition_col) != length(dc)) {
            stop("condition must have the same length as dc and ac")
        }
    } else if (is.matrix(dc) && is.matrix(ac)) {
        if (nrow(dc) != nrow(ac) || ncol(dc) != ncol(ac)) {
            stop("dc and ac must have the same dimensions")
        }
        # Matrices don't support condition comparison
        if (proportion_type == "condition") {
            stop("condition-based proportion not supported with matrix inputs")
        }
    } else {
        stop("dc and ac must be either both vectors or both matrices")
    }

    # Validate proportion_type
    if (!(proportion_type %in% c("overall", "condition"))) {
        warning("Invalid proportion_type. Using default 'overall'.")
        proportion_type <- "overall"
    }
    
    # Validate comparison_type when condition is present
    if (proportion_type == "condition" && !(comparison_type %in% c("separate", "difference"))) {
        warning("Invalid comparison_type. Using default 'separate'.")
        comparison_type <- "separate"
    }
    
    # Validate color palettes
    if (!color_palette %in% rownames(RColorBrewer::brewer.pal.info)) {
        warning("Invalid color palette. Using default Greens palette.")
        color_palette <- "Greens"
    }
    if (!pos_palette %in% rownames(RColorBrewer::brewer.pal.info)) {
        warning("Invalid positive palette. Using default Greens palette.")
        pos_palette <- "Greens"
    }
    if (!neg_palette %in% rownames(RColorBrewer::brewer.pal.info)) {
        warning("Invalid negative palette. Using default Reds palette.")
        neg_palette <- "Reds"
    }

    # Validate plot type
    valid_types <- c("quadrants", "horizontal", "vertical", "constraints", "depth", "cells")
    if (!(type %in% valid_types)) {
        stop("type must be one of ", paste(valid_types, collapse = ", "))
    }
    
    # Process data
    # ------------
    
    # Helper function to create grid and calculate proportions
    create_grid <- function(dc, ac, condition_filter = NULL) {
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
    
    # Calculate proportions based on proportion_type
    if (proportion_type == "overall") {
        # Single grid for overall proportions
        prop_grid <- create_grid(dc, ac)
        condition_grids <- NULL
        
    } else {  # condition-based analysis
        # Get unique conditions
        unique_conditions <- unique(condition_col)
        
        if (length(unique_conditions) < 2) {
            warning("At least 2 unique conditions are needed for condition analysis. Reverting to overall proportion.")
            prop_grid <- create_grid(dc, ac)
            proportion_type <- "overall"
            condition_grids <- NULL
        } else {
            # Calculate proportion grids for each condition
            condition_grids <- lapply(unique_conditions, function(cond) {
                create_grid(dc, ac, cond)
            })
            names(condition_grids) <- unique_conditions
            prop_grid <- NULL  # Not used for condition analysis
        }
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
        warning(paste("Plot type", type, "not implemented. Using 'cells'."))
        type <- "cells"
    }
    
    # Call the appropriate plot function
    plot_fn <- plot_functions[[type]]
    
    # All plot types now support condition-based visualization
    return(plot_fn(prop_grid, proportion_type, color_palette, x_label, y_label,
                 condition_grids, comparison_type, pos_palette, neg_palette, max_legend, min_legend))
}




create_tg_animation <- function(dc, ac, condition_col, type = "cells", proportion_type = "overall", filename = "tg_animation.gif", duration = 1, width = 800, height = 800, sorted_conditions = NULL, color_palette = "Greens", x_label = "Directedness", y_label = "Stickiness", max_legend = NULL, min_legend = NULL) {
  
  # Check required packages
  if(!requireNamespace("gifski", quietly = TRUE)) {
    utils::install.packages("gifski")
  }
  if(!requireNamespace("gganimate", quietly = TRUE)) {
    utils::install.packages("gganimate")
  }

  check_install_package("RColorBrewer")
  check_install_package("ggplot2")
  check_install_package("gganimate")
  check_install_package("gifski")

  
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
      warning(paste("The following values in condition_col are not present in sorted_conditions and will be ignored:", 
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
  
  # Helper function to create basic proportion grid
  create_grid <- function(dc_subset, ac_subset) {
    # Initialize grid
    grid <- matrix(0, nrow = 6, ncol = 6)
    
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
  
  # Function to calculate quadrant proportions
  calculate_quadrant_props <- function(grid) {
    quadrant_grid <- matrix(0, nrow = 2, ncol = 2)
    
    # Define quadrants (bottom-left, bottom-right, top-left, top-right)
    quadrant_grid[1,1] <- sum(grid[1:3, 1:3])  # Bottom-left
    quadrant_grid[1,2] <- sum(grid[1:3, 4:6])  # Bottom-right
    quadrant_grid[2,1] <- sum(grid[4:6, 1:3])  # Top-left
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
                         "depth" = calculate_depth_props,
                         function(grid) grid)  # Default to cells
  
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
  
  for(cond in ordered_conditions) {
    # Subset data for this condition
    df_subset <- df[df$condition == cond, ]
    
    # Create plot using plot_tg function
    p <- plot_tg(df_subset$dc, df_subset$ac, 
                proportion_type = "overall", 
                type = type,
                color_palette = color_palette,
                x_label = x_label,
                y_label = y_label,
                max_legend = max_legend,
                min_legend = min_legend)
    
    # Add a title showing the condition
    p$plot <- p$plot + 
      ggplot2::ggtitle(paste("Condition:", cond)) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5))
    
    # Add to list
    plot_list[[as.character(cond)]] <- p$plot
  }
  
  # Create a gif from the plots
  if(length(plot_list) > 1) {
    message(paste("Creating animation with", length(plot_list), "frames"))
    
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
    
    message(paste("Animation saved as", filename))
    return(invisible(plot_list))
  } else {
    message("Only one condition found, no animation created")
    return(plot_list[[1]])
  }
}