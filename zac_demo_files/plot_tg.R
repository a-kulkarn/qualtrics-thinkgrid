if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    install.packages("RColorBrewer")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
}

library(RColorBrewer)
library(ggplot2)

# Source the polygon coordinate functions
source("zac_demo_files/get_polygons.R")
source("zac_demo_files/plot_types.R")  # Source file with plot type functions

plot_tg <- function(dc, ac, proportion_type = "overall", type = "cells", color_palette = "Greens", x_label = "Directedness", y_label = "Stickiness", condition_col = NULL, comparison_type = "separate", pos_palette = "Greens", neg_palette = "Reds") {
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
    if (!color_palette %in% rownames(brewer.pal.info)) {
        warning("Invalid color palette. Using default Greens palette.")
        color_palette <- "Greens"
    }
    if (!pos_palette %in% rownames(brewer.pal.info)) {
        warning("Invalid positive palette. Using default Greens palette.")
        pos_palette <- "Greens"
    }
    if (!neg_palette %in% rownames(brewer.pal.info)) {
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
                 condition_grids, comparison_type, pos_palette, neg_palette))
}




create_tg_animation <- function(dc, ac, condition_col, type = "cells", proportion_type = "overall", filename = "tg_animation.gif", duration = 1, width = 800, height = 800, sorted_conditions = NULL, color_palette = "Greens", x_label = "Directedness", y_label = "Stickiness") {
  
  # Check required packages
  if(!requireNamespace("gifski", quietly = TRUE)) {
    stop("Please install the 'gifski' package: install.packages('gifski')")
  }
  if(!requireNamespace("gganimate", quietly = TRUE)) {
    stop("Please install the 'gganimate' package: install.packages('gganimate')")
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
                y_label = y_label)
    
    # Add a title showing the condition
    p$plot <- p$plot + 
      ggtitle(paste("Condition:", cond)) +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
    
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
      ggsave(file_path, plot_list[[i]], width = width/100, height = height/100)
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