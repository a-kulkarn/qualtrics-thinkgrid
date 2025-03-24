# Load required packages
# Function for cells plot type
create_cells_plot <- function(prop_grid, proportion_type = "overall", color_palette = "Greens", x_label = "Directedness", y_label = "Stickiness", condition_grids = NULL, comparison_type = "separate", pos_palette = "Greens", neg_palette = "Reds", max_legend = NULL, min_legend = NULL) {
  
  # Common plot theme settings
  plot_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14, face = "italic"),
      legend.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(),
      plot.margin = grid::unit(c(1, 1, 2, 2), "lines")
    )
  
  # Get color palettes
  pal_colors <- RColorBrewer::brewer.pal(9, color_palette)
  pos_colors <- RColorBrewer::brewer.pal(9, pos_palette)
  neg_colors <- RColorBrewer::brewer.pal(9, neg_palette)
  
  # Handle different proportion types
  if (proportion_type == "overall") {
    # Simple cells visualization for overall proportions
    plot_data <- base::expand.grid(dc = 1:6, ac = 1:6)
    plot_data$proportion <- as.vector(t(prop_grid))
    
    # Determine the limits for the legend
    max_value <- max(prop_grid)
    min_value <- 0
    
    # Check if max_legend is provided and valid
    if (!is.null(max_legend)) {
      if (max_legend < max_value) {
        warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
      } else {
        max_value <- max_legend
      }
    }
    
    # Check if min_legend is provided and valid
    if (!is.null(min_legend)) {
      if (min_legend > min_value) {
        warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
      } else {
        min_value <- min_legend
      }
    }
    
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = dc, y = ac, fill = proportion)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                        midpoint = (min_value + max_value)/2, limits = c(min_value, max_value)) +
      ggplot2::coord_fixed() +
      ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
      plot_theme
    
    return(list(plot = p, prop_data = prop_grid))
    
  } else if (proportion_type == "condition") {
    # Handle condition-based visualizations
    if (is.null(condition_grids)) {
      stop("condition_grids must be provided when proportion_type is 'condition'")
    }
    
    unique_conditions <- names(condition_grids)
    
    if (comparison_type == "separate") {
      if (length(unique_conditions) == 2) {
        # Create a single figure with side-by-side plots
        cond1 <- unique_conditions[1]
        cond2 <- unique_conditions[2]
        
        # Find maximum proportion across both conditions for consistent color scale
        max_prop <- max(max(condition_grids[[cond1]]), max(condition_grids[[cond2]]))
        min_prop <- 0
        
        # Check max_legend
        if (!is.null(max_legend)) {
          if (max_legend < max_prop) {
            warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
          } else {
            max_prop <- max_legend
          }
        }
        
        # Check min_legend
        if (!is.null(min_legend)) {
          if (min_legend > min_prop) {
            warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
          } else {
            min_prop <- min_legend
          }
        }
        
        # Create plot data for condition 1
        plot_data1 <- base::expand.grid(dc = 1:6, ac = 1:6)
        plot_data1$proportion <- as.vector(t(condition_grids[[cond1]]))
        plot_data1$condition <- cond1
        
        # Create plot data for condition 2
        plot_data2 <- base::expand.grid(dc = 1:6, ac = 1:6)
        plot_data2$proportion <- as.vector(t(condition_grids[[cond2]]))
        plot_data2$condition <- cond2
        
        # Combine for a single plot
        combined_data <- rbind(plot_data1, plot_data2)
        
        # Create faceted plot
        p <- ggplot2::ggplot(combined_data, ggplot2::aes(x = dc, y = ac, fill = proportion)) +
          ggplot2::geom_tile(color = "white", linewidth = 0.5) +
          ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                            midpoint = (min_prop + max_prop)/2, limits = c(min_prop, max_prop)) +
          ggplot2::facet_wrap(~ condition, ncol = 2) +
          ggplot2::coord_fixed() +
          ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
          plot_theme
        
        return(list(
          plot = p,
          prop_data = condition_grids
        ))
        
      } else {
        # Return a list of separate plots for more than 2 conditions
        condition_plots <- list()
        max_prop <- max(unlist(lapply(condition_grids, max)))
        min_prop <- 0
        
        # Check max_legend
        if (!is.null(max_legend)) {
          if (max_legend < max_prop) {
            warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
          } else {
            max_prop <- max_legend
          }
        }
        
        # Check min_legend
        if (!is.null(min_legend)) {
          if (min_legend > min_prop) {
            warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
          } else {
            min_prop <- min_legend
          }
        }
        
        for (cond in unique_conditions) {
          plot_data <- base::expand.grid(dc = 1:6, ac = 1:6)
          plot_data$proportion <- as.vector(t(condition_grids[[cond]]))
          
          p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = dc, y = ac, fill = proportion)) +
            ggplot2::geom_tile(color = "white", linewidth = 0.5) +
            ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                              midpoint = (min_prop + max_prop)/2, limits = c(min_prop, max_prop)) +
            ggplot2::coord_fixed() +
            ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::labs(x = x_label, y = y_label, 
                 title = paste("Condition:", cond),
                 fill = "Percentage (%)") +
            plot_theme
          
          condition_plots[[cond]] <- p
        }
        
        return(list(
          plot = condition_plots,
          prop_data = condition_grids
        ))
      }
      
    } else if (comparison_type == "difference") {
      # For difference comparison, we use the first two conditions
      if (length(unique_conditions) < 2) {
        stop("At least 2 conditions are required for difference comparison")
      }
      
      first_cond <- unique_conditions[1]
      second_cond <- unique_conditions[2]
      
      # Calculate difference grid
      diff_grid <- condition_grids[[first_cond]] - condition_grids[[second_cond]]
      
      plot_data <- base::expand.grid(dc = 1:6, ac = 1:6)
      plot_data$difference <- as.vector(t(diff_grid))
      
      # Get max absolute difference for symmetric color scale
      max_diff <- max(abs(diff_grid))
      
      # Check max_legend for difference view
      if (!is.null(max_legend)) {
        if (max_legend < max_diff) {
          warning("max_legend value is less than the maximum difference. Using the maximum difference instead.")
        } else {
          max_diff <- max_legend
        }
      }
      
      # Check min_legend for difference view (should be negative of max for diverging scale)
      if (!is.null(min_legend)) {
        if (min_legend > -max_diff) {
          warning("min_legend value is greater than the negative maximum difference. Using the symmetric range instead.")
        } else {
          # Only use min_legend if it's explicitly set and valid
          max_diff <- max(max_diff, abs(min_legend))
        }
      }
      
      # Create diverging plot
      p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = dc, y = ac, fill = difference)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.5) +
        ggplot2::scale_fill_gradient2(low = neg_colors[9], mid = "white", high = pos_colors[9],
                           midpoint = 0,
                           limits = c(-max_diff, max_diff)) +
        ggplot2::coord_fixed() +
        ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::labs(x = x_label, y = y_label, 
             title = paste("Difference (%):", first_cond, "-", second_cond),
             fill = "Difference (%)") +
        plot_theme
      
      return(list(
        plot = p,
        first_condition = first_cond,
        second_condition = second_cond,
        diff_grid = diff_grid
      ))
    }
  }
  
  # If we reach here, there's an unsupported combination
  stop("Unsupported combination of proportion_type and comparison_type")
}


create_quadrants_plot <- function(prop_grid, proportion_type = "overall", color_palette = "Greens", x_label = "Directedness", y_label = "Stickiness", condition_grids = NULL, comparison_type = "separate", pos_palette = "Greens", neg_palette = "Reds", max_legend = NULL, min_legend = NULL) {
  
  # Common plot theme settings
  plot_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14, face = "italic"),
      legend.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(),
      plot.margin = grid::unit(c(1, 1, 2, 2), "lines")
    )
  
  # Get color palettes
  pal_colors <- RColorBrewer::brewer.pal(9, color_palette)
  pos_colors <- RColorBrewer::brewer.pal(9, pos_palette)
  neg_colors <- RColorBrewer::brewer.pal(9, neg_palette)
  
  # Calculate quadrant proportions
  calculate_quadrant_props <- function(grid) {
    quadrant_grid <- matrix(0, nrow = 2, ncol = 2)
    
    # Define quadrants (bottom-left, bottom-right, top-left, top-right)
    quadrant_grid[1,1] <- sum(grid[1:3, 1:3])  # Bottom-left
    quadrant_grid[1,2] <- sum(grid[1:3, 4:6])  # Bottom-right
    quadrant_grid[2,1] <- sum(grid[4:6, 1:3])  # Top-left
    quadrant_grid[2,2] <- sum(grid[4:6, 4:6])  # Top-right
    
    return(quadrant_grid)
  }
  
  # Handle different proportion types
  if (proportion_type == "overall") {
    # Calculate quadrant proportions
    quadrant_grid <- calculate_quadrant_props(prop_grid)
    
    # Create data frame for quadrant plotting
    quad_data <- data.frame(
      dc_quad = rep(c(1, 2), each = 2),
      ac_quad = rep(c(1, 2), times = 2),
      proportion = c(quadrant_grid[1,1], quadrant_grid[2,1], 
                    quadrant_grid[1,2], quadrant_grid[2,2])
    )
    
    # Determine the limits for the legend
    max_value <- max(quad_data$proportion)
    min_value <- 0
    
    # Check if max_legend is provided and valid
    if (!is.null(max_legend)) {
      if (max_legend < max_value) {
        warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
      } else {
        max_value <- max_legend
      }
    }
    
    # Check if min_legend is provided and valid
    if (!is.null(min_legend)) {
      if (min_legend > min_value) {
        warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
      } else {
        min_value <- min_legend
      }
    }
    
    # Create plot for quadrants
    p <- ggplot2::ggplot(quad_data, ggplot2::aes(x = dc_quad, y = ac_quad, fill = proportion)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                         midpoint = (min_value + max_value)/2, 
                         limits = c(min_value, max_value)) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 2.5), ylim = c(0.5, 2.5)) +
      ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
      plot_theme
    
    return(list(plot = p, prop_data = quadrant_grid))
    
  } else if (proportion_type == "condition") {
    # Handle condition-based visualizations
    if (is.null(condition_grids)) {
      stop("condition_grids must be provided when proportion_type is 'condition'")
    }
    
    unique_conditions <- names(condition_grids)
    
    # Calculate quadrant proportions for each condition
    condition_quad_grids <- lapply(condition_grids, calculate_quadrant_props)
    
    if (comparison_type == "separate") {
      if (length(unique_conditions) == 2) {
        # Create a single figure with side-by-side plots for 2 conditions
        cond1 <- unique_conditions[1]
        cond2 <- unique_conditions[2]
        
        # Calculate quadrant proportions
        quad_grid1 <- condition_quad_grids[[cond1]]
        quad_grid2 <- condition_quad_grids[[cond2]]
        
        # Find maximum proportion across both conditions for consistent color scale
        max_prop <- max(max(quad_grid1), max(quad_grid2))
        min_prop <- 0
        
        # Check max_legend
        if (!is.null(max_legend)) {
          if (max_legend < max_prop) {
            warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
          } else {
            max_prop <- max_legend
          }
        }
        
        # Check min_legend
        if (!is.null(min_legend)) {
          if (min_legend > min_prop) {
            warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
          } else {
            min_prop <- min_legend
          }
        }
        
        # Create plot data for condition 1
        quad_data1 <- data.frame(
          dc_quad = rep(c(1, 2), each = 2),
          ac_quad = rep(c(1, 2), times = 2),
          proportion = c(quad_grid1[1,1], quad_grid1[2,1], quad_grid1[1,2], quad_grid1[2,2]),
          condition = cond1
        )
        
        # Create plot data for condition 2
        quad_data2 <- data.frame(
          dc_quad = rep(c(1, 2), each = 2),
          ac_quad = rep(c(1, 2), times = 2),
          proportion = c(quad_grid2[1,1], quad_grid2[2,1], quad_grid2[1,2], quad_grid2[2,2]),
          condition = cond2
        )
        
        # Combine for a single plot
        combined_data <- rbind(quad_data1, quad_data2)
        
        # Create faceted plot
        p <- ggplot2::ggplot(combined_data, ggplot2::aes(x = dc_quad, y = ac_quad, fill = proportion)) +
          ggplot2::geom_tile(color = "white", linewidth = 0.5) +
          ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                             midpoint = (min_prop + max_prop)/2, 
                             limits = c(min_prop, max_prop)) +
          ggplot2::facet_wrap(~ condition, ncol = 2) +
          ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 2.5), ylim = c(0.5, 2.5)) +
          ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
          plot_theme
        
        return(list(
          plot = p,
          prop_data = condition_quad_grids
        ))
        
      } else {
        # Return a list of separate plots for more than 2 conditions
        condition_plots <- list()
        
        # Find maximum proportion across all conditions
        max_prop <- max(unlist(lapply(condition_quad_grids, max)))
        min_prop <- 0
        
        # Check max_legend
        if (!is.null(max_legend)) {
          if (max_legend < max_prop) {
            warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
          } else {
            max_prop <- max_legend
          }
        }
        
        # Check min_legend
        if (!is.null(min_legend)) {
          if (min_legend > min_prop) {
            warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
          } else {
            min_prop <- min_legend
          }
        }
        
        for (cond in unique_conditions) {
          quad_grid <- condition_quad_grids[[cond]]
          
          quad_data <- data.frame(
            dc_quad = rep(c(1, 2), each = 2),
            ac_quad = rep(c(1, 2), times = 2),
            proportion = c(quad_grid[1,1], quad_grid[2,1], quad_grid[1,2], quad_grid[2,2])
          )
          
          p <- ggplot2::ggplot(quad_data, ggplot2::aes(x = dc_quad, y = ac_quad, fill = proportion)) +
            ggplot2::geom_tile(color = "white", linewidth = 0.5) +
            ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                               midpoint = (min_prop + max_prop)/2, 
                               limits = c(min_prop, max_prop)) +
            ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 2.5), ylim = c(0.5, 2.5)) +
            ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::labs(x = x_label, y = y_label, 
                 title = paste("Condition:", cond),
                 fill = "Percentage (%)") +
            plot_theme
          
          condition_plots[[cond]] <- p
        }
        
        return(list(
          plot = condition_plots,
          prop_data = condition_quad_grids
        ))
      }
      
    } else if (comparison_type == "difference") {
      # For difference comparison, we use the first two conditions
      if (length(unique_conditions) < 2) {
        stop("At least 2 conditions are required for difference comparison")
      }
      
      first_cond <- unique_conditions[1]
      second_cond <- unique_conditions[2]
      
      # Calculate difference grid
      diff_quad_grid <- condition_quad_grids[[first_cond]] - condition_quad_grids[[second_cond]]
      
      # Create data frame for difference plot
      diff_data <- data.frame(
        dc_quad = rep(c(1, 2), each = 2),
        ac_quad = rep(c(1, 2), times = 2),
        difference = c(diff_quad_grid[1,1], diff_quad_grid[2,1], 
                      diff_quad_grid[1,2], diff_quad_grid[2,2])
      )
      
      # Get max absolute difference for symmetric color scale
      max_diff <- max(abs(diff_data$difference))
      
      # Check max_legend for difference view
      if (!is.null(max_legend)) {
        if (max_legend < max_diff) {
          warning("max_legend value is less than the maximum difference. Using the maximum difference instead.")
        } else {
          max_diff <- max_legend
        }
      }
      
      # Check min_legend for difference view (should be negative of max for diverging scale)
      if (!is.null(min_legend)) {
        if (min_legend > -max_diff) {
          warning("min_legend value is greater than the negative maximum difference. Using the symmetric range instead.")
        } else {
          # Only use min_legend if it's explicitly set and valid
          max_diff <- max(max_diff, abs(min_legend))
        }
      }
      
      # Create diverging plot
      p <- ggplot2::ggplot(diff_data, ggplot2::aes(x = dc_quad, y = ac_quad, fill = difference)) +
        ggplot2::geom_tile(color = "white", linewidth = 0.5) +
        ggplot2::scale_fill_gradient2(low = neg_colors[9], mid = "white", high = pos_colors[9],
                           midpoint = 0,
                           limits = c(-max_diff, max_diff)) +
        ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 2.5), ylim = c(0.5, 2.5)) +
        ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::labs(x = x_label, y = y_label, 
             title = paste("Difference (%):", first_cond, "-", second_cond),
             fill = "Difference (%)") +
        plot_theme
      
      return(list(
        plot = p,
        first_condition = first_cond,
        second_condition = second_cond,
        diff_grid = diff_quad_grid
      ))
    }
  }
  
  # If we reach here, there's an unsupported combination
  stop("Unsupported combination of proportion_type and comparison_type")
}


create_horizontal_plot <- function(prop_grid, proportion_type = "overall", color_palette = "Greens", x_label = "Directedness", y_label = "Stickiness", condition_grids = NULL, comparison_type = "separate", pos_palette = "Greens", neg_palette = "Reds", max_legend = NULL, min_legend = NULL) {
  
  # Common plot theme settings
  plot_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14, face = "italic"),
      legend.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(),
      plot.margin = grid::unit(c(1, 1, 2, 2), "lines")
    )
  
  # Get color palettes
  pal_colors <- RColorBrewer::brewer.pal(9, color_palette)
  pos_colors <- RColorBrewer::brewer.pal(9, pos_palette)
  neg_colors <- RColorBrewer::brewer.pal(9, neg_palette)
  
  # Calculate horizontal proportions
  calculate_horizontal_props <- function(grid) {
    # Sum across each row (ac value)
    row_sums <- rowSums(grid)
    return(row_sums)
  }
  
  # Handle different proportion types
  if (proportion_type == "overall") {
    # Calculate horizontal proportions
    row_sums <- calculate_horizontal_props(prop_grid)
    
    # Determine the limits for the legend
    max_value <- max(row_sums)
    min_value <- 0
    
    # Check if max_legend is provided and valid
    if (!is.null(max_legend)) {
      if (max_legend < max_value) {
        warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
      } else {
        max_value <- max_legend
      }
    }
    
    # Check if min_legend is provided and valid
    if (!is.null(min_legend)) {
      if (min_legend > min_value) {
        warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
      } else {
        min_value <- min_legend
      }
    }
    
    # Create data frame for horizontal bands
    horizontal_data <- data.frame(
      ac = 1:6,
      proportion = row_sums
    )
    
    # Create plot with horizontal bands
    p <- ggplot2::ggplot(horizontal_data, ggplot2::aes(y = ac, fill = proportion)) +
      ggplot2::geom_tile(ggplot2::aes(x = 3.5, width = 6), color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                        midpoint = (min_value + max_value)/2, 
                        limits = c(min_value, max_value)) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
      ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
      plot_theme
    
    return(list(plot = p, prop_data = row_sums))
    
  } else if (proportion_type == "condition") {
    # Handle condition-based visualizations
    if (is.null(condition_grids)) {
      stop("condition_grids must be provided when proportion_type is 'condition'")
    }
    
    unique_conditions <- names(condition_grids)
    
    # Calculate horizontal proportions for each condition
    condition_horizontal_props <- lapply(condition_grids, calculate_horizontal_props)
    
    if (comparison_type == "separate") {
      if (length(unique_conditions) == 2) {
        # Create a single figure with side-by-side plots for 2 conditions
        cond1 <- unique_conditions[1]
        cond2 <- unique_conditions[2]
        
        # Calculate horizontal proportions
        horz_props1 <- condition_horizontal_props[[cond1]]
        horz_props2 <- condition_horizontal_props[[cond2]]
        
        # Find maximum proportion across both conditions for consistent color scale
        max_prop <- max(max(horz_props1), max(horz_props2))
        min_prop <- 0
        
        # Check max_legend
        if (!is.null(max_legend)) {
          if (max_legend < max_prop) {
            warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
          } else {
            max_prop <- max_legend
          }
        }
        
        # Check min_legend
        if (!is.null(min_legend)) {
          if (min_legend > min_prop) {
            warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
          } else {
            min_prop <- min_legend
          }
        }
        
        # Create plot data for condition 1
        horz_data1 <- data.frame(
          ac = 1:6,
          proportion = horz_props1,
          condition = cond1
        )
        
        # Create plot data for condition 2
        horz_data2 <- data.frame(
          ac = 1:6,
          proportion = horz_props2,
          condition = cond2
        )
        
        # Combine for a single plot
        combined_data <- rbind(horz_data1, horz_data2)
        
        # Create faceted plot
        p <- ggplot2::ggplot(combined_data, ggplot2::aes(y = ac, fill = proportion)) +
          ggplot2::geom_tile(ggplot2::aes(x = 3.5, width = 6), color = "white", linewidth = 0.5) +
          ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                             midpoint = (min_prop + max_prop)/2, 
                             limits = c(min_prop, max_prop)) +
          ggplot2::facet_wrap(~ condition, ncol = 2) +
          ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
          ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
          plot_theme
        
        return(list(
          plot = p,
          prop_data = condition_horizontal_props
        ))
        
      } else {
        # Return a list of separate plots for more than 2 conditions
        condition_plots <- list()
        
        # Find maximum proportion across all conditions
        max_prop <- max(unlist(lapply(condition_horizontal_props, max)))
        min_prop <- 0
        
        # Check max_legend
        if (!is.null(max_legend)) {
          if (max_legend < max_prop) {
            warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
          } else {
            max_prop <- max_legend
          }
        }
        
        # Check min_legend
        if (!is.null(min_legend)) {
          if (min_legend > min_prop) {
            warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
          } else {
            min_prop <- min_legend
          }
        }
        
        for (cond in unique_conditions) {
          horz_props <- condition_horizontal_props[[cond]]
          
          horz_data <- data.frame(
            ac = 1:6,
            proportion = horz_props
          )
          
          p <- ggplot2::ggplot(horz_data, ggplot2::aes(y = ac, fill = proportion)) +
            ggplot2::geom_tile(ggplot2::aes(x = 3.5, width = 6), color = "white", linewidth = 0.5) +
            ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                               midpoint = (min_prop + max_prop)/2, 
                               limits = c(min_prop, max_prop)) +
            ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
            ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::labs(x = x_label, y = y_label, 
                 title = paste("Condition:", cond),
                 fill = "Percentage (%)") +
            plot_theme
          
          condition_plots[[cond]] <- p
        }
        
        return(list(
          plot = condition_plots,
          prop_data = condition_horizontal_props
        ))
      }
      
    } else if (comparison_type == "difference") {
      # For difference comparison, we use the first two conditions
      if (length(unique_conditions) < 2) {
        stop("At least 2 conditions are required for difference comparison")
      }
      
      first_cond <- unique_conditions[1]
      second_cond <- unique_conditions[2]
      
      # Calculate difference 
      diff_horz <- condition_horizontal_props[[first_cond]] - condition_horizontal_props[[second_cond]]
      
      # Create data frame for difference plot
      diff_data <- data.frame(
        ac = 1:6,
        difference = diff_horz
      )
      
      # Get max absolute difference for symmetric color scale
      max_diff <- max(abs(diff_data$difference))
      
      # Check max_legend for difference view
      if (!is.null(max_legend)) {
        if (max_legend < max_diff) {
          warning("max_legend value is less than the maximum difference. Using the maximum difference instead.")
        } else {
          max_diff <- max_legend
        }
      }
      
      # Check min_legend for difference view (should be negative of max for diverging scale)
      if (!is.null(min_legend)) {
        if (min_legend > -max_diff) {
          warning("min_legend value is greater than the negative maximum difference. Using the symmetric range instead.")
        } else {
          # Only use min_legend if it's explicitly set and valid
          max_diff <- max(max_diff, abs(min_legend))
        }
      }
      
      # Create diverging plot
      p <- ggplot2::ggplot(diff_data, ggplot2::aes(y = ac, fill = difference)) +
        ggplot2::geom_tile(ggplot2::aes(x = 3.5, width = 6), color = "white", linewidth = 0.5) +
        ggplot2::scale_fill_gradient2(low = neg_colors[9], mid = "white", high = pos_colors[9],
                           midpoint = 0,
                           limits = c(-max_diff, max_diff)) +
        ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
        ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::labs(x = x_label, y = y_label, 
             title = paste("Difference (%):", first_cond, "-", second_cond),
             fill = "Difference (%)") +
        plot_theme
      
      return(list(
        plot = p,
        first_condition = first_cond,
        second_condition = second_cond,
        diff_data = diff_horz
      ))
    }
  }
  
  # If we reach here, there's an unsupported combination
  stop("Unsupported combination of proportion_type and comparison_type")
}

create_vertical_plot <- function(prop_grid, proportion_type = "overall", color_palette = "Greens", x_label = "Directedness", y_label = "Stickiness", condition_grids = NULL, comparison_type = "separate", pos_palette = "Greens", neg_palette = "Reds", max_legend = NULL, min_legend = NULL) {
  
  # Common plot theme settings
  plot_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14, face = "italic"),
      legend.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(),
      plot.margin = grid::unit(c(1, 1, 2, 2), "lines")
    )
  
  # Get color palettes
  pal_colors <- RColorBrewer::brewer.pal(9, color_palette)
  pos_colors <- RColorBrewer::brewer.pal(9, pos_palette)
  neg_colors <- RColorBrewer::brewer.pal(9, neg_palette)
  
  # Calculate vertical proportions
  calculate_vertical_props <- function(grid) {
    # Sum across each column (dc value)
    col_sums <- colSums(grid)
    return(col_sums)
  }
  
  # Handle different proportion types
  if (proportion_type == "overall") {
    # Calculate vertical proportions
    col_sums <- calculate_vertical_props(prop_grid)
    
    # Determine the limits for the legend
    max_value <- max(col_sums)
    min_value <- 0
    
    # Check if max_legend is provided and valid
    if (!is.null(max_legend)) {
      if (max_legend < max_value) {
        warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
      } else {
        max_value <- max_legend
      }
    }
    
    # Check if min_legend is provided and valid
    if (!is.null(min_legend)) {
      if (min_legend > min_value) {
        warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
      } else {
        min_value <- min_legend
      }
    }
    
    # Create data frame for vertical bands
    vertical_data <- data.frame(
      dc = 1:6,
      proportion = col_sums
    )
    
    # Create plot with vertical bands
    p <- ggplot2::ggplot(vertical_data, ggplot2::aes(x = dc, fill = proportion)) +
      ggplot2::geom_tile(ggplot2::aes(y = 3.5, height = 6), color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                        midpoint = (min_value + max_value)/2, 
                        limits = c(min_value, max_value)) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
      ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
      plot_theme
    
    return(list(plot = p, prop_data = col_sums))
    
  } else if (proportion_type == "condition") {
    # Handle condition-based visualizations
    if (is.null(condition_grids)) {
      stop("condition_grids must be provided when proportion_type is 'condition'")
    }
    
    unique_conditions <- names(condition_grids)
    
    # Calculate vertical proportions for each condition
    condition_vertical_props <- lapply(condition_grids, calculate_vertical_props)
    
    if (comparison_type == "separate") {
      if (length(unique_conditions) == 2) {
        # Create a single figure with side-by-side plots for 2 conditions
        cond1 <- unique_conditions[1]
        cond2 <- unique_conditions[2]
        
        # Calculate vertical proportions
        vert_props1 <- condition_vertical_props[[cond1]]
        vert_props2 <- condition_vertical_props[[cond2]]
        
        # Find maximum proportion across both conditions for consistent color scale
        max_prop <- max(max(vert_props1), max(vert_props2))
        min_prop <- 0
        
        # Check max_legend
        if (!is.null(max_legend)) {
          if (max_legend < max_prop) {
            warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
          } else {
            max_prop <- max_legend
          }
        }
        
        # Check min_legend
        if (!is.null(min_legend)) {
          if (min_legend > min_prop) {
            warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
          } else {
            min_prop <- min_legend
          }
        }
        
        # Create plot data for condition 1
        vert_data1 <- data.frame(
          dc = 1:6,
          proportion = vert_props1,
          condition = cond1
        )
        
        # Create plot data for condition 2
        vert_data2 <- data.frame(
          dc = 1:6,
          proportion = vert_props2,
          condition = cond2
        )
        
        # Combine for a single plot
        combined_data <- rbind(vert_data1, vert_data2)
        
        # Create faceted plot
        p <- ggplot2::ggplot(combined_data, ggplot2::aes(x = dc, fill = proportion)) +
          ggplot2::geom_tile(ggplot2::aes(y = 3.5, height = 6), color = "white", linewidth = 0.5) +
          ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                             midpoint = (min_prop + max_prop)/2, 
                             limits = c(min_prop, max_prop)) +
          ggplot2::facet_wrap(~ condition, ncol = 2) +
          ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
          ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
          plot_theme
        
        return(list(
          plot = p,
          prop_data = condition_vertical_props
        ))
        
      } else {
        # Return a list of separate plots for more than 2 conditions
        condition_plots <- list()
        
        # Find maximum proportion across all conditions
        max_prop <- max(unlist(lapply(condition_vertical_props, max)))
        min_prop <- 0
        
        # Check max_legend
        if (!is.null(max_legend)) {
          if (max_legend < max_prop) {
            warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
          } else {
            max_prop <- max_legend
          }
        }
        
        # Check min_legend
        if (!is.null(min_legend)) {
          if (min_legend > min_prop) {
            warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
          } else {
            min_prop <- min_legend
          }
        }
        
        for (cond in unique_conditions) {
          vert_props <- condition_vertical_props[[cond]]
          
          vert_data <- data.frame(
            dc = 1:6,
            proportion = vert_props
          )
          
          p <- ggplot2::ggplot(vert_data, ggplot2::aes(x = dc, fill = proportion)) +
            ggplot2::geom_tile(ggplot2::aes(y = 3.5, height = 6), color = "white", linewidth = 0.5) +
            ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                               midpoint = (min_prop + max_prop)/2, 
                               limits = c(min_prop, max_prop)) +
            ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
            ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::labs(x = x_label, y = y_label, 
                 title = paste("Condition:", cond),
                 fill = "Percentage (%)") +
            plot_theme
          
          condition_plots[[cond]] <- p
        }
        
        return(list(
          plot = condition_plots,
          prop_data = condition_vertical_props
        ))
      }
      
    } else if (comparison_type == "difference") {
      # For difference comparison, we use the first two conditions
      if (length(unique_conditions) < 2) {
        stop("At least 2 conditions are required for difference comparison")
      }
      
      first_cond <- unique_conditions[1]
      second_cond <- unique_conditions[2]
      
      # Calculate difference 
      diff_vert <- condition_vertical_props[[first_cond]] - condition_vertical_props[[second_cond]]
      
      # Create data frame for difference plot
      diff_data <- data.frame(
        dc = 1:6,
        difference = diff_vert
      )
      
      # Get max absolute difference for symmetric color scale
      max_diff <- max(abs(diff_data$difference))
      
      # Check max_legend for difference view
      if (!is.null(max_legend)) {
        if (max_legend < max_diff) {
          warning("max_legend value is less than the maximum difference. Using the maximum difference instead.")
        } else {
          max_diff <- max_legend
        }
      }
      
      # Check min_legend for difference view (should be negative of max for diverging scale)
      if (!is.null(min_legend)) {
        if (min_legend > -max_diff) {
          warning("min_legend value is greater than the negative maximum difference. Using the symmetric range instead.")
        } else {
          # Only use min_legend if it's explicitly set and valid
          max_diff <- max(max_diff, abs(min_legend))
        }
      }
      
      # Create diverging plot
      p <- ggplot2::ggplot(diff_data, ggplot2::aes(x = dc, fill = difference)) +
        ggplot2::geom_tile(ggplot2::aes(y = 3.5, height = 6), color = "white", linewidth = 0.5) +
        ggplot2::scale_fill_gradient2(low = neg_colors[9], mid = "white", high = pos_colors[9],
                           midpoint = 0,
                           limits = c(-max_diff, max_diff)) +
        ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
        ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::labs(x = x_label, y = y_label, 
             title = paste("Difference (%):", first_cond, "-", second_cond),
             fill = "Difference (%)") +
        plot_theme
      
      return(list(
        plot = p,
        first_condition = first_cond,
        second_condition = second_cond,
        diff_data = diff_vert
      ))
    }
  }
  
  # If we reach here, there's an unsupported combination
  stop("Unsupported combination of proportion_type and comparison_type")
}


create_constraints_plot <- function(prop_grid, proportion_type = "overall", color_palette = "Greens", x_label = "Directedness", y_label = "Stickiness", condition_grids = NULL, comparison_type = "separate", pos_palette = "Greens", neg_palette = "Reds", max_legend = NULL, min_legend = NULL) {
  
  # Common plot theme settings
  plot_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14, face = "italic"),
      legend.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(),
      plot.margin = grid::unit(c(1, 1, 2, 2), "lines")
    )
  
  # Get color palettes
  pal_colors <- RColorBrewer::brewer.pal(9, color_palette)
  pos_colors <- RColorBrewer::brewer.pal(9, pos_palette)
  neg_colors <- RColorBrewer::brewer.pal(9, neg_palette)
  
  # Calculate constraint proportions in a more explicit way
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
  
  diagonal_band_width <- sqrt(72) / 11
  axis_projection <- diagonal_band_width * sqrt(2)
  constraint_polygons <- get_constraint_polygons(axis_projection)
  
  # Remove the existing proportion column to avoid conflicts
  if ("proportion" %in% colnames(constraint_polygons)) {
    constraint_polygons$proportion <- NULL
  }
  
  # Handle different proportion types
  if (proportion_type == "overall") {
    # Calculate constraint proportions
    constraint_props <- calculate_constraint_props(prop_grid)
    
    # Determine the limits for the legend
    max_value <- max(constraint_props)
    min_value <- 0
    
    # Check if max_legend is provided and valid
    if (!is.null(max_legend)) {
      if (max_legend < max_value) {
        warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
      } else {
        max_value <- max_legend
      }
    }
    
    # Check if min_legend is provided and valid
    if (!is.null(min_legend)) {
      if (min_legend > min_value) {
        warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
      } else {
        min_value <- min_legend
      }
    }
    
    # Create data frame for constraints
    constraint_data <- data.frame(
      constraint = 2:12,
      proportion = constraint_props
    )
    
    # Merge with polygon data
    plot_data <- merge(constraint_polygons, constraint_data, by = "constraint")
    
    # Create plot with constraints
    p <- ggplot2::ggplot() +
      ggplot2::geom_polygon(data = plot_data, 
                  ggplot2::aes(x = x, y = y, fill = proportion, group = constraint),
                  color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                         midpoint = (min_value + max_value)/2, 
                         limits = c(min_value, max_value)) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
      ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
      plot_theme
    
    return(list(plot = p, prop_data = constraint_props))
    
  } else if (proportion_type == "condition") {
    # Handle condition-based visualizations
    if (is.null(condition_grids)) {
      stop("condition_grids must be provided when proportion_type is 'condition'")
    }
    
    unique_conditions <- names(condition_grids)
    
    # Calculate constraint proportions for each condition
    condition_constraint_props <- lapply(condition_grids, calculate_constraint_props)
    
    if (comparison_type == "separate") {
      if (length(unique_conditions) == 2) {
        # Create a single figure with side-by-side plots for 2 conditions
        cond1 <- unique_conditions[1]
        cond2 <- unique_conditions[2]
        
        # Calculate constraint proportions
        const_props1 <- condition_constraint_props[[cond1]]
        const_props2 <- condition_constraint_props[[cond2]]
        
        # Find maximum proportion across both conditions for consistent color scale
        max_prop <- max(max(const_props1), max(const_props2))
        min_prop <- 0
        
        # Check max_legend
        if (!is.null(max_legend)) {
          if (max_legend < max_prop) {
            warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
          } else {
            max_prop <- max_legend
          }
        }
        
        # Check min_legend
        if (!is.null(min_legend)) {
          if (min_legend > min_prop) {
            warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
          } else {
            min_prop <- min_legend
          }
        }
        
        # Create data frame for condition 1
        const_data1 <- data.frame(
          constraint = 2:12,
          proportion = const_props1,
          condition = cond1
        )
        
        # Create data frame for condition 2
        const_data2 <- data.frame(
          constraint = 2:12,
          proportion = const_props2,
          condition = cond2
        )
        
        # Combine constraint data
        combined_const_data <- rbind(const_data1, const_data2)
        
        # Create plot data by merging with polygon data
        plot_data <- merge(constraint_polygons, combined_const_data, by = "constraint")
        
        # Create faceted plot
        p <- ggplot2::ggplot() +
          ggplot2::geom_polygon(data = plot_data, 
                      ggplot2::aes(x = x, y = y, fill = proportion, group = interaction(constraint, condition)),
                      color = "white", linewidth = 0.5) +
          ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                             midpoint = (min_prop + max_prop)/2, 
                             limits = c(min_prop, max_prop)) +
          ggplot2::facet_wrap(~ condition, ncol = 2) +
          ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
          ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
          plot_theme
        
        return(list(
          plot = p,
          prop_data = condition_constraint_props
        ))
        
      } else {
        # Return a list of separate plots for more than 2 conditions
        condition_plots <- list()
        
        # Find maximum proportion across all conditions
        max_prop <- max(unlist(lapply(condition_constraint_props, max)))
        min_prop <- 0
        
        # Check max_legend
        if (!is.null(max_legend)) {
          if (max_legend < max_prop) {
            warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
          } else {
            max_prop <- max_legend
          }
        }
        
        # Check min_legend
        if (!is.null(min_legend)) {
          if (min_legend > min_prop) {
            warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
          } else {
            min_prop <- min_legend
          }
        }
        
        for (cond in unique_conditions) {
          const_props <- condition_constraint_props[[cond]]
          
          # Create data frame for constraints
          const_data <- data.frame(
            constraint = 2:12,
            proportion = const_props
          )
          
          # Merge with polygon data
          plot_data <- merge(constraint_polygons, const_data, by = "constraint")
          
          p <- ggplot2::ggplot() +
            ggplot2::geom_polygon(data = plot_data, 
                        ggplot2::aes(x = x, y = y, fill = proportion, group = constraint),
                        color = "white", linewidth = 0.5) +
            ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                               midpoint = (min_prop + max_prop)/2, 
                               limits = c(min_prop, max_prop)) +
            ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
            ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::labs(x = x_label, y = y_label, 
                 title = paste("Condition:", cond),
                 fill = "Percentage (%)") +
            plot_theme
          
          condition_plots[[cond]] <- p
        }
        
        return(list(
          plot = condition_plots,
          prop_data = condition_constraint_props
        ))
      }
      
    } else if (comparison_type == "difference") {
      # For difference comparison, we use the first two conditions
      if (length(unique_conditions) < 2) {
        stop("At least 2 conditions are required for difference comparison")
      }
      
      first_cond <- unique_conditions[1]
      second_cond <- unique_conditions[2]
      
      # Calculate difference
      diff_const <- condition_constraint_props[[first_cond]] - condition_constraint_props[[second_cond]]
      
      # Create data frame for differences
      diff_data <- data.frame(
        constraint = 2:12,
        difference = diff_const
      )
      
      # Merge with polygon data
      plot_data <- merge(constraint_polygons, diff_data, by = "constraint")
      
      # Get max absolute difference for symmetric color scale
      max_diff <- max(abs(diff_const))
      
      # Check max_legend for difference view
      if (!is.null(max_legend)) {
        if (max_legend < max_diff) {
          warning("max_legend value is less than the maximum difference. Using the maximum difference instead.")
        } else {
          max_diff <- max_legend
        }
      }
      
      # Check min_legend for difference view (should be negative of max for diverging scale)
      if (!is.null(min_legend)) {
        if (min_legend > -max_diff) {
          warning("min_legend value is greater than the negative maximum difference. Using the symmetric range instead.")
        } else {
          # Only use min_legend if it's explicitly set and valid
          max_diff <- max(max_diff, abs(min_legend))
        }
      }
      
      # Create diverging plot
      p <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = plot_data, 
                    ggplot2::aes(x = x, y = y, fill = difference, group = constraint),
                    color = "white", linewidth = 0.5) +
        ggplot2::scale_fill_gradient2(low = neg_colors[9], mid = "white", high = pos_colors[9],
                           midpoint = 0,
                           limits = c(-max_diff, max_diff)) +
        ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
        ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::labs(x = x_label, y = y_label, 
             title = paste("Difference (%):", first_cond, "-", second_cond),
             fill = "Difference (%)") +
        plot_theme
      
      return(list(
        plot = p,
        first_condition = first_cond,
        second_condition = second_cond,
        diff_data = diff_const
      ))
    }
  }
  
  # If we reach here, there's an unsupported combination
  stop("Unsupported combination of proportion_type and comparison_type")
}


create_depth_plot <- function(prop_grid, proportion_type = "overall", color_palette = "Greens", x_label = "Directedness", y_label = "Stickiness", condition_grids = NULL, comparison_type = "separate", pos_palette = "Greens", neg_palette = "Reds", max_legend = NULL, min_legend = NULL) {
  
  # Common plot theme settings
  plot_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14, face = "italic"),
      legend.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(),
      plot.margin = grid::unit(c(1, 1, 2, 2), "lines")
    )
  
  # Get color palettes
  pal_colors <- RColorBrewer::brewer.pal(9, color_palette)
  pos_colors <- RColorBrewer::brewer.pal(9, pos_palette)
  neg_colors <- RColorBrewer::brewer.pal(9, neg_palette)
  
  # Calculate depth proportions
  calculate_depth_props <- function(grid) {
    # Initialize matrix to store depth proportions for each quadrant (4 quadrants, 5 depths)
    depth_props <- matrix(0, nrow = 4, ncol = 5)
    
    # Define quadrants and depths based on the actual grid cells
    # This mapping aligns with the depth polygons from get_depth_polygons()
    
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
  
  # Get polygon data for depths from helper function
  depth_polygons <- get_depth_polygons()
  
  # Handle different proportion types
  if (proportion_type == "overall") {
    # Calculate depth proportions
    depth_props <- calculate_depth_props(prop_grid)
    
    # Determine the limits for the legend
    max_value <- max(depth_props)
    min_value <- 0
    
    # Check if max_legend is provided and valid
    if (!is.null(max_legend)) {
      if (max_legend < max_value) {
        warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
      } else {
        max_value <- max_legend
      }
    }
    
    # Check if min_legend is provided and valid
    if (!is.null(min_legend)) {
      if (min_legend > min_value) {
        warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
      } else {
        min_value <- min_legend
      }
    }
    
    # Copy polygon data for modification
    plot_data <- depth_polygons
    
    # Add proportions to the polygon data
    for (q in 1:4) {
      for (d in 1:5) {
        # Find polygons with matching quadrant and depth
        mask <- plot_data$quadrant == q & plot_data$depth == d
        if (any(mask)) {
          plot_data$proportion[mask] <- depth_props[q, d]
        }
      }
    }
    
    # Create plot with depth polygons
    p <- ggplot2::ggplot() +
      ggplot2::geom_polygon(data = plot_data, 
                  ggplot2::aes(x = x, y = y, fill = proportion, 
                      group = interaction(quadrant, depth)),
                  color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                        midpoint = (min_value + max_value)/2, 
                        limits = c(min_value, max_value)) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
      ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
      plot_theme
    
    return(list(plot = p, prop_data = depth_props))
    
  } else if (proportion_type == "condition") {
    # Handle condition-based visualizations
    if (is.null(condition_grids)) {
      stop("condition_grids must be provided when proportion_type is 'condition'")
    }
    
    unique_conditions <- names(condition_grids)
    
    # Calculate depth proportions for each condition
    condition_depth_props <- lapply(condition_grids, calculate_depth_props)
    
    if (comparison_type == "separate") {
      if (length(unique_conditions) == 2) {
        # Create a single figure with side-by-side plots for 2 conditions
        cond1 <- unique_conditions[1]
        cond2 <- unique_conditions[2]
        
        # Calculate depth proportions for each condition
        depth_props1 <- condition_depth_props[[cond1]]
        depth_props2 <- condition_depth_props[[cond2]]
        
        # Find maximum proportion across both conditions for consistent color scale
        max_prop <- max(max(depth_props1), max(depth_props2))
        min_prop <- 0
        
        # Check max_legend
        if (!is.null(max_legend)) {
          if (max_legend < max_prop) {
            warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
          } else {
            max_prop <- max_legend
          }
        }
        
        # Check min_legend
        if (!is.null(min_legend)) {
          if (min_legend > min_prop) {
            warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
          } else {
            min_prop <- min_legend
          }
        }
        
        # Create polygon data for each condition
        depth_polygons1 <- depth_polygons
        depth_polygons2 <- depth_polygons
        
        # Add condition information and proportions
        for (q in 1:4) {
          for (d in 1:5) {
            # Add proportions to matching polygons for condition 1
            mask1 <- depth_polygons1$quadrant == q & depth_polygons1$depth == d
            if (any(mask1)) {
              depth_polygons1$proportion[mask1] <- depth_props1[q, d]
            }
            
            # Add proportions to matching polygons for condition 2
            mask2 <- depth_polygons2$quadrant == q & depth_polygons2$depth == d
            if (any(mask2)) {
              depth_polygons2$proportion[mask2] <- depth_props2[q, d]
            }
          }
        }
        
        depth_polygons1$condition <- cond1
        depth_polygons2$condition <- cond2
        
        # Combine the polygon data
        combined_depth_data <- rbind(depth_polygons1, depth_polygons2)
        
        # Create faceted plot
        p <- ggplot2::ggplot() +
          ggplot2::geom_polygon(data = combined_depth_data, 
                      ggplot2::aes(x = x, y = y, fill = proportion, 
                          group = interaction(quadrant, depth, condition)),
                      color = "white", linewidth = 0.5) +
          ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                             midpoint = (min_prop + max_prop)/2, 
                             limits = c(min_prop, max_prop)) +
          ggplot2::facet_wrap(~ condition, ncol = 2) +
          ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
          ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
          plot_theme
        
        return(list(
          plot = p,
          prop_data = condition_depth_props
        ))
        
      } else {
        # Return a list of separate plots for more than 2 conditions
        condition_plots <- list()
        
        # Find maximum proportion across all conditions
        all_max <- unlist(lapply(condition_depth_props, function(x) max(x)))
        max_prop <- max(all_max)
        min_prop <- 0
        
        # Check max_legend
        if (!is.null(max_legend)) {
          if (max_legend < max_prop) {
            warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
          } else {
            max_prop <- max_legend
          }
        }
        
        # Check min_legend
        if (!is.null(min_legend)) {
          if (min_legend > min_prop) {
            warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
          } else {
            min_prop <- min_legend
          }
        }
        
        for (cond in unique_conditions) {
          depth_props <- condition_depth_props[[cond]]
          cond_polygons <- depth_polygons
          
          # Add proportion values to the polygon data
          for (q in 1:4) {
            for (d in 1:5) {
              mask <- cond_polygons$quadrant == q & cond_polygons$depth == d
              if (any(mask)) {
                cond_polygons$proportion[mask] <- depth_props[q, d]
              }
            }
          }
          
          p <- ggplot2::ggplot() +
            ggplot2::geom_polygon(data = cond_polygons, 
                        ggplot2::aes(x = x, y = y, fill = proportion, 
                            group = interaction(quadrant, depth)),
                        color = "white", linewidth = 0.5) +
            ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                               midpoint = (min_prop + max_prop)/2, 
                               limits = c(min_prop, max_prop)) +
            ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
            ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::labs(x = x_label, y = y_label, 
                 title = paste("Condition:", cond),
                 fill = "Percentage (%)") +
            plot_theme
          
          condition_plots[[cond]] <- p
        }
        
        return(list(
          plot = condition_plots,
          prop_data = condition_depth_props
        ))
      }
      
    } else if (comparison_type == "difference") {
      # For difference comparison, we use the first two conditions
      if (length(unique_conditions) < 2) {
        stop("At least 2 conditions are required for difference comparison")
      }
      
      first_cond <- unique_conditions[1]
      second_cond <- unique_conditions[2]
      
      # Calculate differences
      depth_props1 <- condition_depth_props[[first_cond]]
      depth_props2 <- condition_depth_props[[second_cond]]
      diff_depth <- depth_props1 - depth_props2
      
      # Copy polygon data for difference visualization
      diff_polygons <- depth_polygons
      
      # Add difference values to the polygon data
      for (q in 1:4) {
        for (d in 1:5) {
          mask <- diff_polygons$quadrant == q & diff_polygons$depth == d
          if (any(mask)) {
            diff_polygons$difference[mask] <- diff_depth[q, d]
          }
        }
      }
      
      # Get max absolute difference for symmetric color scale
      max_diff <- max(abs(diff_depth))
      
      # Check max_legend for difference view
      if (!is.null(max_legend)) {
        if (max_legend < max_diff) {
          warning("max_legend value is less than the maximum difference. Using the maximum difference instead.")
        } else {
          max_diff <- max_legend
        }
      }
      
      # Check min_legend for difference view (should be negative of max for diverging scale)
      if (!is.null(min_legend)) {
        if (min_legend > -max_diff) {
          warning("min_legend value is greater than the negative maximum difference. Using the symmetric range instead.")
        } else {
          # Only use min_legend if it's explicitly set and valid
          max_diff <- max(max_diff, abs(min_legend))
        }
      }
      
      # Create diverging plot
      p <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = diff_polygons, 
                    ggplot2::aes(x = x, y = y, fill = difference, 
                        group = interaction(quadrant, depth)),
                    color = "white", linewidth = 0.5) +
        ggplot2::scale_fill_gradient2(low = neg_colors[9], mid = "white", high = pos_colors[9],
                           midpoint = 0,
                           limits = c(-max_diff, max_diff)) +
        ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
        ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::labs(x = x_label, y = y_label, 
             title = paste("Difference (%):", first_cond, "-", second_cond),
             fill = "Difference (%)") +
        plot_theme
      
      return(list(
        plot = p,
        first_condition = first_cond,
        second_condition = second_cond,
        diff_data = diff_depth
      ))
    }
  }
  
  # If we reach here, there's an unsupported combination
  stop("Unsupported combination of proportion_type and comparison_type")
}