## Load required packages

validate_range <- function(value, ref_value, is_max = TRUE) {
    if (!is.null(value)) {
        if ((is_max && value < ref_value) || (!is_max && value > ref_value)) {
            warning(paste(ifelse(is_max, "max_legend", "min_legend"),
                          "value is out of bounds. Using the reference value instead."))
            return(ref_value)
        }
        return(value)
    }
    return(ref_value)
}

create_plot_data <- function(grid, condition = NULL) {
    plot_data <- base::expand.grid(dc = 1:6, ac = 1:6)
    plot_data$proportion <- as.vector(t(grid))
    if (!is.null(condition)) plot_data$condition <- condition
    return(plot_data)
}

create_tile_plot_grid <- function(plot_data, fill_var, limits, condition = NULL) {
  p <- ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "dc", y = "ac", fill = fill_var)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                                  midpoint = mean(limits), limits = limits) +
    ggplot2::coord_fixed() +
    ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
    ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)",
                  title = ifelse(is.null(condition), NULL, paste("Condition:", condition))) +
    plot_theme
  return(p)
}

create_tile_plot <- function(plot_data, fill_var, limits, x_label, y_label, pal_colors, plot_theme) {
  ggplot2::ggplot(plot_data, ggplot2::aes_string(x = "dc", y = "ac", fill = fill_var)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.5) +
    ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                                   midpoint = (limits[1] + limits[2]) / 2,
                                   limits = limits) +
    ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 2.5), ylim = c(0.5, 2.5)) +
    ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
    ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
    plot_theme
}


create_vertical_bands_plot <- function(vertical_data, pal_colors, min_value, max_value, x_label, y_label, plot_theme) {
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
    return(p)
}


## Function for cells plot type
create_cells_plot <- function(prop_grid,
                              proportion_type = "overall",
                              color_palette = "Greens",
                              x_label = "Directedness",
                              y_label = "Stickiness",
                              condition_grids = NULL,
                              comparison_type = "separate",
                              pos_palette = "Greens",
                              neg_palette = "Reds",
                              max_legend = NULL,
                              min_legend = NULL) {
    
  ## Common plot theme settings
  plot_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14, face = "italic"),
      legend.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(),
      plot.margin = grid::unit(c(1, 1, 2, 2), "lines")
    )
  
  ## Get color palettes
  pal_colors <- RColorBrewer::brewer.pal(9, color_palette)
  pos_colors <- RColorBrewer::brewer.pal(9, pos_palette)
  neg_colors <- RColorBrewer::brewer.pal(9, neg_palette)


  if (proportion_type == "overall") {
    plot_data <- create_plot_data(prop_grid)
    limits <- c(validate_range(max_legend, max(prop_grid)), validate_range(min_legend, 0, FALSE))
    p <- create_tile_plot(plot_data, "proportion", limits)
    return(list(plot = p, prop_data = prop_grid))
  
  } else if (proportion_type == "condition") {
      if (is.null(condition_grids)) {
          stop("condition_grids must be provided when proportion_type is 'condition'")
      }
    
    unique_conditions <- names(condition_grids)
    
    if (comparison_type == "separate") {
      if (length(unique_conditions) == 2) {
        cond1 <- unique_conditions[1]
        cond2 <- unique_conditions[2]
        
        max_prop <- validate_range(max_legend, max(c(max(condition_grids[[cond1]]), max(condition_grids[[cond2]]))))
        
        plot_data1 <- create_plot_data(condition_grids[[cond1]], cond1)
        plot_data2 <- create_plot_data(condition_grids[[cond2]], cond2)
        combined_data <- rbind(plot_data1, plot_data2)
        
        limits <- c(0, max_prop)
        p <- create_tile_plot(combined_data, "proportion", limits) + ggplot2::facet_wrap(~ condition, ncol = 2)
        return(list(plot = p, prop_data = condition_grids))
        
      } else {
        condition_plots <- lapply(unique_conditions, function(cond) {
          plot_data <- create_plot_data(condition_grids[[cond]])
          limits <- c(0, validate_range(max_legend, max(unlist(lapply(condition_grids, max)))))
          create_tile_plot(plot_data, "proportion", limits, cond)
        })
        names(condition_plots) <- unique_conditions
        return(list(plots = condition_plots, prop_data = condition_grids))
      }
    
    } else if (comparison_type == "difference") {
      if (length(unique_conditions) < 2) stop("At least 2 conditions are required for difference comparison")
      
      first_cond <- unique_conditions[1]
      second_cond <- unique_conditions[2]
      diff_grid <- condition_grids[[first_cond]] - condition_grids[[second_cond]]
      
      plot_data <- create_plot_data(diff_grid)
      max_diff <- validate_range(max_legend, max(abs(diff_grid)))
      
      p <- create_tile_plot(plot_data, "difference", c(-max_diff, max_diff))
      return(list(plot = p, first_condition = first_cond, second_condition = second_cond, diff_grid = diff_grid))
    }
  }
  
  stop("Unsupported combination of proportion_type and comparison_type")
}


create_quadrants_plot <- function(prop_grid,
                                   proportion_type = "overall",
                                   color_palette = "Greens",
                                   x_label = "Directedness",
                                   y_label = "Stickiness",
                                   condition_grids = NULL,
                                   comparison_type = "separate",
                                   pos_palette = "Greens",
                                   neg_palette = "Reds",
                                   max_legend = NULL,
                                   min_legend = NULL) {
  
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

  # Calculate and plot quadrant proportions
  create_quadrant_data <- function(grid) {
    proportions <- matrix(0, nrow = 2, ncol = 2)
    proportions[1, 1] <- sum(grid[1:3, 1:3])  # Bottom-left
    proportions[1, 2] <- sum(grid[1:3, 4:6])  # Bottom-right
    proportions[2, 1] <- sum(grid[4:6, 1:3])  # Top-left
    proportions[2, 2] <- sum(grid[4:6, 4:6])  # Top-right
    return(proportions)
  }
  
  if (proportion_type == "overall") {
    quadrant_grid <- create_quadrant_data(prop_grid)

    # Prepare data for plotting
    quad_data <- data.frame(
      dc = rep(c(1, 2), each = 2),
      ac = rep(c(1, 2), times = 2),
      proportion = as.vector(quadrant_grid)
    )

    limits <- c(validate_range(max_legend, max(quad_data$proportion)), validate_range(min_legend, 0, FALSE))
    return(create_tile_plot(quad_data, "proportion", limits, x_label, y_label, pal_colors, plot_theme))
    
  } else if (proportion_type == "condition") {
    if (is.null(condition_grids)) {
      stop("condition_grids must be provided when proportion_type is 'condition'")
    }
    
    unique_conditions <- names(condition_grids)
    condition_quad_grids <- lapply(condition_grids, create_quadrant_data)

    if (comparison_type == "separate") {
      if (length(unique_conditions) == 2) {
        cond1 <- unique_conditions[1]
        cond2 <- unique_conditions[2]
        
        quad_data1 <- data.frame(
          dc_quad = rep(c(1, 2), each = 2),
          ac_quad = rep(c(1, 2), times = 2),
          proportion = as.vector(condition_quad_grids[[cond1]]),
          condition = cond1
        )
        
        quad_data2 <- data.frame(
          dc_quad = rep(c(1, 2), each = 2),
          ac_quad = rep(c(1, 2), times = 2),
          proportion = as.vector(condition_quad_grids[[cond2]]),
          condition = cond2
        )
        
        combined_data <- rbind(quad_data1, quad_data2)
        limits <- c(0, validate_range(max_legend, max(unlist(condition_quad_grids))))
        p <- create_tile_plot(combined_data, "proportion", limits, x_label, y_label, pal_colors, plot_theme) + ggplot2::facet_wrap(~ condition, ncol = 2)
        return(list(plot = p, prop_data = condition_quad_grids))
        
      } else {
        condition_plots <- lapply(unique_conditions, function(cond) {
          quad_data <- data.frame(
            dc_quad = rep(c(1, 2), each = 2),
            ac_quad = rep(c(1, 2), times = 2),
            proportion = as.vector(condition_quad_grids[[cond]])
          )
          limits <- c(0, validate_range(max_legend, max(unlist(lapply(condition_quad_grids, max)))))
          
          create_tile_plot(quad_data, "proportion", limits, x_label, y_label, pal_colors, plot_theme)
        })
        names(condition_plots) <- unique_conditions
        return(list(plots = condition_plots, prop_data = condition_quad_grids))
      }
      
    } else if (comparison_type == "difference") {
      if (length(unique_conditions) < 2) {
        stop("At least 2 conditions are required for difference comparison")
      }
      
      first_cond <- unique_conditions[1]
      second_cond <- unique_conditions[2]
      diff_quad_grid <- condition_quad_grids[[first_cond]] - condition_quad_grids[[second_cond]]
      
      diff_data <- data.frame(
        dc_quad = rep(c(1, 2), each = 2),
        ac_quad = rep(c(1, 2), times = 2),
        difference = as.vector(diff_quad_grid)
      )
      
      max_diff <- validate_range(max_legend, max(abs(diff_data$difference)))
      return(create_tile_plot(diff_data, "difference", c(-max_diff, max_diff), x_label, y_label, pal_colors, plot_theme))
    }
  }
  
  stop("Unsupported combination of proportion_type and comparison_type")
}


create_horizontal_plot <- function(prop_grid, proportion_type = "overall", color_palette = "Greens", x_label = "Directedness", y_label = "Stickiness", condition_grids = NULL, comparison_type = "separate", pos_palette = "Greens", neg_palette = "Reds", max_legend = NULL, min_legend = NULL) {
  
  plot_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14, face = "italic"),
      legend.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(),
      plot.margin = grid::unit(c(1, 1, 2, 2), "lines")
    )
  
  pal_colors <- RColorBrewer::brewer.pal(9, color_palette)
  pos_colors <- RColorBrewer::brewer.pal(9, pos_palette)
  neg_colors <- RColorBrewer::brewer.pal(9, neg_palette)
  
  calculate_horizontal_props <- function(grid) {
    rowSums(grid)
  }
  
  create_horizontal_plot_data <- function(row_sums) {
    data.frame(ac = 1:6, proportion = row_sums)
  }
  
  if (proportion_type == "overall") {
    row_sums <- calculate_horizontal_props(prop_grid)
    limits <- c(validate_range(max_legend, max(row_sums)), validate_range(min_legend, 0, FALSE))
    horizontal_data <- create_horizontal_plot_data(row_sums)
    
    p <- create_tile_plot(horizontal_data, "proportion", limits, x_label, y_label, pal_colors, plot_theme) +
      ggplot2::geom_tile(ggplot2::aes(x = 3.5, width = 6), color = "white", linewidth = 0.5)
    
    return(list(plot = p, prop_data = row_sums))
    
  } else if (proportion_type == "condition") {
    if (is.null(condition_grids)) {
      stop("condition_grids must be provided when proportion_type is 'condition'")
    }
    
    unique_conditions <- names(condition_grids)
    condition_horizontal_props <- lapply(condition_grids, calculate_horizontal_props)
    
    if (comparison_type == "separate") {
      if (length(unique_conditions) == 2) {
        cond1 <- unique_conditions[1]
        cond2 <- unique_conditions[2]
        
        horz_props1 <- condition_horizontal_props[[cond1]]
        horz_props2 <- condition_horizontal_props[[cond2]]
        
        limits <- c(0, validate_range(max_legend, max(unlist(condition_horizontal_props))))
        
        horz_data1 <- create_horizontal_plot_data(horz_props1)
        horz_data2 <- create_horizontal_plot_data(horz_props2)
        combined_data <- rbind(horz_data1, horz_data2)
        
        p <- create_tile_plot(combined_data, "proportion", limits, x_label, y_label, pal_colors, plot_theme) +
          ggplot2::facet_wrap(~ condition, ncol = 2)
        
        return(list(plot = p, prop_data = condition_horizontal_props))
      } else {
        condition_plots <- lapply(unique_conditions, function(cond) {
          horz_props <- condition_horizontal_props[[cond]]
          limits <- c(0, validate_range(max_legend, max(unlist(lapply(condition_horizontal_props, max)))))
          horz_data <- create_horizontal_plot_data(horz_props)
          create_tile_plot(horz_data, "proportion", limits, x_label, y_label, pal_colors, plot_theme)
        })
        
        names(condition_plots) <- unique_conditions
        return(list(plots = condition_plots, prop_data = condition_horizontal_props))
      }
    } else if (comparison_type == "difference") {
      if (length(unique_conditions) < 2) {
        stop("At least 2 conditions are required for difference comparison")
      }
      
      first_cond <- unique_conditions[1]
      second_cond <- unique_conditions[2]
      diff_horz <- condition_horizontal_props[[first_cond]] - condition_horizontal_props[[second_cond]]
      
      diff_data <- data.frame(ac = 1:6, difference = diff_horz)
      max_diff <- validate_range(max_legend, max(abs(diff_data$difference)))
      
      p <- create_tile_plot(diff_data, "difference", c(-max_diff, max_diff), x_label, y_label, pal_colors, plot_theme)
      
      return(list(plot = p, first_condition = first_cond, second_condition = second_cond, diff_data = diff_horz))
    }
  }
  
  stop("Unsupported combination of proportion_type and comparison_type")
}



create_vertical_plot <- function(prop_grid, proportion_type = "overall", color_palette = "Greens", x_label = "Directedness", y_label = "Stickiness", condition_grids = NULL, comparison_type = "separate", pos_palette = "Greens", neg_palette = "Reds", max_legend = NULL, min_legend = NULL) {

  ## Common plot theme settings
  plot_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14, face = "italic"),
      legend.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(),
      plot.margin = grid::unit(c(1, 1, 2, 2), "lines")
    )

  ## Get color palettes
  pal_colors <- RColorBrewer::brewer.pal(9, color_palette)
  pos_colors <- RColorBrewer::brewer.pal(9, pos_palette)
  neg_colors <- RColorBrewer::brewer.pal(9, neg_palette)

  ## Calculate vertical proportions
  calculate_vertical_props <- function(grid) {
    colSums(grid)
  }

  ## Create data frame for vertical band plot
  create_vertical_data_frame <- function(col_sums) {
    data.frame(dc = 1:6, proportion = col_sums)
  }

  if (proportion_type == "overall") {
    col_sums <- calculate_vertical_props(prop_grid)
    limits <- c(validate_range(max_legend, max(col_sums)), validate_range(min_legend, 0, FALSE))
    vertical_data <- create_vertical_data_frame(col_sums)

    p <- create_vertical_bands_plot(vertical_data, pal_colors, limits[2], limits[1], x_label, y_label, plot_theme)
    return(list(plot = p, prop_data = col_sums))

  } else if (proportion_type == "condition") {
    if (is.null(condition_grids)) {
      stop("condition_grids must be provided when proportion_type is 'condition'")
    }

    unique_conditions <- names(condition_grids)
    condition_vertical_props <- lapply(condition_grids, calculate_vertical_props)

    if (comparison_type == "separate") {
      if (length(unique_conditions) == 2) {
        cond1 <- unique_conditions[1]
        cond2 <- unique_conditions[2]

        vert_props1 <- condition_vertical_props[[cond1]]
        vert_props2 <- condition_vertical_props[[cond2]]

        max_prop <- max(max(vert_props1), max(vert_props2))
        min_prop <- 0

        limits <- c(validate_range(max_legend, max_prop), validate_range(min_legend, min_prop, FALSE))

        combined_data <- rbind(
          data.frame(dc = 1:6, proportion = vert_props1, condition = cond1),
          data.frame(dc = 1:6, proportion = vert_props2, condition = cond2)
        )

        p <- ggplot2::ggplot(combined_data, ggplot2::aes(x = dc, fill = proportion)) +
          ggplot2::geom_tile(ggplot2::aes(y = 3.5, height = 6), color = "white", linewidth = 0.5) +
          ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                                         midpoint = (limits[2] + limits[1]) / 2,
                                         limits = limits) +
          ggplot2::facet_wrap(~ condition, ncol = 2) +
          ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
          ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
          plot_theme

        return(list(plot = p, prop_data = condition_vertical_props))

      } else {
        condition_plots <- list()
        max_prop <- max(unlist(lapply(condition_vertical_props, max)))
        min_prop <- 0
        limits <- c(validate_range(max_legend, max_prop), validate_range(min_legend, min_prop, FALSE))

        for (cond in unique_conditions) {
          vert_props <- condition_vertical_props[[cond]]

          vert_data <- create_vertical_data_frame(vert_props)

          p <- ggplot2::ggplot(vert_data, ggplot2::aes(x = dc, fill = proportion)) +
            ggplot2::geom_tile(ggplot2::aes(y = 3.5, height = 6), color = "white", linewidth = 0.5) +
            ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9],
                                           midpoint = (limits[2] + limits[1]) / 2,
                                           limits = limits) +
            ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
            ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::labs(x = x_label, y = y_label, title = paste("Condition:", cond), fill = "Percentage (%)") +
            plot_theme

          condition_plots[[cond]] <- p
        }

        return(list(plots = condition_plots, prop_data = condition_vertical_props))
      }

    } else if (comparison_type == "difference") {
      if (length(unique_conditions) < 2) {
        stop("At least 2 conditions are required for difference comparison")
      }

      first_cond <- unique_conditions[1]
      second_cond <- unique_conditions[2]
      diff_vert <- condition_vertical_props[[first_cond]] - condition_vertical_props[[second_cond]]

      diff_data <- data.frame(dc = 1:6, difference = diff_vert)
      max_diff <- validate_range(max_legend, max(abs(diff_data$difference)))

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

      return(list(plot = p, first_condition = first_cond, second_condition = second_cond, diff_data = diff_vert))
    }
  }

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

  # Calculate constraint proportions
  calculate_constraint_props <- function(grid) {
    constraints <- c(
      grid[1, 1],
      grid[1, 2] + grid[2, 1],
      grid[1, 3] + grid[2, 2] + grid[3, 1],
      grid[1, 4] + grid[2, 3] + grid[3, 2] + grid[4, 1],
      grid[1, 5] + grid[2, 4] + grid[3, 3] + grid[4, 2] + grid[5, 1],
      grid[1, 6] + grid[2, 5] + grid[3, 4] + grid[4, 3] + grid[5, 2] + grid[6, 1],
      grid[2, 6] + grid[3, 5] + grid[4, 4] + grid[5, 3] + grid[6, 2],
      grid[3, 6] + grid[4, 5] + grid[5, 4] + grid[6, 3],
      grid[4, 6] + grid[5, 5] + grid[6, 4],
      grid[5, 6] + grid[6, 5],
      grid[6, 6]
    )
    return(constraints)
  }
  
  diagonal_band_width <- sqrt(72) / 11
  axis_projection <- diagonal_band_width * sqrt(2)
  constraint_polygons <- get_constraint_polygons(axis_projection)

  if ("proportion" %in% colnames(constraint_polygons)) {
    constraint_polygons$proportion <- NULL
  }

  if (proportion_type == "overall") {
    constraint_props <- calculate_constraint_props(prop_grid)
    limits <- range(validate_range(max_legend, max(constraint_props)), validate_range(min_legend, 0, FALSE))
    
    constraint_data <- data.frame(
      constraint = 2:12,
      proportion = constraint_props
    )
    
    plot_data <- merge(constraint_polygons, constraint_data, by = "constraint")
    
    p <- ggplot2::ggplot() +
      ggplot2::geom_polygon(data = plot_data, ggplot2::aes(x = x, y = y, fill = proportion, group = constraint), color = "white", linewidth = 0.5) +
      ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9], midpoint = mean(limits), limits = limits) +
      ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
      ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
      ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
      plot_theme
    
    return(list(plot = p, prop_data = constraint_props))
    
  } else if (proportion_type == "condition") {
    if (is.null(condition_grids)) {
      stop("condition_grids must be provided when proportion_type is 'condition'")
    }

    unique_conditions <- names(condition_grids)
    condition_constraint_props <- lapply(condition_grids, calculate_constraint_props)

    if (comparison_type == "separate") {
      if (length(unique_conditions) == 2) {
        cond1 <- unique_conditions[1]
        cond2 <- unique_conditions[2]
        const_props1 <- condition_constraint_props[[cond1]]
        const_props2 <- condition_constraint_props[[cond2]]
        
        limits <- range(validate_range(max_legend, max(c(const_props1, const_props2))), validate_range(min_legend, 0, FALSE))

        const_data1 <- data.frame(constraint = 2:12, proportion = const_props1, condition = cond1)
        const_data2 <- data.frame(constraint = 2:12, proportion = const_props2, condition = cond2)

        combined_const_data <- rbind(const_data1, const_data2)
        
        plot_data <- merge(constraint_polygons, combined_const_data, by = "constraint")
        
        p <- ggplot2::ggplot() +
          ggplot2::geom_polygon(data = plot_data, aes(x = x, y = y, fill = proportion, group = interaction(constraint, condition)), color = "white", linewidth = 0.5) +
          ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9], midpoint = mean(limits), limits = limits) +
          ggplot2::facet_wrap(~ condition, ncol = 2) +
          ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
          ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
          ggplot2::labs(x = x_label, y = y_label, fill = "Percentage (%)") +
          plot_theme
        
        return(list(plot = p, prop_data = condition_constraint_props))
        
      } else {
        condition_plots <- list()
        
        max_prop <- max(unlist(lapply(condition_constraint_props, max)))
        limits <- range(validate_range(max_legend, max_prop), validate_range(min_legend, 0, FALSE))

        for (cond in unique_conditions) {
          const_props <- condition_constraint_props[[cond]]
          const_data <- data.frame(constraint = 2:12, proportion = const_props)
          plot_data <- merge(constraint_polygons, const_data, by = "constraint")
          
          p <- ggplot2::ggplot() +
            ggplot2::geom_polygon(data = plot_data, aes(x = x, y = y, fill = proportion, group = constraint), color = "white", linewidth = 0.5) +
            ggplot2::scale_fill_gradient2(low = "white", mid = pal_colors[3], high = pal_colors[9], midpoint = mean(limits), limits = limits) +
            ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
            ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
            ggplot2::labs(x = x_label, y = y_label, title = paste("Condition:", cond), fill = "Percentage (%)") +
            plot_theme
            
          condition_plots[[cond]] <- p
        }
        
        return(list(plots = condition_plots, prop_data = condition_constraint_props))
      }
      
    } else if (comparison_type == "difference") {
      if (length(unique_conditions) < 2) {
        stop("At least 2 conditions are required for difference comparison")
      }

      first_cond <- unique_conditions[1]
      second_cond <- unique_conditions[2]
      diff_const <- condition_constraint_props[[first_cond]] - condition_constraint_props[[second_cond]]
      
      diff_data <- data.frame(constraint = 2:12, difference = diff_const)
      plot_data <- merge(constraint_polygons, diff_data, by = "constraint")
      
      max_diff <- max(abs(diff_const))
      limits <- c(-max_diff, max_diff)

      p <- ggplot2::ggplot() +
        ggplot2::geom_polygon(data = plot_data, aes(x = x, y = y, fill = difference, group = constraint), color = "white", linewidth = 0.5) +
        ggplot2::scale_fill_gradient2(low = neg_colors[9], mid = "white", high = pos_colors[9], midpoint = 0, limits = limits) +
        ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
        ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::labs(x = x_label, y = y_label, title = paste("Difference (%):", first_cond, "-", second_cond), fill = "Difference (%)") +
        plot_theme
      
      return(list(plot = p, first_condition = first_cond, second_condition = second_cond, diff_data = diff_const))
    }
  }
  
  stop("Unsupported combination of proportion_type and comparison_type")
}



create_depth_plot <- function(prop_grid, proportion_type = "overall", color_palette = "Greens", x_label = "Directedness", y_label = "Stickiness", condition_grids = NULL, comparison_type = "separate", pos_palette = "Greens", neg_palette = "Reds", max_legend = NULL, min_legend = NULL) {
  
  ## Common plot theme settings
  plot_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14, face = "italic"),
      legend.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(),
      plot.margin = grid::unit(c(1, 1, 2, 2), "lines")
    )
  
  ## Get color palettes
  pal_colors <- RColorBrewer::brewer.pal(9, color_palette)
  pos_colors <- RColorBrewer::brewer.pal(9, pos_palette)
  neg_colors <- RColorBrewer::brewer.pal(9, neg_palette)
  
  ## Calculate depth proportions
  calculate_depth_props <- function(grid) {
    ## Initialize matrix to store depth proportions for each quadrant (4 quadrants, 5 depths)
    depth_props <- matrix(0, nrow = 4, ncol = 5)
    
    ## Define quadrants and depths based on the actual grid cells
    ## This mapping aligns with the depth polygons from get_depth_polygons()
    
    ## Quadrant 1 (bottom-left, dc 1-3, ac 1-3)
    depth_props[1, 1] <- grid[3, 3]  ## Depth 1 - cell (3,3)
    depth_props[1, 2] <- grid[3, 2] + grid[2, 3]  ## Depth 2 - cells (3,2) and (2,3)
    depth_props[1, 3] <- grid[3, 1] + grid[2, 2] + grid[1, 3]  ## Depth 3 - cells (3,1), (2,2), (1,3)
    depth_props[1, 4] <- grid[2, 1] + grid[1, 2]  ## Depth 4 - cells (2,1) and (1,2)
    depth_props[1, 5] <- grid[1, 1]  ## Depth 5 - cell (1,1)
    
    ## Quadrant 2 (bottom-right, dc 4-6, ac 1-3)
    depth_props[2, 1] <- grid[3, 4]  ## Depth 1
    depth_props[2, 2] <- grid[3, 5] + grid[2, 4]  ## Depth 2
    depth_props[2, 3] <- grid[3, 6] + grid[2, 5] + grid[1, 4]  ## Depth 3
    depth_props[2, 4] <- grid[2, 6] + grid[1, 5]  ## Depth 4
    depth_props[2, 5] <- grid[1, 6]  ## Depth 5
    
    ## Quadrant 3 (top-left, dc 1-3, ac 4-6)
    depth_props[3, 1] <- grid[4, 3]  ## Depth 1
    depth_props[3, 2] <- grid[4, 2] + grid[5, 3]  ## Depth 2
    depth_props[3, 3] <- grid[4, 1] + grid[5, 2] + grid[6, 3]  ## Depth 3
    depth_props[3, 4] <- grid[5, 1] + grid[6, 2]  ## Depth 4
    depth_props[3, 5] <- grid[6, 1]  ## Depth 5
    
    ## Quadrant 4 (top-right, dc 4-6, ac 4-6)
    depth_props[4, 1] <- grid[4, 4]  ## Depth 1
    depth_props[4, 2] <- grid[4, 5] + grid[5, 4]  ## Depth 2
    depth_props[4, 3] <- grid[4, 6] + grid[5, 5] + grid[6, 4]  ## Depth 3
    depth_props[4, 4] <- grid[5, 6] + grid[6, 5]  ## Depth 4
    depth_props[4, 5] <- grid[6, 6]  ## Depth 5
    
    return(depth_props)
  }
  
  ## Get polygon data for depths from helper function
  depth_polygons <- get_depth_polygons()
  
  ## Handle different proportion types
  if (proportion_type == "overall") {
    ## Calculate depth proportions
    depth_props <- calculate_depth_props(prop_grid)
    
    ## Determine the limits for the legend
    max_value <- max(depth_props)
    min_value <- 0
    
    ## Check if max_legend is provided and valid
    if (!is.null(max_legend)) {
      if (max_legend < max_value) {
        warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
      } else {
        max_value <- max_legend
      }
    }
    
    ## Check if min_legend is provided and valid
    if (!is.null(min_legend)) {
      if (min_legend > min_value) {
        warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
      } else {
        min_value <- min_legend
      }
    }
    
    ## Copy polygon data for modification
    plot_data <- depth_polygons
    
    ## Add proportions to the polygon data
    for (q in 1:4) {
      for (d in 1:5) {
        ## Find polygons with matching quadrant and depth
        mask <- plot_data$quadrant == q & plot_data$depth == d
        if (any(mask)) {
          plot_data$proportion[mask] <- depth_props[q, d]
        }
      }
    }
    
    ## Create plot with depth polygons
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
    ## Handle condition-based visualizations
    if (is.null(condition_grids)) {
      stop("condition_grids must be provided when proportion_type is 'condition'")
    }
    
    unique_conditions <- names(condition_grids)
    
    ## Calculate depth proportions for each condition
    condition_depth_props <- lapply(condition_grids, calculate_depth_props)
    
    if (comparison_type == "separate") {
      if (length(unique_conditions) == 2) {
        ## Create a single figure with side-by-side plots for 2 conditions
        cond1 <- unique_conditions[1]
        cond2 <- unique_conditions[2]
        
        ## Calculate depth proportions for each condition
        depth_props1 <- condition_depth_props[[cond1]]
        depth_props2 <- condition_depth_props[[cond2]]
        
        ## Find maximum proportion across both conditions for consistent color scale
        max_prop <- max(max(depth_props1), max(depth_props2))
        min_prop <- 0
        
        ## Check max_legend
        if (!is.null(max_legend)) {
          if (max_legend < max_prop) {
            warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
          } else {
            max_prop <- max_legend
          }
        }
        
        ## Check min_legend
        if (!is.null(min_legend)) {
          if (min_legend > min_prop) {
            warning("min_legend value is greater than the minimum proportion. Using the minimum proportion instead.")
          } else {
            min_prop <- min_legend
          }
        }
        
        ## Create polygon data for each condition
        depth_polygons1 <- depth_polygons
        depth_polygons2 <- depth_polygons
        
        ## Add condition information and proportions
        for (q in 1:4) {
          for (d in 1:5) {
            ## Add proportions to matching polygons for condition 1
            mask1 <- depth_polygons1$quadrant == q & depth_polygons1$depth == d
            if (any(mask1)) {
              depth_polygons1$proportion[mask1] <- depth_props1[q, d]
            }
            
            ## Add proportions to matching polygons for condition 2
            mask2 <- depth_polygons2$quadrant == q & depth_polygons2$depth == d
            if (any(mask2)) {
              depth_polygons2$proportion[mask2] <- depth_props2[q, d]
            }
          }
        }
        
        depth_polygons1$condition <- cond1
        depth_polygons2$condition <- cond2
        
        ## Combine the polygon data
        combined_depth_data <- rbind(depth_polygons1, depth_polygons2)
        
        ## Create faceted plot
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
        ## Return a list of separate plots for more than 2 conditions
        condition_plots <- list()
        
        ## Find maximum proportion across all conditions
        all_max <- unlist(lapply(condition_depth_props, function(x) max(x)))
        max_prop <- max(all_max)
        min_prop <- 0
        
        ## Check max_legend
        if (!is.null(max_legend)) {
          if (max_legend < max_prop) {
            warning("max_legend value is less than the maximum proportion. Using the maximum proportion instead.")
          } else {
            max_prop <- max_legend
          }
        }
        
        ## Check min_legend
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
          
          ## Add proportion values to the polygon data
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
          plots = condition_plots,
          prop_data = condition_depth_props
        ))
      }
      
    } else if (comparison_type == "difference") {
      ## For difference comparison, we use the first two conditions
      if (length(unique_conditions) < 2) {
        stop("At least 2 conditions are required for difference comparison")
      }
      
      first_cond <- unique_conditions[1]
      second_cond <- unique_conditions[2]
      
      ## Calculate differences
      depth_props1 <- condition_depth_props[[first_cond]]
      depth_props2 <- condition_depth_props[[second_cond]]
      diff_depth <- depth_props1 - depth_props2
      
      ## Copy polygon data for difference visualization
      diff_polygons <- depth_polygons
      
      ## Add difference values to the polygon data
      for (q in 1:4) {
        for (d in 1:5) {
          mask <- diff_polygons$quadrant == q & diff_polygons$depth == d
          if (any(mask)) {
            diff_polygons$difference[mask] <- diff_depth[q, d]
          }
        }
      }
      
      ## Get max absolute difference for symmetric color scale
      max_diff <- max(abs(diff_depth))
      
      ## Check max_legend for difference view
      if (!is.null(max_legend)) {
        if (max_legend < max_diff) {
          warning("max_legend value is less than the maximum difference. Using the maximum difference instead.")
        } else {
          max_diff <- max_legend
        }
      }
      
      ## Check min_legend for difference view (should be negative of max for diverging scale)
      if (!is.null(min_legend)) {
        if (min_legend > -max_diff) {
          warning("min_legend value is greater than the negative maximum difference. Using the symmetric range instead.")
        } else {
          ## Only use min_legend if it's explicitly set and valid
          max_diff <- max(max_diff, abs(min_legend))
        }
      }
      
      ## Create diverging plot
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
  
  ## If we reach here, there's an unsupported combination
  stop("Unsupported combination of proportion_type and comparison_type")
}
