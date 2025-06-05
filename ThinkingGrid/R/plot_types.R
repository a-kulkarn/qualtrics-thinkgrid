## Load required packages

################################################################################
## Util.

get_quadrant_6x6 <- function(i, j) {
    (i < 4) && (j < 4) && return(1)
    (i < 4) && (j > 3) && return(2)
    (i > 3) && (j < 4) && return(3)    
    return(4)
}

################################################################################
## Validation.

validate_range <- function(value, ref_value, is_max = TRUE) {
    if (!is.null(value)) {
        if ((is_max && value < ref_value) || (!is_max && value > ref_value)) {
            stop(paste(ifelse(is_max, "max_legend", "min_legend"),
                          "value is out of bounds."))
            return(ref_value)
        }
        return(value)
    }
    return(ref_value)
}

################################################################################
## Plot creation base.

base_plot_theme <- function() {
    plot_theme <- ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 14, face = "italic"),
      legend.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(),
      plot.margin = grid::unit(c(1, 1, 2, 2), "lines")
    )
}

default_colorer <- function(with_negatives = FALSE) {
    ## Get color palettes
    pal_colors <- RColorBrewer::brewer.pal(9, "Greens")
    pos_colors <- RColorBrewer::brewer.pal(9, "Greens")
    neg_colors <- RColorBrewer::brewer.pal(9, "Reds")


    if (with_negatives) {
        function (limits) {
            ggplot2::scale_fill_gradient2(
                         low = neg_colors[9],
                         mid = "white",
                         high = pos_colors[9],
                         midpoint = 0,
                         limits = limits
                     )
        }

    } else {
        function (limits) {
            ggplot2::scale_fill_gradient2(
                         low = "white",
                         mid = pal_colors[3],
                         high = pal_colors[9],
                         midpoint = mean(limits), 
                         limits = limits
                     )
        }
    }
}

################################################################################
## General Toolsing.

## YYY
plot_engine <- function(data,
                        limits,
                        x_label,
                        y_label,
                        colorer,
                        aesthetics,
                        geometry,
                        comparison_type = NULL,
                        title = NULL,
                        plot_title = NULL,
                        legend_title = NULL,
                        plot_subtitle = NULL) {
    
    p <- ggplot2::ggplot(data) +
        aesthetics + 
        geometry(data) +
        colorer(limits) +
        ggplot2::coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
        ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
        ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
        base_plot_theme()

    # For difference plots, generate a title if not provided
    if (comparison_type == "difference") {
        # Use user-provided plot_title if available, otherwise use default difference title
        diff_title <- ifelse(!is.null(plot_title), plot_title, title)
        p <- p + ggplot2::labs(title = diff_title)
    } else if (!is.null(plot_title)) {
        # For other plots, use plot_title if provided
        p <- p + ggplot2::labs(title = plot_title)
    }
    
    # Add subtitle if provided
    if (!is.null(plot_subtitle)) {
        p <- p + ggplot2::labs(subtitle = plot_subtitle)
    }

    # Set fill label
    if (!is.null(comparison_type) && comparison_type == "difference") {
        fill_title <- ifelse(is.null(legend_title), "Difference (%)", legend_title)
    } else {
        fill_title <- ifelse(is.null(legend_title), "Percentage (%)", legend_title)
    }
    
    # Set axis labels and fill title
    p <- p + ggplot2::labs(x = x_label, y = y_label, fill = fill_title)

    return(p)
}


create_overall_plot <- function(data,
                                proportioner,
                                plotter,
                                framer,
                                min_legend,
                                max_legend,
                                plot_subtitle = NULL) {

    proportions <- proportioner(data)
    limits <- c(validate_range(min_legend, 0, FALSE),
                validate_range(max_legend, max(proportions)))

    vertical_data <- framer()
    vertical_data$fill_value <- proportions

    p <- plotter(vertical_data, limits, plot_subtitle = plot_subtitle)
    
    return(list(plot = p, prop_data = proportions))
}

create_separate_plot <- function(data,
                               proportioner,
                               plotter,
                               framer,
                               min_legend,
                               max_legend,
                               plot_subtitle = NULL) {

    condition_grids <- data
    unique_conditions <- names(condition_grids)
    proportions <- lapply(condition_grids, proportioner)

    # Check if we have subtitles and validate them
    has_subtitles <- !is.null(plot_subtitle) && length(plot_subtitle) > 0
    if (has_subtitles) {
        if (length(plot_subtitle) != length(unique_conditions)) {
            stop(paste("Number of subtitles (", length(plot_subtitle), 
                     ") does not match number of conditions (", 
                     length(unique_conditions), "). Please provide exactly one subtitle per condition."))
        }
        
        # Print which condition uses which subtitle for clarity
        for (i in seq_along(unique_conditions)) {
            message(paste("Condition '", unique_conditions[i], "' uses subtitle: '", plot_subtitle[i], "'", sep=""))
        }
    }

    if (length(unique_conditions) == 2) {
        cond1 <- unique_conditions[1]
        cond2 <- unique_conditions[2]

        props1 <- proportions[[cond1]]
        props2 <- proportions[[cond2]]

        max_prop <- max(max(props1), max(props2))
        min_prop <- 0

        limits <- c(validate_range(min_legend, min_prop, FALSE),
                    validate_range(max_legend, max_prop))

        data1 <- framer()
        data1$fill_value <- props1
        data1$condition <- cond1

        data2 <- framer()
        data2$fill_value <- props2
        data2$condition <- cond2
        
        combined_data <- rbind(data1, data2)

        # Create the plot with facets
        p <- plotter(combined_data, limits)
        
        # If we have subtitles, use them as facet labels instead of condition names
        if (has_subtitles) {
            # Create custom facet labels using just the subtitles
            facet_labels <- setNames(plot_subtitle, unique_conditions)
            
            # Apply custom labelled facets
            p <- p + ggplot2::facet_wrap(~ condition, ncol = 2, labeller = ggplot2::as_labeller(facet_labels))
        } else {
            # Default facets with condition names
            p <- p + ggplot2::facet_wrap(~ condition, ncol = 2)
        }
        
        return(list(plot = p, prop_data = proportions))

    } else {
        condition_plots <- list()
        max_prop <- max(unlist(lapply(proportions, max)))
        min_prop <- 0
        
        limits <- c(validate_range(min_legend, min_prop, FALSE),
                    validate_range(max_legend, max_prop))

        for (i in seq_along(unique_conditions)) {
            cond <- unique_conditions[i]
            data <- framer()
            data$fill_value <- proportions[[cond]]
            
            # Use corresponding subtitle if available
            subtitle <- NULL
            if (has_subtitles) {
                subtitle <- plot_subtitle[i]
            }
            
            condition_plots[[cond]] <- plotter(data, limits, plot_subtitle = subtitle)
        }

        return(list(plot = condition_plots, prop_data = proportions))
    }
}

create_difference_plot <- function(data,
                                   proportioner,
                                   plotter,
                                   framer,
                                   min_legend,
                                   max_legend,
                                   plot_subtitle = NULL) {

    condition_grids <- data
    unique_conditions <- names(condition_grids)
    proportions <- lapply(condition_grids, proportioner)

    if (length(unique_conditions) != 2) {
        stop("Exactly 2 conditions are required for difference comparison.")
    }

    first_cond <- unique_conditions[1]
    second_cond <- unique_conditions[2]

    diff_data <- framer()
    diff_data$fill_value <- proportions[[first_cond]] - proportions[[second_cond]]
    
    max_diff <- validate_range(max_legend, max(abs(diff_data$fill_value)))
    limits <- c(-max_diff, max_diff)
    
    # Generate the difference description
    diff_desc <- paste("Difference (%):", first_cond, "-", second_cond)
    
    # Pass diff_desc as title parameter - this is what was missing!
    p <- plotter(diff_data, limits, title = diff_desc, plot_subtitle = plot_subtitle)

    return(list(
        plot = p,
        first_condition = first_cond,
        second_condition = second_cond,
        diff_data = diff_data
    ))
}

get_plot_method <- function(proportion_type, comparison_type) {
    if (proportion_type == "overall") {
        return(create_overall_plot)

    } else if (proportion_type == "condition") {
        if (comparison_type == "separate") {
            return(create_separate_plot)

        } else if (comparison_type == "difference") {
            ## NOTE: The aesthetics fill might be different.
            return(create_difference_plot)
        }
    } else {
        stop("Unsupported combination of proportion_type and comparison_type")
    }
}

compile_plot_creator <- function(proportioner,
                                 framer,
                                 aesthetics,
                                 geometry) {

    function(prop_grid,
             proportion_type = "overall",
             colorer = NULL,
             x_label = "Directedness",
             y_label = "Stickiness",
             condition_grids = NULL,
             comparison_type = "separate",
             max_legend = NULL,
             min_legend = NULL,
             plot_title = NULL,
             legend_title = NULL,
             plot_subtitle = NULL) {

        if (is.null(colorer)) {
            colorer <- default_colorer(with_negatives = (comparison_type == "difference"))
        } else {
            stop("Not Implemented.")
        }

        plotter <- function(data, limits, title = NULL, plot_subtitle = NULL) {
            plot_engine(
                data,
                limits,
                x_label,
                y_label,
                colorer,
                aesthetics,
                geometry,
                comparison_type = comparison_type,
                title = title,
                plot_title = plot_title,
                legend_title = legend_title,
                plot_subtitle = plot_subtitle
            )
        }

        meth <- get_plot_method(proportion_type, comparison_type)

        if (proportion_type == "overall") {
            data <- prop_grid
        } else {
            data <- condition_grids
        }

        ## Create the plot.
        return(meth(
            data = data,
            proportioner = proportioner,
            plotter = plotter,
            framer = framer,            
            min_legend,
            max_legend,
            plot_subtitle = plot_subtitle
        ))
    }

}

################################################################################
## Cells.

# TODO: Implement this function.
compile_cells_plot_creator <- function() {

  create_plot_data <- function(grid, condition = NULL) {
      plot_data <- base::expand.grid(dc = 1:6, ac = 1:6)
      plot_data$proportion <- as.vector(t(grid))
      if (!is.null(condition)) plot_data$condition <- condition
      return(plot_data)
  }
  # For cells plot, the proportioner simply returns the cell values in vector form
  proportioner <- function(grid) {
    as.vector(t(grid))  # Convert matrix to vector in column-major order
  }

  # Create data frame for cells plot
  framer <- function() {
    data.frame(dc = rep(1:6, each = 6), ac = rep(1:6, times = 6))
  }
  
  # Define the aesthetics for cell tiles
  aesthetics <- ggplot2::aes(x = dc, y = ac, fill = fill_value)
  
  # Define the geometry for cell tiles
  geometry <- function(data) {
    ggplot2::geom_tile(color = "white", linewidth = 0.5)
  }
  
  # Compile the plot creator
  compile_plot_creator(proportioner, framer, aesthetics, geometry)
}
create_cells_plot <- compile_cells_plot_creator()

################################################################################
## Quadrants.

# TODO: Implement this function.
compile_quadrant_plot_creator <- function() {

  # Calculate and plot quadrant proportions
  create_quadrant_data <- function(grid) {
    proportions <- matrix(0, nrow = 2, ncol = 2)
    proportions[1, 1] <- sum(grid[1:3, 1:3])  # Bottom-left
    proportions[1, 2] <- sum(grid[1:3, 4:6])  # Bottom-right
    proportions[2, 1] <- sum(grid[4:6, 1:3])  # Top-left
    proportions[2, 2] <- sum(grid[4:6, 4:6])  # Top-right
    return(proportions)
  }

    # Define the proportioner function to extract values from the grid
  proportioner <- function(grid) {
    quad_data <- create_quadrant_data(grid)
    return(as.vector(t(quad_data)))  # Convert to vector in column-major order
  }
  
  # Create data frame for quadrant plot
  framer <- function() {
    data.frame(
      x = c(2, 5, 2, 5),  # centers of the quadrants
      y = c(2, 2, 5, 5),
      width = rep(3, 4),  # each quadrant is 3x3
      height = rep(3, 4),
      quadrant = 1:4      # quadrant identifiers
    )
  }
  
  # Define the aesthetics for quadrant tiles
  aesthetics <- ggplot2::aes(x = x, y = y, fill = fill_value, group = quadrant)
  
  # Define the geometry for quadrant tiles
  geometry <- function(data) {
    ggplot2::geom_tile(
      ggplot2::aes(width = width, height = height),
      color = "white", 
      linewidth = 0.5
    )
  }
  
  # Compile the plot creator
  compile_plot_creator(proportioner, framer, aesthetics, geometry)
}
create_quadrants_plot <- compile_quadrant_plot_creator()

################################################################################
## Vertical.

compile_vertical_plot_creator <- function() {

    ## Calculate vertical proportions
    proportioner <- function(grid) {
        colSums(grid)
    }

    ## Create data frame for vertical band plot
    framer <- function() {
        data.frame(dc = 1:6)
    }
    
    aesthetics <- ggplot2::aes(x = dc, fill = fill_value)

    geometry <- function(data) {
        ggplot2::geom_tile(
                     ggplot2::aes(y = 3.5, height = 6),
                     color = "white",
                     linewidth = 0.5
                 )
    }

    compile_plot_creator(proportioner, framer, aesthetics, geometry)
}

create_vertical_plot <- compile_vertical_plot_creator()

################################################################################
## Horizontal.

compile_horizontal_plot_creator <- function() {

    ## Calculate vertical proportions
    proportioner <- function(grid) {
        rowSums(grid)
    }

    ## Create data frame for vertical band plot
    framer <- function(row_sums) {
        data.frame(ac = 1:6)
    }
    
    aesthetics <- ggplot2::aes(y = ac, fill = fill_value)

    geometry <- function(data) {
        ggplot2::geom_tile(
                     ggplot2::aes(x = 3.5, width = 6),
                     color = "white",
                     linewidth = 0.5
                 )
    }

    compile_plot_creator(proportioner, framer, aesthetics, geometry)
}

create_horizontal_plot <- compile_horizontal_plot_creator()


################################################################################
## Diagonal constraint.

## TODO: Implement this function.
compile_constraints_plot_creator <- function() {
    
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
  
  proportioner <- function(prop_grid) {
    constraint_props <- calculate_constraint_props(prop_grid)
    
    # Get fresh polygon data for each call
    plot_data <- get_constraint_polygons(axis_projection)
    # Adjust for the constraint numbering (2-12 instead of 1-11)
    for (i in 2:12) {
      mask <- plot_data$constraint == i
      if (any(mask)) {
        # Use i-1 to access the correct element in constraint_props
        plot_data$proportion[mask] <- constraint_props[i-1]
      }
    }
    return(plot_data$proportion)
  }
  
  framer <- function() {
    get_constraint_polygons(axis_projection)
  }
  
  # Rest of function remains the same
  aesthetics <- ggplot2::aes(x = x,
                             y = y,
                             fill = fill_value,
                             group = constraint)
  
  geometry <- function(data) {
    ggplot2::geom_polygon(
      color = "white",
      linewidth = 0.5
    )
  }
  
  return(compile_plot_creator(proportioner, framer, aesthetics, geometry))
}
create_constraints_plot <- compile_constraints_plot_creator()
################################################################################
## Depth.

calculate_depth_props <- function(grid) {
    depth_props <- matrix(0, nrow = 4, ncol = 5)
    for (i in 1:6) {
        for (j in 1:6) {
            q <- get_quadrant_6x6(i, j)
            d <- round(abs(i - 3.5) + abs(j - 3.5))
            depth_props[q, d] <- depth_props[q, d] + grid[i, j]
        }
    }
    return(depth_props)
}

## YYY:
compile_depth_plot_creator <- function() {
    
    proportioner <- function(prop_grid) {
        depth_props <- calculate_depth_props(prop_grid)

        depth_polygons <- get_depth_polygons()
        plot_data <- depth_polygons

        for (q in 1:4) {
            for (d in 1:5) {
                mask <- plot_data$quadrant == q & plot_data$depth == d
                if (any(mask)) {
                    plot_data$proportion[mask] <- depth_props[q, d]
                }
            }
        }
        return(plot_data$proportion)
    }
    
    framer <- function() {
        get_depth_polygons()
    }
    
    aesthetics <- ggplot2::aes(x = x,
                               y = y,
                               fill = fill_value,
                               group = interaction(quadrant, depth))
    
    geometry <- function(data) {
        ggplot2::geom_polygon(
                     data = data,
                     color = "white",
                     linewidth = 0.5
                 )
    }

    return(compile_plot_creator(proportioner, framer, aesthetics, geometry))
}

create_depth_plot <- compile_depth_plot_creator()

