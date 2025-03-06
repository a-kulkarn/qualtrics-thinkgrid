if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    install.packages("RColorBrewer")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
}

library(RColorBrewer)
library(ggplot2)

plot_tg <- function(dc, ac, type = "cells", color_palette = "Greens") {
    # check if ac and dc are either both vectors or both matrices
    if (is.vector(dc) && is.vector(ac)) {
        if (length(dc) != length(ac)) {
            stop("dc and ac must have the same length")
        }
    } else if (is.matrix(dc) && is.matrix(ac)) {
        if (nrow(dc) != nrow(ac) || ncol(dc) != ncol(ac)) {
            stop("dc and ac must have the same dimensions")
        }
    } else {
        stop("dc and ac must be either both vectors or both matrices")
    }

    # Validate color palette
    if (!color_palette %in% rownames(brewer.pal.info)) {
        warning("Invalid color palette. Using default Greens palette.")
        color_palette <- "Greens"
    }

    valid_types <- c("quadrants", "horizontal", "vertical", "constraints", "depth", "cells")
    if (!(type %in% valid_types)) {
        stop("type must be one of ", paste(valid_types, collapse = ", "))
    }

    grid <- matrix(0, nrow = 6, ncol = 6)
    
    # Count occurrences of each (dc, ac) pair
    for (i in 1:length(dc)) {
        # Ensure values are within 1-6 range
        if (dc[i] >= 1 && dc[i] <= 6 && ac[i] >= 1 && ac[i] <= 6) {
            grid[ac[i], dc[i]] <- grid[ac[i], dc[i]] + 1
        }
    }
    
    # Calculate proportions - common for all plot types
    total <- sum(grid)
    if (total > 0) {
        prop_grid <- grid / total
    } else {
        prop_grid <- grid
    }
    
    # Common plot theme settings
    plot_theme <- theme_minimal() +
        theme(
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 14, face = "italic"),
            legend.title = element_text(size = 12),
            panel.grid = element_blank(),
            plot.margin = unit(c(1, 1, 2, 2), "lines")
        )
    
    # Common labels
    plot_labels <- labs(
        x = "Directedness",
        y = "Stickiness",
        fill = "Proportion"
    )
    
    # Plot based on type
    if (type == "cells") {
        # Convert matrix to data frame for plotting
        plot_data <- expand.grid(dc = 1:6, ac = 1:6)
        plot_data$proportion <- as.vector(t(prop_grid))
        
        # Create plot with origin at bottom left
        p <- ggplot(plot_data, aes(x = dc, y = ac, fill = proportion)) +
            geom_tile(color = "white", linewidth = 0.5) +
            scale_fill_distiller(palette = color_palette, direction = 1, limits = c(0, max(prop_grid))) +
            coord_fixed() +
            scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
            scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
            plot_labels +
            plot_theme
        
        return(list(plot = p, prop_data = plot_data$proportion))
    }
    
    else if (type == "quadrants") {
        # Calculate proportions by quadrant
        quadrant_grid <- matrix(0, nrow = 2, ncol = 2)
        
        # Define quadrants (bottom-left, bottom-right, top-left, top-right)
        quadrant_grid[1,1] <- sum(prop_grid[1:3, 1:3])  # Bottom-left
        quadrant_grid[1,2] <- sum(prop_grid[1:3, 4:6])  # Bottom-right
        quadrant_grid[2,1] <- sum(prop_grid[4:6, 1:3])  # Top-left
        quadrant_grid[2,2] <- sum(prop_grid[4:6, 4:6])  # Top-right
        
        # Create data frame for quadrant plotting
        quad_data <- data.frame(
            dc_quad = rep(c(1, 2), each = 2),
            ac_quad = rep(c(1, 2), times = 2),
            proportion = c(quadrant_grid[1,1], quadrant_grid[2,1], 
                          quadrant_grid[1,2], quadrant_grid[2,2])
        )
        
        # Create plot for quadrants
        p <- ggplot(quad_data, aes(x = dc_quad, y = ac_quad, fill = proportion)) +
            geom_tile(color = "white", linewidth = 0.5) +
            scale_fill_distiller(palette = color_palette, direction = 1, limits = c(0, max(quad_data$proportion))) +
            coord_fixed(ratio = 1, xlim = c(0.5, 2.5), ylim = c(0.5, 2.5)) +
            scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
            scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
            plot_labels +
            plot_theme
        
        return(p)
    }
    
    else if (type == "horizontal") {
        # Aggregate horizontally - sum across each row (ac value)
        row_sums <- rowSums(prop_grid)
        
        # Create data frame for horizontal bands - one entry per row
        horizontal_data <- data.frame(
            ac = 1:6,
            proportion = row_sums
        )
        
        # Create plot with horizontal bands - spanning the full width
        p <- ggplot(horizontal_data, aes(y = ac, fill = proportion)) +
            geom_tile(aes(x = 3.5, width = 6), color = "white", linewidth = 0.5) +
            scale_fill_distiller(palette = color_palette, direction = 1, limits = c(0, max(row_sums))) +
            coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
            scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
            scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
            plot_labels +
            plot_theme
        
        # Return both plot and proportion data
        return(list(plot = p, prop_data = row_sums))
    }

    else if (type == "vertical") {
        # Aggregate vertically - sum across each column (dc value)
        col_sums <- colSums(prop_grid)
        
        # Create data frame for vertical bands - one entry per column
        vertical_data <- data.frame(
            dc = 1:6,
            proportion = col_sums
        )
        
        # Create plot with vertical bands - spanning the full height
        p <- ggplot(vertical_data, aes(x = dc, fill = proportion)) +
            geom_tile(aes(y = 3.5, height = 6), color = "white", linewidth = 0.5) +
            scale_fill_distiller(palette = color_palette, direction = 1, limits = c(0, max(col_sums))) +
            coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
            scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
            scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
            plot_labels +
            plot_theme
        
        # Return both plot and proportion data
        return(list(plot = p, prop_data = col_sums))
    }

    else if (type == "constraints") {
        # Calculate constraint levels (ac + dc)
        constraint_levels <- 2:12  # All constraint levels
        constraint_props <- numeric(length(constraint_levels))
        
        # Calculate proportions for each constraint level
        for (i in 1:length(constraint_levels)) {
            constraint <- constraint_levels[i]
            constraint_sum <- 0
            for (ac_val in 1:6) {
                for (dc_val in 1:6) {
                    if (ac_val + dc_val == constraint) {
                        constraint_sum <- constraint_sum + prop_grid[ac_val, dc_val]
                    }
                }
            }
            constraint_props[i] <- constraint_sum
        }
        
        # Calculate band width
        diagonal_length <- sqrt(72)  # From (0.5,0.5) to (6.5,6.5)
        diagonal_band_width <- diagonal_length / 11  # Width perpendicular to diagonal
        axis_projection <- diagonal_band_width * sqrt(2)  # Projection onto axes
        
        # Create diagonal bands data frame with properly calculated polygons
        diagonal_data <- data.frame()
        
        # Constraint = 2 (bottom-left triangle)
        diagonal_data <- rbind(diagonal_data, data.frame(
            x = c(0.5, 0.5 + axis_projection, 0.5),
            y = c(0.5, 0.5, 0.5 + axis_projection),
            constraint = 2,
            proportion = constraint_props[1]
        ))
        
        # Constraint = 3 (parallelogram) 
        diagonal_data <- rbind(diagonal_data, data.frame(
            x = c(0.5, 0.5 + axis_projection, 0.5 + 2*axis_projection, 0.5),
            y = c(0.5 + axis_projection, 0.5, 0.5, 0.5 + 2*axis_projection),
            constraint = 3,
            proportion = constraint_props[2]
        ))
        
        # Constraint = 4 (parallelogram) 
        diagonal_data <- rbind(diagonal_data, data.frame(
            x = c(0.5, 0.5 + 2*axis_projection, 0.5 + 3*axis_projection, 0.5),
            y = c(0.5 + 2*axis_projection, 0.5, 0.5, 0.5 + 3*axis_projection),
            constraint = 4,
            proportion = constraint_props[3]
        ))
        
        # Constraint = 5 (parallelogram)
        diagonal_data <- rbind(diagonal_data, data.frame(
            x = c(0.5, 0.5 + 3*axis_projection, 0.5 + 4*axis_projection, 0.5),
            y = c(0.5 + 3*axis_projection, 0.5, 0.5, 0.5 + 4*axis_projection),
            constraint = 5,
            proportion = constraint_props[4]
        ))
        
        # Constraint = 6 (parallelogram)
        diagonal_data <- rbind(diagonal_data, data.frame(
            x = c(0.5, 0.5 + 4*axis_projection, 0.5 + 5*axis_projection, 0.5),
            y = c(0.5 + 4*axis_projection, 0.5, 0.5, 0.5 + 5*axis_projection),
            constraint = 6,
            proportion = constraint_props[5]
        ))
        
        # Constraint = 7 (hexagon)
        diagonal_data <- rbind(diagonal_data, data.frame(
            x = c(0.5, 0.5, 0.5 + 5*axis_projection, 6.5, 6.5, 0.5 + axis_projection),
            y = c(6.5, 0.5 + 5*axis_projection, 0.5, 0.5, 0.5 + axis_projection, 6.5),
            constraint = 7,
            proportion = constraint_props[6]
        ))
        
        # Constraint = 8 (parallelogram) 
        diagonal_data <- rbind(diagonal_data, data.frame(
            x = c(6.5 - 5*axis_projection, 6.5, 6.5, 6.5 - 4*axis_projection),
            y = c(6.5, 6.5 - 5*axis_projection, 6.5 - 4*axis_projection, 6.5),
            constraint = 8,
            proportion = constraint_props[7]
        ))
        
        # Constraint = 9 (parallelogram) 
        diagonal_data <- rbind(diagonal_data, data.frame(
            x = c(6.5 - 4*axis_projection, 6.5, 6.5, 6.5 - 3*axis_projection),
            y = c(6.5, 6.5 - 4*axis_projection, 6.5 - 3*axis_projection, 6.5),
            constraint = 9,
            proportion = constraint_props[8]
        ))
        
        # Constraint = 10 (parallelogram) 
        diagonal_data <- rbind(diagonal_data, data.frame(
            x = c(6.5 - 3*axis_projection, 6.5, 6.5, 6.5 - 2*axis_projection),
            y = c(6.5, 6.5 - 3*axis_projection, 6.5 - 2*axis_projection, 6.5),
            constraint = 10,
            proportion = constraint_props[9]
        ))
        
        # Constraint = 11 (parallelogram) 
        diagonal_data <- rbind(diagonal_data, data.frame(
            x = c(6.5 - 2*axis_projection, 6.5, 6.5, 6.5 - axis_projection),
            y = c(6.5, 6.5 - 2*axis_projection, 6.5 - axis_projection, 6.5),
            constraint = 11,
            proportion = constraint_props[10]
        ))
        
        # Special case: Constraint = 12 (top-right triangle)
        diagonal_data <- rbind(diagonal_data, data.frame(
            x = c(6.5 - axis_projection, 6.5, 6.5),
            y = c(6.5, 6.5 - axis_projection, 6.5),
            constraint = 12,
            proportion = constraint_props[11]
        ))
        
        # Then modify the factor levels definition to use numeric constraint values
        diagonal_data$constraint <- factor(diagonal_data$constraint, 
                                       levels = c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))

        p <- ggplot(diagonal_data, aes(x = x, y = y, fill = proportion, group = constraint)) +
            geom_polygon(color = "white", linewidth = 0.5) +
            scale_fill_distiller(palette = color_palette, direction = 1, limits = c(0, max(constraint_props))) +
            # Add quadrant dividing lines in white
            
            coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
            scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
            scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
            labs(x = "Directedness", y = "Stickiness") +
            plot_theme +
            theme(
                plot.title = element_blank(),
                panel.grid = element_blank()
            )
        
        # Return both plot and proportion data
        return(list(
            plot = p,
            prop_data = setNames(constraint_props, constraint_levels)
        ))
    }
    else if (type == "depth") {
        # Create a data frame for all cells with quadrant and depth info
        quad_depth_data <- expand.grid(dc = 1:6, ac = 1:6)

        # Assign quadrant to each cell
        quad_depth_data$quadrant <- ifelse(quad_depth_data$dc <= 3 & quad_depth_data$ac <= 3, 1,
                                       ifelse(quad_depth_data$dc > 3 & quad_depth_data$ac <= 3, 2,
                                           ifelse(quad_depth_data$dc <= 3 & quad_depth_data$ac > 3, 3, 4)))

        # Calculate quadrant depths
        q1_cells <- quad_depth_data$quadrant == 1
        quad_depth_data$depth[q1_cells] <- 6 - (quad_depth_data$dc[q1_cells] + quad_depth_data$ac[q1_cells] - 1)

        q2_cells <- quad_depth_data$quadrant == 2
        quad_depth_data$depth[q2_cells] <- 6 - ((7 - quad_depth_data$dc[q2_cells]) + quad_depth_data$ac[q2_cells] - 1)

        q3_cells <- quad_depth_data$quadrant == 3
        quad_depth_data$depth[q3_cells] <- 6 - (quad_depth_data$dc[q3_cells] + (7 - quad_depth_data$ac[q3_cells]) - 1)

        q4_cells <- quad_depth_data$quadrant == 4
        quad_depth_data$depth[q4_cells] <- 6 - ((7 - quad_depth_data$dc[q4_cells]) + (7 - quad_depth_data$ac[q4_cells]) - 1)

        # Add proportion values to each cell
        quad_depth_data$proportion <- 0
        for (i in 1:nrow(quad_depth_data)) {
            dc_val <- quad_depth_data$dc[i]
            ac_val <- quad_depth_data$ac[i]
            quad_depth_data$proportion[i] <- prop_grid[ac_val, dc_val]
        }

        # Calculate proportions by quadrant and depth
        quad_depth_summary <- aggregate(proportion ~ quadrant + depth, 
                                      data = quad_depth_data, 
                                      FUN = sum)

        # Create an empty data frame to store polygon data
        polygon_data <- data.frame()

        diagonal_band_width <- sqrt(18) / 5 
        axis_projection <- diagonal_band_width * sqrt(2)

        # Create bands for bottom-left quadrant (quadrant 1)

        # Depth 1 - triangle at center
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(3.5, 3.5 - axis_projection, 3.5),
            y = c(3.5, 3.5, 3.5 - axis_projection),
            quadrant = 1,
            depth = 1,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 1 & quad_depth_summary$depth == 1]
        ))

        # Depth 2 - parallelogram with proper 4 corners
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(3.5 - axis_projection, 3.5 - 2*axis_projection, 3.5, 3.5),
            y = c(3.5, 3.5, 3.5 - 2 * axis_projection, 3.5 - axis_projection),
            quadrant = 1,
            depth = 2,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 1 & quad_depth_summary$depth == 2]
        ))

        # Depth 3 - hexagon (middle)
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(3.5 - 2*axis_projection, 0.5, 0.5, 0.5 + 2 * axis_projection, 3.5, 3.5),
            y = c(3.5,3.5, 0.5 + 2* axis_projection, 0.5, 0.5, 3.5 - 2*axis_projection),
            quadrant = 1,
            depth = 3,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 1 & quad_depth_summary$depth == 3]
        ))

        # Depth 4 - parallelogram
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(0.5, 0.5, 0.5 + 2*axis_projection, 0.5 + axis_projection),
            y = c(0.5 + 2*axis_projection, 0.5, 0.5, 0.5 + axis_projection),
            quadrant = 1,
            depth = 4,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 1 & quad_depth_summary$depth == 4]
        ))

        # Depth 5 - triangle at corner
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(0.5, 0.5 + axis_projection, 0.5),
            y = c(0.5, 0.5, 0.5 + axis_projection),
            quadrant = 1,
            depth = 5,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 1 & quad_depth_summary$depth == 5]
        ))

        # Create bands for bottom-right quadrant (quadrant 2)

        # Depth 1 - triangle at center
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(3.5, 3.5 + axis_projection, 3.5),
            y = c(3.5, 3.5, 3.5 - axis_projection),
            quadrant = 2,
            depth = 1,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 2 & quad_depth_summary$depth == 1]
        ))

        # Depth 2 - parallelogram with proper 4 corners
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(3.5 + axis_projection, 3.5 + 2*axis_projection, 3.5, 3.5),
            y = c(3.5, 3.5, 3.5 - 2*axis_projection, 3.5 - axis_projection),
            quadrant = 2,
            depth = 2,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 2 & quad_depth_summary$depth == 2]
        ))

        # Depth 3 - hexagon (middle)
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(3.5 + 2*axis_projection, 6.5, 6.5, 6.5 - 2*axis_projection, 3.5, 3.5),
            y = c(3.5, 3.5, 0.5 + 2*axis_projection, 0.5, 0.5, 3.5 - 2*axis_projection),
            quadrant = 2,
            depth = 3,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 2 & quad_depth_summary$depth == 3]
        ))

        # Depth 4 - parallelogram
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(6.5, 6.5, 6.5 - 2*axis_projection, 6.5 - axis_projection),
            y = c(0.5 + 2*axis_projection, 0.5, 0.5, 0.5 + axis_projection),
            quadrant = 2,
            depth = 4,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 2 & quad_depth_summary$depth == 4]
        ))

        # Depth 5 - triangle at corner
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(6.5, 6.5 - axis_projection, 6.5),
            y = c(0.5, 0.5, 0.5 + axis_projection),
            quadrant = 2,
            depth = 5,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 2 & quad_depth_summary$depth == 5]
        ))

        # Create bands for top-left quadrant (quadrant 3)

        # Depth 1 - triangle at center
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(3.5, 3.5 - axis_projection, 3.5),
            y = c(3.5, 3.5, 3.5 + axis_projection),
            quadrant = 3,
            depth = 1,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 3 & quad_depth_summary$depth == 1]
        ))

        # Depth 2 - parallelogram with proper 4 corners
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(3.5 - axis_projection, 3.5 - 2*axis_projection, 3.5, 3.5),
            y = c(3.5, 3.5, 3.5 + 2*axis_projection, 3.5 + axis_projection),
            quadrant = 3,
            depth = 2,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 3 & quad_depth_summary$depth == 2]
        ))

        # Depth 3 - hexagon (middle)
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(3.5 - 2*axis_projection, 0.5, 0.5, 0.5 + 2*axis_projection, 3.5, 3.5),
            y = c(3.5, 3.5, 6.5 - 2*axis_projection, 6.5, 6.5, 3.5 + 2*axis_projection),
            quadrant = 3,
            depth = 3,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 3 & quad_depth_summary$depth == 3]
        ))

        # Depth 4 - parallelogram
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(0.5, 0.5, 0.5 + 2*axis_projection, 0.5 + axis_projection),
            y = c(6.5 - 2*axis_projection, 6.5, 6.5, 6.5 - axis_projection),
            quadrant = 3,
            depth = 4,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 3 & quad_depth_summary$depth == 4]
        ))

        # Depth 5 - triangle at corner
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(0.5, 0.5 + axis_projection, 0.5),
            y = c(6.5, 6.5, 6.5 - axis_projection),
            quadrant = 3,
            depth = 5,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 3 & quad_depth_summary$depth == 5]
        ))

        # Create bands for top-right quadrant (quadrant 4)

        # Depth 1 - triangle at center
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(3.5, 3.5 + axis_projection, 3.5),
            y = c(3.5, 3.5, 3.5 + axis_projection),
            quadrant = 4,
            depth = 1,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 4 & quad_depth_summary$depth == 1]
        ))

        # Depth 2 - parallelogram with proper 4 corners
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(3.5 + axis_projection, 3.5 + 2*axis_projection, 3.5, 3.5),
            y = c(3.5, 3.5, 3.5 + 2*axis_projection, 3.5 + axis_projection),
            quadrant = 4,
            depth = 2,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 4 & quad_depth_summary$depth == 2]
        ))

        # Depth 3 - hexagon (middle)
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(3.5 + 2*axis_projection, 6.5, 6.5, 6.5 - 2*axis_projection, 3.5, 3.5),
            y = c(3.5, 3.5, 6.5 - 2*axis_projection, 6.5, 6.5, 3.5 + 2*axis_projection),
            quadrant = 4,
            depth = 3,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 4 & quad_depth_summary$depth == 3]
        ))

        # Depth 4 - parallelogram
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(6.5, 6.5, 6.5 - 2*axis_projection, 6.5 - axis_projection),
            y = c(6.5 - 2*axis_projection, 6.5, 6.5, 6.5 - axis_projection),
            quadrant = 4,
            depth = 4,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 4 & quad_depth_summary$depth == 4]
        ))

        # Depth 5 - triangle at corner
        polygon_data <- rbind(polygon_data, data.frame(
            x = c(6.5, 6.5 - axis_projection, 6.5),
            y = c(6.5, 6.5, 6.5 - axis_projection),
            quadrant = 4,
            depth = 5,
            proportion = quad_depth_summary$proportion[quad_depth_summary$quadrant == 4 & quad_depth_summary$depth == 5]
        ))

        # Create factor variables for visualization
        polygon_data$depth_factor <- factor(polygon_data$depth)
        polygon_data$group_id <- paste(polygon_data$quadrant, polygon_data$depth, sep = "_")

        # Create the visualization with axis labels and white dividing lines
        p <- ggplot(polygon_data, aes(x = x, y = y, fill = proportion, group = group_id)) +
            geom_polygon(color = "white", linewidth = 0.5) +
            scale_fill_distiller(palette = color_palette, direction = 1, limits = c(0, max(polygon_data$proportion))) +
            coord_fixed(ratio = 1, xlim = c(0.5, 6.5), ylim = c(0.5, 6.5)) +
            scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
            scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
            # Add back axis labels
            labs(x = "Directedness", y = "Stickiness") +
            plot_theme +
            theme(
                plot.title = element_blank(),  # Remove title
                panel.grid = element_blank()   # Remove any grid lines from theme
            )

        # Return the visualization
        return(list(
            diagonal_plot = p,
            data = quad_depth_data
        ))
    }
    
}
