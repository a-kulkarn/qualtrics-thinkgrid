get_depth_polygons <- function(axis_projection = NULL) {
    # Calculate band width if not provided
    if (is.null(axis_projection)) {
        diagonal_band_width <- sqrt(18) / 5 
        axis_projection <- diagonal_band_width * sqrt(2)
    }
    
    # Create an empty data frame to store polygon data
    polygon_data <- data.frame()
    
    # Quadrant 1 (bottom-left)
    # Depth 1 - triangle at center
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(3.5, 3.5 - axis_projection, 3.5),
        y = c(3.5, 3.5, 3.5 - axis_projection),
        quadrant = 1,
        depth = 1,
        proportion = NA
    ))
    
    # Depth 2 - parallelogram
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(3.5 - axis_projection, 3.5 - 2*axis_projection, 3.5, 3.5),
        y = c(3.5, 3.5, 3.5 - 2 * axis_projection, 3.5 - axis_projection),
        quadrant = 1,
        depth = 2,
        proportion = NA
    ))
    
    # Depth 3 - hexagon (middle)
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(3.5 - 2*axis_projection, 0.5, 0.5, 0.5 + 2 * axis_projection, 3.5, 3.5),
        y = c(3.5, 3.5, 0.5 + 2* axis_projection, 0.5, 0.5, 3.5 - 2*axis_projection),
        quadrant = 1,
        depth = 3,
        proportion = NA
    ))
    
    # Depth 4 - parallelogram
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(0.5, 0.5, 0.5 + 2*axis_projection, 0.5 + axis_projection),
        y = c(0.5 + 2*axis_projection, 0.5, 0.5, 0.5 + axis_projection),
        quadrant = 1,
        depth = 4,
        proportion = NA
    ))
    
    # Depth 5 - triangle at corner
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(0.5, 0.5 + axis_projection, 0.5),
        y = c(0.5, 0.5, 0.5 + axis_projection),
        quadrant = 1,
        depth = 5,
        proportion = NA
    ))
    
    # Quadrant 2 (bottom-right)
    # Depth 1 - triangle at center
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(3.5, 3.5 + axis_projection, 3.5),
        y = c(3.5, 3.5, 3.5 - axis_projection),
        quadrant = 2,
        depth = 1,
        proportion = NA
    ))
    
    # Depth 2 - parallelogram
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(3.5 + axis_projection, 3.5 + 2*axis_projection, 3.5, 3.5),
        y = c(3.5, 3.5, 3.5 - 2*axis_projection, 3.5 - axis_projection),
        quadrant = 2,
        depth = 2,
        proportion = NA
    ))
    
    # Depth 3 - hexagon (middle)
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(3.5 + 2*axis_projection, 6.5, 6.5, 6.5 - 2*axis_projection, 3.5, 3.5),
        y = c(3.5, 3.5, 0.5 + 2*axis_projection, 0.5, 0.5, 3.5 - 2*axis_projection),
        quadrant = 2,
        depth = 3,
        proportion = NA
    ))
    
    # Depth 4 - parallelogram
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(6.5, 6.5, 6.5 - 2*axis_projection, 6.5 - axis_projection),
        y = c(0.5 + 2*axis_projection, 0.5, 0.5, 0.5 + axis_projection),
        quadrant = 2,
        depth = 4,
        proportion = NA
    ))
    
    # Depth 5 - triangle at corner
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(6.5, 6.5 - axis_projection, 6.5),
        y = c(0.5, 0.5, 0.5 + axis_projection),
        quadrant = 2,
        depth = 5,
        proportion = NA
    ))
    
    # Quadrant 3 (top-left)
    # Depth 1 - triangle at center
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(3.5, 3.5 - axis_projection, 3.5),
        y = c(3.5, 3.5, 3.5 + axis_projection),
        quadrant = 3,
        depth = 1,
        proportion = NA
    ))
    
    # Depth 2 - parallelogram
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(3.5 - axis_projection, 3.5 - 2*axis_projection, 3.5, 3.5),
        y = c(3.5, 3.5, 3.5 + 2*axis_projection, 3.5 + axis_projection),
        quadrant = 3,
        depth = 2,
        proportion = NA
    ))
    
    # Depth 3 - hexagon (middle)
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(3.5 - 2*axis_projection, 0.5, 0.5, 0.5 + 2*axis_projection, 3.5, 3.5),
        y = c(3.5, 3.5, 6.5 - 2*axis_projection, 6.5, 6.5, 3.5 + 2*axis_projection),
        quadrant = 3,
        depth = 3,
        proportion = NA
    ))
    
    # Depth 4 - parallelogram
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(0.5, 0.5, 0.5 + 2*axis_projection, 0.5 + axis_projection),
        y = c(6.5 - 2*axis_projection, 6.5, 6.5, 6.5 - axis_projection),
        quadrant = 3,
        depth = 4,
        proportion = NA
    ))
    
    # Depth 5 - triangle at corner
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(0.5, 0.5 + axis_projection, 0.5),
        y = c(6.5, 6.5, 6.5 - axis_projection),
        quadrant = 3,
        depth = 5,
        proportion = NA
    ))
    
    # Quadrant 4 (top-right)
    # Depth 1 - triangle at center
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(3.5, 3.5 + axis_projection, 3.5),
        y = c(3.5, 3.5, 3.5 + axis_projection),
        quadrant = 4,
        depth = 1,
        proportion = NA
    ))
    
    # Depth 2 - parallelogram
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(3.5 + axis_projection, 3.5 + 2*axis_projection, 3.5, 3.5),
        y = c(3.5, 3.5, 3.5 + 2*axis_projection, 3.5 + axis_projection),
        quadrant = 4,
        depth = 2,
        proportion = NA
    ))
    
    # Depth 3 - hexagon (middle)
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(3.5 + 2*axis_projection, 6.5, 6.5, 6.5 - 2*axis_projection, 3.5, 3.5),
        y = c(3.5, 3.5, 6.5 - 2*axis_projection, 6.5, 6.5, 3.5 + 2*axis_projection),
        quadrant = 4,
        depth = 3,
        proportion = NA
    ))
    
    # Depth 4 - parallelogram
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(6.5, 6.5, 6.5 - 2*axis_projection, 6.5 - axis_projection),
        y = c(6.5 - 2*axis_projection, 6.5, 6.5, 6.5 - axis_projection),
        quadrant = 4,
        depth = 4,
        proportion = NA
    ))
    
    # Depth 5 - triangle at corner
    polygon_data <- rbind(polygon_data, data.frame(
        x = c(6.5, 6.5 - axis_projection, 6.5),
        y = c(6.5, 6.5, 6.5 - axis_projection),
        quadrant = 4,
        depth = 5,
        proportion = NA
    ))
    
    # Set factor levels for depth
    polygon_data$depth <- factor(polygon_data$depth, levels = c("1", "2", "3", "4", "5"))
    polygon_data$quadrant <- factor(polygon_data$quadrant, levels = c("1", "2", "3", "4"))
    
    return(polygon_data)
}


get_constraint_polygons <- function(axis_projection) {
    # Create diagonal bands data frame with properly calculated polygons
    diagonal_data <- data.frame()
    
    # Constraint = 2 (bottom-left triangle)
    diagonal_data <- rbind(diagonal_data, data.frame(
        x = c(0.5, 0.5 + axis_projection, 0.5),
        y = c(0.5, 0.5, 0.5 + axis_projection),
        constraint = 2,
        proportion = NA
    ))
    
    # Constraint = 3 (parallelogram) 
    diagonal_data <- rbind(diagonal_data, data.frame(
        x = c(0.5, 0.5 + axis_projection, 0.5 + 2*axis_projection, 0.5),
        y = c(0.5 + axis_projection, 0.5, 0.5, 0.5 + 2*axis_projection),
        constraint = 3,
        proportion = NA
    ))
    
    # Constraint = 4 (parallelogram) 
    diagonal_data <- rbind(diagonal_data, data.frame(
        x = c(0.5, 0.5 + 2*axis_projection, 0.5 + 3*axis_projection, 0.5),
        y = c(0.5 + 2*axis_projection, 0.5, 0.5, 0.5 + 3*axis_projection),
        constraint = 4,
        proportion = NA
    ))
    
    # Constraint = 5 (parallelogram)
    diagonal_data <- rbind(diagonal_data, data.frame(
        x = c(0.5, 0.5 + 3*axis_projection, 0.5 + 4*axis_projection, 0.5),
        y = c(0.5 + 3*axis_projection, 0.5, 0.5, 0.5 + 4*axis_projection),
        constraint = 5,
        proportion = NA
    ))
    
    # Constraint = 6 (parallelogram)
    diagonal_data <- rbind(diagonal_data, data.frame(
        x = c(0.5, 0.5 + 4*axis_projection, 0.5 + 5*axis_projection, 0.5),
        y = c(0.5 + 4*axis_projection, 0.5, 0.5, 0.5 + 5*axis_projection),
        constraint = 6,
        proportion = NA
    ))
    
    # Constraint = 7 (hexagon)
    diagonal_data <- rbind(diagonal_data, data.frame(
        x = c(0.5, 0.5, 0.5 + 5*axis_projection, 6.5, 6.5, 0.5 + axis_projection),
        y = c(6.5, 0.5 + 5*axis_projection, 0.5, 0.5, 0.5 + axis_projection, 6.5),
        constraint = 7,
        proportion = NA
    ))
    
    # Constraint = 8 (parallelogram) 
    diagonal_data <- rbind(diagonal_data, data.frame(
        x = c(6.5 - 5*axis_projection, 6.5, 6.5, 6.5 - 4*axis_projection),
        y = c(6.5, 6.5 - 5*axis_projection, 6.5 - 4*axis_projection, 6.5),
        constraint = 8,
        proportion = NA
    ))
    
    # Constraint = 9 (parallelogram) 
    diagonal_data <- rbind(diagonal_data, data.frame(
        x = c(6.5 - 4*axis_projection, 6.5, 6.5, 6.5 - 3*axis_projection),
        y = c(6.5, 6.5 - 4*axis_projection, 6.5 - 3*axis_projection, 6.5),
        constraint = 9,
        proportion = NA
    ))
    
    # Constraint = 10 (parallelogram) 
    diagonal_data <- rbind(diagonal_data, data.frame(
        x = c(6.5 - 3*axis_projection, 6.5, 6.5, 6.5 - 2*axis_projection),
        y = c(6.5, 6.5 - 3*axis_projection, 6.5 - 2*axis_projection, 6.5),
        constraint = 10,
        proportion = NA
    ))
    
    # Constraint = 11 (parallelogram) 
    diagonal_data <- rbind(diagonal_data, data.frame(
        x = c(6.5 - 2*axis_projection, 6.5, 6.5, 6.5 - axis_projection),
        y = c(6.5, 6.5 - 2*axis_projection, 6.5 - axis_projection, 6.5),
        constraint = 11,
        proportion = NA
    ))
    
    # Special case: Constraint = 12 (top-right triangle)
    diagonal_data <- rbind(diagonal_data, data.frame(
        x = c(6.5 - axis_projection, 6.5, 6.5),
        y = c(6.5, 6.5 - axis_projection, 6.5),
        constraint = 12,
        proportion = NA
    ))
    
    # Then modify the factor levels definition to use numeric constraint values
    diagonal_data$constraint <- factor(diagonal_data$constraint, 
                                   levels = c("2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))
    
    return(diagonal_data)
}