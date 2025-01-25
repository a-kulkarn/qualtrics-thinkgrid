#This code includes two functions to analyze the thinking grid: thinking_grid gives you one grid with one condition, whereas thinking_grid_compare compares two conditions.

#thinking grid is the basic function to analyze the thinking grid.

#The manditory arguments are data and condition_string. The former calls your dataset.
# condition_str specifies what conditions your data must meet in order to be included in the analysis.

#The optional arguments (so far) are title, legend, and intiial max percentage.
#title relables the data as something other than Thinking Grid
#legend makes the legend disappear
#color_palette allows the user to select a different pallet from RColorBrewer than the standard "Blues" 

#initial_max_percentage allows you to set what percentage is labelled as completely blue.
#This is most useful if you're comparing multiple different conditions, so you want colors to mean the same thing across conditions.
#If you don't set anything, dark blue will default to the densest square in your current dataset.

thinking_grid <- function(data, condition_str, title = condition_str, legend = TRUE, initial_max_percentage = 0, color_palette = "Blues") {
  
  library(RColorBrewer)  # Load RColorBrewer
  
  # Evaluate condition on the dataset
  data <- subset(data, eval(parse(text = condition_str)))
  
  # Define the coordinates of the polygons as lists
  x_coords_list <- list(
    #free
    c(3, 5, 5, 3),  #free.1
    c(1, 3, 5, 5, 1),  #free.2
    c(0, 0, 1, 5, 5, 4, 0),  #free.3
    c(0, 0, 4, 2, 0),  #free.4
    c(0, 0, 2, 0),  #free.5
    
    #sticky
    c(5.0, 3.0, 5.0, 5.0),  #sticky.1
    c(3.0, 1.0, 5.0, 5.0, 3.0),  #sticky.2
    c(1.0, 0.0, 0.0, 4.0, 5.0, 5.0, 1.0),  #sticky.3
    c(0.0, 0.0, 2.0, 4.0, 0.0),  #sticky.4
    c(0.0, 0.0, 2.0, 0.0),  #sticky.5,
    
    #directed
    c(5.0, 5.0, 7.0, 5.0),  #directed.1
    c(5.0, 5.0, 7.0, 9.0, 5.0),  #directed.2
    c(6.0, 5.0, 5.0, 9.0, 10.0, 10.0, 6.0),  #directed.3
    c(8.0, 6.0, 10.0, 10.0, 8.0),  #directed.4
    c(10.0, 8.0, 10.0, 10.0),  #directed.5,
    
    #hybrid
    c(5, 5, 7, 5),  #hybrid.1
    c(5, 5, 9, 7, 5),  #hybrid.2
    c(5, 5, 6, 10, 10, 9, 5),  #hybrid.3
    c(6, 8, 10, 10, 6),  #hybrid.4
    c(8, 10, 10, 8)  #hybrid.5
  )
  
  y_coords_list <- list(
    #free
    c(5, 5, 3, 5),  #free.1
    c(5, 5, 3, 1, 5),  #free.2
    c(4, 5, 5, 1, 0, 0, 4),  #free.3
    c(2, 4, 0, 0, 2),  #free.4
    c(0, 2, 0, 0),  #free.5,
    
    #sticky
    c(5.0, 5.0, 7.0, 5.0),  #sticky.1
    c(5.0, 5.0, 9.0, 7.0, 5.0),  #sticky.2
    c(5.0, 5.0, 6.0, 10.0, 10.0, 9.0, 5.0),  #sticky.3
    c(6.0, 8.0, 10.0, 10.0, 6.0),  #sticky.4
    c(8.0, 10.0, 10.0, 8.0),  #sticky.5,
    
    #directed
    c(3.0, 5.0, 5.0, 3.0),  #directed.1
    c(1.0, 3.0, 5.0, 5.0, 1.0),  #directed.2
    c(0.0, 0.0, 1.0, 5.0, 5.0, 4.0, 0.0),  #directed.3
    c(0.0, 0.0, 4.0, 2.0, 0.0),  #directed.4
    c(0.0, 0.0, 2.0, 0.0),  #directed.5,
    
    #hybrid
    c(5, 7, 5, 5),  #hybrid.1
    c(7, 9, 5, 5, 7),  #hybrid.2
    c(9, 10, 10, 6, 5, 5, 9),  #hybrid.3
    c(10, 10, 8, 6, 10),  #hybrid.4
    c(10, 10, 8, 10)  #hybrid.5
  )  
  
  n <- length(x_coords_list)  # Number of polygons
  
  # Calculate the maximum percentage across the dataset (same as in your original code)
  max_percentage <- initial_max_percentage
  for (i in 1:n) {
    if (i < 6) {
      quadrant <- "free"
      depth <- i
    } else if (i < 11) {
      quadrant <- "sticky"
      depth <- (i - 5)
    } else if (i < 16) {
      quadrant <- "directed"
      depth <- (i - 10)
    } else {
      quadrant <- "salience_directed"
      depth <- (i - 15)
    }
    
    total_cells <- sum(data[[quadrant]] >= 0, na.rm = TRUE)
    matching_cells <- sum(data[[quadrant]] == depth, na.rm = TRUE)
    percentage <- matching_cells / total_cells
    max_percentage <- max(percentage, max_percentage)
  }
  
  # Set up the color palette using RColorBrewer
  n_steps <- 100  # Number of gradient steps
  palette <- brewer.pal(9, color_palette)  # Choose a palette from RColorBrewer (e.g., "Blues")
  color_gradient <- colorRampPalette(palette)(n_steps)
  
  # Plotting
  plot(1, type = "n", xlim = c(-3, 12), ylim = c(-2, 12), xlab = "", ylab = "", asp = 1, axes = FALSE)
  title(main = title, line = -3, cex.main = 2, adj = .54) # title with custom location and size
  lines(x = c(0, 10), y = c(-1, -1), col = "black", lwd = 2)  # X-axis
  lines(x = c(-1, -1), y = c(0, 10), col = "black", lwd = 2)  # Y-axis
  arrows(10, -1, 10.5, -1, length = 0.1, angle = 20, col = "black", lwd = 2)  # X-axis arrow
  arrows(-1, 10, -1, 10.5, length = 0.1, angle = 20, col = "black", lwd = 2)  # Y-axis arrow
  
  mtext("Executive Control", side = 1, line = -1, cex = 1.5, at = 5)
  mtext("Salience", side = 2, line = -4, cex = 1.5, at = 5)
  
  # Draw the legend unless requested
  if (legend) {
    for (i in 1:n_steps) {
      color <- color_gradient[i]
      rect(xleft = 11, ybottom = 1 + (i - 1) / n_steps * 8, xright = 12, ytop = 1 + i / n_steps * 8, col = color, border = NA)
    }

    # Add "0" at the bottom of the legend and max_percentage at the top
    text(x = 11.5, y = 0.5, labels = "0%", cex = 1.5)
    text(x = 11.5, y = 9.5, labels = paste(round(max_percentage * 100, 0), "%"), cex = 1.5)
    }
  
  # Drawing the polygons
  for (i in 1:n) {
    if (i < 6) {
      quadrant <- "free"
      depth <- i
    } else if (i < 11) {
      quadrant <- "sticky"
      depth <- (i - 5)
    } else if (i < 16) {
      quadrant <- "directed"
      depth <- (i - 10)
    } else {
      quadrant <- "salience_directed"
      depth <- (i - 15)
    }
    
    total_cells <- sum(data[[quadrant]] >= 0, na.rm = TRUE)
    matching_cells <- sum(data[[quadrant]] == depth, na.rm = TRUE)
    percentage <- matching_cells / total_cells
    
    # Determine the color from the gradient
    value <- ceiling((percentage / max_percentage) * (n_steps - 1)) + 1
    value <- max(1, min(value, n_steps))  # Ensure value is within bounds
    fill_color <- color_gradient[value]
    
    # Use the ith set of coordinates from the lists
    x_coords_i <- x_coords_list[[i]]
    y_coords_i <- y_coords_list[[i]]
    
    # Draw the ith polygon
    polygon(x_coords_i, y_coords_i, col = fill_color, border = "black")
  }
}


#thinking_grid_compare allows you to compare two different conditions to see which cells increase and decrease.

#The manditory arguments are data, condition1_str, and condition2_str. The first calls your dataset.
#The next two allow you to specify the conditions you are comparing. It's condition 1 - condition 2.

#The optional arguments (so far) are title, legend, and intiial max percentage.
#title relables the data as something other than Thinking Grid
#legend makes the legend disappear
#color_palette allows the user to select a different pallet from RColorBrewer than the standard "Blues" 

#initial_max_percentage allows you to set what percentage is labelled as completely blue.
#This is most useful if you're comparing multiple different conditions, so you want colors to mean the same thing across conditions.
#If you don't set anything, dark blue will default to the densest square in your current dataset.


# Function to compare two conditions and plot the grid
thinking_grid_compare <- function(data, condition1_str, condition2_str, title = "Thinking Grid Differences", legend = TRUE,initial_max_percentage=0, color_palette = "RdBu") {

  # Create subsets of data based on the two conditions
  data_condition1 <- subset(data, eval(parse(text = condition1_str)))
  data_condition2 <- subset(data, eval(parse(text = condition2_str)))

  # Define the coordinates of the polygons as lists
  x_coords_list <- list(
    #free
    c(3, 5, 5, 3),  #free.1
    c(1, 3, 5, 5, 1),  #free.2
    c(0, 0, 1, 5, 5, 4, 0),  #free.3
    c(0, 0, 4, 2, 0),  #free.4
    c(0, 0, 2, 0),  #free.5

    #sticky
    c(5.0, 3.0, 5.0, 5.0),  #sticky.1
    c(3.0, 1.0, 5.0, 5.0, 3.0),  #sticky.2
    c(1.0, 0.0, 0.0, 4.0, 5.0, 5.0, 1.0),  #sticky.3
    c(0.0, 0.0, 2.0, 4.0, 0.0),  #sticky.4
    c(0.0, 0.0, 2.0, 0.0),  #sticky.5,

    #directed
    c(5.0, 5.0, 7.0, 5.0),  #directed.1
    c(5.0, 5.0, 7.0, 9.0, 5.0),  #directed.2
    c(6.0, 5.0, 5.0, 9.0, 10.0, 10.0, 6.0),  #directed.3
    c(8.0, 6.0, 10.0, 10.0, 8.0),  #directed.4
    c(10.0, 8.0, 10.0, 10.0),  #directed.5,

    #hybrid
    c(5, 5, 7, 5),  #hybrid.1
    c(5, 5, 9, 7, 5),  #hybrid.2
    c(5, 5, 6, 10, 10, 9, 5),  #hybrid.3
    c(6, 8, 10, 10, 6),  #hybrid.4
    c(8, 10, 10, 8)  #hybrid.5
  )

  y_coords_list <- list(
    #free
    c(5, 5, 3, 5),  #free.1
    c(5, 5, 3, 1, 5),  #free.2
    c(4, 5, 5, 1, 0, 0, 4),  #free.3
    c(2, 4, 0, 0, 2),  #free.4
    c(0, 2, 0, 0),  #free.5,

    #sticky
    c(5.0, 5.0, 7.0, 5.0),  #sticky.1
    c(5.0, 5.0, 9.0, 7.0, 5.0),  #sticky.2
    c(5.0, 5.0, 6.0, 10.0, 10.0, 9.0, 5.0),  #sticky.3
    c(6.0, 8.0, 10.0, 10.0, 6.0),  #sticky.4
    c(8.0, 10.0, 10.0, 8.0),  #sticky.5,

    #directed
    c(3.0, 5.0, 5.0, 3.0),  #directed.1
    c(1.0, 3.0, 5.0, 5.0, 1.0),  #directed.2
    c(0.0, 0.0, 1.0, 5.0, 5.0, 4.0, 0.0),  #directed.3
    c(0.0, 0.0, 4.0, 2.0, 0.0),  #directed.4
    c(0.0, 0.0, 2.0, 0.0),  #directed.5,

    #hybrid
    c(5, 7, 5, 5),  #hybrid.1
    c(7, 9, 5, 5, 7),  #hybrid.2
    c(9, 10, 10, 6, 5, 5, 9),  #hybrid.3
    c(10, 10, 8, 6, 10),  #hybrid.4
    c(10, 10, 8, 10)  #hybrid.5
  )

  n <- length(x_coords_list)  # Number of polygons

  # Initialize max_diff to 0
  max_diff <- initial_max_percentage

  # Loop through the polygons and calculate percentage differences
  for (i in 1:n) {

    # Define quadrant and depth as per the logic
    if(i < 6) {
      quadrant <- "free"
      depth <- i
    } else if(i < 11) {
      quadrant <- "sticky"
      depth <- (i - 5)
    } else if(i < 16) {
      quadrant <- "directed"
      depth <- (i - 10)
    } else {
      quadrant <- "salience_directed"
      depth <- (i - 15)
    }

    # Calculate percentages for both conditions
    total_cells_cond1 <- sum(data_condition1[[quadrant]] >= 0, na.rm = TRUE)
    matching_cells_cond1 <- sum(data_condition1[[quadrant]] == depth, na.rm = TRUE)
    percentage_cond1 <- matching_cells_cond1 / total_cells_cond1

    total_cells_cond2 <- sum(data_condition2[[quadrant]] >= 0, na.rm = TRUE)
    matching_cells_cond2 <- sum(data_condition2[[quadrant]] == depth, na.rm = TRUE)
    percentage_cond2 <- matching_cells_cond2 / total_cells_cond2

    # Calculate the difference in percentages
    percentage_diff <- percentage_cond1 - percentage_cond2
    max_diff <- max(abs(percentage_diff), abs(max_diff))
  }

  # Plotting
  plot(1, type = "n", xlim = c(-3, 12), ylim = c(-2, 12), xlab = "", ylab = "", asp = 1, axes = FALSE)
  title(main = title, line = -3, cex.main = 2, adj = .54) # title with custom location and size
  lines(x = c(0, 10), y = c(-1, -1), col = "black", lwd = 2)  # X-axis
  lines(x = c(-1, -1), y = c(0, 10), col = "black", lwd = 2)  # Y-axis
  arrows(10, -1, 10.5, -1, length = 0.1, angle = 20, col = "black", lwd = 2)  # X-axis arrow
  arrows(-1, 10, -1, 10.5, length = 0.1, angle = 20, col = "black", lwd = 2)  # Y-axis arrow
  
  mtext("Executive Control", side = 1, line = -1, cex = 1.5, at = 5)
  mtext("Salience", side = 2, line = -4, cex = 1.5, at = 5)

  library(RColorBrewer)
  if (!(color_palette %in% rownames(brewer.pal.info))) {
    stop(paste("Invalid color palette:", color_palette))
  }
  n_steps <- 100
  palette <- brewer.pal(9, color_palette)
  color_gradient <- colorRampPalette(palette)(n_steps)
  
  
  # Draw the legend
  if (legend) {
    for (i in 1:n_steps) {
      color <- color_gradient[i]
      rect(xleft = 11, ybottom = 1 + (i - 1) / n_steps * 8, xright = 12, ytop = 1 + i / n_steps * 8, col = color, border = NA)
    }
  }

  # Add max diff to the bottom and top of the legend
  text(x = 11.5, y = 0.5, labels = paste("-", round(max_diff * 100, 0), "%"), cex = 1.5)
  text(x = 11.5, y = 9.5, labels = paste(round(max_diff * 100, 0), "%"), cex = 1.5)


  # Drawing the difference polygons
  for (i in 1:n) {

    if(i < 6) {
      quadrant <- "free"
      depth <- i
    } else if(i < 11) {
      quadrant <- "sticky"
      depth <- (i - 5)
    } else if(i < 16) {
      quadrant <- "directed"
      depth <- (i - 10)
    } else {
      quadrant <- "salience_directed"
      depth <- (i - 15)
    }

    total_cells_cond1 <- sum(data_condition1[[quadrant]] >= 0, na.rm = TRUE)
    matching_cells_cond1 <- sum(data_condition1[[quadrant]] == depth, na.rm = TRUE)
    percentage_cond1 <- matching_cells_cond1 / total_cells_cond1

    total_cells_cond2 <- sum(data_condition2[[quadrant]] >= 0, na.rm = TRUE)
    matching_cells_cond2 <- sum(data_condition2[[quadrant]] == depth, na.rm = TRUE)
    percentage_cond2 <- matching_cells_cond2 / total_cells_cond2

    percentage_diff <- percentage_cond1 - percentage_cond2
    normalized_value <- percentage_diff / max_diff  # value between -1 and 1

    # Gradient coloring for polygons
    if (!is.na(normalized_value)) {
      value <- ceiling((normalized_value + 1) / 2 * (n_steps - 1)) + 1
      value <- max(1, min(value, n_steps))  # Ensure value is within bounds
      fill_color <- color_gradient[value]
    } else {
      fill_color <- "white"
    }

    # Draw the ith polygon
    polygon(x_coords_list[[i]], y_coords_list[[i]], col = fill_color, border = "black")
  }
}
