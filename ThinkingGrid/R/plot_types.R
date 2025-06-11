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
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_blank(),
      plot.margin = grid::unit(c(2, 2, 2, 2), "lines"),  # More generous margins
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),  # Ensure title is visible
      plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5)  # Ensure subtitle is visible
    )
}

#' Create custom color scale with enhanced visibility for small values
#' 
#' @param palette Name of diverging palette from colorspace package (default: "RdYlBu"). Other options include "RdBu", "PiYG", "BrBG", "PuOr", "RdGy".
#' @param zero_color Color for zero values in hex (default: "#FFFFFF" - white for clear distinction)
#' @param n_colors Number of color steps (default: 11)
#' @param gradient_scaling Type of gradient scaling: "linear" or "enhanced" (default: "enhanced"). "linear" uses standard color mapping. "enhanced" gives more color distinction to smaller values.
#' @param enhanced_threshold_pct Percentage of maximum value to use as threshold for enhanced scaling (default: 50). Values below this percentage get more color distinction.
#' @param enhanced_expansion_factor Factor controlling how much more color distinction small values get (default: 1.5). Higher values mean more distinction for small differences. Only used when gradient_scaling = "enhanced".
#' 
#' @return A colorer function that can be passed to plot_tg() or create_tg_animation()
#' 
#' @details 
#' The enhanced scaling works by compressing small values in the scaled space, which gives them more colors in the final gradient. For example, if your data ranges from 0-20 percent and you use enhanced_threshold_pct = 50 with enhanced_expansion_factor = 2.0, then values 0-10 percent will get twice as much color distinction compared to linear scaling.
#' 
#' @examples
#' # Create dummy data for testing
#' dummy_data <- data.frame(
#'   Deliberate.Constraints = sample(1:6, 100, replace = TRUE),
#'   Automatic.Constraints = sample(1:6, 100, replace = TRUE)
#' )
#' 
#' # Basic usage with linear scaling
#' colorer <- create_custom_colorer(gradient_scaling = "linear")
#' 
#' # Enhanced scaling for better small value distinction
#' colorer_enhanced <- create_custom_colorer(
#'   gradient_scaling = "enhanced",
#'   enhanced_threshold_pct = 30,
#'   enhanced_expansion_factor = 2.0
#' )
#' 
#' @export
create_custom_colorer <- function(palette = "RdYlBu",
                                  zero_color = "#FFFFFF",
                                  n_colors = 11,
                                  gradient_scaling = "enhanced",
                                  enhanced_threshold_pct = 50,
                                  enhanced_expansion_factor = 1.5) {
  
  # Check if colorspace is available
  if (!requireNamespace("colorspace", quietly = TRUE)) {
    stop("Package 'colorspace' is required for palette generation. Please install it with: install.packages('colorspace')")
  }
  
  function(limits, data_values = NULL) {
    min_val <- limits[1]
    max_val <- limits[2]
    
    # Determine if we need diverging or sequential scale
    if (min_val < 0 && max_val > 0) {
      # Diverging scale
      create_diverging_scale_palette(palette, zero_color, 
                                   limits, n_colors, data_values, gradient_scaling, 
                                   enhanced_threshold_pct, enhanced_expansion_factor)
    } else if (min_val >= 0) {
      # Sequential scale (positive only)
      create_sequential_scale_palette(palette, zero_color, "positive",
                                    limits, n_colors, data_values, gradient_scaling,
                                    enhanced_threshold_pct, enhanced_expansion_factor)
    } else {
      # Sequential scale (negative only)
      create_sequential_scale_palette(palette, zero_color, "negative",
                                    limits, n_colors, data_values, gradient_scaling,
                                    enhanced_threshold_pct, enhanced_expansion_factor)
    }
  }
}

create_diverging_scale_palette <- function(palette_name, zero_color, limits, n_colors, data_values, 
                                         gradient_scaling = "enhanced", enhanced_threshold_pct = 50,
                                         enhanced_expansion_factor = 1.5) {
  min_val <- limits[1]
  max_val <- limits[2]
  
  # Create symmetric range around zero
  max_abs <- max(abs(min_val), abs(max_val))
  
  # Apply gradient scaling transformation
  scaled_values <- apply_gradient_scaling(seq(0, max_abs, length.out = ceiling(n_colors/2)), 
                                        gradient_scaling, enhanced_threshold_pct, 
                                        enhanced_expansion_factor)
  
  # Create breaks with proper scaling
  pos_breaks_scaled <- scaled_values
  neg_breaks_scaled <- -rev(scaled_values[-1])  # Remove duplicate zero
  
  # Combine breaks
  all_breaks_scaled <- c(neg_breaks_scaled, pos_breaks_scaled)
  
  # Convert back to original scale for the actual breaks
  pos_breaks_original <- inverse_gradient_scaling(pos_breaks_scaled, gradient_scaling, enhanced_threshold_pct, enhanced_expansion_factor, max_abs)
  neg_breaks_original <- -rev(pos_breaks_original[-1])
  all_breaks_original <- c(neg_breaks_original, pos_breaks_original)
  
  # Generate the palette using colorspace
  rdylbu_standard <- colorspace::divergingx_hcl(12, palette = palette_name)
  
  # Create the full color palette
  negative_colors <- rdylbu_standard[1:6]
  positive_colors <- rdylbu_standard[7:12]
  all_palette_colors <- c(negative_colors, zero_color, positive_colors)
  
  # Create color ramp
  complete_palette <- grDevices::colorRampPalette(all_palette_colors)
  final_colors <- complete_palette(length(all_breaks_scaled))
  
  # Create legend breaks based on original values
  legend_breaks <- create_nice_breaks(min_val, max_val, max_abs)
  
  ggplot2::scale_fill_gradientn(
    colors = final_colors,
    values = scales::rescale(all_breaks_scaled),
    limits = limits,
    breaks = legend_breaks,
    na.value = "#ffffff",
    guide = ggplot2::guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 1.2,
      barheight = 15,
      ticks.colour = "black",
      ticks.linewidth = 0.5,
      frame.colour = "black",
      frame.linewidth = 0.5
    )
  )
}

create_sequential_scale_palette <- function(palette_name, zero_color, side, limits, n_colors, data_values, 
                                          gradient_scaling = "enhanced", enhanced_threshold_pct = 50,
                                          enhanced_expansion_factor = 1.5) {
  min_val <- max(0, limits[1]) 
  max_val <- limits[2]

  # Apply gradient scaling to create breaks
  original_breaks <- seq(min_val, max_val, length.out = n_colors)
  scaled_breaks <- apply_gradient_scaling(original_breaks, gradient_scaling, 
                                        enhanced_threshold_pct, enhanced_expansion_factor)
  
  # Generate the palette using colorspace
  rdylbu_standard <- colorspace::divergingx_hcl(12, palette = palette_name)
  
  # Choose colors based on side
  if (side == "positive") {
    colors <- c(zero_color, rdylbu_standard[7:12])
  } else {
    colors <- c(rdylbu_standard[1:6], zero_color)
  }
  
  # Create color ramp
  color_ramp <- grDevices::colorRampPalette(colors)
  final_colors <- color_ramp(length(scaled_breaks))
  
  # Create legend breaks
  legend_breaks <- create_nice_breaks(min_val, max_val, max_val)
  
  ggplot2::scale_fill_gradientn(
    colors = final_colors,
    values = scales::rescale(scaled_breaks),
    limits = limits,
    breaks = legend_breaks,
    na.value = "#ffffff",
    guide = ggplot2::guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = 1.2,
      barheight = 15,
      ticks.colour = "black",
      ticks.linewidth = 0.5,
      frame.colour = "black",
      frame.linewidth = 0.5
    )
  )
}

# Helper function to apply gradient scaling transformations
apply_gradient_scaling <- function(values, gradient_scaling = "enhanced", enhanced_threshold_pct = 50, enhanced_expansion_factor = 1.5) {
  switch(gradient_scaling,
    "linear" = values,
    "enhanced" = {
      # Enhanced scaling for better distinction of small values
      # CORRECTED LOGIC: We want to compress the SCALED values for small numbers
      # so they get MORE color distinction in the final gradient
      max_val <- max(abs(values), na.rm = TRUE)
      if (max_val == 0) return(values)
      
      # Use the specified percentage of the maximum as the threshold for "small values"
      threshold <- max_val * (enhanced_threshold_pct / 100)
      
      # Calculate compression factor for the lower range to give it MORE colors
      # If we compress the lower X% by factor 1/F, we expand the upper (100-X)% by factor E
      # To maintain overall scale: X% * (1/F) + (100-X)% * E ≈ 100%
      # Solving: E = (100 - X%/F) / (100 - X%)
      threshold_pct_decimal <- enhanced_threshold_pct / 100
      enhanced_compression_factor <- 1 / enhanced_expansion_factor
      upper_expansion_factor <- (1 - threshold_pct_decimal * enhanced_compression_factor) / (1 - threshold_pct_decimal)
      
      sign(values) * ifelse(
        abs(values) <= threshold,
        # COMPRESS lower range (values below threshold get LESS scaled space but MORE color distinction)
        abs(values) * enhanced_compression_factor,
        # EXPAND upper range (values above threshold get MORE scaled space but LESS color distinction)
        threshold * enhanced_compression_factor + (abs(values) - threshold) * upper_expansion_factor
      )
    },
    values  # default to linear
  )
}

# Helper function to inverse gradient scaling (for creating proper breaks)
inverse_gradient_scaling <- function(scaled_values, gradient_scaling = "enhanced", 
                                   enhanced_threshold_pct = 50, enhanced_expansion_factor = 1.5, 
                                   max_val) {
  switch(gradient_scaling,
    "linear" = scaled_values,
    "enhanced" = {
      # Inverse of the enhanced scaling
      if (max_val == 0) return(scaled_values)
      
      threshold <- max_val * (enhanced_threshold_pct / 100)
      enhanced_compression_factor <- 1 / enhanced_expansion_factor
      threshold_scaled <- threshold * enhanced_compression_factor
      
      # Calculate upper expansion factor the same way as in apply_gradient_scaling
      threshold_pct_decimal <- enhanced_threshold_pct / 100
      upper_expansion_factor <- (1 - threshold_pct_decimal * enhanced_compression_factor) / (1 - threshold_pct_decimal)
      
      sign(scaled_values) * ifelse(
        abs(scaled_values) <= threshold_scaled,
        # Scale back up from compressed lower range
        abs(scaled_values) / enhanced_compression_factor,
        # Scale back down from expanded upper range
        (abs(scaled_values) - threshold_scaled) / upper_expansion_factor + threshold
      )
    },
    scaled_values  # default
  )
}

# Helper function to create nice legend breaks
create_nice_breaks <- function(min_val, max_val, max_abs) {
  if (max_abs <= 20) {
    breaks <- seq(-ceiling(max_abs/2)*2, ceiling(max_abs/2)*2, by = 2)
  } else if (max_abs <= 50) {
    breaks <- seq(-ceiling(max_abs/5)*5, ceiling(max_abs/5)*5, by = 5)
  } else {
    breaks <- seq(-ceiling(max_abs/10)*10, ceiling(max_abs/10)*10, by = 10)
  }
  
  # Filter breaks to be within limits and include 0
  breaks <- breaks[breaks >= min_val & breaks <= max_val]
  if (!0 %in% breaks) breaks <- sort(c(breaks, 0))
  
  return(breaks)
}

# Updated default_colorer with linear scaling as default
default_colorer <- function(palette = "RdYlBu", gradient_scaling = "linear",
                          enhanced_threshold_pct = 50, enhanced_expansion_factor = 1.5) {
  create_custom_colorer(
    palette = palette,
    zero_color = "#FFFFFF",
    gradient_scaling = gradient_scaling,
    enhanced_threshold_pct = enhanced_threshold_pct,
    enhanced_expansion_factor = enhanced_expansion_factor
  )
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
    
    # Determine if this is a faceted plot based on if 'condition' column exists
    is_faceted <- "condition" %in% names(data)
    
    # Create a balanced theme with optimized margins
    custom_theme <- ggplot2::theme_minimal() +
        ggplot2::theme(
            axis.text = ggplot2::element_blank(),
            # Make axis titles smaller, italic, and remove bold styling
            axis.title = ggplot2::element_text(size = 12, face = "italic"),
            # Move x-axis title closer to axis
            axis.title.x = ggplot2::element_text(vjust = 0, hjust = 0.5, margin = ggplot2::margin(t = 0.5, b = 2, unit = "lines")),
            axis.title.y = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 0.5),
            legend.title = ggplot2::element_text(size = 11),
            legend.text = ggplot2::element_text(size = 9),
            # Make legend more compact
            legend.key.size = ggplot2::unit(0.8, "lines"),
            legend.spacing = ggplot2::unit(0.2, "cm"),
            # Position legend consistently on the right for all plots
            legend.position = "right",
            # Add margin between the legend and the plot
            legend.margin = ggplot2::margin(t = 15, r = 0, b = 0, l = 0),
            # Remove the border around the entire plot
            panel.border = ggplot2::element_blank(),
            panel.grid = ggplot2::element_blank(),
            plot.margin = ggplot2::unit(c(3, 2, 2, 4), "lines"),
            plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 10)),
            plot.subtitle = ggplot2::element_text(size = 14, hjust = 0.5, margin = ggplot2::margin(b = 10))
        )
    
    # Adjust coordinate limits based on whether we're in a facet or not
    x_limits <- if(is_faceted) c(0, 7) else c(0, 7)
    y_limits <- if(is_faceted) c(0, 7) else c(0, 7)
    
    # Create base plot with fixed aspect ratio
    p <- ggplot2::ggplot(data) +
        aesthetics + 
        geometry(data) +
        colorer(limits) +
        # Fixed coordinate system with breathing room
        ggplot2::coord_fixed(
            ratio = 1,  # Force 1:1 aspect ratio
            xlim = x_limits,  
            ylim = y_limits,
            expand = FALSE,
            clip = "off"    
        ) +
        ggplot2::scale_x_continuous(breaks = NULL) +
        ggplot2::scale_y_continuous(breaks = NULL) +
        custom_theme
    
    # Position axis elements depending on whether this is a faceted plot
    x_pos <- 0.3
    y_pos <- 0.3
    
    # Add axes with arrows that respect the coordinate system
    p <- p + 
        # X-axis with arrow head
        ggplot2::geom_segment(
            data = data.frame(x = x_pos, y = y_pos, xend = 7, yend = y_pos),
            mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
            color = "black", linewidth = 0.7,  # Changed 'size' to 'linewidth'
            arrow = ggplot2::arrow(length = ggplot2::unit(0.4, "cm"), type = "closed", ends = "last"),
            inherit.aes = FALSE
        ) +
        # Y-axis with arrow head
        ggplot2::geom_segment(
            data = data.frame(x = x_pos, y = y_pos, xend = x_pos, yend = 7),
            mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
            color = "black", linewidth = 0.7,  # Changed 'size' to 'linewidth'
            arrow = ggplot2::arrow(length = ggplot2::unit(0.4, "cm"), type = "closed", ends = "last"),
            inherit.aes = FALSE
        )
    
    # Apply titles with explicit calls that work better with complex plots
    # For difference plots
    if (comparison_type == "difference") {
        diff_title <- if(!is.null(plot_title)) plot_title else title
        if (!is.null(diff_title)) {
            p <- p + ggplot2::labs(title = diff_title)
        }
    } else if (!is.null(plot_title)) {
        # For other plots
        p <- p + ggplot2::labs(title = plot_title)
    }
    
    # Add subtitle if provided - using labs instead of +labs for consistency
    if (!is.null(plot_subtitle)) {
        p <- p + ggplot2::labs(subtitle = plot_subtitle)
    }
    
    # Use standard axis title positions through labs() for better placement
    p <- p + ggplot2::labs(
        x = x_label,
        y = y_label,
        fill = ifelse(!is.null(comparison_type) && comparison_type == "difference",
                      ifelse(is.null(legend_title), "Difference (%)", legend_title),
                      ifelse(is.null(legend_title), "Percentage (%)", legend_title))
    )
    
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

        # Create the plot with facets - use a custom plotter for faceted plots
        p <- plotter(combined_data, limits)
        
        # If we have subtitles, use them as facet labels instead of condition names
        if (has_subtitles) {
            # Create custom facet labels using just the subtitles
            facet_labels <- setNames(plot_subtitle, unique_conditions)
            
            # Apply custom labelled facets - optimize layout for better plot size
            p <- p + ggplot2::facet_wrap(~ condition, ncol = 2, labeller = ggplot2::as_labeller(facet_labels)) +
                # Optimize for better plot sizes in faceted view
                ggplot2::theme(
                    panel.spacing = ggplot2::unit(2, "lines"),
                    strip.text = ggplot2::element_text(size = 14, face = "bold"),
                    plot.margin = ggplot2::unit(c(2, 1, 1, 1), "lines"),
                    plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5)),
                    # Improve legend separation from x-axis label
                    legend.box.margin = ggplot2::margin(t = 15, b = 0, l = 0, r = 0),
                    legend.box.spacing = ggplot2::unit(0.5, "lines"),
                    # Remove panel borders
                    panel.border = ggplot2::element_blank(),
                    # Remove strip background and borders completely
                    strip.background = ggplot2::element_blank()
                ) +
                # Use consistent vertical legend for faceted plots
                ggplot2::guides(
                    fill = ggplot2::guide_colorbar(
                        barwidth = 1.2, barheight = 15,  # Vertical legend dimensions
                        title.position = "top", 
                        title.hjust = 0.5,
                        title.vjust = 0
                    )
                )
        } else {
            p <- p + ggplot2::facet_wrap(~ condition, ncol = 2) +
                ggplot2::theme(
                    panel.spacing = ggplot2::unit(2, "lines"),
                    strip.text = ggplot2::element_text(size = 14, face = "bold"),
                    plot.margin = ggplot2::unit(c(2, 1, 1, 1), "lines"),
                    plot.title = ggplot2::element_text(margin = ggplot2::margin(b = 5)),
                    # Improve legend separation from x-axis label
                    legend.box.margin = ggplot2::margin(t = 15, b = 0, l = 0, r = 0),
                    legend.box.spacing = ggplot2::unit(0.5, "lines"),
                    # Remove panel borders
                    panel.border = ggplot2::element_blank(),
                    # Remove strip background and borders completely
                    strip.background = ggplot2::element_blank()
                ) +
                # Use consistent vertical legend for faceted plots
                ggplot2::guides(
                    fill = ggplot2::guide_colorbar(
                        barwidth = 1.2, barheight = 15,  # Vertical legend dimensions
                        title.position = "top", 
                        title.hjust = 0.5,
                        title.vjust = 0
                    )
                )
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
            colorer <- default_colorer(gradient_scaling = "linear")
        }

        plotter <- function(data, limits, title = NULL, plot_subtitle = NULL) {
            # Pass actual data values to colorer for quantile-based scaling
            data_values <- if("fill_value" %in% names(data)) data$fill_value else NULL
            
            plot_engine(
                data,
                limits,
                x_label,
                y_label,
                function(lims) colorer(lims, data_values),  # Pass data to colorer
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
    ggplot2::geom_tile(color = "black", linewidth = 0.5)
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
      color = "black", 
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
            color = "black",
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
            color = "black",
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
      color = "black",
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
            color = "black",
            linewidth = 0.5
        )
    }

    return(compile_plot_creator(proportioner, framer, aesthetics, geometry))
}

create_depth_plot <- compile_depth_plot_creator()

