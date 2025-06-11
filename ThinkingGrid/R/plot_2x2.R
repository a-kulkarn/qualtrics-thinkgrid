##################################################
## Quadrant background.

#' Illustration of thinkgrid_quadrant_background function.
#' @param arrowwidth {integer, optional} The width of the arrow objects.
#' @param xlab {character, optional} Label for the x-axis.
#' @param ylab {character, optional} Label for the y-axis.
#' 
#' @return A plot containing the 6x6 grid background image for the 2x2 thinkgrid plots
#' which also contains the xy-axis arrows and labels.
#'
thinkgrid_quadrant_background <- function(arrowwidth = 1,
                                          xlab = "Executive Control",
                                          ylab = "Salience"
                                          ) {
    ## Imports
    unit <- ggplot2::unit
    arrow <- ggplot2::arrow
    aes <- ggplot2::aes
    gpar <- grid::gpar
    unit.c <- grid::unit.c

    ## Create the axis labels and arrows manually.
    labelx <- grid::textGrob(xlab)
    red_arrow <- grid::segmentsGrob(
        x0 = unit(0.1, "npc"), y0 = unit(0.5, "npc"),
        x1 = unit(0.9, "npc"), y1 = unit(0.5, "npc"),
        arrow = arrow(length = unit(0.5, "npc"), type = "closed"),
        gp=gpar(col = "red", lwd = arrowwidth)
    )

    labely <- grid::textGrob(ylab, rot = 90)
    blue_arrow <- grid::segmentsGrob(
        x0 = unit(0.5, "npc"), y0 = unit(0.1, "npc"),
        x1 = unit(0.5, "npc"), y1 = unit(0.9, "npc"),
        arrow = arrow(length = unit(0.5, "npc"), type = "closed"),
        gp=gpar(col = "navy", lwd = arrowwidth)
    )

    ## Prepare the 3x3 sub-grids.
    sdf <- expand.grid(
        x = 1:3,
        y = 1:3
    )

    quadrants <- vector("list", 4)
    i <- 1
    for (color in list("#FFE6E6", "#F9EBEE", "#E8F0F8", "#E6F3F2")) {
        P <- ggplot2::ggplot(sdf, aes(x = x, y = y)) + 
            ggplot2::geom_tile(aes(fill = color), color = "white", linewidth = 5) +
            ggplot2::scale_fill_identity() +
            ggplot2::theme_void() +
            ggplot2::theme(
                plot.background = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank()
            )

        ## Append.
        quadrants[[i]] <- ggplot2::ggplotGrob(P)
        i <- i + 1
    }

    ## Insert the four quadrants as a 2x2 grid.
    two_x_two <- do.call(gridExtra::arrangeGrob, append(quadrants, list(nrow=2, ncol=2)))

    ## Arrange the elements
    grid_grob <- gridExtra::arrangeGrob(
        labely,
        blue_arrow,
        two_x_two,
        ggplot2::zeroGrob(),
        ggplot2::zeroGrob(),        
        red_arrow,
        ggplot2::zeroGrob(),
        ggplot2::zeroGrob(),
        labelx,
        nrow=3,
        ncol=3,
        widths = unit.c(unit(0.05, "npc"), unit(0.01, "npc"), unit(0.80, "npc")),
        heights = unit.c(unit(0.80, "npc"), unit(0.01, "npc"), unit(0.05, "npc"))
    )
    
    ## Return.
    return(grid_grob)
}

##################################################
## Quadrant plot.

#' Illustration of thinkgrid_quadrant_plot function
#'
#' The default theme settings for the subplots of a 2x2 quadrant plot. In order to display
#' correctly, the important properties of the theme are:
#' 1. The background are set to transparant elements.
#' 2. The main plot titles and legends are removed. (Axis labels are kept.)
#' 3. The aspect ratio is set to 1.00.
#' 4. Margins are adjusted to set the plot within the background squares.
#' 
#' @param inner_margin {integer, optional} Controls the padding between the inner subplots.
#' 
#' @return A theme object with the settings for the subplots of a 2x2 quadrant plot.
#'
#' @export
default_inner_theme <- function(inner_margin = 20) {
    im <- inner_margin
    
    ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),      
          plot.margin = ggplot2::margin(im, im, im, im),
          legend.position="none",
          aspect.ratio = 1,
          axis.title = ggplot2::element_text(size = 14),
          axis.text = ggplot2::element_text(size = 12),
          axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
          panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.2),
          panel.grid.minor = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(color = "black", linewidth = 0.5)
          ) +
        ggplot2::labs(title = ggplot2::element_blank())
}


#' Illustration of thinkgrid_quadrant_plot function
#'
#' Creates a 2x2 quadrant plot with four ggplot objects.
#' @param p_sticky (ggplot or rastergrob) A ggplot object for the "Sticky" quadrant.
#' @param p_salience (ggplot or rastergrob) A ggplot object for the "Salience" quadrant.
#' @param p_free (ggplot or rastergrob) A ggplot object for the "Free" quadrant.
#' @param p_directed (ggplot or rastergrob) A ggplot object for the "Directed" quadrant.
#' @param inner_theme (theme, optional) A theme for the inner subplots.
#' See `default_inner_theme` for more details.
#' @param arrowwidth (integer, optional) Controls the thickness of the axis arrows.
#' @param xlab (character, optional) Label for the x-axis.
#' @param ylab (character, optional) Label for the y-axis.
#' 
#' @return A ggplot object (created via cowplot) which consists of the thinking grid
#' background and the inlayed 2x2 subplots (or images) corresponding to the respective quadrant.
#'
#' @examples
#' # This is a list of four plots generated by some regression. Their exact nature is not
#' # important aside from the fact that they are ggplot objects.
#' plots <- ThinkingGrid:::create_test_2x2_plots()
#' p1 <- plots[[1]]
#' p2 <- plots[[2]]
#' p3 <- plots[[3]]
#' p4 <- plots[[4]]
#' 
#' # This is the usual syntax.
#' thinkgrid_quadrant_plot(p1, p2, p3, p4)
#'
#' # However, it is possible to use images as the subplots. IDEALLY, one uses an image
#' # type without backgrounds such as a PNG.
#' img_path <- system.file("test_data", "rabbiduck.png", package = "ThinkingGrid")
#' rabbi <- png::readPNG(img_path)
#' rabbigrob <- grid::rasterGrob(rabbi)  # Note you must raster the image!
#'
#' thinkgrid_quadrant_plot(p1, p2, p3, rabbigrob)
#'
#' @export
thinkgrid_quadrant_plot <- function(p_sticky,
                                    p_salience,
                                    p_free,
                                    p_directed,
                                    inner_theme = NULL,
                                    arrowwidth = 1,
                                    xlab = "Executive Control",
                                    ylab = "Salience"
                                    ) {
    ## Imports.
    unit <- ggplot2::unit
    unit.c <- grid::unit.c

    ## Handle options.
    if (is.null(inner_theme)) {
        inner_theme <- default_inner_theme()
    }
    
    ## Pack arguments.
    plots <- list(p_sticky, p_salience, p_free, p_directed)

    ## Extract legend from the first element if possible.
    legend <- tryCatch({
        g <- ggplot2::ggplotGrob(plots[[1]] + ggplot2::theme(legend.position="right"))$grobs
        g[[which(sapply(g, function(x) x$name) == "guide-box")]]
        
    }, error = function(e) {
        ggplot2::zeroGrob()
        
    })
    
    lwidth <- grid::widthDetails(legend)

    ## Construct grid arrangement.
    inner <- do.call(
        gridExtra::arrangeGrob,
        lapply(
            plots,
            function(x)
                if (is(x, "rastergrob")) {
                    x
                } else {
                    x + inner_theme
                }
        )
    )

    width <- sum(inner$width)
    height <- sum(inner$height)
    
    ## Construct the Grob structure and background.
    outer_args <- list(
        arrowwidth = arrowwidth, xlab = xlab, ylab = ylab
    )
    img_grob <- do.call(thinkgrid_quadrant_background, outer_args)
    frame_grob <- do.call(thinkgrid_quadrant_background, outer_args)

    ## Insert plots into the appropriate place in the Grob structure.
    frame_grob$grobs[[3]] <- inner

    g1 <- gridExtra::arrangeGrob(
        frame_grob,
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
    )

    g2 <- gridExtra::arrangeGrob(
        img_grob,
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
    )

    ## Return.
    return(
        cowplot::ggdraw() + cowplot::draw_grob(g2) + cowplot::draw_grob(g1)
    )
}
