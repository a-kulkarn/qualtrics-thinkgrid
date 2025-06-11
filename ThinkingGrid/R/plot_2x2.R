##################################################
## Quadrant background.

#' Illustration of thinkgrid_quadrant_background function
#'
#' Does something.
#'
#' @export
thinkgrid_quadrant_background <- function() {
    ## Imports
    unit <- ggplot2::unit
    arrow <- ggplot2::arrow
    aes <- ggplot2::aes
    gpar <- grid::gpar
    unit.c <- grid::unit.c


    ## Create 6x6 grid background

    ## Create the axis labels and arrows manually.
    labelx <- grid::textGrob("Executive Control")
    red_arrow <- grid::segmentsGrob(
        x0 = unit(0.1, "npc"), y0 = unit(0.5, "npc"),
        x1 = unit(0.9, "npc"), y1 = unit(0.5, "npc"),
        arrow = arrow(length = unit(0.5, "npc"), type = "closed"),
        gp=gpar(col = "red", linewidth = 3)
    )

    labely <- grid::textGrob("Salience", rot = 90)
    blue_arrow <- grid::segmentsGrob(
        x0 = unit(0.5, "npc"), y0 = unit(0.1, "npc"),
        x1 = unit(0.5, "npc"), y1 = unit(0.9, "npc"),
        arrow = arrow(length = unit(0.5, "npc"), type = "closed"),
        gp=gpar(col = "navy", linewidth = 3)
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
            ## coord_fixed() +
            ggplot2::theme_void() +
            ggplot2::theme(
                ## axis.text = element_blank(),
                ## plot.background = element_rect(fill = "white", color = "white"),
                ## panel.background = element_rect(fill = "white", color = "white"),
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
#' Creates a 2x2 quadrant plot with four ggplot objects.
#' @param p_sticky {ggplot} A ggplot object for the "Sticky" quadrant.
#' @param p_salience {ggplot} A ggplot object for the "Salience" quadrant.
#' @param p_free {ggplot} A ggplot object for the "Free" quadrant.
#' @param p_directed {ggplot} A ggplot object for the "Directed" quadrant.
#' 
#' #' @return A list containing two grid objects: one with the quadrant background and the plots, and another with the legend.
#'
#' @export
thinkgrid_quadrant_plot <- function(p_sticky, p_salience, p_free, p_directed) {
    ## Imports.
    unit <- ggplot2::unit
    unit.c <- grid::unit.c

    plots <- list(p_sticky, p_salience, p_free, p_directed)

    ## if (length(plots) == 1 & is.list(plots)) {
    ##     plots = plots[[1]]
    ## } else if (length(plots) == 1 & is.vector(plots)) {
    ##     plots = plots[1]
    ## }

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
                    x +
                        ggplot2::theme(legend.position="none",
                                       plot.margin = ggplot2::margin(20,20,20,20)
                                       )
                }
        )
    )

    width <- sum(inner$width)
    height <- sum(inner$height)
    
    ## Construct the Grob structure and background.
    img_grob <- thinkgrid_quadrant_background()
    frame_grob <- thinkgrid_quadrant_background()

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
    return(list(g1, g2))
}
