library(grid)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(jpeg)

library(gridExtra)

source("demo_refactor.R")

## url <- "http://mathworld.wolfram.com/images/gifs/rabbduck.jpg"
## download.file(url, destfile = "rabbduck.jpg")
img <- readJPEG("rabbduck.jpg")


## bkground <- ggplotGrob(thinkgrid_quadrant_background())
bkground <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

## # Render and capture the plot as a grid object
## grid::grid.newpage()  # Clear the graphics device
## grid::grid.draw(some_other_plot)  # Draw the plot
## captured_plot <- recordPlot()  # Record the plot

## # Convert the recorded plot to a grob
## bkground <- grid::grobTree(captured_plot)

## grid_arrange_shared_legend <- function(...) {
##     plots <- list(...)
##     g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
##     legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
##     lheight <- sum(legend$height)

##     inner <- do.call(arrangeGrob, lapply(plots, function(x)
##         x + theme(legend.position="none")))

##     width <- sum(inner$width)
##     height <- sum(inner$height)

##     print(c(width, height))
    
##     g1 <- arrangeGrob(
##         inner,
##         legend,
##         ncol = 1,
##         heights = unit.c(unit(1, "npc") - lheight, lheight)
##     )

##     ## Construct the underlay image.
##     ## img_grob <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc") - lheight)
##     img_grob <- thinkgrid_quadrant_background()
    
    
##     ## g2 <- arrangeGrob(
##     ##     img_grob,
##     ##     legend,
##     ##     ncol = 1,
##     ##     heights = unit.c(unit(1, "npc") - lheight, lheight)
##     ## )

##     return(list(g1, img_grob))
## }


# Create individual ggplot objects
p1 <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(gear))) +
    geom_point() +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank()
    )

p2 <- ggplot(mtcars, aes(x = hp, y = mpg, color = factor(gear))) +
    geom_point() +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank()
    )

p3 <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(gear))) +
    geom_point() +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank()
    )

p4 <- ggplot(mtcars, aes(x = hp, y = mpg, color = factor(gear))) +
    geom_point() +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank()
    )

## Combine the plots into a 2x2 grid with a common legend
## combined_plot <- arrangeGrob(p1, p2, p3, p4, nrow=2, ncol=2, common.legend = TRUE, )

result <- grid_arrange_shared_legend(p1, p2, p3, p4)
combined_plot <- result[[1]]
img_grob <- result[[2]]

## combined_plot <- combined_plot +
##     theme(plot.background = element_rect(fill="transparent"))


## grid.draw(gList(ggplotGrob(bkground), ggplotGrob(combined_plot)))

## grid.draw(rasterGrob(img, width = unit(0.5,"npc"), height = unit(0.5,"npc")))

grid.draw(img_grob)
grid.draw(combined_plot)

##     grobTree(##rectGrob(gp=gpar(fill="grey80", lwd=0)),
##     
##     ggplotGrob(combined_plot)
## ))

## combined_plot <- combined_plot + background_image(img) 

## plot_annotation(title = "2x2 Grid of Plots with Common Legend") +
## annotation_custom(bkground, xmin = -1, xmax = 1, ymin = -1, ymax = 1)

## Display the combined plot
## print(combined_plot)

