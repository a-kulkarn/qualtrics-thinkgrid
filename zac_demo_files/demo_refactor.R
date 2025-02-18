## Remove all variables
rm(list = ls())
ls()
graphics.off()

## Load libraries
library(dplyr)
library(ragg)
library(magick)
library(lme4)
library(sjPlot)
library(ggplot2)
library(png)
library(grid)
library(gridExtra)
library(tidyr)
library(ggeffects)
library(cowplot)

## Read data and rename variables
## data <- read.csv("taxicab_Affect_Induction.csv")
data <- read.csv("~/thinking-grid/demo-files/taxicab_Affect_Induction.csv")

data <- data %>%
  rename(valence = val) %>%
  rename(id = pid)

data$total_correct <- data$directed_correct + data$sticky_correct + data$free_correct

data <- data[data$total_correct == 3, ]



## Create block variable
data$probe <- as.numeric(as.character(data$probe))
data$block <- factor(ifelse(data$probe < 3, "Emotional Task", "Rest"),
                     levels = c("Emotional Task", "Rest"))

## Fit the models
model3_free <- lmer(free ~ valence + I(valence^2) + block + block:valence + block:I(valence^2) +  (1 | id), data, 
                    control = lmerControl(optimizer = "bobyqa"))
model3_sticky <- lmer(sticky ~ valence + I(valence^2) + block + block:valence + block:I(valence^2) + (1 | id), data, 
                      control = lmerControl(optimizer = "bobyqa"))
model3_directed <- lmer(directed ~ valence + I(valence^2) + block + block:valence + block:I(valence^2) + (1 | id), data, 
                        control = lmerControl(optimizer = "bobyqa"))
model3_salience_directed <- lmer(salience_directed ~ valence + I(valence^2) + block + block:valence + block:I(valence^2) + (1 | id), data, 
                                 control = lmerControl(optimizer = "bobyqa"))

## New prediction function
get_predictions <- function(model, newdata) {
  ## Create prediction data frames for both conditions
  pred_data_emotion <- data.frame(
    valence = newdata$valence,
    block = factor("Emotional Task", levels = c("Emotional Task", "Rest"))
  )
  
  pred_data_rest <- data.frame(
    valence = newdata$valence,
    block = factor("Rest", levels = c("Emotional Task", "Rest"))
  )
  
  ## Get predictions with standard errors
  preds_emotion <- predict(model, newdata = pred_data_emotion, re.form = NA, se.fit = TRUE)
  preds_rest <- predict(model, newdata = pred_data_rest, re.form = NA, se.fit = TRUE)
  
  ## Return predictions and confidence intervals for both conditions
  list(
    emotion = data.frame(
      block = "Emotional Task",
      fit = preds_emotion$fit,
      ci_lower = preds_emotion$fit - 1.96 * preds_emotion$se.fit,
      ci_upper = preds_emotion$fit + 1.96 * preds_emotion$se.fit
    ),
    rest = data.frame(
      block = "Rest",
      fit = preds_rest$fit,
      ci_lower = preds_rest$fit - 1.96 * preds_rest$se.fit,
      ci_upper = preds_rest$fit + 1.96 * preds_rest$se.fit
    )
  )
}

## Subplot creation function
create_subplot <- function(predictions, valence_seq, measure_name) {
  y_labels <- c(
    "sticky" = "Salient",
    "salience_directed" = "Salience Directed",
    "free" = "Spontaneous",
    "directed" = "Directed"
  )
  
  ## Combine predictions for both conditions
  plot_data <- rbind(
    cbind(data.frame(valence = valence_seq), predictions$emotion),
    cbind(data.frame(valence = valence_seq), predictions$rest)
  )
  
  plot_data$block <- factor(plot_data$block, levels = c("Emotional Task", "Rest"))
  
  ## Set y-axis limits based on all data
  y_max <- max(plot_data$ci_upper, na.rm = TRUE)
  y_max <- ceiling(y_max * 2) / 2

  ## Return the plot.
  (ggplot(plot_data, aes(x = valence, group = block)) +
    geom_ribbon(aes(ymin = pmax(0, ci_lower), ymax = ci_upper, fill = block), 
                alpha = 0.3) +
    geom_line(aes(y = fit, color = block), size = 0.8) +
    scale_x_continuous(limits = c(-2, 2)) +
    scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
    scale_color_manual(values = c("Emotional Task" = "darkorange", "Rest" = "steelblue")) +
    scale_fill_manual(values = c("Emotional Task" = "darkorange", "Rest" = "steelblue")) +
    theme_minimal() +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank(),      
      ## legend.position = "none",
      plot.margin = margin(5, 5, 5, 5),
      aspect.ratio = 1,
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      axis.title.y = element_text(margin = margin(r = 10)),
      panel.grid.major = element_line(color = "gray90", size = 0.2),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", size = 0.5)
    ) +
    labs(
      title = element_blank(),
      y = y_labels[measure_name],
      x = element_blank()
    ))
}

thinkgrid_quadrant_background <- function() {
    ## Create 6x6 grid background
    df <- expand.grid(
        x = 1:6,
        y = 1:6
    )

    df$color <- with(df, case_when(
                             x <= 3 & y >= 4 ~ "#FFE6E6",  # Top left - Pink
                             x > 3 & y >= 4 ~ "#F9EBEE",   # Top right - Light pink
                             x <= 3 & y < 4 ~ "#E8F0F8",   # Bottom left - Blue
                             TRUE ~ "#E6F3F2"              # Bottom right - Green
                         ))

    ## Create background grid plot
    grid_plot <- ggplot(df, aes(x = x, y = y)) + 
        geom_tile(aes(fill = color), color = "white", size = 5) +
        scale_fill_identity() +
        coord_fixed() +
        theme_void() +
        theme(
            axis.text = element_blank(),
            plot.background = element_rect(fill = "white", color = "white"),
            panel.background = element_rect(fill = "white", color = "white"),
            axis.title.x = element_text(face = "italic", color = "navy", size = 24, 
                                        margin = margin(t = 20)),
            axis.title.y = element_text(face = "italic", color = "#D35400", size = 24, 
                                        margin = margin(r = 20), angle = 90),
            axis.ticks = element_blank(),
            plot.margin = margin(20, 20, 20, 20)
        ) +
        labs(
            x = "Executive Control",
            y = "Salience"
        ) +
        annotate("segment", x = 1, xend = 6, y = -0.2, yend = -0.2,
                 arrow = arrow(length = unit(0.5, "cm"), type = "closed"),
                 color = "navy", size = 3) +
        annotate("segment", x = -0.2, xend = -0.2, y = 1, yend = 6,
                 arrow = arrow(length = unit(0.5, "cm"), type = "closed"),
                 color = "red", size = 3)

    ## Return.
    return(grid_plot)
}

thinkgrid_quadrant_plot <- function(subplots) {
    ## Typecast arguments.
    if (!is.list(subplots) & is.vector(subplots)) {
        subplots <- list(
            sticky = subplots[1],
            salience = subplots[2],
            free = subplots[3],
            directed = subplots[4]
        )
    }

    if (!is.list(subplots)) {
        stop(c(
            "Error: 'subplots' must be a list with keys ",
            "'(sticky, salience, free, directed)' or ",
            "a vector of four plots."
        ))
    }
    
    ## Create a more robust legend plot with a single guide component
    legend_data <- data.frame(
        x = rep(1, 2),
        y = rep(1, 2),
        block = factor(c("Emotional Task", "Rest"), levels = c("Emotional Task", "Rest"))
    )

    legend_plot <- ggplot(legend_data, aes(x = x, y = y, color = block)) +
        geom_point() +
        scale_color_manual(values = c("Emotional Task" = "darkorange", "Rest" = "steelblue")) +
        guides(color = guide_legend(title = NULL, nrow = 1)) +
        theme_void() +
        theme(
            legend.position = "bottom",
            legend.box = "horizontal",
            legend.text = element_text(size = 12),
            legend.spacing.x = unit(0.5, "cm")
        )

    ## Extract the legend using a more specific approach
    legend <- cowplot::get_legend(
                           legend_plot + 
                           theme(
                               legend.box.margin = margin(0, 0, 0, 0),
                               legend.margin = margin(0, 0, 0, 0)
                           )
                       )

    ## Create the background.
    grid_plot <- thinkgrid_quadrant_background()

    ## Create the subplot .
    subplot_ranges <- list(
        free = list(xmin = .5, xmax = 3.5, ymin = 3.5, ymax = 6.5),
        sticky = list(xmin = 3.5, xmax = 6.5, ymin = 3.5, ymax = 6.5),
        directed = list(xmin = .5, xmax = 3.5, ymin = .5, ymax = 3.5),
        salience = list(xmin = 3.5, xmax = 6.5, ymin = .5, ymax = 3.5)
    )

    ## Assemble the final plot.
    final_plot <- grid_plot
    for (key in names(subplots)) {
        P <- subplots[[key]]
        R <- subplot_ranges[[key]]
        
        final_plot <- final_plot + annotation_custom(
            P,
            xmin = R[["xmin"]],
            xmax = R[["xmax"]],
            ymin = R[["ymin"]],
            ymax = R[["ymax"]]
        )
    }
    final_plot <- final_plot +
        ## annotation_custom(legend, xmin = 2.5, xmax = 4.5, ymin = -0.8, ymax = -0.5)
        annotation_custom(legend, xmin = 3.5, xmax = 6.5, ymin = .5, ymax = 3.5)

    ## final_plot <- legend_plot
    
    ## Return.
    return(final_plot)
}

##################################################
## Grid arrange share legend.

grid_arrange_shared_legend <- function(...) {
    plots <- list(...)

    ## if (length(plots) == 1 & is.list(plots)) {
    ##     plots = plots[[1]]
    ## } else if (length(plots) == 1 & is.vector(plots)) {
    ##     plots = plots[1]
    ## }

    g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    
    inner <- do.call(arrangeGrob, lapply(plots, function(x)
        x + theme(legend.position="none")))
    
    width <- sum(inner$width)
    height <- sum(inner$height)

    print(lheight)
    print(legend)
    print(g)
    
    g1 <- arrangeGrob(
        inner,
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
    )

    ## Construct the underlay image.
    ## img_grob <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc") - lheight)
    img_grob <- thinkgrid_quadrant_background()
    
    
    ## g2 <- arrangeGrob(
    ##     img_grob,
    ##     legend,
    ##     ncol = 1,
    ##     heights = unit.c(unit(1, "npc") - lheight, lheight)
    ## )

    return(list(g1, img_grob))
}


##################################################
## Main script.
 
## Calculate predictions
valence_seq <- seq(min(data$valence), max(data$valence), length.out = 100)
newdata <- data.frame(valence = valence_seq)

## Get predictions for each model
free_preds <- get_predictions(model3_free, newdata)
sticky_preds <- get_predictions(model3_sticky, newdata)
directed_preds <- get_predictions(model3_directed, newdata)
salience_preds <- get_predictions(model3_salience_directed, newdata)

## Create individual plots
p_sticky <- create_subplot(sticky_preds, valence_seq, "sticky")
p_salience <- create_subplot(salience_preds, valence_seq, "salience_directed")
p_free <- create_subplot(free_preds, valence_seq, "free")
p_directed <- create_subplot(directed_preds, valence_seq, "directed")

## Update the final plot with adjusted legend positioning
## plots = list(
##     sticky = p_sticky,
##     salience = p_salience,
##     free = p_free,
##     directed = p_directed
## )

# A <- thinkgrid_quadrant_plot(plots)

result <- grid_arrange_shared_legend(p_sticky, p_salience, p_free, p_directed)

A = result[[1]]
B <- result[[2]]

grid.draw(B)
grid.draw(A)
