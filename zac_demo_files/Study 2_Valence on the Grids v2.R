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


# Remove all variables
rm(list = ls())
ls()
graphics.off()

# Set WD, load function, and load data
setwd("/Users/zci7c/Library/CloudStorage/OneDrive-UniversityofVirginia/UVA/Year-Independent/Writing/Ongoing/Thinking-Grid/Taxicab Reanalysis/Affect Induction (First Experiment)/")
source("ThinkingGridFunctions_ColorBrewer.R")
data <- read.csv("emoIndCorrect.csv")

#Rename variables. Important to always rename the free, sticky, directed, and salience_directed.
data <- data %>%
  rename(valence = valence)  %>%
  rename(free = Free)  %>%
  rename(sticky = Sticky)  %>%
  rename(directed = Deliberate)  %>%
  rename(salience_directed = ADT)

  
data$condition <- as.factor(data$condition)
data$probe <- as.factor(data$probe)
data$valence <- as.numeric(data$valence)
data$block <- ifelse(data$probe == "2", "valenced", "Neutral")

pdf("thinking_grid_plots.pdf", width = 10, height = 10)

#DYNAMICS ~ VALENCE
# Fit quadratic models
model3_free <- lmer(free ~ valence + I(valence^2) + (1 | id), data, 
                    control = lmerControl(optimizer = "bobyqa"))
model3_sticky <- lmer(sticky ~ valence + I(valence^2) + (1 | id), data, 
                      control = lmerControl(optimizer = "bobyqa"))
model3_directed <- lmer(directed ~ valence + I(valence^2) + (1 | id), data, 
                        control = lmerControl(optimizer = "bobyqa"))
model3_salience_directed <- lmer(salience_directed ~ valence + I(valence^2) + (1 | id), data, 
                                 control = lmerControl(optimizer = "bobyqa"))

# Fixed prediction function
get_predictions <- function(model, newdata) {
  # Get model frame and matrix using model's formula
  X <- model.matrix(formula(model)[-2], newdata)
  
  # Get fixed effects and vcov
  beta <- fixef(model)
  vcov_beta <- vcov(model)
  
  # Calculate fitted values and SE
  fit <- X %*% beta
  se <- sqrt(diag(X %*% vcov_beta %*% t(X)))
  
  data.frame(
    fit = as.vector(fit),
    ci_lower = as.vector(fit - 1.96 * se),
    ci_upper = as.vector(fit + 1.96 * se)
  )
}

# Calculate predictions
valence_seq <- seq(min(data$valence), max(data$valence), length.out = 100)
newdata <- data.frame(valence = valence_seq)

# Get predictions for each model
free_preds <- get_predictions(model3_free, newdata)
sticky_preds <- get_predictions(model3_sticky, newdata)
directed_preds <- get_predictions(model3_directed, newdata)
salience_preds <- get_predictions(model3_salience_directed, newdata)

# Create subplot function
create_subplot <- function(predictions, valence_seq, measure_name) {
  y_labels <- c(
    "sticky" = "Salient",
    "salience_directed" = "Salience Directed",
    "free" = "Spontaneous",
    "directed" = "Directed"
  )
  
  # Set individual y-axis limits based on the data
  y_max <- max(predictions$ci_upper, na.rm = TRUE)
  # Round up to next 0.5 increment
  y_max <- ceiling(y_max * 2) / 2
  
  plot_data <- data.frame(
    valence = valence_seq,
    fit = predictions$fit,
    ci_lower = predictions$ci_lower,
    ci_upper = predictions$ci_upper
  )
  
  ggplot(plot_data, aes(x = valence)) +
    geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper),
                fill = "gray80", alpha = 0.5) +
    geom_line(aes(y = fit),
              color = "black", size = 0.8) +
    scale_x_continuous(limits = c(-2, 2)) +
    scale_y_continuous(limits = c(0, y_max), 
                       expand = c(0, 0)) +  # Remove padding
    theme_minimal() +
    theme(
      plot.background = element_blank(),
      panel.background = element_blank(),      
      legend.position = "none",
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
    )
}

# Create individual plots
p_sticky <- create_subplot(sticky_preds, valence_seq, "sticky")
p_salience <- create_subplot(salience_preds, valence_seq, "salience_directed")
p_free <- create_subplot(free_preds, valence_seq, "free")
p_directed <- create_subplot(directed_preds, valence_seq, "directed")

# Create your 6x6 grid background
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

# Create background grid plot
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

# Create final plot
final_plot <- grid_plot + 
  annotation_custom(ggplotGrob(p_sticky), xmin = .5, xmax = 3.5, ymin = 3.5, ymax = 6.5) +
  annotation_custom(ggplotGrob(p_salience), xmin = 3.5, xmax = 6.5, ymin = 3.5, ymax = 6.5) +
  annotation_custom(ggplotGrob(p_free), xmin = .5, xmax = 3.5, ymin = .5, ymax = 3.5) +
  annotation_custom(ggplotGrob(p_directed), xmin = 3.5, xmax = 6.5, ymin = .5, ymax = 3.5)

# Save final plot
ggsave("final_quadratic_overlay.png", final_plot, width = 13, height = 9, 
       bg = "white", dpi = 300)

stop("Hammer time!")



