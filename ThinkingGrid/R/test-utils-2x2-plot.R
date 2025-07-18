## New prediction function
get_predictions <- function(model, newdata) {
    predict <- stats::predict
    
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
  values = c("Emotional Task" = "darkorange", "Rest" = "steelblue")
      
  ggplot2::ggplot(plot_data, ggplot2::aes(x = valence, group = block)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = pmax(0, ci_lower), ymax = ci_upper, fill = block), 
                           alpha = 0.3) +
      ggplot2::geom_line(ggplot2::aes(y = fit, color = block), linewidth = 0.8) +
      ggplot2::scale_x_continuous(limits = c(-2, 2)) +
      ggplot2::scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
      ggplot2::scale_color_manual(values = values) +
      ggplot2::scale_fill_manual(values = values) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
          plot.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),      
          ## legend.position = "none",
          plot.margin = ggplot2::margin(5, 5, 5, 5),
          aspect.ratio = 1,
          axis.title = ggplot2::element_text(size = 14),
          axis.text = ggplot2::element_text(size = 12),
          axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10)),
          panel.grid.major = ggplot2::element_line(color = "gray90", linewidth = 0.2),
          panel.grid.minor = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(color = "black", linewidth = 0.5)
      ) +
      ggplot2::labs(
          title = ggplot2::element_blank(),
          y = y_labels[measure_name],
          x = ggplot2::element_blank()
      )
}


create_test_2x2_plots <- function() {
    ## Load pre-computed rastergrob objects for ultimate deterministic testing
    p_sticky <- readRDS(system.file("extdata", "r_sticky.rds", package = "ThinkingGrid"))
    p_salience <- readRDS(system.file("extdata", "r_salience.rds", package = "ThinkingGrid"))
    p_free <- readRDS(system.file("extdata", "r_free.rds", package = "ThinkingGrid"))
    p_directed <- readRDS(system.file("extdata", "r_directed.rds", package = "ThinkingGrid"))

    return(list(p_sticky=p_sticky, p_salience=p_salience, p_free=p_free, p_directed=p_directed))
}
