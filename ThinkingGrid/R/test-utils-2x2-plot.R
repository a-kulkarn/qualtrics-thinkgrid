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
    ## Read data and rename variables
    data_path <- system.file("test_data", "test-2x2-data.csv", package = "ThinkingGrid")
    data <- read.csv(data_path)

    ## Create block variable
    data$probe <- as.numeric(as.character(data$probe))
    data$block <- factor(ifelse(data$probe < 3, "Emotional Task", "Rest"),
                         levels = c("Emotional Task", "Rest"))

    ## Fit the models
    formula1 <- (
        free ~ valence + I(valence^2) + block + block:valence + block:I(valence^2) +  (1 | id)
    )
    
    model3_free <- lme4::lmer(
                             formula1,
                             data,
                             control = lme4::lmerControl(optimizer = "bobyqa")
                         )
    model3_sticky <- lme4::lmer(
                               sticky ~ valence + I(valence^2) + block + block:valence + block:I(valence^2) +  (1 | id),
                               data,
                               control = lme4::lmerControl(optimizer = "bobyqa")
                           )
    model3_directed <- lme4::lmer(
                                 directed ~ valence + I(valence^2) + block + block:valence + block:I(valence^2) + (1 | id),
                                 data, 
                                 control = lme4::lmerControl(optimizer = "bobyqa")
                             )
    model3_salience_directed <- lme4::lmer(
                                          salience_directed ~ (
                                              valence + I(valence^2) + block + block:valence + block:I(valence^2) + (1 | id)
                                          ),
                                          data, 
                                          control = lme4::lmerControl(optimizer = "bobyqa"))


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

    return(list(p_sticky=p_sticky, p_salience=p_salience, p_free=p_free, p_directed=p_directed))
}
