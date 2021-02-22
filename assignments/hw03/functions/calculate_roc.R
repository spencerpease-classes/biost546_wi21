calculate_roc <- function(model, X, Y, thresholds = seq(0, 1, by = .01)) {

  roc_data <- thresholds %>%
    purrr::map(~predict_bayes(model, X, threshold = .x)) %>%
    purrr::map_dfr(~performance_table(Y, .x), .id = "index")

  roc_func <- with(roc_data, approxfun(x = fpr, y = tpr))
  auc <- integrate(roc_func, 0, 1)

  roc_plot <- ggplot(roc_data, aes(x = fpr, y = tpr)) +
    geom_path(color = "red3") +
    geom_abline(slope = 1, intercept = 0, lty = "dashed", color = "gray") +
    theme_bw(base_family = "serif") +
    labs(
      title = "ROC Curve",
      subtitle = sprintf("AUC: %0.4f", auc$value),
      x = "False Positive Rate (FPR)",
      y = "True Positive Rate (TPR)"
    )

  list(data = roc_data, plot = roc_plot, AUC = auc)

}
