
calculate_roc <- function(data, pred_func, thresholds) {

  metric_tbl <- function(obs, pred) {

    cf_tbl <- table(obs, pred)

    P <- sum(cf_tbl["malignant",])
    N <- sum(cf_tbl["benign",])
    TP <- cf_tbl["malignant", "malignant"]
    TN <- cf_tbl["benign", "benign"]

    tibble(tpr = TP/P, fpr = 1 - TN/N)


  }

  roc_data <- pred_func(data, thresholds) %>%
    group_by(threshold) %>%
    summarise(metric_tbl(diagnosis, pred_class))

  roc_func <- with(roc_data, approxfun(x = fpr, y = tpr))
  auc <- integrate(roc_func, 0, 1)

  roc_plot <- ggplot(roc_data, aes(x = fpr, y = tpr)) +
    geom_path(color = "red") +
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
