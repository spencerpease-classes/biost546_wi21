plot_lambda_coef <- function(model, beta_indices) {

  data <- model$beta[beta_indices, ] %>%
    as.matrix() %>%
    t() %>%
    as_tibble() %>%
    setNames(paste("beta", beta_indices)) %>%
    mutate(lambda = model$lambda) %>%
    tidyr::pivot_longer(-lambda, names_to = "Coefficients")

  ggplot(data, aes(x = log(lambda), y = value, color = Coefficients)) +
    geom_line() +
    theme_bw(base_family = "serif") +
    labs(title = "Coefficients vs Lambda Penalty")

}


plot_cv_error <- function(model) {

  data <- tibble(
    lambda = model$lambda,
    cv_err_mean = model$cvm,
    cv_err_lo = model$cvlo,
    cv_err_up = model$cvup
  )

  ggplot(data, aes(x = log(lambda), y = cv_err_mean)) +
    geom_errorbar(aes(ymin = cv_err_lo, ymax = cv_err_up), color = "gray") +
    geom_point(color = "red3") +
    geom_vline(
      xintercept = log(c(model$lambda.min, model$lambda.1se)),
      lty = "dashed"
    ) +
    theme_bw(base_family = "serif") +
    labs(
      title = "CV Error vs Lambda Penalty",
      x = "log(lambda)",
      y = "Misclassification error"
    )

}
