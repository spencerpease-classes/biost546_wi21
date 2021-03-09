
evaluate_grid <- function(X, Y, pred_list, grid = seq(-1, 1, .01)) {

  data_point <- tibble(x = X, y = Y)

  data_grid <-
    tibble(x = grid) %>%
    mutate(across(x, .fns = pred_list, .names = "{.fn}")) %>%
    tidyr::pivot_longer(-x, names_to = "method", values_to = "y")

  plot <-
    ggplot(data_point, aes(x, y)) +
    geom_point() +
    geom_line(aes(color = method), data = data_grid) +
    theme_bw(base_family = "serif") +
    theme(legend.position = "top") +
    labs(
      x = "X",
      y = "Y",
      color = "Method"
    )

  return(list(data = data, plot = plot))

}
