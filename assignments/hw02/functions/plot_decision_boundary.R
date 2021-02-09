
plot_decision_boundary <- function(data, pred_func, thresholds) {

  grid_points <- expand.grid(
    radius = seq(5, 30, length.out = 100),
    texture = seq(9, 40, length.out = 100)
  )

  data_rep <- thresholds %>%
    purrr::map_dfr(~dplyr::mutate(data, threshold = .x))

  grid_classes <- pred_func(grid_points, thresholds)

  ggplot(grid_classes, aes(x = radius, y = texture, color = pred_class)) +
    geom_point(shape = ".", alpha = .8) +
    geom_point(data = data_rep, aes(color = diagnosis), shape = 1) +
    facet_wrap(vars(threshold), ncol = 2, labeller = "label_both") +
    theme_bw(base_family = "serif") +
    theme(legend.position = "top") +
    labs(
      x = "Radius",
      y = "Texture",
      color = "Diagnosis"
    )

}
