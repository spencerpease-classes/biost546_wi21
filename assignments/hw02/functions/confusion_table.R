
confusion_table <- function(data, obs_class, pred_class) {

  data %>%
    dplyr::group_by({{obs_class}}, {{pred_class}}, .add = TRUE) %>%
    dplyr::summarise(count = n()) %>%
    tidyr::pivot_wider(
      names_from = {{pred_class}},
      values_from = count,
      values_fill = 0
    ) %>%
    dplyr::ungroup({{obs_class}}) %>%
    dplyr::rename(`obs / pred` = {{obs_class}})

}
