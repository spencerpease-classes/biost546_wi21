
gen_predict_bayes <- function(pred_func) {

  ## Arguments:
  #
  #  pred_func
  #    A function accepting a single nod-default argument `data`, which returns
  #    a vector of probabilities of a malignant diagnosis

  function(data, thresholds) {

    add_classification <- function(threshold) {

      pred_class <- factor(
        pred_func(data) > threshold,
        levels = c(FALSE, TRUE),
        labels = c("benign", "malignant")
      )

      data %>% dplyr::mutate(
        pred_class = pred_class,
        threshold = threshold
      )

    }

    thresholds %>% purrr::map_dfr(add_classification)

  }

}
