predict_bayes <- function(model, data, threshold = 0.5) {

  factor(
    predict(model, data, type = "response") > threshold,
    levels = c(FALSE, TRUE),
    labels = c("benign", "malignant")
  )

}
