misclass_table <- function(model, df_train, df_test, bayes_thresh = NULL, ...) {

  train_preds <- predict(model, df_train, ...)
  test_preds <- predict(model, df_test, ...)

  if (is.numeric(bayes_thresh)) {
    train_preds <- if_else(train_preds > bayes_thresh, 1L, 0L)
    test_preds <- if_else(test_preds > bayes_thresh, 1L, 0L)
  }

  tibble(
    train = mean(train_preds != df_train$Disease),
    test = mean(test_preds != df_test$Disease)
  )

}
