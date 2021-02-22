confusion_table <- function(obs, pred) {

  cf_table <- table(obs, pred)
  names(dimnames(cf_table)) <- c("observed", "predicted")

  return(cf_table)

}

performance_table <- function(obs, pred) {

  cf_tbl <- confusion_table(obs, pred)

  P <- sum(cf_tbl["malignant",])
  N <- sum(cf_tbl["benign",])
  TP <- cf_tbl["malignant", "malignant"]
  TN <- cf_tbl["benign", "benign"]

  tibble(
    acc = mean(obs == pred),
    tpr = TP/P,
    fpr = 1 - TN/N
  )

}
