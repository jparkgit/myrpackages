#' Find the threshold that maximizes the Youden Index by using only the existing values from the dataset as thresholds
#'
#' @param roc.object object that is defined by the roc() function output
#' @param predictor defined as dataset$predictor, predictor variable of the same dataset used in the roc.object
#'
#' @return Existing threshold value that maximizes the Youden Index is returned
#' @export existing_youden
#'
#' @examples existing_youden(ad.roc, AD$Tau)

existing_youden <- function(roc.object, predictor){
  # Use only existing values from the dataset as thresholds
  thresholds <- unique(predictor)  # Get unique values from the predictor variable

  # Compute the Youden Index for each exact threshold in the dataset
  youden_indices <- sapply(thresholds, function(thr) {
    coords(roc.object, x = thr, input = "threshold", ret = c("specificity", "sensitivity"))
  })

  # Calculate the Youden Index for these thresholds
  youden_values <- unlist(youden_indices["specificity",]) +
    unlist(youden_indices["sensitivity",]) - 1

  # Find the threshold that maximizes the Youden Index
  max_youden_threshold <- thresholds[which.max(youden_values)]

  print(max_youden_threshold)
}
