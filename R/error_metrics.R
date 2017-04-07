### Common model error metric functions

#' @title Root Mean Squared error
#'
#' @description
#' Root Mean Squared error of supplied values. NA values will be removed
#'
#' @param df Data.frame with two columns for observation and
#' modeled value. Additional columns beyond index 1 and 2 will be
#' ignored.
#'
#'@export
RMSE = function(df){
  sqrt(mean((df[,1] - df[,2])^2, na.rm=TRUE))
}

#' @title Value Bias
#'
#' @description
#' Mean overall bias in supplied values. NA values will be removed
#'
#' @inheritParams RMSE
#'
#' @export
BIAS = function(df){
  mean((df[,1] - df[,2]), na.rm=TRUE)
}


#' @title Mean average error
#'
#' @description
#' Mean of the absolute value error for all supplied values.
#' NA values will be removed
#'
#' @inheritParams RMSE
#'
#'
#' @export
MAE = function(df){
  mean(abs(df[,1] - df[,2]), na.rm=TRUE)
}

