####################################
# sibling functions
####################################


#' sibling forecast
#'
#' forecast next years return of a given age class using a linnear regression of sibling returns
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @param input The numeric value of previous year sibling return
#' @keywords sibling
#' @export
#' @examples
#' sib.forecast()

sib.forecast <- function(sibling,forecast.age,input){
  sibling <- apply(sibling,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  dat <- data.frame(sibling,forecast.age)
  mod <- lm(forecast.age~sibling,data = dat)
  summary(mod)$coefficients[2,4]
  predict(mod,data.frame(sibling = input))
}

#' sibling R-square
#'
#' Returns the R-square of a linear regression of sibling returns
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @keywords sibling
#' @export
#' @examples
#' sib.rsquare()

sib.rsquare <- function(sibling,forecast.age){
  sibling <- apply(sibling,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  dat <- data.frame(sibling,forecast.age)
  mod <- lm(forecast.age~sibling,data = dat)
  summary(mod)$adj.r.squared
}


#' sibling p-value
#'
#' Returns the slope p-value of a linear regression of sibling returns
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @keywords sibling
#' @export
#' @examples
#' sib.pvalue()

sib.pvalue <- function(sibling,forecast.age,bb.riv){
  sibling <- apply(sibling,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  dat <- data.frame(sibling,forecast.age)
  mod <- lm(forecast.age~sibling,data = dat)
  summary(mod)$coefficients[2,4]
}
