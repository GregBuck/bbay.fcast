#####################################################
# log Y sibling functions
####################################################



#' log Y sibling forecast
#'
#' forecast next years log return of a given age class using a linnear regression of the sibling returns
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @keywords log Y sibling
#' @export
#' @examples
#' lnY.sib.forecast()

lnY.sib.forecast <- function(sibling,forecast.age,input){
  sibling <- apply(sibling,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  dat <- data.frame(sibling,forecast.age)
  mod <- lm(log(forecast.age)~sibling,data = dat)
  exp(predict(mod,data.frame(sibling = input)))
}


#' log Y sibling R-square value
#'
#' Returns the R-square value of a linnear regression of log forcasted returns and sibling returns
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @keywords log Y sibling
#' @export
#' @examples
#' lnY.sib.rsquare()

lnY.sib.rsquare <- function(sibling,forecast.age){
  sibling <- apply(sibling,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  dat <- data.frame(sibling,forecast.age)
  mod <- lm(log(forecast.age)~sibling,data = dat)
  summary(mod)$adj.r.square
}


#' log Y sibling R-square value
#'
#' Returns the slope p-value of a linnear regression of log forcasted returns and sibling returns
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @keywords log Y sibling
#' @export
#' @examples
#' lnY.sib.pvalue()

lnY.sib.pvalue <- function(sibling,forecast.age){
  sibling <- apply(sibling,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  dat <- data.frame(sibling,forecast.age)
  mod <- lm(log(forecast.age)~sibling,data = dat)
  summary(mod)$coefficients[2,4]
}

