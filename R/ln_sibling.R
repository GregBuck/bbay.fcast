########################################################
# log sibling functions
########################################################



#' log sibling forecast
#'
#' forecast next years log return of a given age class using a linnear regression of the log sibling returns
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @keywords log sibling
#' @export
#' @examples
#' ln.sib.forecast()

ln.sib.forecast <- function(sibling,forecast.age,input){
  sibling <- apply(sibling,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  dat <- data.frame(sibling,forecast.age)
  mod <- lm(log(forecast.age)~log(sibling+1),data = dat)
  exp(predict(mod,data.frame(sibling = input)))
}


#' log sibling R-square
#'
#' Returns the R-square value of a linnear regression of log forcasted returns and log sibling returns
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @keywords log sibling
#' @export
#' @examples
#' ln.sib.rsquare()

ln.sib.rsquare <- function(sibling,forecast.age){
  sibling <- apply(sibling,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  dat <- data.frame(sibling,forecast.age)
  mod <- lm(log(forecast.age)~log(sibling+1),data = dat)
  summary(mod)$adj.r.squared
}



#' log sibling p-value
#'
#' Returns the slope p-value of a linnear regression of log forcasted returns and log sibling returns
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @keywords log sibling
#' @export
#' @examples
#' ln.sib.pvalue()

ln.sib.pvalue <- function(sibling,forecast.age){
  sibling <- apply(sibling,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  dat <- data.frame(sibling,forecast.age)
  mod <- lm(log(forecast.age)~log(sibling+1),data = dat)
  summary(mod)$coefficients[2,4]
}

