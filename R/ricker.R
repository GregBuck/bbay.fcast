#################################################
# Ricker functions
#################################################


#' Ricker forecast
#'
#' forecast next years return of a given age class using a linnear transformation of the Ricker relationship
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @keywords ricker
#' @export
#' @examples
#' ricker.forecast()

ricker.forecast <- function(spawners,forecast.age,input){
  spawners <- apply(spawners,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  forecast.age <- log((forecast.age+1)/spawners)
  dat <- data.frame(spawners,forecast.age)
  mod <- lm(forecast.age~spawners,data = dat)
  exp(predict(mod,data.frame(spawners = input)))*input
}

#' Ricker R-square
#'
#' Returns the R-square value of a linnear transformation of the Ricker relationship
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @keywords ricker
#' @export
#' @examples
#' ricker.rsquare()

ricker.rsquare <- function(spawners,forecast.age){
  spawners <- apply(spawners,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  forecast.age <- log((forecast.age+1)/spawners)
  dat <- data.frame(spawners,forecast.age)
  mod <- lm(forecast.age~spawners,data = dat)
  summary(mod)$adj.r.squared
}


#' Ricker p-value
#'
#' Returns the slope p-value of a linnear transformation of the Ricker relationship
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @keywords ricker
#' @export
#' @examples
#' ricker.pvalue()

ricker.pvalue <- function(spawners,forecast.age){
  spawners <- apply(spawners,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  forecast.age <- log((forecast.age+1)/spawners)
  dat <- data.frame(spawners,forecast.age)
  mod <- lm(forecast.age~spawners,data = dat)
  summary(mod)$coefficients[2,4]
}

