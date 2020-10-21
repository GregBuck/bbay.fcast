################################################
# log spawner log recruit functions
################################################


#' log spawner log recruit forecast
#'
#' forecast next years log return of a given age class using a linnear regression of the log spawner returns
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @param input The numeric value of previous year log(spawner) return
#' @keywords log spawner log recruit
#' @export
#' @examples
#' lnS.lnR.forecast()

lnS.lnR.forecast <- function(spawners,forecast.age,input){
  spawners <- apply(spawners,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  dat <- data.frame(spawners,forecast.age)
  mod <- lm(log(forecast.age+1)~log(spawners),data = dat)
  exp(predict(mod,data.frame(spawners = input)))
}

#' lnS.lnR sibling R-square
#'
#' Returns the R-square of a linear regression of the log return of a given age class and the log spawner returns
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @keywords log spawner log recruit
#' @export
#' @examples
#' lnS.lnR.rsquare()

lnS.lnR.rsquare <- function(spawners,forecast.age){
  spawners <- apply(spawners,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  dat <- data.frame(spawners,forecast.age)
  mod <- lm(log(forecast.age+1)~log(spawners),data = dat)
  summary(mod)$adj.r.squared
}

#' lnS.lnR sibling p-value
#'
#' Returns the slope p-value of a linear regression of the log return of a given age class and the log spawner returns
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @keywords log spawner log recruit
#' @export
#' @examples
#' lnS.lnR.pvalue()

lnS.lnR.pvalue <- function(spawners,forecast.age){
  spawners <- apply(spawners,2,unlist)
  forecast.age <- apply(forecast.age,2,unlist)
  dat <- data.frame(spawners,forecast.age)
  mod <- lm(log(forecast.age+1)~log(spawners),data = dat)
  summary(mod)$coefficients[2,4]
}
