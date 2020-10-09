#####################################################
# forecast plots (data + residual
####################################################


#' Forecast plots
#'
#' linnear regression plots and residual plot for sibling, ln.sibling, lnY.sibling, lnS.lnR and ricker models
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @param f.cast.yr year to forecast (numeric)
#' @param riv.nam A character string, river
#' @param start.year oldest year to include in the forecast model
#' @param model sibling, ln.sibling, lnY.sibling, lnS.lnR, ricker
#' @keywords forecast
#' @export
#' @examples
#' plot.data()
#' @author Greg Buck



plot.data <- function(f.cast.age,sibling,f.cast.yr,riv.name,start.yr,model){

  #read brood tables into memory
  riv <- read_excel(data,
                    sheet = riv.name , col_types = "numeric", skip = 1)

  t <- c("brood.year","age.01","age.02","age.03","age.04","age.05","age.11","age.12",
         "age.13","age.14","age.15","age.21","age.22","age.23","age.24","age.31","age.32",
         "age.33","age.34","escapement","recruits","r.per.s")
  tt <- as.vector(t)
  names(riv)<-t

  riv$return.year <- riv$brood.year + as.numeric(substr(f.cast.age,5,5)) + as.numeric(substr(f.cast.age,6,6)) + 1
  dat <- riv[(riv$brood.year >= start.yr & riv$return.year <= f.cast.yr),
             c("brood.year","return.year","escapement",sibling,f.cast.age)]
  names(dat) <- c("brood.year","return.year","spawners","sibling","f.cast.age")

  #data transformations
  master <- dat %>%
    mutate(ln.f.cast.age = log(f.cast.age + 1)) %>%
    mutate(lnS = log(spawners)) %>%
    mutate(ln.sibling = log(sibling +1)) %>%
    mutate(lnRS = log(f.cast.age / spawners))


  if(model == "sibling"){

    p <- ggplot(data = master, aes(x = sibling, y = f.cast.age)) +
      geom_point() +
      stat_smooth(method = "lm", col = "red")

    r <- ggplot(lm(data=master,f.cast.age~sibling)) +
      geom_point(aes(x=.fitted, y=.resid)) +
      geom_hline(yintercept=0)

    print(p)
    print(r)
  } # end if

  if(model == "ln.sibling"){

    p <- ggplot(data = master, aes(x = ln.sibling, y = ln.f.cast.age)) +
      geom_point() +
      stat_smooth(method = "lm", col = "red")

    r <- ggplot(lm(data=master,ln.f.cast.age~ln.sibling)) +
      geom_point(aes(x=.fitted, y=.resid)) +
      geom_hline(yintercept=0)

    print(p)
    print(r)
  } # end if

  if(model == "lnY.sibling"){

    p <- ggplot(data = master, aes(x = sibling, y = ln.f.cast.age)) +
      geom_point() +
      stat_smooth(method = "lm", col = "red")

    r <- ggplot(lm(data=master,ln.f.cast.age~sibling)) +
      geom_point(aes(x=.fitted, y=.resid)) +
      geom_hline(yintercept=0)

    print(p)
    print(r)
  } # end if

  if(model == "lnS.lnR"){

    p <- ggplot(data = master, aes(x = lnS, y = ln.f.cast.age)) +
      geom_point() +
      stat_smooth(method = "lm", col = "red")

    r <- ggplot(lm(data=master,ln.f.cast.age~lnS)) +
      geom_point(aes(x=.fitted, y=.resid)) +
      geom_hline(yintercept=0)

    print(p)
    print(r)
  } # end if

  if(model == "ricker"){

    p <- ggplot(data = master, aes(x = lnS, y = lnRS)) +
      geom_point() +
      stat_smooth(method = "lm", col = "red")

    r <- ggplot(lm(data=master,lnRS~lnS)) +
      geom_point(aes(x=.fitted, y=.resid)) +
      geom_hline(yintercept=0)

    print(p)
    print(r)
  } # end if

} # end plot data function
