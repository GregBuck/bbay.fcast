#####################################################
# Forecast summary table
####################################################


#' Forecast summary table
#'
#' Forecast summary table
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @keywords forecast
#' @export
#' @examples
#' f.cast.tbl()

f.cast.tbl <- function(f.cast.age,sibling,f.cast.yr,riv.name,start.yr,model){

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

  # create empty data frame to collect stats
  a <- nrow(dat)
  df <- data.frame(matrix(vector(), a + 1, 3,
                          dimnames=list(c(), c("r.sq", "p.value", "forecast"))),
                   stringsAsFactors=F)


  # loop through models collecting stats--SIBLING
  if(model == "sibling"){
    for (i in 0:9){
      tmp <- dat[1:(nrow(dat)-i),]
      df[a-i,"forecast"] <- sib.forecast(tmp[tmp$return.year <= f.cast.yr -1-i,"sibling"],
                                         tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"],
                                         tmp[tmp$return.year==f.cast.yr-i,"sibling"])

      df[a-i,"r.sq"] <- sib.rsquare(tmp[tmp$return.year <= f.cast.yr -1-i,"sibling"],
                                    tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"])

      df[a-i,"p.value"] <- sib.pvalue(tmp[tmp$return.year <= f.cast.yr -1-i,"sibling"],
                                      tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"])

    } # end for

    # merge initial dataframe with stats dataframe
    df <- df[1:(nrow(df)-1),]
    master <- cbind(dat,df)
  } # end if


  # loop through models collecting stats--LN.SIBLING
  if(model == "ln.sibling"){
    for (i in 0:9){
      tmp <- dat[1:(nrow(dat)-i),]
      df[a-i,"forecast"] <- ln.sib.forecast(tmp[tmp$return.year <= f.cast.yr -1-i,"sibling"],
                                            tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"],
                                            tmp[tmp$return.year==f.cast.yr-i,"sibling"])

      df[a-i,"r.sq"] <- ln.sib.rsquare(tmp[tmp$return.year <= f.cast.yr -1-i,"sibling"],
                                       tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"])

      df[a-i,"p.value"] <- ln.sib.pvalue(tmp[tmp$return.year <= f.cast.yr -1-i,"sibling"],
                                         tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"])

    } # end for

    # merge initial dataframe with stats dataframe
    df <- df[1:(nrow(df)-1),]
    master <- cbind(dat,df)
  } # end if


  # loop through models collecting stats--LNY.SIBLING
  if(model == "lnY.sibling"){
    for (i in 0:9){
      tmp <- dat[1:(nrow(dat)-i),]
      df[a-i,"forecast"] <- lnY.sib.forecast(tmp[tmp$return.year <= f.cast.yr -1-i,"sibling"],
                                             tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"],
                                             tmp[tmp$return.year==f.cast.yr-i,"sibling"])

      df[a-i,"r.sq"] <- lnY.sib.rsquare(tmp[tmp$return.year <= f.cast.yr -1-i,"sibling"],
                                        tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"])

      df[a-i,"p.value"] <- lnY.sib.pvalue(tmp[tmp$return.year <= f.cast.yr -1-i,"sibling"],
                                          tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"])

    } # end for

    # merge initial dataframe with stats dataframe
    df <- df[1:(nrow(df)-1),]
    master <- cbind(dat,df)
  } # end if

  # loop through models collecting stats--RICKER
  if(model == "ricker"){
    for (i in 0:9){
      tmp <- dat[1:(nrow(dat)-i),]
      df[a-i,"forecast"] <- ricker.forecast(tmp[tmp$return.year <= f.cast.yr -1-i,"spawners"],
                                            tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"],
                                            tmp[tmp$return.year==f.cast.yr-i,"spawners"])

      df[a-i,"r.sq"] <- ricker.rsquare(tmp[tmp$return.year <= f.cast.yr -1-i,"spawners"],
                                       tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"])

      df[a-i,"p.value"] <- ricker.pvalue(tmp[tmp$return.year <= f.cast.yr -1-i,"spawners"],
                                         tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"])

    } # end for

    # merge initial dataframe with stats dataframe
    df <- df[1:(nrow(df)-1),]
    master <- cbind(dat,df)
  } # end if


  # loop through models collecting stats--LNS.LNR
  if(model == "lnS.lnR"){
    for (i in 0:9){
      tmp <- dat[1:(nrow(dat)-i),]
      df[a-i,"forecast"] <- lnS.lnR.forecast(tmp[tmp$return.year <= f.cast.yr -1-i,"spawners"],
                                             tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"],
                                             tmp[tmp$return.year==f.cast.yr-i,"spawners"])

      df[a-i,"r.sq"] <- lnS.lnR.rsquare(tmp[tmp$return.year <= f.cast.yr -1-i,"spawners"],
                                        tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"])

      df[a-i,"p.value"] <- lnS.lnR.pvalue(tmp[tmp$return.year <= f.cast.yr -1-i,"spawners"],
                                          tmp[tmp$return.year <= f.cast.yr -1- i,"f.cast.age"])

    } # end for

    # merge initial dataframe with stats dataframe
    df <- df[1:(nrow(df)-1),]
    master <- cbind(dat,df)
  } # end if


  # loop through models collecting stats--average
  if(model == "average"){
    dat <-  mutate(dat, temp = frollmean(f.cast.age, n = 5))
    a <- as.vector(dat$temp)
    b <- length(a)
    a <- prepend(a,0,before = 1)
    a <- a[1:b]
    dat <- cbind(dat,a)
    dat <- dat[,c("brood.year","return.year","spawners","sibling","f.cast.age","a")]
    names(dat) <- c("brood.year","return.year","spawners","sibling","f.cast.age","forecast")
    dat$r.sq <- ""
    dat$p.value <- ""
    master <- dat
  } # end if

  # truncates table to most recent years
  temp <- master %>%
    filter(return.year > f.cast.yr - 10) %>%
    mutate(AD = abs(forecast - f.cast.age)) %>%
    mutate(PE = 1 - f.cast.age/forecast) %>%
    mutate(APE = abs(PE)) %>%
    mutate(AAPE = atan(abs((f.cast.age - forecast)/f.cast.age))) %>%
    mutate(brood.year = as.character(brood.year)) %>%
    mutate(return.year = as.character(return.year))

if(model == "ricker" | model == "lnS.lnR"){
  temp <- temp[,c("return.year","brood.year","f.cast.age","spawners","r.sq",
                  "p.value","forecast","AD","APE","PE","AAPE")]

  names(temp) <- c("Return Yr","Brood Yr","Observed Return","Spawner Return",
                   "R Sq","P-val","Forecasted Return","AD","APE","PE","AAPE")
} # end if

if(model == "sibling" | model == "ln.sibling" | model == "lnY.sibling"){
  temp <- temp[,c("return.year","brood.year","f.cast.age","sibling","r.sq",
                  "p.value","forecast","AD","APE","PE","AAPE")]

  names(temp) <- c("Return Yr","Brood Yr","Observed Return","Sibling Return",
                   "R Sq","P-val","Forecasted Return","AD","APE","PE","AAPE")
}

  if(model == "average"){
    temp <- temp[,c("return.year","brood.year","f.cast.age",
                    "forecast","AD","APE","PE","AAPE")]

    names(temp) <- c("Return Yr","Brood Yr","Observed Return",
                     "Forecasted Return","AD","APE","PE","AAPE")
  }

  if(model == "average"){
  temp %>%
    kable(digits = c(0,3,3,0,0,2,2,2), format.args = list(big.mark = ",")) %>%
    kable_styling() %>%
    column_spec(1:2,color = "blue") %>%
    column_spec(3,bold = T) %>%
    column_spec(4,bold = T, background = "yellow") %>%
    column_spec(5:8,color = "green")
  }else{
  temp %>%
    kable(digits = c(0,0,0,0,3,3,0,0,2,2,2), format.args = list(big.mark = ",")) %>%
    kable_styling() %>%
    column_spec(1:2,color = "blue") %>%
    column_spec(3:4,bold = T) %>%
    column_spec(5:6, color = "red") %>%
    column_spec(7,bold = T, background = "yellow") %>%
    column_spec(8:11,color = "green")
} # end if

} # end f.cast.tbl function
