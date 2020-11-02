#####################################################
# summary performance table
####################################################


#' Summary Performance table
#'
#' summarizes the performance metrics at 3 and 5 year means
#' @param sibling A tible with one column
#' @param forecast.age A tible with one column
#' @param f.cast.yr year to forecast (numeric)
#' @param riv.nam A character string, river
#' @param start.year oldest year to include in the forecast model
#' @keywords forecast
#' @export
#' @export f.cast.tbl.summary
#' @examples
#' f.cast.tbl.summary()
#' @author Greg Buck




f.cast.tbl.summary <- function(f.cast.age,sibling,f.cast.yr,riv.name,start.yr){

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
  a <- nrow(dat)

  # create empty data frame to collect stats
  master.summary <- data.frame(model=character(),
                               range=character(),
                               MAD=integer(),
                               MAPE=integer(),
                               MPE=integer(),
                               MAAPE=integer(),
                               stringsAsFactors=FALSE)
  df <- data.frame(matrix(vector(), a + 1, 3,
                          dimnames=list(c(), c("r.sq", "p.value", "forecast"))),
                   stringsAsFactors=F)

  # model list
  model.list <- c("sibling","ln.sibling","lnY.sibling","lnS.lnR","ricker","average")
  #model.list <- c("sibling","ln.sibling","lnY.sibling","lnS.lnR","ricker")


  # loop through model list
  for(x in 1:6){


    # loop through models collecting stats--SIBLING
    if(model.list[x] == "sibling"){
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
    if(model.list[x] == "ln.sibling"){
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
    if(model.list[x] == "lnY.sibling"){
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
    if(model.list[x] == "ricker"){
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
    if(model.list[x] == "lnS.lnR"){
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
    if(model.list[x] == "average"){
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
      mutate(AAPE = atan(abs((f.cast.age - forecast)/f.cast.age)))


    summary5.temp <- temp %>%
      filter((return.year < f.cast.yr) & (return.year > (f.cast.yr - 6)))

    MAD.5 <- mean(summary5.temp$AD)
    MAPE.5 <- mean(summary5.temp$APE)
    MPE.5 <- mean(summary5.temp$PE)
    MAAPE.5 <- mean(summary5.temp$AAPE)

    t <- c(model.list[x],"5 years",MAD.5,MAPE.5,MPE.5,MAAPE.5)
    master.summary[nrow(master.summary) + 1,] <- t

    summary3.temp <- temp %>%
      filter((return.year < f.cast.yr) & (return.year > (f.cast.yr - 4)))

    MAD.3 <- mean(summary3.temp$AD)
    MAPE.3 <- mean(summary3.temp$APE)
    MPE.3 <- mean(summary3.temp$PE)
    MAAPE.3 <- mean(summary3.temp$AAPE)

    tt <- c(model.list[x],"3 years",MAD.3,MAPE.3,MPE.3,MAAPE.3)
    master.summary[nrow(master.summary) + 1,] <- tt

    df <- data.frame(matrix(vector(), a + 1, 3,
                            dimnames=list(c(), c("r.sq", "p.value", "forecast"))),
                     stringsAsFactors=F)
  } # end model.list loop


  temp2 <- master.summary %>%
    mutate(MAD = as.numeric(MAD)) %>%
    mutate(MAD = round(MAD)) %>%
    mutate(MAD = format(MAD, big.mark = ",")) %>%
    mutate(MAPE = as.numeric(MAPE)) %>%
    mutate(MAPE = formattable:::percent(MAPE)) %>%
    mutate(MPE = as.numeric(MPE)) %>%
    mutate(MPE = formattable:::percent(MPE)) %>%
    mutate(MAAPE = as.numeric(MAAPE)) %>%
    mutate(MAAPE = round(MAAPE, digits = 2)) %>%
    group_by(model)


temp3 <- formattable(temp2,
              align = c("l",rep("r", NCOL(temp2) - 1)),
              list("model" = formatter("span", style = ~ style(color = "grey", font.weight = "bold")),
                   "MAD" = color_bar("#FA614B"),
                   "MAPE" = color_bar("#6b4596b2"),
                   "MPE" = color_bar("#95D840FF"),
                   "MAAPE" = color_bar("#DCE319FF")))
temp3

} # end f.cast.tbl function




