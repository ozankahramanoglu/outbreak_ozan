library(forecast)
library(MASS)
library(nlme)
library(lubridate)

# setwd('/Users/ZFED/Dropbox/RESIDUAL CUSUM for RESPIRATORY DATA/R')
# resp_table_child<- read.csv("resp_child_07_14.csv", header=TRUE)
# rm(data)
# data <- resp_table_child[,c(1,2,22)]
# colnames(data) <- c("Date","Number","Holiday")
# 
#--------------------------------------------------------------------
#                     1- DATA PREPERATION
# -------------------------------------------------------------------

arima_run <- function(data,nyearstrain,slidingWinSize,CusumParam) {
  

train_startdate <- as.Date(c("2007-09-24"))
#numyears_train <- 1
numyears_train <- as.numeric(nyearstrain)

#test_startdate <- as.Date(c("2008-09-24"))
test_startdate <- train_startdate %m+% years(numyears_train) #burası...

test_enddate <- data[dim(data)[1],"Date"]

train_enddate <- as.Date(train_startdate %m+% years(numyears_train)-days(1))

b <- as.numeric(which(data[,"Date"] == train_startdate))
T <- as.numeric(which(data[,"Date"] == train_enddate))


data[,"Dayofyear"]  <- yday(data[,"Date"])

data[,"Mon_dual"] <- ifelse( as.POSIXlt(data[,"Date"])$wday == 1, 1,
                             ifelse( as.POSIXlt(data[,"Date"])$wday != 1 , 0,
                                     NA))

data[,"Tues_dual"] <- ifelse( as.POSIXlt(data[,"Date"])$wday == 2, 1,
                              ifelse( as.POSIXlt(data[,"Date"])$wday != 2 , 0,
                                      NA))

data[,"Wed_dual"] <- ifelse( as.POSIXlt(data[,"Date"])$wday == 3, 1,
                             ifelse( as.POSIXlt(data[,"Date"])$wday != 3 , 0,
                                     NA))

data[,"Thu_dual"] <- ifelse( as.POSIXlt(data[,"Date"])$wday == 4, 1,
                             ifelse( as.POSIXlt(data[,"Date"])$wday != 4 , 0,
                                     NA))

data[,"Fri_dual"] <- ifelse( as.POSIXlt(data[,"Date"])$wday == 5, 1,
                             ifelse( as.POSIXlt(data[,"Date"])$wday != 5 , 0,
                                     NA))

data[,"Sat_dual"] <- ifelse( as.POSIXlt(data[,"Date"])$wday == 6, 1,
                             ifelse( as.POSIXlt(data[,"Date"])$wday != 6 , 0,
                                     NA))

data[,"Sun_dual"] <- ifelse( as.POSIXlt(data[,"Date"])$wday == 0, 1,
                             ifelse( as.POSIXlt(data[,"Date"])$wday != 0 , 0,
                                     NA))

data[,c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")] <- 
  data[,"Number"]*data[,c("Mon_dual", "Tues_dual", "Wed_dual", "Thu_dual", "Fri_dual", "Sat_dual", "Sun_dual")] 

### same for months
data[,"Month"] <- month(data[,"Date"])

data[,"Jan_dual"] <- ifelse( month(data[,"Month"]) == 1, 1,
                             ifelse( month(data[,"Month"]) != 1, 0,
                                     NA)) 

data[,"Feb_dual"] <- ifelse( month(data[,"Month"]) == 2, 1,
                             ifelse( month(data[,"Month"]) != 2, 0,
                                     NA)) 

data[,"Mar_dual"] <- ifelse( month(data[,"Month"]) == 3, 1,
                             ifelse( month(data[,"Month"]) != 3, 0,
                                     NA)) 

data[,"Apr_dual"] <- ifelse( month(data[,"Month"]) == 4, 1,
                             ifelse( month(data[,"Month"]) != 4, 0,
                                     NA)) 

data[,"May_dual"] <- ifelse( month(data[,"Month"]) == 5, 1,
                             ifelse( month(data[,"Month"]) != 5, 0,
                                     NA)) 

data[,"Jun_dual"] <- ifelse( month(data[,"Month"]) == 6, 1,
                             ifelse( month(data[,"Month"]) != 6, 0,
                                     NA)) 

data[,"Jul_dual"] <- ifelse( month(data[,"Month"]) == 7, 1,
                             ifelse( month(data[,"Month"]) != 7, 0,
                                     NA)) 

data[,"Aug_dual"] <- ifelse( month(data[,"Month"]) == 8, 1,
                             ifelse( month(data[,"Month"]) != 8, 0,
                                     NA)) 
data[,"Sep_dual"] <- ifelse( month(data[,"Month"]) == 9, 1,
                             ifelse( month(data[,"Month"]) != 9, 0,
                                     NA)) 
data[,"Oct_dual"] <- ifelse( month(data[,"Month"]) == 10, 1,
                             ifelse( month(data[,"Month"]) != 10, 0,
                                     NA)) 
data[,"Nov_dual"] <- ifelse( month(data[,"Month"]) == 11, 1,
                             ifelse( month(data[,"Month"]) != 11, 0,
                                     NA)) 
data[,"Dec_dual"] <- ifelse( month(data[,"Month"]) == 12, 1,
                             ifelse( month(data[,"Month"]) != 12, 0,
                                     NA)) 
###### end for months
# We need to extract some variables:


colnames <- c("Date","Number","Holiday", "Dayofyear","Mon_dual", "Tues_dual", "Thu_dual","Fri_dual", "Sat_dual", "Sun_dual",
              "Jan_dual", "Feb_dual", "Mar_dual", "Apr_dual", "May_dual", "Jul_dual","Aug_dual", "Sep_dual", 
              "Oct_dual", "Nov_dual", "Dec_dual")
data <- cbind( data[ ,names(data) %in% colnames],
               sin((2*pi*data[,"Dayofyear"])/365.25),cos((2*pi*data[,"Dayofyear"])/365.25),
               sin((4*pi*data[,"Dayofyear"])/365.25),cos((4*pi*data[,"Dayofyear"])/365.25) )
colnames(data) <- c(colnames, c("sin2","cos2","sin4","cos4"))

#--------------------------------------------------------------------
#                     2- OLS MODEL --  BOX-COX
# -------------------------------------------------------------------

traindata <- data[b:T,-which(names(data)=="Date")]
data_ts <- ts (traindata, frequency = (T-b+1), start = c (year(train_startdate),month(train_startdate)) )
model_ts<-lm( Number ~., data=data_ts ) 
plot(residuals(model_ts))   # ERRORLAR STATIONARY DEGIL

# Implement the Box-Cox transformation
# if we set lambda2 to zero, it becomes the one parameter transformation

bc <- boxcox(Number ~., data=data_ts) 
lambda <- bc$x[which.max(bc$y)]
data[ ,"Transformednumber"] <-  c()


if (lambda <= 0.2 && lambda >= -0.2) {
  data[ ,"Transformednumber"] <- log(data[ ,"Number"])
} else {
  data[ ,"Transformednumber"] <- ((data[ ,"Number"]^lambda )-1)/lambda
}


# We can clear the Number column now and create new traindata and new data_ts with transformed values
rm(traindata)

traindata <- data[b:T,-which(names(data) %in% c("Date","Number"))]

data_ts <- ts (traindata, frequency = (T-b+1), start = c (year(train_startdate),month(train_startdate)) )

model_ts<-lm(Transformednumber~., data=data_ts )

summary(model_ts) 
plot(residuals(model_ts))   # ERRORLAR STATIONARY DEGIL
tsdisplay(residuals(model_ts), main="ARIMA errors")
auto.arima(residuals(model_ts))$coef 

# Extract AR and MA coefficient outputs from the auto.arima function and call them as ar_coef and ma_coef


coefficients_arima <- as.data.frame(strsplit(rownames( data.frame(auto.arima(residuals(model_ts))$coef)), 
                                             "(?=[A-Za-z])(?<=[0-9])|(?=[0-9])(?<=[A-Za-z])", perl=TRUE))

if ( length (coefficients_arima[2,][coefficients_arima[1,]=="ar"] ) == 0 ) {
  ar_coef <- 0
} else {
  ar_coef <-as.numeric(max(coefficients_arima[2,][coefficients_arima[1,]=="ar"]))
}

if (length(coefficients_arima[2,][coefficients_arima[1,]=="ma"] ) == 0 ) {
  ma_coef <- 0
} else {
  ma_coef <-as.numeric(max(coefficients_arima[2,][coefficients_arima[1,]=="ma"]))
}
ar_coef 
ma_coef
#--------------------------------------------------------------------
#                     3- GLS FIT WITH ARMA ERRORS (use nmle)  -- BOX-COX
# -------------------------------------------------------------------

gls_fit_ts <- gls(Transformednumber ~ ., data=data_ts, correlation = corARMA(p=ar_coef,q=ma_coef),
                  method="ML")

tsdisplay(residuals(gls_fit_ts, type="normalized"), main="ARIMA errors")

#--------------------------------------------------------------------
#                     4- CUSUM AND SIGNAL DETECTION
# -------------------------------------------------------------------

test_enddate <- data[dim(data)[1],"Date"]

gls_predict <-  c()

#cusum parameters:
# kk <- 1    # agressive:0.5  ---  moderate:1     ----  routine:1   # cusum algorithms aggresive | moderate | routine
# H <- 0.695 # agressive:0.365 --- moderate:0.695 ----  routine:1.2
# win <- 7 #sliding window size                                     # 7 | 14 | 21 days or 1|2|3 weeks   

#Shiny cusum parameters:
win <- as.numeric(slidingWinSize)

if (all(CusumParam == "Aggresive")) {

  kk <- 0.5
  H <- 0.365

}
else if (all(CusumParam == "Moderate")){

  kk <- 1
  H <- 0.695

}
else {

  kk <- 1
  H <- 1.2

}



cusum_result <- data.frame()

while (test_startdate < test_enddate) {
  
  # elimizdeki test data sini yillik yillik cekiyoruz, taa ki test_startdate >= test_enddate olana kadar
  # test data her sene icin yeniden olusturulacak
  
  # test data list oldugu icin onu dataNum diye bir numeric'e donusturup islemlere devam ettim
  
  testdata <-data[which(data[,"Date"] >= test_startdate & data[,"Date"]< test_startdate %m+%years(numyears_train)),
                  -which(names(data)==c("Date","Number"))]
  
  # prediction icin gls model:
  
  # EGER DATA FRAME OLARAK KABUL ETMEZSE BUNU KULLAN:
  # dataNum <- matrix(data = NA, nrow = dim(testdata)[1], ncol = dim(testdata)[2])
  # colnames(dataNum) <- colnames(testdata)
  # 
  # for (i in 1:dim(testdata)[2]) {
  #   dataNum[,i] <- as.numeric(testdata[[i]])
  # }
  # gls_predict <-  predict(gls_fit_ts,as.data.frame(dataNum))[1:dim(dataNum)[1]]
  # 
  gls_predict <-  predict(gls_fit_ts,as.data.frame(testdata))[1:dim(testdata)[1]]
  
  
  #pb ve pT bu testdata'ya aldigimiz verilerin normal data'da kacinci veriye denk geldigini soyluyor
  
  pb <- as.numeric(which(data[,"Date"]==test_startdate))
  
  if (test_enddate >= test_startdate %m+% years(1)-days(1)) {
    pT <- as.numeric(which(data[,"Date"]==test_startdate %m+% years(1)-days(1)))
  } else {
    pT <- pb + as.numeric(length( which(data[,"Date"]>=test_startdate & data[,"Date"]<=test_enddate )))-1
  }
  
  # SON SENE TAM YIL DEGILSE PT DEGISECEK
  
  testperiod <- paste(year(data[which(data[,"Date"]==test_startdate),"Date"]),"-",year(test_startdate %m+% years(1)-days(1)))
  testdata[,"Year"] <- c(rep(testperiod,dim(testdata)[1]))
  testdata[,"Day"] <- c(1:dim(testdata)[1])
  testdata[,"CumDay"] <- c(pb:pT)
  testdata[,"s"] <- c(rep(win,dim(testdata)[1]))
  
  # inverse box-cox to transform predicted gls_predict values
  # invBoxCox <- function(x, lambda) 
  # if (lambda <= 0.2 && lambda >= -0.2) 
  #   exp(x) else (lambda*x + 1)^(1/lambda) 
  
  if (lambda <= 0.2 && lambda >= -0.2) {
    testdata[ ,"Predicted"] <- exp(gls_predict)
    testdata[ ,"Observed"] <- exp(testdata[ ,"Transformednumber"])
  } else {
    testdata[ ,"Predicted"] <- (lambda*gls_predict + 1)^(1/lambda) 
    testdata[ ,"Observed"] <- (lambda*testdata[ ,"Transformednumber"] + 1)^(1/lambda) 
  }
  
  # Find error terms
  testdata[,"Residuals"] <- scale(testdata[ ,"Observed"]-testdata[ ,"Predicted"])
  
  ll <- pT-pb+1
  #resid <- testdata$Residuals
  
  # CUSUM CHARTS 
  testdata[,"Cp"]=NULL
  testdata[1,"Cp"]=0
  
  # -------------------------------------------------------------------
  #for(i in 2:ll){Cp[i]=max(0,resid[i]-kk+Cp[i-1])
  #                 if(Cp[i]<Cp[i-1]){Cp[i]=0}}
  # -------------------------------------------------------------------
  for(i in 2:ll){testdata[i,"Cp"]=max(0,testdata[i,"Residuals"]-kk+testdata[i-1,"Cp"])}
  
  for(i in (win+1):ll){
    mn=(i-win):i
    if(testdata[i,"Cp"]>H){mod=lm(testdata[mn,"Cp"]~mn)
    if(mod$coef[2]<0){testdata[i,"Cp"]=0}}}
  
  
  testdata[,"signal"]<- ifelse( testdata[,"Cp"]>H, 1,
                                ifelse( testdata[,"Cp"]<=H, 0,
                                        NA))
  testdata[,"Cp2"]<- ifelse( testdata[,"Cp"]>H, 200,
                             ifelse( testdata[,"Cp"]<=H, NA,
                                     NA)) 
  
  tmp <- testdata[,c("Observed", "Predicted","Year","CumDay","Day","s","Residuals","Cp","Cp2","signal")]
  
  cusum_result <- rbind(cusum_result,tmp)
  
  test_startdate <- test_startdate %m+% years(1)
  rm(testdata)
  rm(tmp)
  #rm(dataNum)
  
}
return_data <- list("data" = data , "ts" = gls_fit_ts, "cusum" = cusum_result, "tsmodel" = model_ts)
return(return_data)
}
