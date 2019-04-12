# This script is just to call the data

#setwd('/Users/ZFED/Dropbox/RESIDUAL CUSUM for RESPIRATORY DATA/R')
#setwd('/Users/ZFED/Dropbox/OutbreakDetector_ozan')
setwd('E:/Dropbox/OutbreakDetector_esra')
#######
# resp_table_child<- read.csv("resp_child_07_14.csv", header=TRUE)
# data <- resp_table_child[,c(1,2,22)]
data <- read.csv("OutbreakDetector_sampledata.csv")
data <- data[,-4]
#######
colnames(data) <- c("Date","Number","Holiday")
#colnames(data) <- c("Date","numberofpatients","Holiday")
#colnames(data) <- c("Date","Number","Holiday")
###########################################################################################