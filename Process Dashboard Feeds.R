##################################################
### R project to process SafeTravelPH vehicle feeds
### Developed by Noriel Christopher C. Tiglao
### 26 Jan 2021
### Update 11 Feb 2021
##################################################

#install packages
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("dbscan")
install.packages("sp")
install.packages("dplyr")
install.packages("rgdal")
install.packages("geosphere")
install.packages("leaflet")
install.packages("leaflet.extras")
install.packages("cluster")
install.packages("factoextra")

#load packages
library(ggplot2)
library(tidyverse)
library(dbscan)
library(sp)
library(dplyr)
library(rgdal)
library(lubridate)
library(geosphere)
library(leaflet)
library(leaflet.extras)


#use integer, not scientific formatting
options(scipen=999)


######################################
### read vehicle feeds into dataframe
######################################

#get working directory
workdir <- getwd()

#get dashboard data
df_db <- read.csv(paste(workdir, "Dashboard_Data\\Juavan_2020-11-27.csv", sep="\\"), header=TRUE)


#create Datetime column
df_db$Datetime <- ymd_hms(paste(df_db$Date, df_db$Time))

#arrange rows by Datetime
df_db2 <- df_db %>%
  mutate(Datetime = ymd_hms(Datetime),
         Hour = as.numeric(hour(Datetime)),
         Min = as.numeric(minute(Datetime)),
         Sec = as.numeric(second(Datetime))) %>%
  arrange(Hour, Min, Sec) 


###EWMA
ewma <- function(x, lambda, target=x[1]){
  N <- length(x)
  y <- numeric(N)
  y[1] = target
  for (k in 2:N){
    error = x[k - 1] - y[k - 1]
    y[k] = y[k - 1] + lambda*error
  }
  return(y)
}


#rpm correction
df_db2$RPMcorr <- ewma(df_db2$RPM, lambda = 0.4, target = 50)

#am peak data
am_peak <- subset(df_db2, Hour >= 6 & Hour <= 6.5)

ggplot(data=am_peak) + 
  geom_point(aes(x=Datetime, y=RPM),color='black') + 
  geom_line(aes(x=Datetime, y=RPMcorr),color='blue') + 
  ylab('RPM')+xlab('Time')


#pm peak data
am_offpeak <- subset(df_db2, Hour >= 9 & Hour <= 10)

ggplot(data=am_offpeak) + 
  geom_point(aes(x=Datetime, y=RPM),color='black') + 
  geom_line(aes(x=Datetime, y=RPMcorr),color='blue') + 
  ylab('RPM')+xlab('Time')


