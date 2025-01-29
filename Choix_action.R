# Les packages 
rm(list=ls())
library(xts)
library(moments)
library(yfR)
library(scales)
library(rugarch)
library(PerformanceAnalytics)
library(usethis)
library(devtools)
library(forecast)
library(FinTS)








##Merck & Co., Inc. (MRK)----
# set options for algorithm
my_ticker1 <- 'MRK'
first_date <- "2010-01-04"
last_date <-"2023-09-25"
# fetch data
MRK_yf <- yf_get(tickers = my_ticker1, 
                 first_date = first_date,
                 last_date = last_date,
                 freq_data='daily',type_return='log')

str(MRK_yf)
MRK_rt<-MRK_yf$price_adjusted
MRK_rendement=MRK_yf$ret_adjusted_prices[-1]

dates<-MRK_yf$ref_date[-1]
N<-length(MRK_rendement)
N

plot(MRK_rt)


MRK_rte=MRK_rendement[1:2013]#rt sur ensemble estimation de 2010-01-04  à 2017-12_31 inclu
MRK_rtt=MRK_rendement[2014:N]#rt sur l'ensemble de test de 2018-01_31  à 2023-09-25 inclu


ArchTest(MRK_rte, lag=12)
ArchTest(MRK_rtt,lag=12)