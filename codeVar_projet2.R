### R code from vignette source 'C:/R-4.2.0/library/utils/Sweave/VaRfiche3 -2022.Rnw'


rm(list=ls(all=TRUE))
#setwd("C:/Users/mlebreto/Documents/gretha/2022-2023/VaR")
library(xts)
library(forecast) 
library(moments)
library(yfR)
library(scales)


# set options for algorithm
my_ticker <- 'MRK'
first_date <- "2010-01-04"
last_date <-"2023-09-25"
# fetch data
# fetch data
df_yf <- yf_get(tickers = my_ticker,
                first_date = first_date,
                last_date = last_date,
                freq_data='daily',type_return='log')
pt<-df_yf$price_adjusted
dpt=diff(pt)
datesp<-df_yf$ref_date
dates<-datesp[-1]
rt=df_yf$ret_adjusted_prices[-1]

N<-length(rt)
rte<-rt[1:2013]
T<-length(rte)
rtt<-rt[2014:N]

rendement=df_yf$ret_adjusted_prices[-1]
N<-length(rendement)
rt<-xts(x=rendement,order.by=dates)
rte=rt[1:2013]#rt sur ensemble estimation de 2010-01-04  Ã  2017-12_31 inclu
datesrte<-dates[1:2013]
rtt=rt[2014:N]#rt sur l'ensemble de test de 2018-01_01  Ã  2023-09-25 inclu
library(rugarch)







## Code qqplot de rendemnet comparer avec la distribution théorique de la loi noremale
qqnorm(rte)
qqline(rte, col = 2)




## Estimation des distribution 

library(ghyp)
#estimation d'une distribution normale
fitn<-fit.gaussuv(data=rte)
summary(fitn)
#estimation student asym�trique
fitstu<-fit.tuv(rte,silent=T)
summary(fitstu)

#gaussienne inverse asym�trique
fitnig<-fit.NIGuv(data=rte,silent=T)
summary(fitnig)

#hyperbolique asym�trique
fithyp<-fit.hypuv(rte,silent=T)
summary(fithyp)


#hyperbolique g�n�ralis�e asym�trique
fitghypuv<-fit.ghypuv(rte,silent=T)
summary(fitghypuv)


## Graphique des distribution estimer sur rte
plot(density(rte))
lines(fitstu,col=2)#student
lines(fitnig,col=3)#estimateur de la densit� de rte
lines(fithyp,col=4)
lines(fitghypuv,col=5)
legend("topleft",legend =c("rte","student","nig","hyp","ghyp"), col =1:5,lty=rep(1,5))





## Esitmation de modèle ARMA(1,1)-GARCH(1,1) avec une distribution de student pour ϵ

spec1 = ugarchspec(mean.model =list(armaOrder=c(1,1)),distribution.model = "std")
fit1 = ugarchfit(spec = spec1, data = rt,out.sample=length(rtt))#� privil�gier pour faire de la pr�vision
#identique � fit1=ugarchfit(spec=spec1,data=rte) 
show(fit1)



## Prise en compte de la saisonnalté 
jour=format(dates, format = "%A")
mois=format(dates, format = "%B")
moisrte=mois[1:2013]
janvier=as.integer(moisrte=="janvier")
jourrte=jour[1:2013]#comme rte
lundi=as.integer(jourrte=="lundi")
spec1bis = ugarchspec(mean.model=list(external.regressors=as.matrix(cbind(lundi,janvier))), distribution.model = "std")
fit1bis = ugarchfit(spec = spec1bis, data = rt,out.sample=length(rtt))#� privil�gier pour faire de la pr�vision
show(fit1bis)















# Modèle ARCH-M
spec3 = ugarchspec(mean.model=list(armaOrder=c(1,1),archm=TRUE),distribution.model="std")
fit3 = ugarchfit(spec = spec3,data = rt,out.sample=length(rtt),solver="hybrid")
fit3


# Le modèle IGARCH
spec4 = ugarchspec(variance.model=list(model="iGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="std")
fit4 = ugarchfit(spec = spec4, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit4)



## Le modèle eGARCH 
spec5 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="std")
fit5 = ugarchfit(spec = spec5, data = rt,out.sample=length(rtt),solver="hybrid")
show(fit5)


## Modèle GJR-GARCH (Glosten Jagannathan Runkle-GARCH (1993))

spec6 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="std")
fit6= ugarchfit(spec = spec6,data = rt,out.sample=length(rtt),solver="hybrid")
show(fit6)



## Le modèle APARCH
spec7 = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="std")
fit7= ugarchfit(spec = spec7,data = rt,out.sample=length(rtt),solver="hybrid")
show(fit7)



## On fixe le delta à 2
spec7b = ugarchspec(variance.model=list(model="apARCH", garchOrder=c(1,1)),
                    mean.model=list(armaOrder=c(1,1), include.mean=FALSE ) ,distribution.model="std",fixed.pars=list(delta=2, gamma1=0))
fit7b= ugarchfit(spec = spec7b, data = rte,out.sample=length(rtt),solver="hybrid" )
show(fit7b)





## Estimation et backtestng avec ARMA(1,1)-GARCH(1,1)----

library(parallel)
# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)
spec = ugarchspec(mean.model =list(armaOrder=c(1,1)),distribution.model = "std")
                 
roll=ugarchroll(spec, data=rt,n.ahead=1,forecast.length=length(rtt),refit.every=5,
                refit.window="moving",solver = "hybrid", cluster=cl,fit.control = list(),calculate.VaR=TRUE,VaR.alpha=0.05,keep.coef = TRUE)
stopCluster(cl)
library(zoo)
valueatrisk<-zoo(roll@forecast$VaR[,1])
reelles<-zoo(roll@forecast$VaR[,2])#=rtt
index<-rownames(roll@forecast$VaR)


plot(dates[2014:N],reelles,type='b',xlab="Dates",ylab="Rendements et VaR")
lines(dates[2014:N],valueatrisk,type='l',col="red")
legend("topright",inset=.05,c("rt","VaR"),col=1:2,lty=c(1,1))



plot(dates[2014:N],reelles,type='b',xlab="Dates",ylab="Rendements et VaR")
lines(dates[2014:N],valueatrisk,type='l',col="red")
legend("topright",inset=.05,c("rt","VaR"),col=1:2,lty=c(1,1))

report(roll,type="VaR",VaR.alpha=0.05,conf.level=0.95)


## Estimation et backtesting avec eGARCH(1,1)
library(parallel)
# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

spec5 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="std")

roll5 = ugarchroll(spec5, data=rt,n.ahead=1,forecast.length=length(rtt),refit.every=5,
                refit.window="moving",solver = "hybrid", cluster=cl,fit.control = list(),calculate.VaR=TRUE,VaR.alpha=0.05,keep.coef = TRUE)
stopCluster(cl)
library(zoo)
valueatrisk<-zoo(roll5@forecast$VaR[,1])
reelles<-zoo(roll5@forecast$VaR[,2])#=rtt
index<-rownames(roll5@forecast$VaR)


plot(dates[2014:N],reelles,type='b',xlab="Dates",ylab="Rendements et VaR")
lines(dates[2014:N],valueatrisk,type='l',col="red")
legend("topright",inset=.05,c("rt","VaR"),col=1:2,lty=c(1,1))



plot(dates[2014:N],reelles,type='b',xlab="Dates",ylab="Rendements et VaR")
lines(dates[2014:N],valueatrisk,type='l',col="red")
legend("topright",inset=.05,c("rt","VaR"),col=1:2,lty=c(1,1))

report(roll5,type="VaR",VaR.alpha=0.05,conf.level=0.95)



## Estimation et backtesting avec gjrGARCH(1,1)----
library(parallel)
# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

spec6 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,1)),
                   mean.model=list(armaOrder=c(1,1)),distribution.model="std")

roll6 = ugarchroll(spec6, data=rt,n.ahead=1,forecast.length=length(rtt),refit.every=5,
                   refit.window="moving",solver = "hybrid", cluster=cl,fit.control = list(),calculate.VaR=TRUE,VaR.alpha=0.05,keep.coef = TRUE)
stopCluster(cl)
library(zoo)
valueatrisk<-zoo(roll6@forecast$VaR[,1])
reelles<-zoo(roll6@forecast$VaR[,2])#=rtt
index<-rownames(roll6@forecast$VaR)


plot(dates[2014:N],reelles,type='b',xlab="Dates",ylab="Rendements et VaR")
lines(dates[2014:N],valueatrisk,type='l',col="red")
legend("topright",inset=.05,c("rt","VaR"),col=1:2,lty=c(1,1))



plot(dates[2014:N],reelles,type='b',xlab="Dates",ylab="Rendements et VaR")
lines(dates[2014:N],valueatrisk,type='l',col="red")
legend("topright",inset=.05,c("rt","VaR"),col=1:2,lty=c(1,1))

report(roll6,type="VaR",VaR.alpha=0.05,conf.level=0.95)








   





## Bactesting pour VaR normale, CF, Simulation----

library(xts)
library(forecast) 
library(moments)
library(yfR)
library(scales)
library(rugarch)
library(PerformanceAnalytics)



#alpha=0.99 #VaR � 99%
alpha=0.95 #VaR � 95%

backTestVaR <- function(x, p = alpha) {
  normal.VaR = as.numeric(VaR(x, p=p, method="gaussian"))
  historical.VaR = as.numeric(VaR(x, p=p, method="historical"))
  modified.VaR = as.numeric(VaR(x, p=p, method="modified"))
  ans = c(normal.VaR, historical.VaR, modified.VaR)
  names(ans) = c("Normal", "HS", "Modified")
  return(ans)
}
Nt=length(rtt)
Ne=length(rte)
# rolling 1-step ahead estimates of VaR
VaR.results = rollapply(as.zoo(rt), width=Ne, 
                        FUN = backTestVaR, p=alpha, by.column = FALSE,
                        align = "right")

#VaR.results = lag(VaR.results, k=-1)
chart.TimeSeries(merge(rt, VaR.results),legend.loc="topright")



violations.mat = matrix(0, 3, 5)
rownames(violations.mat) = c("Normal", "HS", "Modified")
colnames(violations.mat) = c("En1", "n1", "1-alpha", "Percent", "VR")
violations.mat[, "En1"] = (1-alpha)*Nt
violations.mat[, "1-alpha"] = 1 - alpha

# Show Normal VaR violations
normalVaR.violations = as.numeric(as.zoo(rt[index(VaR.results)])) < VaR.results[, "Normal"]
violation.dates = index(normalVaR.violations[which(normalVaR.violations)])


for(i in colnames(VaR.results)) {
  VaR.violations = as.numeric(as.zoo(rt[index(VaR.results)])) < VaR.results[, i]
  violations.mat[i, "n1"] = sum(VaR.violations)
  violations.mat[i, "Percent"] = sum(VaR.violations)/Nt
  violations.mat[i, "VR"] = violations.mat[i, "n1"]/violations.mat[i, "En1"]
}
violations.mat

resultats<-data.frame(matrix(NA,ncol=4,nrow=3))
colnames(resultats)<-c("expected.exceed","actual.exceed","Kupiecpv","Christoffersenpv")
rownames(resultats)<-c("Normale","HS","CF")

# normale
VaR.test1 = VaRTest(1-alpha,actual=coredata(rt[index(VaR.results)]), VaR=coredata(VaR.results[,"Normal"]))
resultats[1,1]=VaR.test1$expected.exceed
resultats[1,2]=VaR.test1$actual.exceed
resultats[1,3]=VaR.test1$uc.LRp
resultats[1,4]=VaR.test1$cc.LRp

# historique
VaR.test2 = VaRTest(1-alpha,actual=coredata(rt[index(VaR.results)]), VaR=coredata(VaR.results[,"HS"]))
resultats[2,1]=VaR.test2$expected.exceed
resultats[2,2]=VaR.test2$actual.exceed
resultats[2,3]=VaR.test2$uc.LRp
resultats[2,4]=VaR.test2$cc.LRp



# modifie
VaR.test3 = VaRTest(1-alpha, actual=coredata(rt[index(VaR.results)]), VaR=coredata(VaR.results[,"Modified"]))

resultats[3,1]=VaR.test3$expected.exceed
resultats[3,2]=VaR.test3$actual.exceed
resultats[3,3]=VaR.test3$uc.LRp
resultats[3,4]=VaR.test3$cc.LRp

resultats

 


