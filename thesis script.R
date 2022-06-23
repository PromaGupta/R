library(plm)
#install.packages("plm")
setwd("C://Users//HP//Desktop//Jupyter_practice//R")
a1<-read.csv("C://Users//HP//Desktop//Jupyter_practice//R//bank new-2.csv")
#data
pdata = pdata.frame(a1, index = c("ID","Year"))

###Pooled Method###
pooledmethod=plm(ni~ta+ffr+cc, data=pdata, model = "pooling")
summary(pooledmethod)

pooledmethod=plm(nig~ta+ffr+cc, data=pdata, model = "pooling")
summary(pooledmethod)

pooledmethod=plm(roe~ta+ffr+cc, data=pdata, model = "pooling")
summary(pooledmethod)

##with growth
b1<-read.csv("C://Users//HP//Desktop//Jupyter_practice//R//bank new-3.csv")
#data
pdata = pdata.frame(b1, index = c("ID","Year"))

###Pooled Method###
pooledmethod=plm(nig~tag+ffrg+ccg+lfg, data=pdata, model = "pooling")
summary(pooledmethod)

pooledmethod=plm(nig~ta+ffr+cc, data=pdata, model = "pooling")
summary(pooledmethod)

pooledmethod=plm(roe~ta+ffr+cc, data=pdata, model = "pooling")
summary(pooledmethod)

##with growth
c1<-read.csv("C://Users//HP//Desktop//Jupyter_practice//R//bank new-3.csv")
#data
pdata = pdata.frame(c1, index = c("ID","Year"))

###Pooled Method###
pooledmethod=plm(nig~tag+ffrg+cg+lfg, data=pdata, model = "pooling")
summary(pooledmethod)

pooledmethod=plm(nig~ta+ffr+cc, data=pdata, model = "pooling")
summary(pooledmethod)

pooledmethod=plm(roe~ta+ffr+cc, data=pdata, model = "pooling")
summary(pooledmethod)
####MAIN CODE
pooledmethod=plm(roe~ta+ffr+cc+lf, data=pdata, model = "pooling")
summary(pooledmethod)

pooledmethod=plm(roe~tag+ffrg+cg+lfg, data=pdata, model = "pooling")
summary(pooledmethod)

pooledmethod=plm(roe~ffrg+cg+lfg, data=pdata, model = "pooling")
summary(pooledmethod)

pooledmethod=plm(roe~ta+cc+lf, data=pdata, model = "pooling")
summary(pooledmethod)

pooledmethod=plm(de~ta+ffr+cc+lf, data=pdata, model = "pooling")
summary(pooledmethod)

pooledmethod=plm(de~tag+ffrg+cg+lfg, data=pdata, model = "pooling")
summary(pooledmethod)

pooledmethod=plm(roe~ta+ffr+cc+lf+de, data=pdata, model = "pooling")
summary(pooledmethod)

##ta, cc, lf, roe
##fixed method
femethod=plm(roe~ta+ffr+cc+lf, data=pdata, model = "within")
summary(femethod)

##random method
remethod=plm(roe~ta+ffr+cc+lf, data=pdata, model = "random")
summary(remethod)


##Poolabilty Test
#null: pooled ols is stable

pooltest(roe~ta+ffr+cc+lf, data=pdata, model = "within")

##as p is less than 5%, pooled ols is unstable here

##Pooled vs Fixed
# Null: Pooled is consistent
# Alter: Fixed is consistent

pFtest(femethod,pooledmethod)

## ##as p is less than 5%, pooled ols is unstable here and Fixed effect model is consistent

##Hausman Test

# Null: Random is consistent
# Alter: Fixed is constant

phtest(femethod,remethod)

##we can say out of both, random is more consistent

##Panel Unit Root Testing, null is that the variable is non stationary

w = data.frame(split(pdata$roe, pdata$ID))
purtest(w, pmax=2,exo="intercept", test = "levinlin" )

w = data.frame(split(pdata$roe, pdata$ID))
purtest(w, pmax=2,exo="intercept", test = "levinlin" )

w = data.frame(split(pdata$ta, pdata$ID))
purtest(w, pmax=2,exo="intercept", test = "levinlin" )

w = data.frame(split(pdata$cc, pdata$ID))
purtest(w, pmax=2,exo="intercept", test = "levinlin" )

w = data.frame(split(pdata$ffr, pdata$ID))
purtest(w, pmax=2,exo="intercept", test = "levinlin" )

w = data.frame(split(pdata$lf, pdata$ID))
purtest(w, pmax=2,exo="intercept", test = "levinlin" )

#Durbin Watson for Autocorrelation;Null: there is no autocorrelation

pdwtest(roe~ta+ffr+cc+lf, data=pdata, model = "random")

# The error term generated from random effect has autocorrelation problem of serial correlation

####Homoscedasticity Test: Null-there is homoskedasticity
library(lmtest)
#install.packages("zoo")
bptest(roe~ta+ffr+cc+lf, data=pdata, studentize = F)

#Durbin Watson for Autocorrelation;Null: there is no autocorrelation

pdwtest(roe~ta+ffr+cc+lf, data=pdata, model = "random")

# The error term generated from random effect has autocorrelation problem of serial correlation

####Homoscedasticity Test: Null-there is homoskedasticity
library(lmtest)
#install.packages("zoo")
bptest(roe~ta+ffr+cc+lf, data=pdata, studentize = F)

#install.packages("lmtest")

##There is no homoscedasticity

###Controlling of Heteroscedasticity and autocorrelation of Panel Data###

#for the fixed effect effect model:


install.packages("sandwich")
library(sandwich)

coeftest(femethod,vcovHC(femethod,method = "arellano"))

#for the random effect effect model
coeftest(remethod,vcovHC(remethod,method = "arellano"))

# remethod <- plm(roe~ta+ffr+cc+lf, data=pdata, index=c("ID", "Year"), model="random")

#####Wooldridge Test for Autocorrelation in Panel Data
##null: There is no autocorrelation

pbgtest(femethod)
pbgtest(remethod)

####ADF TEST, NULL:Non- STATIONARY
#install.packages("tseries")
library(tseries)
adf.test(pdata$roe)
adf.test(pdata$ta)
adf.test(pdata$cc)
adf.test(pdata$lf)
adf.test(pdata$ffr)
##as p is 0.01, roe, ta, ffr, cc, lf are stationary

#install.packages("systemfit")
###SUR
library(systemfit)
sur=systemfit(roe~ta+ffr,method = "SUR", data = pdata)
summary(sur)


###granger causality test, Null:  does not cause granger 
pgrangertest(roe~ta,data = pdata)
pgrangertest(roe~cc,data = pdata)
pgrangertest(roe~lf,data = pdata)
pgrangertest(roe~ffr,data = pdata)
#ta,cc,lf,ffr does not cause roe as p is greater than 5%

###if we know the lag order

pgrangertest(roe~ta,data = pdata, order = 3)
pgrangertest(roe~cc,data = pdata,order = 3)
pgrangertest(roe~lf,data = pdata,order = 3)
pgrangertest(roe~ffr,data = pdata,order = 3)

pgrangertest(roe~ta,data = pdata, order = 4)
pgrangertest(roe~cc,data = pdata,order = 4)
pgrangertest(roe~lf,data = pdata,order = 4)
pgrangertest(roe~ffr,data = pdata,order = 4)
