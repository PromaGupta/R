library(plm)
install.packages("plm")
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
install.packages("zoo")
bptest(roe~ta+ffr+cc+lf, data=pdata, studentize = F)

install.packages("lmtest")

##There is no homoscedasticity

###Controlling of Heteroscedasticity and autocorrelation of Panel Data###

#for the fixed effect effect model:


install.packages("sandwich")
library(sandwich)

coeftest(femethod,vcovHC(femethod,method = "arellano"))

remethod <- plm(roe~ta+ffr+cc+lf, data=pdata, index=c("ID", "Year"), model="random")
coeftest(random)
summary(remethod)