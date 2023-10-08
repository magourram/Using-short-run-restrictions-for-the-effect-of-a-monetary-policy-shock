# Main site reference: "Economics 343, “Macroeconomic Data Analysis”
## Link: https://github.com/hegerty/ECON343/

# Site: "Extracting Business Cycles and Calculating Cross-correlations in R"
## Link: https://www.youtube.com/watch?v=bxNgHGeTOCM

# Site: "How to write a Macreconomic Report"
## Link : https://www.youtube.com/watch?v=V2MMgGsPyuQ

# References: "HP Filter -  Trend Estimation Method"
## Link : https://www.oecd-ilibrary.org/sites/361fabc3-en/index.html?itemId=/content/component/361fabc3-en

# Refernces: "JD Hamilton: Never use HP FIlter, and alternative way to decompose"
## Link : https://voxeu.org/article/why-you-should-never-use-hodrick-prescott-filter

# Data : GDP Current (US$) Italy (1960-2020)
## Link : https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?locations=IT

# Data : GDP EU Countries (1975-2020 Quarterly)
## Link : https://ec.europa.eu/eurostat/databrowser/view/NAMQ_10_GDP__custom_2427223/default/table?lang=en
### Explanatory Texts: https://ec.europa.eu/eurostat/cache/metadata/en/namq_10_esms.htm

#-------------------------------------------------------------------------------#
setwd("~/Desktop/Project MACRO/Data")
rm(list = ls())

#Importing Libraries
library(dplyr)
library(tidyr)
library(mFilter) #HP Filter
library(forecast)
library(ggplot2)
library(seasonal) #Seasonality decomposition for TS

#Importing dataset and removing useless columns\rows
data = read.table(file="dataset.csv", sep=';', header=TRUE)
colnames(data)

#Selecting only GDP for FR, DE, IT
data_interest = data[,c(1, 8, 14, 18)]
it = ts(data_interest$IT, start=c(1995,1), frequency=4)
fr = ts(data_interest$FR, start=c(1995,1), frequency=4)
de = ts(data_interest$DE, start=c(1995,1), frequency=4)
data = cbind(it, fr, de)
head(data)
tail(data)
plot(data, main="GDP for DE; FR; IT (Levels)")

#Decompose our time series objects
it_d = decompose(data[,1], type='additive')
plot(it_d)
fr_d = decompose(data[,2], type='additive')
plot(fr_d)
de_d = decompose(data[,3], type='additive')
plot(de_d)

#Decompose our time series objects using "seasonal" X11 object
it_d1 = seas(x=data[,1], x11="")
autoplot(it_d1)
fr_d1 = seas(x=data[,2], x11="")
autoplot(fr_d1)
de_d1 = seas(x=data[,3], x11="")
autoplot(de_d1)

#Remove dats, taking natural logs
lndata_i = log(data)
colnames(lndata_i) = c("ITLN", "FRLN", "DELN")
head(lndata_i)
tail(lndata_i)

######## ITALY ########
#Plot IT data side by side
par(mfrow=c(1,2))
plot(data[,1], xlab="", ylab="", main="IT GDP (levels)")
plot(lndata_i[, 1], xlab="", ylab="", main="IT GDP(logs)")
par(mfrow=c(1,1))

#Apply HP filter to separate the cycle component from the trend one (HP-filter)
it_fil = hpfilter(lndata_i[, 1])
head(it_fil)
head(it_fil$cycle)

#Plot: just the cycle and data without trend
#Include zero line for cycle: change colors and fonts.
par(mfrow=c(1,2))
plot(it_fil$cycle, xlab="", ylab="", main="IT cycle", lwd=3)
abline(h=0, lty=3, col="dark gray", lwd=5)
par(new=FALSE)
plot(it_fil$trend, xlab="", ylab="", main="IT trend+cycle", ylim=c(12.3,13.0), lwd=3, col="dark grey")
par(new=TRUE)
plot(lndata_i[,1], xlab="", ylab="", ylim=c(12.3,13.0), lwd=2)
par(mfrow=c(1,1))

######## GERMANY ########
#Apply HP filter to separate the cycle component from the trend one (HP-filter)
de_fil = hpfilter(lndata_i[, 3])
head(de_fil$cycle)

######## FRANCE ########
#Apply HP filter to separate the cycle component from the trend one (HP-filter)
fr_fil = hpfilter(lndata_i[, 2])
head(fr_fil$cycle)

#Make a cycle dataframe
cycles = cbind(it_fil$cycle, de_fil$cycle)
autoplot(cycles, main="IT and DE cycles")

#Plot together
par(new=FALSE)
plot(cycles[,1], lwd=2, lty=1, col="black", ylim=c(-1,1), xlab="", ylab="", main="Business Cycles")
par(new=TRUE)
plot(cycles[,2], lwd=2, lty=2, col="#505050", ylim=c(-1,1), xlab="", ylab="")
#par(new=TRUE)
#plot(cycles[,3], lwd=2, lty=2, col="blue", ylim=c(-1,1), xlab="", ylab="")
abline(h=0,lty=2,col="dark grey")
legend("topright",legend=c("IT","DE"),lty=c(1,4),lwd=c(2,2),col=c("black","#505050"))
#legend("topright",legend=c("IT","DE", "FR"),lty=c(1,4),lwd=c(2,2),col=c("black","#505050", "blue"))

#test for correlations between business cycles
#Will use cross-correlation functions (ccfs)
#first: leads and lag
it = as.numeric(cycles[,1])
ll1<-cbind(it,lag(it, 1))
colnames(ll1) = c("IT", "LAG_1_IT")
head(ll1)

#Now for Germany
de = as.numeric(cycles[,2])
ll2 = cbind(ll1, de)
colnames(ll2) = c("IT", "IT_L1", "DE")
head(ll2)

#Any correlation between DE GDP and lagged IT GDP captured by correlationn between Column 2 and 3
#Germany NOW asssociated with IT Last Quarter
#Germany lags the IT
#IT leads DE

#Calculate Correlation between DE and lagged IT manually
cor(na.omit(ll2[,2:3]))
round(cor(na.omit(ll2[,2:3]))[2,1],3)
#remember this value so you dont get leads and lags backward! (0.412)


#Now do CCFs using ccf(), with 4 leads and lags.
#IT is x, DE is y (4 LEADS AND 4 LAGS)
ccf(x=cycles[,1], y=cycles[,2], lag.max=4)
ccf = ccf(x=cycles[,1], y=cycles[,2], lag.max=4)
ccf$acf
#Note it is centered at 5 (lags), and the top of the list is "X leads Y".
# Also, slight difference in number

#Make a nice x-axis
xaxis = c(-4:4)
xaxis
ccftable = cbind(xaxis, ccf$acf)
colnames(ccftable) = c("Lag", "Correlation")
ccftable

#Plot your own CCF graph
#Include 5% significance calue with 100 d.f.
plot(ccftable, pch=20, xlab = "lead/lag", ylab="Correlation", ylim=c(0,1), main="Cross-correlation , IT and DE GDP")
abline(h=0.195, lty=3, col="dark grey")
lines(ccftable)

#Write to a hi-res .jpg
jpeg("ITCycles.jpg", height = 3, width = 5, units="in", res=300)
par(mfrow=c(1,2))
par(mar=c(3,3,3,3))
par(cex.axis=.5)
plot(it_fil$cycle, xlab = "", ylab="", main="Cycle", lwd=1, ylim=c(-.1,.1))
abline(h=0, lty=2, col="dark gray", lwd=2)
par(new=FALSE)
plot(it_fil$trend, xlab= "", ylab= "", main="trend + cycle", ylim=c(11,14), lwd=1.5, col="dark grey", cex=1)
par(new=TRUE)
plot(lndata_i[,1], xlab="", ylab="", ylim=c(11,13), lwd=2, cex=1, lty=6)
par(mfrow=c(1,1))
dev.off()

jpeg("ITDECycles.jpg",height = 3.5,width=5,units="in",res = 300)
par(new=FALSE)
plot(cycles[,1],lwd=2,lty=1,col="black",ylim=c(-.1,.1),xlab="",ylab="",main="Business Cycles")
par(new=TRUE)
plot(cycles[,2],lwd=2,lty=4,col="#505050",ylim=c(-.1,.1),xlab="",ylab="")
abline(h=0,lty=2,col="dark grey")
legend("topright",legend=c("IT","DE"),lty=c(1,4),lwd=c(2,2),col=c("black","#505050"))
dev.off()

jpeg("CCFgraph.jpg",height = 3,width=5,units="in",res = 300)
plot(ccftable,pch=20,xlab="lead/lag",ylab="Correlation",ylim=c(0,1),main="Cross-correlations, IT and DE GDP")
abline(h=0.195,lty=3,col="dark grey",lwd=2)
lines(ccftable)
dev.off()

remove(ll1,ll2,xaxis)