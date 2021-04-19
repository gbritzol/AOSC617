if (!require("xlsx")) install.packages("xlsx")
if (!require("xts")) install.packages("xts")
if (!require("seastests")) install.packages("seastests")
if (!require("forecast")) install.packages("forecast")

library(xlsx)
library(xts)
library(seastests)
library(forecast)

mydata <- read.xlsx(	"/homes/metogra/gbritzol/AOSC617Project/TimeSeriesAnalysis/polar-cap-temp-trend.xlsx",
			"T2m 70-90N trend",
			header=FALSE,
			rowIndex = 1:42, colIndex = 2:13)
amydata <- t(mydata)
amydata <- array(amydata)
amydata <- matrix(amydata, ncol=1)
myts <- ts(amydata,start=c(1980,1),end=c(2020,12),frequency=12)


#Analyse the time series and detact if it is seasonal i.e Find the appropriate ARIMA model or if seasonal SARIMA model
mod1 <- auto.arima(myts, seasonal=isSeasonal(myts))
summary(mod1)
isSeasonal(myts)

#Calculate the Sample - Autocorrelation Function / Sample - Partial Autocorrelation function
cr  <- acf(myts, lag=200)
pcr <- pacf(myts, lag=200)

pdf("figure1.pdf")
plot(cr)
invisible(dev.off())

pdf("figure2.pdf")
plot(pcr)
invisible(dev.off())

pdf("figure3.pdf")
plot(forecast(mod1, h=12))
invisible(dev.off())