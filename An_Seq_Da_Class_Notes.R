#Week 1

#a time series basis of year
y <- ts(c(123,122,121,120), start = 1996)

#a time series basis of months, quarters
y <- ts(c(123,122,121,120), frequency = 12, start = c(1996,9))
y <- ts(c(123,122,121,120), frequency = 4, start = c(1996,9))
y

#library(fpp2) ornek datasetler
?ausgdp #quarterly data
ausgdp
autoplot(ausgdp) #quarterly time series, no seasonality, long term increasing trend

?melsyd
melsyd #weekly data, contains 3 time series
autoplot(melsyd)
autoplot(melsyd[,'First.Class']) #get a time series 

#plotting
?a10 #monthly, overly year clearly increasing trend, every year getting larger, might due to the policies, populations etc., strong seasonality in yearly pattern
autoplot(a10) + ylab() + xlab + ggtitle()

#visualize seasonality against months individually to see seasonal pattern clearly
ggseasonplot(a10, year.labels = TRUE, year.labels.left = TRUE) + ylab("asd") + ggtitle("Seasonal Plot")
ggsubseriesplot(a10) + ylab("") + ggtitle("") #blue line is the median, from the data can be seen that increasing over the years                                                                         


?ausbeer #quarterly data
beer <- window(ausbeer, start = 1992) #window function, used to take ts slice by adding starting point
autoplot(beer) #seasonality, going regularly up and down, no trend basically horizontally
ggseasonplot(beer) #strong seasonal behaviour, beer consumption is more in summer (australia, summer q4)
ggsubseriesplot(beer) #consumption going up and down, ts oscilating around median

?arrivals #quarterly data 
autoplot(arrivals)
arr <- window(arrivals, start = 2000)
autoplot(arr[,"Japan"]) #decreasing trend over the year, anomaly is the Japan, goes up and down
ggseasonplot(arr[,"Japan"]) #in q2 lowest
autoplot(arr[,"UK"]) #no increasing or decreasing, but seasonality
ggseasonplot(arr[,"UK"]) #in q2 lowest
autoplot(arr[,"US"]) #seasonality
ggseasonplot(arr[,"US"]) #in q2 lowest
autoplot(arr[,"NZ"]) #increasing trend, seasonality
ggseasonplot(arr[,"NZ"]) #in q1 lowest

#trend: long term increase or decrease in data 
#seasonal: pattern exists when series influenced by seasonal factors
#cyclic: pattern when data exhibit rises or falls that are not fixed period (at least 2 years)
             
?elec #monthly electric production
autoplot(window(elec, start = 1980)) #seasonality, increasing trend

?lynx
autoplot(lynx) #has some pattern, sequence of highes and lowes, each peak and each low has big different, which is a cyclic time series

#autocorrelation measures linear relation between lagged values of a time series y 
#when yt and yt-k acts the same, covariance increases
#if you have quarterly seasonsal time series, its expected to have high autocorr at lagged 4

#autocorrelation rk, rk = ck / c0
#autocovariance ck,  ck = 1/T sum((yt - ymean)(yt-k - ymean))
#var(y) c0

#correlogram
ggAcf(beer) #seasonal, q2 min(below the average), q4 max(above the average), every year same like this
#peaks 4 lag apart, troughs 2 lag apart

autoplot(goog) #shows trend, value of time t, close to time t+1, no seasonality
ggAcf(goog, lag.max =100) #correlation is strong, decades quite slowly, very trendy

autoplot(window(elec, start = 1980)) #shows a trend and seasonality, monthly
ggAcf(window(elec, start = 1980)) #peaks are higher at frequency 12~seasonality, large autocorelation due to trend

gglagplot(beer) #“gglagplot” will plot time series against lagged versions of themselves. 


autoplot(hsales) #seasonality, monthly; no trend; cyclic
ggAcf(hsales, lag.max = 100) #peaks are high at 0, 12, 24.... low at 6, 6k; no trend so ACF is considerably small

autoplot(usdeaths) #seasonality, monthly; no trend
ggAcf(usdeaths) #peaks are high at 12k, low at 6(2k-1); no trend, low ACf

autoplot(bricksq) #seasonality, increasing trend
ggAcf(bricksq) #high and positive ACF, quarterly seasonality so 2(2k-1) lowest, 2k highest peak

?sunspotarea #daily
autoplot(sunspotarea) #daily, seasonal, no trend
ggAcf(sunspotarea, lag.max = 1000) #small ACF, ??

?gasoline #weekly data
autoplot(gasoline) #seasonality, increasing trend
ggAcf(gasoline) #lags at 52k the highest peak, since no trend, high and positive ACF values

#white noise, N(0,1/T)
wn <- ts(rnorm(36))
autoplot(wn)
ggAcf(wn)

#difference, to observe diff random walk or not
diff(goog)
autoplot(diff(goog)) #if distributed around 0 and horizontal then random walk
ggAcf(diff(goog)) #inside of the blue bars 


#Week2

#Simple Forecasting Methods
#Mean
autoplot(meanf(usdeaths, h=20))
#Naive ~random walk
autoplot(naive(usdeaths, h=20))
#Seasonal Naive
autoplot(snaive(usdeaths, h=20))
#Drift
autoplot(rwf(usdeaths, drift = TRUE, h=20))

goog
auscafe

autoplot(auscafe) #increasing trend and monthly seasonal
autoplot(meanf(goog,h=20))         
autoplot(naive(goog,h=20))        
autoplot(snaive(goog,h=20))        
autoplot(rwf(goog, drift = TRUE, h=20))      

autoplot(meanf(auscafe,h=20))         
autoplot(naive(auscafe,h=20))        
autoplot(snaive(auscafe,h=20))        
autoplot(rwf(auscafe, drift = TRUE, h=20))      

#residuals
res <- residuals(naive(goog200))
autoplot(res) #one outlier but white noise (no trend, unpredictable, randomly drifting around zero)
ggAcf(res) #clearly WN since autocorrelation value so small, within the blue bar

#BoxTest for residuals
Box.test(residuals(naive(goog200)), lag=10, type = 'Lj') #lag = 10 since nonseasonal data


#Check Residuals öncesi forecasting yap, bigger p value is better on residuals
checkresiduals(naive(goog200))

checkresiduals(rwf(goog200, drift = TRUE)) #residuals RW, uncorrelated, ACF also looks uncorrelated, the distribution is gaussian dist. p value is 0.2736, you can prefer, but still cloes to wn


beer <- window(ausbeer, start = 1992)
checkresiduals(beer)

beer <- window(ausbeer, start = 1992)
fc <- snaive(beer)
autoplot(fc)
checkresiduals(fc) #quarterly, model of seasonality not right,has a trend we ignroe

autoplot(snaive(beer))
autoplot(fitted(snaive(beer)))
autoplot(beer) + autolayer(fitted(snaive(beer)))


#measures of forecast accuracy accuracy(data, forecasted data w lag)
#forecast error e = observed value - forecast value 
#mae = mean(error)
#mse = mean(error^2)
#mape = 100*mean(e/y)
#rmse = sqrt(mse)

beer #starts at 1992,1 ends 2010,2, use 2007,4
beer1 <- window(beer, start = c(2007,4))
fc <- snaive()

beer2 <- window(ausbeer, start = 1992)
fc <- snaive(beer2)
autoplot(fc) #seasonal
checkresiduals(fc) #8.336e-05

#after you decide on forecasting by comparising pvalues, autolayer the fitted vals
#residuals checks the train data,
#to measure the model, check on out of data, test data
#accuracy indicators are correlated

beerX <- window(ausbeer, start=1992, end=c(2007,4))
beerY <- window(ausbeer, start = 2008)
beerfit1 <- meanf(beerX, h=10)
beerfit2 <- rwf(beerX, h=10)
beerfit3 <- snaive(beerX, h=10)
accuracy(beerfit1, beerY)
accuracy(beerfit2, beerY)
accuracy(beerfit3, beerY)

#lowest value is the best.
#to compare different time series, mape should be used since it is scale independent.
#when y becomes closer zero, mape is not reliable
#mae more interpretable than mse since it is absolute val of error

#mase, scale independent, calculates the performance over naive/snaive model

#cross validation + mse
e <- tsCV(goog200, rwf, drift = TRUE, h=1)
sqrt(mean(e^2, na.rm = TRUE)) #mse on 1 step ahead test data
sqrt(mean(residuals(rwf(goog200, drift=TRUE))^2, na.rm=TRUE)) #mse on residual of train data

#residual = observed - prediction
#variance = sum(residual^2)/n-1

#Prediction interval calculations
#simple forecasting methods
#mean: std = std * sqrt(1+1/T)
#naive: std = std * sqrt(h)
#snaive: std = std * sqrt(k+1)
#drift: std = std * sqrt(h(1+h)/T)


#simple exponential smoothing
#the weight of the past values decreasing, the latest ones weight is the highest

oildata <- window(oil, start=1996)
fc <- ses(oildata, h=5) #simple exp smoothing, gives forecasting
#gives alpha, l initals, 
#calculates sigma, AIC, and error measures 
summary(fc) 
#plotting forecast with train data
autoplot(fc)
#forecast is stable but the intervals are increasing

#autolayer is the estimated y values by using l and alpha 
autoplot(fc) + autolayer(fitted(fc), series = "fitted")


#holts linear trend
window(ausair, start=1990, end=2004) %>%
  holt(h=5, PI =FALSE) %>%
  autoplot()

#damped
window(ausair, start = 1990, end = 2004) %>%
  holt(damped = TRUE, h=5, PI = FALSE) %>%
  autoplot()

livestock2 <- window(livestock, start=1970, end=2000)
fit1 <- ses(livestock2)
fit2 <- holt(livestock2)
fit3 <- holt(livestock2, damped=TRUE)
accuracy(fit1, livestock)
accuracy(fit2, livestock) #lowest mape, and the other indicators
accuracy(fit3, livestock)
autoplot(livestock) #linear increasing trend
autoplot(fit1)
autoplot(livestock2) + autolayer(fitted(fit1))
autoplot(livestock2) + autolayer(fitted(fit2))
autoplot(livestock2) + autolayer(fitted(fit3))

eggs
autoplot(eggs) #decreasing trend linear
eggs2 <- window(eggs, start=1900, end=1990)
fit1 <- ses(eggs2, h=3)
fit2 <- holt(eggs2, h=3)
fit3 <- holt(eggs2, damped=TRUE, h=3)
accuracy(fit1, eggs)
accuracy(fit2, eggs)
accuracy(fit3, eggs)

checkresiduals(fit1) 
checkresiduals(fit2) 
checkresiduals(fit3) 

#holt-winters method(w seasonality)
library(fpp2)
austourists

autoplot(austourists) #increasing trend, seasonality, multiplicative
aust <- window(austourists, start=2005)

aust2 <- hw(aust, seasonal = "multiplicative")
aust2
autoplot(aust) + autolayer(aust2)

checkresiduals(hw(aust, seasonal = "multiplicative"))
checkresiduals(hw(aust, seasonal = "additive"))

gas
autoplot(gas) #increasing linear trend, multiplicative seasonality, monthly trend
gasX <- window(gas, start = 1956, end = 1992)
gas1 <- hw(gasX, seasonal = "additive")
gas2 <- hw(gasX, seasonal = "multiplicative")
gas3 <- hw(gasX, damped=TRUE, seasonal = "multiplicative")
accuracy(gas1, gas) #6,1857
accuracy(gas2, gas) #5,639
accuracy(gas3, gas) #5,070

checkresiduals(gas1)
checkresiduals(gas2)
checkresiduals(gas3)

(N,N) #ses(data)
(A,N) #holt(data)
(Ad,N) #holt(data, damped=TRUE)
(A,A) #hw(data, seasonal = "additive")
(A,M) #hw(data, seasonal = "multiplicative")
(Ad,M) #hw(data, damped=TRUE, seasonal = "multiplicative")

#use ETS to decide (Error, Trend, Seasonality)
#can not do (A, ..., M)

aust <- window(austourists, start =2005)
autoplot(aust) #linear trend, multiplicative seasonality
fit <- ets(aust)
summary(fit) #(M,A,M)
autoplot(fit)

checkresiduals(fit)


bicoal
autoplot(bicoal)
fit <- ets(bicoal)
summary(fit) #(M,A,M)
autoplot(fit)


autoplot(bicoal)
checkresiduals(bicoal)
ets(bicoal) #ETS(M,N,N), no trend, no seasonality

autoplot(chicken) ##MNN
checkresiduals(chicken)
ets(chicken)
autoplot(ets(chicken))
chicken2 <- window(chicken, start = 1960)
ets(chicken2)
autoplot(ets(chicken2)) #AAdN

autoplot(dole)
ets(dole) #MAdM
autoplot(ets(dole))

autoplot(usdeaths) #seasonality, no trend
checkresiduals(usdeaths)
ets(usdeaths)
autoplot(ets(usdeaths)) #ANA


autoplot(bricksq)
checkresiduals(bricksq) #trend, seasonality
ets(bricksq) #MAM, multiplicative
autoplot(ets(bricksq))
summary(ets(bricksq))

autoplot(lynx) #seasonality, no trend
checkresiduals(lynx) 
ets(lynx) #MNN
autoplot(ets(lynx))
summary(ets(lynx))

autoplot(ibmclose)
checkresiduals(ibmclose) #trend, no seasonality
ets(ibmclose) #ANN 
autoplot(ets(ibmclose))
#summary gives indicators, aic 
summary(ets(ibmclose))


autoplot(austourists) #increasing trend, seasonality, m
aust <- window(austourists, start = 2005)
fit1 <- hw(aust, seasonal = "additive")
fit2 <- hw(aust, seasonal = "multiplicative")
summary(fit1) #aic 234, mape 2,97
summary(fit2) #aic 221, mape 2,70

autoplot(h02)
ets(h02) #MAdM, multiplicative error, multiplicative seasonality and damped linear trend

autoplot(eggs)
ets(eggs) #MAdM, with aic -122
fit_eggs_trend <- ets(eggs, model = 'ANN') #with aic 1049
autoplot(fit_eggs_trend)
fc_Trend <- forecast(fit_eggs_trend)
autoplot(fc_Trend)


autoplot(bicoal)
fit1 <- ets(bicoal) #MNN
autoplot(fit1)
accuracy(fit1)
autoplot(forecast(fit1))

#Arima models
#make the data stationary before arima by differencing

?usmelec #monthly data
autoplot(usmelec) #increasing trend, seasonality monthly
log(usmelec)
autoplot(log(usmelec))
autoplot(diff(log(usmelec), lag = 12))
autoplot(diff(diff(log(usmelec), lag=12)))

#log stabilize the variance by applying log, 
#first log, then diff
usmelec %>% log() %>% diff(lag=12) %>% autoplot()  


autoplot(goog200) 
ndiffs(goog200) #1, take difference
nsdiffs(goog200) #non seasonal data, no need to take differencing with lags
autoplot(diff(goog200))
ndiffs(diff(goog200)) #0, no need to take any differencing more

autoplot(usdeaths) #seasonal
ndiffs(usdeaths)
nsdiffs(usdeaths) #since seasonal, take differencing w lags
autoplot(diff(usdeaths, lag = 12))
ndiffs(diff(usdeaths, lag = 12)) #still not RW, take another diff

autoplot(diff(diff(usdeaths, lag = 12)))
nsdiffs(diff(diff(usdeaths, lag = 12))) #now stop

autoplot(eggs)
nsdiffs(eggs) #not seasonal
ndiffs(eggs)
autoplot(diff(eggs))
ndiffs(diff(eggs))

autoplot(lynx)
nsdiffs(lynx) 
ndiffs(lynx) #can not be stationary

autoplot(ausbeer) #seasonal data, can be seen
frequency(ausbeer) #quarterly
nsdiffs(ausbeer)
diff(ausbeer, lag=4)
autoplot(diff(ausbeer, lag = 4))  
nsdiffs(diff(ausbeer, lag = 4)) #0

autoplot(guinearice)
autoplot(diff(guinearice))
ndiffs(diff(guinearice))

fit <- auto.arima(guinearice) #arima(0,1,0) rw w drift
autoplot(forecast(fit)) #straight line w confidence interval, larger w time

?internet
autoplot(internet)
ndiffs(internet)
ndiffs(diff(internet))
autoplot(diff(internet))
autoplot(diff(diff(internet)))
fit <- auto.arima(diff(diff(internet)))
fit #arima(1,0,1) w zero mean 
autoplot(fit)
checkresiduals(fit)


