#download required library and the data set
install.packages('fpp')
library(fpp)
data(ukcars)
#Plot the time series. Try to indentify seasonal flucuations and/or a trend.
plot(ukcars)
#b)	Use a classical decomposition to calculate the trend-cycle and seasonal indices
fitd <- decompose(ukcars) 
plot(fitd)
#Do the results support the graphical interpretation from findings made prior?
#Compute and plot the seasonally adjusted data. 
eeadj <- seasadj(fitd)
plot(eeadj)
#Change one observation to be an outlier (e.g., add 500 to one observation), and recompute the seasonally adjusted data. What is the effect of the outlier? 
ukcars2 <-ts(c(ukcars[1:54],ukcars[55]+500,ukcars[56:121] ),start=c(1977,1),frequency=12) 
plot(ukcars2)
eeadj2<-seasadj(decompose(ukcars2))
plot(eeadj2)
# Does it make any difference if the outlier is near the end rather than in the middle of the time series? 
ukcars3 <-ts(c(ukcars[1:119],ukcars[120]+500,ukcars[121] ),start=c(1977,1),frequency=12) 
plot(ukcars3)
eeadj3<-seasadj(decompose(ukcars3))
plot(eeadj3)
#Use STL to decompose the series
fit <- stl(ukcars, s.window=5) 
plot(fit)
plot(ukcars, col="gray",
     main="UK Cars",
     ylab="Car", xlab="")
lines(fit$time.series[,2],col="red",ylab="Trend")
