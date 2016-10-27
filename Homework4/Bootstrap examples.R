#Sample of minutes per mile pace time from MapMyRun
P<-c(13.39, 10.3, 9.31, 10.16, 11.42, 10.56, 11.53, 10.17, 11.04, 10.42, 10.49, 10.57)
mean(P) #mean of the original sample, now being used as the population
sd(P) #Standard Deviation of the sample now being used as the population
#Histogram and QQPlot of the Population
hist(P)
qqnorm(P)
qqline(P)
s<-1000
#Bootstrap 1 example of means using 12 samples from the original population P vector
Bootstrap1<-numeric(s)
for (i in 1:s) {
  bootsample<-sample(P,size=length(P),replace=T)
  Bootstrap1[i]<-mean(bootsample)}
#Mean and Standard Deviation of Bootstrap 1 example with 12 sample size
mean(Bootstrap1)
sd(Bootstrap1)
#Histogram and QQPlot of the Bootstrap 1 example with 12 sample size
hist(Bootstrap1)
qqnorm(Bootstrap1)
qqline(Bootstrap1)
#Bootstrap 2 example of means using 6 samples from the original population P vector
Bootstrap2<-numeric(s)
for (i in 1:s) {
  bootsample<-sample(P,size=6,replace=T)
  Bootstrap2[i]<-mean(bootsample)}
#Mean and Standard Deviation of Bootstrap 1 example with 6 sample size
mean(Bootstrap2)
sd(Bootstrap2)
#Histogram and QQPlot of the Bootstrap 1 example with 6 sample size
hist(Bootstrap2)
qqnorm(Bootstrap2)
qqline(Bootstrap2)
