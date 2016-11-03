#load necessary R libraries for data clean up and analysis
library(repmis)
library(RCurl)
#Set the working directory and check it to make sure it is right.
getwd()
setwd("C:/Users/anna_frowein/Documents/SMU/Doing Data Science/CaseStudy1")
getwd()
#Downloand the raw data sets into the working directory from the websites and check to see that they are in the working directory.
siteGDP<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
siteEDU<-"https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(siteGDP,destfile="./GDP_Raw.csv")
download.file(siteEDU,destfile="./Education_Raw.csv")
list.files()
#Load data files into R and review their headers and data
GDP_Raw<-read.csv("./GDP_Raw.csv",header=TRUE)
Education_Raw<-read.csv("./Education_Raw.csv",header=TRUE)
head(GDP_Raw)
head(Education_Raw)
summary(GDP_Raw)
View(Education_Raw)
View(GDP_Raw)
#Cleaning the data files
#copying files for manipulation to ensure raw data stays entact
file.copy("./Education_Raw.csv","./Education_Clean.csv")
file.copy("./GDP_Raw.csv","./GDP_Clean.csv")
#Create Clean Data files removing unwated columns and rows as well as applying common headers for merging
GDP_Clean<-read.csv("./GDP_Clean.csv",header=TRUE)
GDP_Clean<-GDP_Clean[-c(1:4,195,220,222,236:330),-c(3,6:10)]
setnames(GDP_Clean,1,"shortcode")
setnames(GDP_Clean,2,"GDPRanking")
setnames(GDP_Clean,3,"Economy")
setnames(GDP_Clean,4, "GDPinUSMill")
GDP_Clean$GDPinUSMill <- as.numeric(gsub("[^[:digit:]]",0, GDP_Clean$GDPinUSMill))
GDP_Clean$GDPRanking <- as.numeric(gsub("[^[:digit:]]",0, GDP_Clean$GDPRanking))
Education_Clean<-read.csv("./Education_Clean.csv",header=TRUE)
Education_Clean<-Education_Clean[,-c(4:31)]
setnames(Education_Clean,"CountryCode","shortcode")
View(Education_Clean)
View(GDP_Clean)
#merge data sets
MergeGDP<-merge(Education_Clean,GDP_Clean,by="shortcode")
nrow(MergeGDP)
#sort by GDP with USA being at the end, and give the Country listed 13th.
SortedMergeGDP<-MergeGDP[order(MergeGDP$GDPRanking,na.last = FALSE,decreasing = TRUE),]
View(SortedMergeGDP)
SortedMergeGDP[13,"Long.Name"]
#Average GDP Rankings by Income Group for "High income:OECD" and "High income:nonOECD"
aggregate(MergeGDP$GDPRanking, list(MergeGDP$Income.Group), mean, trim=0, na.rm=TRUE)
#Plot GDP for all Countries
library(ggplot2)
PlotGDP<-ggplot(MergeGDP,aes(shortcode,GDPinUSMill))
PlotGDP+geom_point(aes(color=factor(Income.Group)))
#Create 5 Quantile Groups
library(gtools)
Quant<-MergeGDP[quantcut(MergeGDP$GDPRanking,q=5,na.rm=TRUE)]
QuantMergeGDP<-mutate(MergeGDP,Quantile=quantcut(MergeGDP$GDPRanking,q=5,na.rm=TRUE))
View(QuantMergeGDP)
QITable<-table(QuantMergeGDP$Quantile,QuantMergeGDP$Income.Group)
QITable
QITable["[1,38.6]","Lower middle income"]
