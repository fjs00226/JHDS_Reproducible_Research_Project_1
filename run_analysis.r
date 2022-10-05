# create path to save data
if(!file.exists("./raw_data")){
  dir.create("./raw_data")
}

# download, unzip and read data
url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url,"./raw_data/download.zip")
unzip(zipfile="./raw_data/download.zip",exdir="./raw_data")
data<-read.csv("./raw_data/activity.csv")
head(data)
str(data)

# convert date to Date format
data$date<-as.Date(data$date,"%Y-%m-%d")
class(data$date)

# check how many NA are in each column
steps_na<-sum(is.na(data$steps)=="TRUE") # 2304 na
date_na<-sum(is.na(data$date)=="TRUE") # 0
interval_na<-sum(is.na(data$interval)=="TRUE") # 0
print(paste("missing steps:",steps_na))
print(paste("missing date:",date_na))
print(paste("missing interval:",interval_na))

# How to deal with missing steps
# Leave the NA, don't fill it with 0. 0 will affect calculation of mean and median.

# total numbers of steps for each day
steps_day<-aggregate(data$steps, list(data$date), sum)
colnames(steps_day)<-c("date","steps")

# get histogram
hist(steps_day$steps,main="Histogram of daily steps",xlab="Daily steps",ylab="Counts")

# get mean and median
mean<-mean(steps_day$steps,na.rm=TRUE)
median<-median(steps_day$steps,na.rm=TRUE)
print(paste("The mean steps taken per day is:",mean))
print(paste("The median steps taken per day is:",median))

# time series plot of average
steps_interval<-aggregate(data$steps, list(data$interval), mean,na.rm=TRUE)
colnames(steps_interval)<-c("time_interval","mean_steps")
with(steps_interval,plot(time_interval, mean_steps,type="l",main="Average steps per time interval across the day"))
time_interval_max<-steps_interval[which.max(steps_interval$mean_steps),"time_interval"]
print(paste("The",time_interval_max,"interval has the max average steps across the day."))

# impute missing values
row_NA<-sum(complete.cases(data)=="FALSE")
print(paste("There are",row_NA,"rows with NA."))

# fill missing steps with mean of that 5 min interval
# create a new data frame
data_v1<-data
na_index<-which(is.na(data_v1$steps))

for(i in na_index){
  time_interval<-data_v1[i,"interval"]
  time_index<-which(steps_interval$time_interval==time_interval)
  data_v1[i,"steps"]<-steps_interval[time_index,"mean_steps"]
}

steps_day_v1<-aggregate(data_v1$steps, list(data_v1$date), sum)
colnames(steps_day_v1)<-c("date","steps")

# make a histogram
hist(steps_day_v1$steps,main="Histogram of daily steps",sub="missing data imputed",xlab="Daily steps",ylab="Counts")

# get mean and median
mean_v1<-mean(steps_day_v1$steps,na.rm=TRUE)
median_v1<-median(steps_day_v1$steps,na.rm=TRUE)
print(paste("The mean steps taken per day is:",mean_v1))
print(paste("The median steps taken per day is:",median_v1))

# compare mean and median of original and imputed missing data
raw<-c(mean, median)
missing_value_imputed<-c(mean_v1,median_v1)
compare<-data.frame(raw,missing_value_imputed)
rownames(compare)<-c("mean", "median")
compare

# differences between weekdays and weekends
data_v1$days<-weekdays(data_v1$date)
library(data.table)
DT<-data.table(data_v1)
DT<-DT[,Labels:="weekday"]
DT<-DT[days %in% c("Saturday","Sunday"),Labels:="weekend"]

steps_time_Wdays<-aggregate(DT$steps,list(DT$Labels,DT$interval),mean)
colnames(steps_time_Wdays)<-c("wday","time_interval","steps")

install.packages("lattice")
library(lattice)
xyplot(steps_time_Wdays$steps~steps_time_Wdays$time_interval|steps_time_Wdays$wday,
       type="l",layout=c(1,2),xlab="Time interval",ylab = "Average steps",
       main="Comparison of average steps per time interval\n between weekend and weekday")




