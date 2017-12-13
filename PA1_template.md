---
title: "Activity Monitor Project"
author: "ChaoLi"
date: "12/13/2017"
output: html_document
---

##Loading and preprocessing the data

1. Load the data
```{r}
activity<-read.csv("activity.csv")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis
```{r}
activity_noNa<-na.omit(activity)
activity_noNa[,2]<-as.Date(activity_noNa[,2])
```

##What is mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day
```{r}
sumofsteps<-tapply(activity_noNa$steps,activity_noNa$date,sum)
hist(sumofsteps,xlab='steps per day',main = "histogram of steps per day")
```

2. Calculate and report the mean and median total number of steps taken per day
```{r}
median(sumofsteps)
mean(sumofsteps)
```
The median total number of steps taken per day is 10765 and the mean total number of steps taken per day is 10766.19.

##What is the average daily activity pattern?

1.Make a time series plot (i.e. type='l') of the 5-miniute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r}
library(data.table)
interval5<-unique(activity_noNa$interval)
mean_steps<-data.table(tapply(activity_noNa$steps,activity_noNa$interval,mean))
avestep<-cbind(interval5,mean_steps)
colnames(avestep)<-c("interval5","mean_steps")
plot(avestep$interval5,avestep$mean_steps,type = "l",xlab="time interval",ylab="average steps")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r}
maxtimeinterval<-avestep$interval5[which.max(avestep$mean_steps)]
```
According to calculation, the maximum number of steps happens at 835.


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset
```{r}
Nas<-sum(is.na(activity$steps))
```
Based on the output, there are 2304 rows which contain missing values.

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. The strategy I chose is to use the mean value for that 5-miniute interval to fill the NULL value
```{r}
newdataset<- activity
for (i in 1:nrow(activity)) {
    if (is.na(newdataset$steps[i])==TRUE) {
        newnumber<-which(newdataset$interval[i] == avestep$interval5)
        newdataset$steps[i]<-avestep[newnumber,]$mean_steps
    }
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in. (The new dataset has been created in step 2)

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r}
newdataset$date<-as.Date(newdataset$date)
sumofsteps_new<-tapply(newdataset$steps,newdataset$date,sum)
hist(sumofsteps_new)
mean(sumofsteps_new)
median(sumofsteps_new)
```

The mean and median total number of steps per day are both 10766.19. Compared to the old data, which has the same mean value, but with slightly smaller median, we can make a conclusion that by replacing all the missing values to mean values, there won't be too much changes of the data analysis.

## Are there differences in activity patterns between weekends and weekdays?

1. Use the new filled-in missing values, create a new factor variable in the dataset with two levels -- "weekdays" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r}
library(dplyr)
newdataset <- newdataset %>%
    mutate(ifweekdays= ifelse((weekdays(date)=="Sunday") |(weekdays(date)=="Saturday") , "weekends","weekdays"))
```

2. Make a panel plot containing a time series plot (i.e. type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekdays or weekend days (y-axis).
```{r}
weekdaysdata<- newdataset %>%
    filter(ifweekdays=="weekdays")
weekendaysdata<-newdataset %>%
    filter(ifweekdays=="weekends")
interval5_weekdays<-unique(weekdaysdata$interval)
avestep_weekdays<-data.table(tapply(weekdaysdata$steps,weekdaysdata$interval,mean))
ifweekdays_new<-rep("weekdays",length(interval5_weekdays))
avestep_weekdays<-cbind(interval5_weekdays,avestep_weekdays,ifweekdays_new)
colnames(avestep_weekdays)<-c("interval","average_steps","ifweekdays")

interval5_weekenddays<-unique(weekendaysdata$interval)
avestep_weekenddays<-data.table(tapply(weekendaysdata$steps,weekendaysdata$interval,mean))
ifweekenddays_new<-rep("weekend",length(interval5_weekenddays))
avestep_weekenddays<-cbind(interval5_weekenddays,avestep_weekenddays,ifweekenddays_new)
colnames(avestep_weekenddays)<-c("interval","average_steps","ifweekdays")

finaldataset<-rbind(avestep_weekdays,avestep_weekenddays)
```

For plotting data:
```{r}
library(lattice)
finaldataset$ifweekdays<-as.factor(finaldataset$ifweekdays)
xyplot(average_steps~interval | ifweekdays, finaldataset, type="l",layout=c(1,2),xlab="Interval", ylab="Number of steps")
```
