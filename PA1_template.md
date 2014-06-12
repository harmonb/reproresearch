Reproducible Research Peer Assignment 1
=========================================

Load the data and calculate daily steps taken

```r
activity<-read.csv('activity.csv')
dailysteps<-tapply(activity$steps,activity$date,sum)
```

Make a histogram of the total number of steps taken per day

```r
hist(dailysteps, xlab='Steps')
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Calculate the mean and median total steps taken per day


```r
meansteps<-mean(dailysteps, na.rm=T)
mediansteps<-median(dailysteps, na.rm=T)
meansteps
```

```
## [1] 10766
```

```r
mediansteps
```

```
## [1] 10765
```

Calculate and plot the average number of steps taken by time interval over all days

```r
avgsteps<-tapply(activity$steps, activity$interval, mean, na.rm=T)
intervals<-unique(activity$interval)
intervalsteps<-cbind(intervals,avgsteps)
plot(avgsteps, type='l', xlab='Intervals', ylab='Mean of Steps')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Which interval has the maximum number of steps (on average) across all days?

```r
maxsteps<-c(0,0)
for (a in 1:nrow(intervalsteps)){
  if (avgsteps[a]>maxsteps[2]){
    maxsteps[1]<-intervals[a]
    maxsteps[2]<-avgsteps[a]
  }  
}
as.integer(maxsteps[1])
```

```
## [1] 835
```

Determine the number of NA's in the data

```r
x<-is.na(activity)
xna<-activity[x]
length(xna)
```

```
## [1] 2304
```

Fill in missing values with median for the given time interval

```r
activitynona<-activity
stepsfiller<-tapply(activitynona$steps, activitynona$interval, median, na.rm=T)
stepsfiller<-cbind(intervals,stepsfiller)
for (a in 1:nrow(activitynona)){
  if (is.na(activitynona$steps[a])){
    missinginterval=as.numeric(activitynona$interval[a])
    for (b in 1:nrow(stepsfiller)){
      if (missinginterval==stepsfiller[b,1]){
        activitynona$steps[a]=stepsfiller[b,2]
      }
    }
  }
}
```

Recalculate and plot number of steps taken daily

```r
dailystepsnona<-tapply(activitynona$steps,activitynona$date,sum)
hist(dailystepsnona, xlab='Steps',main="Daily steps with NA's replaced")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

Recalculate the mean and median total steps taken per day


```r
meanstepsnona<-mean(dailystepsnona, na.rm=T)
medianstepsnona<-median(dailystepsnona, na.rm=T)
meanstepsnona
```

```
## [1] 9504
```

```r
medianstepsnona
```

```
## [1] 10395
```

Create a factor variable for weekdays and weekends and add it to dataset

```r
dates<-as.POSIXct(activitynona$date)
days<-weekdays(dates, abbreviate=T)
for (a in 1:length(days)){
  if (days[a]=="Mon" | days[a]=="Tue" | days[a]=='Wed' | days[a]=='Thu' |
      days[a]=='Fri'){
    days[a]="Weekday"
  }else{
    days[a]='Weekend'
  }
}
activitynona$day<-factor(days)
```

Create a panel plot showing the different mean steps by interval over the weekdays and weekends

```r
intervaldays<-tapply(activitynona$steps, activitynona[,3:4], mean)
intervaldays<-cbind(intervals,intervaldays)
par(mfrow=c(2,1))
  plot(intervaldays[,1], intervaldays[,2], type='l', xlab='Intervals',
       ylab='Mean Steps', main='Weekdays')
  plot(intervaldays[,1], intervaldays[,3], type='l', xlab='Intervals',
       ylab='Mean Steps', main='Weekends')
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11.png) 
