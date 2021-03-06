# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
library("knitr")
opts_chunk$set(echo=TRUE , warning=FALSE, message=FALSE )

temp <- tempfile()
setInternet2(use = TRUE)
download.file(url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = temp, mode ="wb" )
activityDs <-  read.csv(file = unz(temp , "activity.csv"), sep = ',' , colClasses = c("integer", "Date", "integer"))
unlink(temp)

## The above code run well in window 8 , if you encounter error , you may download the zip file manually  , enable the code below , set the working directory according to the .zip file location and set the setInternet2.

##setwd("E:/myGitFolder/m5_Coursera/RepData_PeerAssessment1")
##activityDs <- read.csv(file = 'activity.csv' , sep = ',' , colClasses = c("integer", "Date", "integer"))
```

## What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day


```r
activityWithoutNA <- subset(x = activityDs, !is.na(activityDs$steps))
totalNoOfStepsByDate <- aggregate(steps~ date , data =  activityWithoutNA, FUN = sum)
```


```r
head(totalNoOfStepsByDate)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
2. Make a histogram of the total number of steps taken each day


```r
library(ggplot2)
ggplot(totalNoOfStepsByDate, aes(steps)) + geom_histogram(aes(fill = ..count..)) + labs(title= 'Total number of steps taken each day' , x = 'Steps' , y = "Count" ) 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day


```r
meanTotalStep <-  mean(totalNoOfStepsByDate$steps, na.rm = T)
medianTotalStep <- median(totalNoOfStepsByDate$steps, na.rm = T)
```

*Mean of the total number of steps taken per day : 10766.19*

*Median of the total number of steps taken per day : 10765.00*

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
avgIntervalStep <- aggregate( steps ~ interval , data = activityWithoutNA, FUN = mean)

plot(avgIntervalStep$interval , avgIntervalStep$steps , type = 'l', main="Time series plot of the 5-minute interval", xlab="5-min Interval", ylab="Average number of step taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxInterval <- avgIntervalStep[ which.max(avgIntervalStep$steps),]
```

*Interval 835 contains the maximum number of steps*

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset


```r
sumOfNA <- sum(is.na(activityDs$steps))
```

*Total number of missing values in the dataset : 2304*

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

*Uses mean steps value store in "avgIntervalStep" to replace the NA steps in dataset based on the interval value*

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
newActivityDs <- activityDs

for( i in 1:nrow(newActivityDs))
{
    if(is.na(newActivityDs$steps[i])){
      newActivityDs$steps[i] <- 
        avgIntervalStep[which(avgIntervalStep$interval == newActivityDs$interval[i]),]$steps
    }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalNoOfStepsByDateForNewActivity <- aggregate(steps~ date , data =  newActivityDs, FUN = sum)

ggplot(totalNoOfStepsByDateForNewActivity, aes(steps)) + geom_histogram(aes(fill = ..count..)) + labs(title= 'Total number of steps taken each day with Imputing missing values' , x = 'Steps' , y = "Count" ) 
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 


```r
meanTotalStep2 <-  mean(totalNoOfStepsByDateForNewActivity$steps, na.rm = T)
medianTotalStep2 <- median(totalNoOfStepsByDateForNewActivity$steps, na.rm = T)
```

*New mean of the total number of steps taken per day : 10766.19*

*New median of the total number of steps taken per day : 10766.19*

*(Mean = Median) after imputing missing value with mean value of steps.*

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
newActivityDs$day <- weekdays(newActivityDs$date)
definedWeekend <- c("Saturday" ,"Sunday")
newActivityDs$type <- factor(ifelse(is.element(newActivityDs$day , definedWeekend), "weekend", "weekday"), c( "weekday" , "weekend"))

head(newActivityDs)
```

```
##       steps       date interval    day    type
## 1 1.7169811 2012-10-01        0 Monday weekday
## 2 0.3396226 2012-10-01        5 Monday weekday
## 3 0.1320755 2012-10-01       10 Monday weekday
## 4 0.1509434 2012-10-01       15 Monday weekday
## 5 0.0754717 2012-10-01       20 Monday weekday
## 6 2.0943396 2012-10-01       25 Monday weekday
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
avgIntervalStepNew <- aggregate(newActivityDs$steps, list(interval = newActivityDs$interval , 
                                                          type = newActivityDs$type), FUN = mean)
names(avgIntervalStepNew)[3] <- "steps"

library(lattice)
xyplot(steps ~ interval | type , data = avgIntervalStepNew, type = "l", xlab = "Interval", 
    ylab = "Number of steps", layout = c(1, 2))
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

