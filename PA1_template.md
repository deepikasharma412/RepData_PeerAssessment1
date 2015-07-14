---
title: "Peer Assessment 1"
output: html_document
---

##Loading and preprocessing the data

```{r}
activityData <- read.csv('activity.csv')
#activityData$interval <- strptime(gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", activityData$interval), format='%H:%M')
```


##What is mean total number of steps taken per day?

```{r}
stepsByDay <- tapply(activityData$steps, activityData$date, sum, na.rm=TRUE)
```

1. HISTOGRAM 

```{r}
library("ggplot2")
qplot(stepsByDay, xlab='Total steps per day', ylab='Frequency using binwith 500', binwidth=500)
```

2. Calculate and report the mean and median total number of steps taken per day

```{r}
stepsByDayMean <- mean(stepsByDay)
stepsByDayMedian <- median(stepsByDay)
```
- mean is 9354.23
- median is 10395


##What is the average daily activity pattern?

1. TIME SERIES PLOT 

```{r}
averageStepsPerTimeBlock <- aggregate(x=list(meanSteps=activityData$steps), by=list(interval=activityData$interval), FUN=mean, na.rm=TRUE)

ggplot(data=averageStepsPerTimeBlock, aes(x=interval, y=meanSteps)) +
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r}
mostSteps <- which.max(averageStepsPerTimeBlock$meanSteps)
timeMostSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", averageStepsPerTimeBlock[mostSteps,'interval'])
```
- most steps 8:35


##Imputing missing values

1. Calculate and report the total number of missing values in the dataset

```{r}
numMissingValues <- length(which(is.na(activityData$steps)))
```
- missing values 2304

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```{r}
library("Hmisc")
activityDataImputed <- activityData
activityDataImputed$steps <- impute(activityData$steps, fun=mean)
```

4. HISTOGRAM

```{r}
stepsByDayImputed <- tapply(activityDataImputed$steps, activityDataImputed$date, sum)
qplot(stepsByDayImputed, xlab='Total steps per day (Imputed)', ylab='Frequency using binwith 500', binwidth=500)
```

5. Mean and median 

```{r}
stepsByDayMeanImputed <- mean(stepsByDayImputed)
stepsByDayMedianImputed <- median(stepsByDayImputed)
```
- mean 10766.19
- median 10766.19


##Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r}
activityDataImputed$dateType <-  ifelse(as.POSIXlt(activityDataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
```

2. PANEL / TIME SERIES PLOT 

```{r}
averagedActivityDataImputed <- aggregate(steps ~ interval + dateType, data=activityDataImputed, mean)
ggplot(averagedActivityDataImputed, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dateType ~ .) +
    xlab("5-minute interval") + 
    ylab("avarage number of steps")
```







