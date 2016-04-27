# Reproducible research course project 1: activity monitoring
Fang-Ke Huang  
April 24, 2016  
# 
The instruction of this project can be found here: https://github.com/hfkltc/RepData_PeerAssessment1/blob/master/README.md

##Loading and preprocessing the data

###1. Load the data (i.e. read.csv())
First, let us clean up the working environment and set the working directory to the folder contains the "activity.csv" file. Then, we can load the activity data.

```r
rm(list = ls())
setwd("C:/Users/Fang-Ke/Dropbox/courses/DataScienceTool/activity monitoring")
activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
```

###2. Process/transform the data
Now, we can change the date format to facilitate the following analysis.

```r
activity$date <- as.Date(activity$date)
```

##What is mean total number of steps taken per day?
For this part of the analysis, we will ignore the missing values in the dataset.

###1. Calculate the total number of steps taken per day
We can calculate the total number of steps taken per day using aggregate function in R. When calculating the total number, we will remove the missing values.

```r
total.steps <- aggregate(steps ~ date, activity, FUN = function(x) {sum(x, na.rm = TRUE)})
total.steps
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

###2. Make a histogram of the total number of steps taken each day

```r
hist(total.steps$steps, main = "Total number of steps taken each day", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)

###3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean.steps <- mean(total.steps$steps)
cat("The mean of the total number of steps taken per day is", mean.steps, "\n")
```

```
## The mean of the total number of steps taken per day is 10766.19
```

```r
median.steps <- median(total.steps$steps)
cat("The median of the total number of steps taken per day is", median.steps, "\n")
```

```
## The median of the total number of steps taken per day is 10765
```

##What is the average daily activity pattern?

###1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
We need to calculate the averaged number of steps taken in each 5-mitute interval.

```r
interval.steps <- aggregate(steps ~ interval, activity, FUN = function(x) {mean(x, na.rm = TRUE)})
with(interval.steps, plot(interval, steps, type = "l", main = "Average daily activity pattern", ylab = "Averaged number of steps"))
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)

###2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
cat("The interval", interval.steps[which.max(interval.steps$steps), 1], "contains the maximum number of steps\n")
```

```
## The interval 835 contains the maximum number of steps
```

##Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

###1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
cat("The total number of rows with NAs is", sum(is.na(activity$steps)), "\n")
```

```
## The total number of rows with NAs is 2304
```

###2. Use the mean steps for each interval to fill in all of the missing values.
We first create a table containing the information of missing activity and then use the left outer join to impute the missing values (with mean inverval steps information).

```r
missing.activity <- activity[is.na(activity$steps), ]
names(missing.activity) <- c("NAs", "date", "interval")
missing.activity.imputed <- merge(x = missing.activity, y = interval.steps, by = "interval", all.x = TRUE)
missing.activity.imputed <- missing.activity.imputed[, c("steps", "date", "interval")]
```

###3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
filled.activity <- activity[!is.na(activity$steps), ]
imputed.activity <- rbind(filled.activity, missing.activity.imputed)
```

###4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
total.steps.imputed <- aggregate(steps ~ date, imputed.activity, FUN = function(x) {sum(x, na.rm = TRUE)})
hist(total.steps.imputed$steps, main = "Total number of steps taken each day (imputed)", xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)

```r
mean.steps.imputed <- mean(total.steps.imputed$steps)
cat("The mean (with imputed values) of the total number of steps taken per day is", mean.steps.imputed, "\n")
```

```
## The mean (with imputed values) of the total number of steps taken per day is 10766.19
```

```r
median.steps.imputed <- median(total.steps.imputed$steps)
cat("The median (with imputed values) of the total number of steps taken per day is", median.steps.imputed, "\n")
```

```
## The median (with imputed values) of the total number of steps taken per day is 10766.19
```

```r
impact <- merge(total.steps, total.steps.imputed, by = "date")
names(impact) <- c("names", "w/o.Impute", "w/.Impute")
impact$diff <- impact$`w/o.Impute` - impact$`w/.Impute`
impact
```

```
##         names w/o.Impute w/.Impute diff
## 1  2012-10-02        126       126    0
## 2  2012-10-03      11352     11352    0
## 3  2012-10-04      12116     12116    0
## 4  2012-10-05      13294     13294    0
## 5  2012-10-06      15420     15420    0
## 6  2012-10-07      11015     11015    0
## 7  2012-10-09      12811     12811    0
## 8  2012-10-10       9900      9900    0
## 9  2012-10-11      10304     10304    0
## 10 2012-10-12      17382     17382    0
## 11 2012-10-13      12426     12426    0
## 12 2012-10-14      15098     15098    0
## 13 2012-10-15      10139     10139    0
## 14 2012-10-16      15084     15084    0
## 15 2012-10-17      13452     13452    0
## 16 2012-10-18      10056     10056    0
## 17 2012-10-19      11829     11829    0
## 18 2012-10-20      10395     10395    0
## 19 2012-10-21       8821      8821    0
## 20 2012-10-22      13460     13460    0
## 21 2012-10-23       8918      8918    0
## 22 2012-10-24       8355      8355    0
## 23 2012-10-25       2492      2492    0
## 24 2012-10-26       6778      6778    0
## 25 2012-10-27      10119     10119    0
## 26 2012-10-28      11458     11458    0
## 27 2012-10-29       5018      5018    0
## 28 2012-10-30       9819      9819    0
## 29 2012-10-31      15414     15414    0
## 30 2012-11-02      10600     10600    0
## 31 2012-11-03      10571     10571    0
## 32 2012-11-05      10439     10439    0
## 33 2012-11-06       8334      8334    0
## 34 2012-11-07      12883     12883    0
## 35 2012-11-08       3219      3219    0
## 36 2012-11-11      12608     12608    0
## 37 2012-11-12      10765     10765    0
## 38 2012-11-13       7336      7336    0
## 39 2012-11-15         41        41    0
## 40 2012-11-16       5441      5441    0
## 41 2012-11-17      14339     14339    0
## 42 2012-11-18      15110     15110    0
## 43 2012-11-19       8841      8841    0
## 44 2012-11-20       4472      4472    0
## 45 2012-11-21      12787     12787    0
## 46 2012-11-22      20427     20427    0
## 47 2012-11-23      21194     21194    0
## 48 2012-11-24      14478     14478    0
## 49 2012-11-25      11834     11834    0
## 50 2012-11-26      11162     11162    0
## 51 2012-11-27      13646     13646    0
## 52 2012-11-28      10183     10183    0
## 53 2012-11-29       7047      7047    0
```
You can see the mean estimates are the same (no impact of imputing missing values) but the median estimates have slightly increased (there is a small impact of imputing missing values). And there is no impact of imputing missing data on the estimates of the total daily number of steps.

##Are there differences in activity patterns between weekdays and weekends?
###1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
imputed.activity$wday <- weekdays(imputed.activity$date)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
imputed.activity$wDayEnd <- factor((imputed.activity$wday %in% weekdays1), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
```

###2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
library(ggplot2)
interval.steps.wday <- aggregate(steps ~ interval + wDayEnd, imputed.activity, FUN = function(x) {mean(x)})
qplot(interval, steps, data = interval.steps.wday, facets = wDayEnd~., geom = "line")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)
