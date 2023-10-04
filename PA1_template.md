---
title: "Reproduclible Research - Project 1"
author: "Arohi Parlikar"
date: "October 3, 2023"
output: html_document
---





This report makes use of data from a personal activity monitoring device which collects data at 5 minute intervals through out the day. The data for anonymous individuals is collected for two months - October & November, 2012.

The data has 17568 observations for the following variables:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

date: The date on which the measurement was taken in YYYY-MM-DD format

interval: Identifier for the 5-minute interval in which measurement was taken

## Loading & Processing the Data

```r
# Loading the necessary libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(magrittr)
library(multipanelfigure)

# Reading activity data
actData <- data.table(read.csv("repdata_data_activity/activity.csv"))

# Quick data exploration
summary(actData)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
names(actData)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(actData)
```

```
##    steps       date interval
## 1:    NA 2012-10-01        0
## 2:    NA 2012-10-01        5
## 3:    NA 2012-10-01       10
## 4:    NA 2012-10-01       15
## 5:    NA 2012-10-01       20
## 6:    NA 2012-10-01       25
```

```r
dim(actData)
```

```
## [1] 17568     3
```


## What is mean total number of steps taken per day?

Histogram of the total number of steps taken each day

```r
# Calculating total steps taken per day
StepsEachDay <- aggregate(steps ~ date, actData, sum, na.rm = TRUE)
colnames(StepsEachDay) <- c("Date","Steps")

# Plotting a histogram
Steps1 <- ggplot(StepsEachDay, aes(x = Steps)) + 
  geom_histogram(breaks = seq(0, 25000, by=2500), fill = "#CACAE2") + 
  xlab("Total steps taken each day") + ylab("Frequency") + ggtitle("Total Steps Taken on a Day") + ylim(0,30)

Steps1
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

#### Mean and median number of steps taken each day
Mean of total number of steps taken per day:

```r
mean(StepsEachDay$Steps)
```

```
## [1] 10766.19
```
Median of total number of steps taken per day:

```r
median(StepsEachDay$Steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Time series plot of the average number of steps taken


```r
# Calculating average number steps taken for all the day based on the interval
avgDailyAct <- aggregate(steps ~ interval, actData, mean, na.rm = TRUE)
colnames(avgDailyAct) <- c("Interval","Mean")

# Plotting average daily activity pattern
ggplot(avgDailyAct, aes(Interval, Mean)) + 
  geom_line(col = "#DA1884") + xlab("Interval (5 minute)") + ylab("Average Number of Steps Across All Days") + ggtitle("Time Series Plot: Average Number of Steps Taken")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avgDailyAct[which.max(avgDailyAct$Mean),]$Interval
```

```
## [1] 835
```

## Imputing missing values
Code to describe and show a strategy for imputing missing data

#### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NAs <- sum(is.na(actData$steps))
print(NAs)
```

```
## [1] 2304
```

#### Strategy for filling in all of the missing values in the dataset

```r
# Imputing missing values with the mean of daily activity (interval 5)
imputedSteps <- avgDailyAct$Mean[match(actData$interval, avgDailyAct$Interval)]
```

#### Create a new dataset that is equal to the original dataset but with the missing data filled in

```r
imputedActData <- actData %>% 
  mutate(
    steps_new = ifelse(is.na(steps), yes = imputedSteps, no = steps)) %>%
      select(-c(steps)) %>%
      rename(steps = steps_new)

# Looking at the summary to check if it still has NAs
summary(imputedActData)
```

```
##      date              interval          steps       
##  Length:17568       Min.   :   0.0   Min.   :  0.00  
##  Class :character   1st Qu.: 588.8   1st Qu.:  0.00  
##  Mode  :character   Median :1177.5   Median :  0.00  
##                     Mean   :1177.5   Mean   : 37.38  
##                     3rd Qu.:1766.2   3rd Qu.: 27.00  
##                     Max.   :2355.0   Max.   :806.00
```

#### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# Calculating total steps taken per day
StepsEachDay2 <- aggregate(steps ~ date, imputedActData, sum)
colnames(StepsEachDay2) <- c("Date","Steps")

# Plotting a histogram
Steps2 <- ggplot(StepsEachDay2, aes(x = Steps)) + 
  geom_histogram(breaks = seq(0, 25000, by=2500), fill = "#EE5E5F") + 
  xlab("Total steps taken each day") + ylab("Frequency") + ggtitle("Total Steps Taken on a Day") +
  ylim(0,30)

Steps2
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
# Mean & Median of daily steps in imputed data set
mean(StepsEachDay2$Steps)
```

```
## [1] 10766.19
```

```r
median(StepsEachDay2$Steps)
```

```
## [1] 10766.19
```
#### Comparison of daily steps before & after imputing missing data
Since the missing activity data was imputed with the mean values, the new data has higher frequency of mean daily steps.

```r
steps_comp <- multi_panel_figure(columns = 2, rows = 1, panel_label_type = "none")
Steps1 <- Steps1 + labs(caption = "Steps data contains missing values")
Steps2 <- Steps2 + labs(caption = "Missing data imputed with mean of steps per day")
steps_comp %<>%
  fill_panel(Steps1, column = 1, row = 1) %<>%
  fill_panel(Steps2, column = 2, row = 1)
steps_comp
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

## Are there differences in activity patterns between weekdays and weekends?
#### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
# Formatting the dates column
imputedActData$date <- as.Date(strptime(imputedActData$date, format="%Y-%m-%d"))

# Function to distinguish between weekdays from weekends
imputedActData <- imputedActData %>% 
  mutate(dayType = ifelse(weekdays(date) == "Saturday" | weekdays(date) == "Sunday", "Weekend", "Weekday"))
```

#### Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
# Creating the data to be plotted
actDayType <-  aggregate(steps ~ interval + dayType, imputedActData, mean)

# Plotting time series using ggplot2
ggplot(actDayType, aes(x = interval , y = steps, color = dayType)) + 
  geom_line() + ggtitle("Average Daily Steps by Day Type") + 
  xlab("Interval") + 
  ylab("Average Number of Steps") +
  facet_wrap(~dayType, ncol = 1, nrow=2) +
  scale_color_discrete(name = "Day Type")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
