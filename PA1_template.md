---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

**Loading the library needed**


```r
library(dplyr)
library(ggplot2)
```

**Extract and read the data**


```r
unzip("activity.zip")

activity <- read.csv("activity.csv", stringsAsFactors = FALSE)

activity$date <- as.Date(activity$date) #convert to 'date' format
```


## What is mean total number of steps taken per day?

**Sum of steps, grouped by date**


```r
(activity_perday <- activity %>%
  group_by(date) %>%
  summarize(totalsteps = sum(steps)))
```

```
## # A tibble: 61 x 2
##    date       totalsteps
##    <date>          <int>
##  1 2012-10-01         NA
##  2 2012-10-02        126
##  3 2012-10-03      11352
##  4 2012-10-04      12116
##  5 2012-10-05      13294
##  6 2012-10-06      15420
##  7 2012-10-07      11015
##  8 2012-10-08         NA
##  9 2012-10-09      12811
## 10 2012-10-10       9900
## # ... with 51 more rows
```

**Plot a histogram of mean total number of steps taken each day**


```r
activity_perday %>%
  ggplot(aes(x=totalsteps)) +
  geom_histogram()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

## What is the average daily activity pattern?

**Calculate the mean and median of steps taken each day**


```r
(mean_median_perday <- activity %>%
  group_by(date) %>%
  summarize(totalsteps = sum(steps, na.rm = TRUE)) %>%
  summarize(mean = mean(totalsteps),
            median = median(totalsteps)))
```

```
## # A tibble: 1 x 2
##    mean median
##   <dbl>  <int>
## 1 9354.  10395
```

**Calculate the mean of steps based on certain intervals**


```r
(groupedbyinterval <- activity %>%
  group_by(interval) %>%
  summarize(mean = mean(steps, na.rm = TRUE)))
```

```
## # A tibble: 288 x 2
##    interval   mean
##       <int>  <dbl>
##  1        0 1.72  
##  2        5 0.340 
##  3       10 0.132 
##  4       15 0.151 
##  5       20 0.0755
##  6       25 2.09  
##  7       30 0.528 
##  8       35 0.868 
##  9       40 0     
## 10       45 1.47  
## # ... with 278 more rows
```

**Plot a line plot of mean of steps based on certain intervals**


```r
groupedbyinterval %>%
  ggplot(aes(x=interval, y=mean)) + 
  geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

**Finding the maximum interval and mean of steps**


```r
(groupedbyinterval[which(groupedbyinterval$mean == max(groupedbyinterval$mean)),])
```

```
## # A tibble: 1 x 2
##   interval  mean
##      <int> <dbl>
## 1      835  206.
```


## Imputing missing values

**Calculate the sum of missing values**


```r
(totalna = sum(is.na(activity$steps)))
```

```
## [1] 2304
```

**Impute the missing steps values using the mean of the related intervals**


```r
activity_imp <- activity #copying the whole dataset
activity_imp$steps_imp <- activity_imp$steps #copy original steps to new column

for (x in 1:length(activity_imp$steps_imp)){
    if(is.na(activity_imp$steps[x]) == TRUE){
      activity_imp$steps_imp[x] <- round(groupedbyinterval$mean[which(groupedbyinterval$interval == activity_imp$interval[x])])
    }
}
```

**Calculate the sum of steps per day using imputed values from the previous code chunk**


```r
(activity_imp_perday <- activity_imp %>%
  group_by(date) %>%
  summarize(totalsteps = sum(steps_imp)))
```

```
## # A tibble: 61 x 2
##    date       totalsteps
##    <date>          <dbl>
##  1 2012-10-01      10762
##  2 2012-10-02        126
##  3 2012-10-03      11352
##  4 2012-10-04      12116
##  5 2012-10-05      13294
##  6 2012-10-06      15420
##  7 2012-10-07      11015
##  8 2012-10-08      10762
##  9 2012-10-09      12811
## 10 2012-10-10       9900
## # ... with 51 more rows
```

**Plot a histogram of mean total number of steps taken per day using the imputed values**


```r
activity_imp_perday %>%
  ggplot(aes(x=totalsteps)) +
  geom_histogram()
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

**Calculate the mean and median of steps taken per day using the imputed values**


```r
(mean_median_imp_perday <- activity_imp %>%
  group_by(date) %>%
  summarize(totalsteps = sum(steps_imp, na.rm = TRUE)) %>%
  summarize(mean = mean(totalsteps),
            median = median(totalsteps)))
```

```
## # A tibble: 1 x 2
##     mean median
##    <dbl>  <dbl>
## 1 10766.  10762
```


## Are there differences in activity patterns between weekdays and weekends?

**Calculate the mean of steps taken, grouped by Weekend or Weekday**


```r
activity_imp$dayend <- ifelse(weekdays(activity_imp$date) == "Saturday" | weekdays(activity_imp$date) == "Sunday", 
                              "Weekend", 
                              "Weekday")

(activity_imp_grouped_dayend <- activity_imp %>%
  group_by(dayend, interval) %>%
  summarize(mean = mean(steps_imp, na.rm = TRUE)))
```

```
## # A tibble: 576 x 3
## # Groups:   dayend [2]
##    dayend  interval   mean
##    <chr>      <int>  <dbl>
##  1 Weekday        0 2.29  
##  2 Weekday        5 0.4   
##  3 Weekday       10 0.156 
##  4 Weekday       15 0.178 
##  5 Weekday       20 0.0889
##  6 Weekday       25 1.58  
##  7 Weekday       30 0.756 
##  8 Weekday       35 1.16  
##  9 Weekday       40 0     
## 10 Weekday       45 1.73  
## # ... with 566 more rows
```

**Plot a line plot of mean of steps based on certain interval using the imputed values, grouped by Weekend or Weekday**


```r
activity_imp_grouped_dayend %>%
  ggplot(aes(x=interval, y=mean)) +
  geom_line() +
  facet_wrap(~dayend)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
