---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)

unzip("activity.zip")

activity <- read.csv("activity.csv", stringsAsFactors = FALSE)

activity$date <- as.Date(activity$date)
```


## What is mean total number of steps taken per day?


```r
activity_perday <- activity %>%
  group_by(date) %>%
  summarize(totalsteps = sum(steps))

activity_perday %>%
  ggplot(aes(x=totalsteps)) +
  geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


## What is the average daily activity pattern?


```r
mean_median_perday <- activity %>%
  group_by(date) %>%
  summarize(totalsteps = sum(steps, na.rm = TRUE)) %>%
  summarize(mean = mean(totalsteps),
            median = median(totalsteps))

groupedbyinterval <- activity %>%
  group_by(interval) %>%
  summarize(mean = mean(steps, na.rm = TRUE))

groupedbyinterval %>%
  ggplot(aes(x=interval, y=mean)) + 
  geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
groupedbyinterval[which(groupedbyinterval$mean == max(groupedbyinterval$mean)),]
```

```
## # A tibble: 1 x 2
##   interval  mean
##      <int> <dbl>
## 1      835  206.
```


## Imputing missing values


```r
(totalna = sum(is.na(activity$steps)))
```

```
## [1] 2304
```

```r
activity_imp <- activity #copying the whole dataset
activity_imp$steps_imp <- activity_imp$steps #copy original steps to new column

for (x in 1:length(activity_imp$steps_imp)){
    if(is.na(activity_imp$steps[x]) == TRUE){
      activity_imp$steps_imp[x] <- round(groupedbyinterval$mean[which(groupedbyinterval$interval == activity_imp$interval[x])])
    }
}

activity_imp_perday <- activity_imp %>%
  group_by(date) %>%
  summarize(totalsteps = sum(steps_imp))

activity_imp_perday %>%
  ggplot(aes(x=totalsteps)) +
  geom_histogram()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
mean_median_imp_perday <- activity_imp %>%
  group_by(date) %>%
  summarize(totalsteps = sum(steps_imp, na.rm = TRUE)) %>%
  summarize(mean = mean(totalsteps),
            median = median(totalsteps))
```


## Are there differences in activity patterns between weekdays and weekends?


```r
activity_imp$dayend <- ifelse(weekdays(activity_imp$date) == "Saturday" | weekdays(activity_imp$date) == "Sunday", 
                              "Weekend", 
                              "Weekday")

activity_imp_grouped_dayend <- activity_imp %>%
  group_by(dayend, interval) %>%
  summarize(mean = mean(steps_imp, na.rm = TRUE))

activity_imp_grouped_dayend %>%
  ggplot(aes(x=interval, y=mean)) +
  geom_line() +
  facet_wrap(~dayend)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

