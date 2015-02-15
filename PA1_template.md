---
title: "Untitled"
output: html_document
---

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

1.Loading & Preprocessing data


```r
activity <- read.csv("activity.csv", colClasses = c("numeric", "character", 
    "numeric"))
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


```r
names(activity)
```

```
## [1] "steps"    "date"     "interval"
```


```r
library(lattice)
```


```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

2.What is mean total number of steps taken per day?


```r
StepsTotal <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```


```r
hist(StepsTotal$steps, main = "Total steps by day", xlab = "day", col = "blue")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 


```r
mean(StepsTotal$steps)
```

```
## [1] 10766.19
```


```r
median(StepsTotal$steps)
```

```
## [1] 10765
```

3.What is the average daily activity pattern?


```r
time_series <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
```


```r
plot(row.names(time_series), time_series, type = "l", xlab = "5-min interval", 
    ylab = "Average across all Days", main = "Average number of steps taken", 
    col = "red")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 


```r
max_interval <- which.max(time_series)
names(max_interval)
```

```
## [1] "835"
```

4.Imputing missing values


```r
activity_NA <- sum(is.na(activity))
activity_NA
```

```
## [1] 2304
```


```r
StepsAverage <- aggregate(steps ~ interval, data = activity, FUN = mean)
fillNA <- numeric()
for (i in 1:nrow(activity)) {
    obs <- activity[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(StepsAverage, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}
```


```r
new_activity <- activity
new_activity$steps <- fillNA
```


```r
StepsTotal2 <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
```


```r
hist(StepsTotal2$steps, main = "Total steps by day", xlab = "day", col = "green")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 


```r
mean(StepsTotal2$steps)
```

```
## [1] 10766.19
```


```r
median(StepsTotal2$steps)
```

```
## [1] 10766.19
```

5.Are there differences in activity patterns between weekdays and weekends?


```r
day <- weekdays(activity$date)
daylevel <- vector()
for (i in 1:nrow(activity)) {
    if (day[i] == "Saturday") {
        daylevel[i] <- "Weekend"
    } else if (day[i] == "Sunday") {
        daylevel[i] <- "Weekend"
    } else {
        daylevel[i] <- "Weekday"
    }
}
activity$daylevel <- daylevel
activity$daylevel <- factor(activity$daylevel)

stepsByDay <- aggregate(steps ~ interval + daylevel, data = activity, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")
```


```r
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
    xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-20](figure/unnamed-chunk-20-1.png) 








