---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---




## Load Libraries 

```r
library(data.table)
library(ggplot2)
library(tidyverse)
```

```
## -- Attaching packages -------------------------------------------------------------------------------------------- tidyverse 1.2.1 --
```

```
## v tibble  2.1.1       v purrr   0.3.2  
## v tidyr   0.8.3       v dplyr   0.8.0.1
## v readr   1.3.1       v stringr 1.4.0  
## v tibble  2.1.1       v forcats 0.4.0
```

```
## -- Conflicts ----------------------------------------------------------------------------------------------- tidyverse_conflicts() --
## x dplyr::between()   masks data.table::between()
## x dplyr::filter()    masks stats::filter()
## x dplyr::first()     masks data.table::first()
## x dplyr::lag()       masks stats::lag()
## x dplyr::last()      masks data.table::last()
## x purrr::transpose() masks data.table::transpose()
```

## Read in data

```r
data <- fread('./activity.csv')
```

## Mean number of steps taken per day

### 1. Make a histogram of the total number of steps. 2. Take the mean of the total number of steps taken

Since we can ignore the missing values in this part of the assignment we will create a new dataset wihout any n/a rows and then make the histogram using that dataset. Then afterward we will take and print out the mean


```r
data_2 <- na.omit(data)
ggplot(data = data_2, aes(x = steps)) +geom_histogram(color = 'blue', fill = 'green') +
  labs(x = 'Step Bins', y = 'Counts') + theme_bw() + ggtitle('Histogram of the Number of Total Steps Taken')
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
average_steps <- mean(data_2$steps)
print(average_steps)
```

```
## [1] 37.3826
```


## Time Series Curve and max interval

```r
data_3 <- data_2 %>% group_by(interval) %>% summarise(avg_steps = mean(steps))

ggplot(data = data_3, aes(x = interval, y = avg_steps)) +
   geom_line(color = 'red') + labs(x = 'Interval', y = 'Average Steps Taken') + ggtitle('Average Steps Time Series Plot') 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
row_desired <- data_3 %>% filter(avg_steps == max(data_3$avg_steps))
print(row_desired[1,1])
```

```
## # A tibble: 1 x 1
##   interval
##      <int>
## 1      835
```


## Imput missing values 


```r
missing <- nrow(data) - nrow(data_2)
print(missing)
```

```
## [1] 2304
```
The number of missing rows was `missing`

To fill these rows we will use the averge number for the interval found in the above steps from data_3. Then remake the histogram,


```r
data$steps <- case_when(is.na(as.double(data$steps)) ~ data_3$avg_steps[match(data$interval,data_3$interval)],
                         TRUE ~ as.double(data$steps))
ggplot(data = data, aes(x = steps)) +geom_histogram(color = 'blue', fill = 'green') +
labs(x = 'Step Bins', y = 'Counts') + theme_bw() + ggtitle('Histogram of the Number of Total Steps Taken')
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
## Are there differences in activity patterns between weekdays and weekends 

Create the new factor variable 

```r
data$factor_day <- as.factor(case_when(weekdays(as.POSIXct(data$date)) %in% c('Saturday','Sunday') ~ 'Weekend',
                                       TRUE ~ 'Weekday'))
```

Now we can create the new plot


```r
data_4 <- data %>% group_by(interval,factor_day) %>% summarise(avg_steps = mean(steps))

ggplot(data = data_4, aes(x = interval, y = avg_steps)) +
   geom_line(color = 'red') + labs(x = 'Interval', y = 'Average Steps Taken') + ggtitle('Average Steps Time Series Plot')  + facet_grid(factor_day ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
