## Loading and Preprocessing the Data

    library(dplyr)

    activity <- read.csv("activity.csv")
    activity$date <- as.Date(activity$date)

## What is mean total number of steps taken per day?

    total_steps_day <- activity %>%
      group_by(date) %>%
      summarize(total_steps = sum(steps, na.rm = TRUE))

    hist(total_steps_day$total_steps, main = "Total Steps per Day", 
         xlab = "Steps", col = "skyblue", breaks = 20)

![](activity_report_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    mean_steps <- mean(total_steps_day$total_steps, na.rm = TRUE)
    median_steps <- median(total_steps_day$total_steps, na.rm = TRUE)

    mean_steps

    ## [1] 9354.23

    median_steps

    ## [1] 10395

## What is the average daily activity pattern?

    interval_avg <- activity %>%
      group_by(interval) %>%
      summarize(mean_steps = mean(steps, na.rm = TRUE))

    plot(interval_avg$interval, interval_avg$mean_steps, type = "l",
         main = "Average Steps per 5-minute Interval",
         xlab = "5-minute Interval", ylab = "Average Steps", col = "blue")

![](activity_report_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    interval_avg[which.max(interval_avg$mean_steps), ]

    ## # A tibble: 1 × 2
    ##   interval mean_steps
    ##      <int>      <dbl>
    ## 1      835       206.

## Imputing Missing Values

    missing_values <- sum(is.na(activity$steps))
    missing_values

    ## [1] 2304

    activity_imputed <- activity %>%
      left_join(interval_avg, by = "interval") %>%
      mutate(steps = ifelse(is.na(steps), mean_steps, steps)) %>%
      select(steps, date, interval)

    total_steps_imputed <- activity_imputed %>%
      group_by(date) %>%
      summarize(total_steps = sum(steps))

    hist(total_steps_imputed$total_steps, main = "Total Steps per Day (Imputed)", 
         xlab = "Steps", col = "lightgreen", breaks = 20)

![](activity_report_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    mean(total_steps_imputed$total_steps)

    ## [1] 10766.19

    median(total_steps_imputed$total_steps)

    ## [1] 10766.19

## Are there differences in activity patterns between weekdays and weekends?

    activity_imputed$day_type <- ifelse(weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"), 
                                        "weekend", "weekday")
    activity_imputed$day_type <- as.factor(activity_imputed$day_type)

    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 4.4.3

    interval_daytype <- activity_imputed %>%
      group_by(interval, day_type) %>%
      summarize(mean_steps = mean(steps), .groups = 'drop')

    ggplot(interval_daytype, aes(x = interval, y = mean_steps)) +
      geom_line(color = "steelblue") +
      facet_wrap(~day_type, ncol = 1) +
      labs(title = "Average Daily Activity Patterns", x = "Interval", y = "Steps")

![](activity_report_files/figure-markdown_strict/unnamed-chunk-5-1.png)
