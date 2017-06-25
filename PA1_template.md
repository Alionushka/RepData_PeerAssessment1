This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012 and
include the number of steps taken in 5 minute intervals each day.The
original dataset can be found here
<https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>

The variables included in this dataset are:

-   **steps:** Number of steps taking in a 5-minute interval (missing
    values are coded as NA)  
-   **date:** The date on which the measurement was taken in YYYY-MM-DD
    format
-   **interval:** Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this dataset.

Loading and preprocessing the data
----------------------------------

Set the working directory on my local computer setwd

    setwd("C:/Users/Aliona/Desktop/COURSERA/Course 5 Reproducable Research/Week 2/")

The following three packages will be used - dplyr, ggplot2, and lattice;
warning=FALSE was set in order to suppress warning for this code chunk.

    require("dplyr") ## dplyr is used for structuring the data for analysis

    ## Loading required package: dplyr

    require("ggplot2") ## ggplot2 is required for several plots

    ## Loading required package: ggplot2

    require("lattice") ## lattice plot is required for the weekday-weekend plot

    ## Loading required package: lattice

    options(scipen = 999) ## eliminate scientific notation

Then read in the data from the local zipped file to the object activty,
and convert the activity object to a tbl class.

    # Load the data file into a data frame
    activity <- read.csv("activity.csv", as.is = TRUE)
    # Remove the NA values and store in a separate structure for future use
    good_act <- activity[complete.cases(activity), ]

What is mean total number of steps taken per day?
-------------------------------------------------

For this part of the assignment, you can ignore the missing values in
the dataset.

1.Calculate the total number of steps taken per day

2.If you do not understand the difference between a histogram and a
barplot, research the difference between them. Make a histogram of the
total number of steps taken each day

3.Calculate and report the mean and median of the total number of steps
taken per day

    # Calculate the total number of steps taken per day
    steps_per_day <- aggregate(steps ~ date, good_act, sum)

    # Create a histogram of no of steps per day
    hist(steps_per_day$steps, main = "Histogram of total number of steps per day", xlab = "Steps per day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    # Calculate the mean and median of the total number of steps taken per day
    round(mean(steps_per_day$steps))

    ## [1] 10766

    median(steps_per_day$steps)

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

1.Make a time series plot (i.e. type = "l") of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all days
(y-axis)

2.Which 5-minute interval, on average across all the days in the
dataset, contains the maximum number of steps?

    # Calculate average steps per interval for all days 
    avg_steps_per_interval <- aggregate(steps ~ interval, good_act, mean)

    # Calculate average steps per day for all intervals - Not required, but for my own sake 
    avg_steps_per_day <- aggregate(steps ~ date, good_act, mean)

    # Plot the time series with appropriate labels and heading
    plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps, type='l', col=1, main="Average number of steps by Interval", xlab="Time Intervals", ylab="Average number of steps")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

    # Identify the interval index which has the highest average steps
    interval_idx <- which.max(avg_steps_per_interval$steps)

    # Identify the specific interval and the average steps for that interval
    print (paste("The interval with the highest avg steps is ", avg_steps_per_interval[interval_idx, ]$interval, " and the no of steps for that interval is ", round(avg_steps_per_interval[interval_idx, ]$steps, digits = 1)))

    ## [1] "The interval with the highest avg steps is  835  and the no of steps for that interval is  206.2"

Imputing missing values
-----------------------

Note that there are a number of days/intervals where there are missing
values (coded as NA). The presence of missing days may introduce bias
into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

2.Devise a strategy for filling in all of the missing values in the
dataset. The strategy does not need to be sophisticated. For example,
you could use the mean/median for that day, or the mean for that
5-minute interval, etc.

3.Create a new dataset that is equal to the original dataset but with
the missing data filled in.

4.Make a histogram of the total number of steps taken each day and
Calculate and report the mean and median total number of steps taken per
day. Do these values differ from the estimates from the first part of
the assignment? What is the impact of imputing missing data on the
estimates of the total daily number of steps?

We have adopted a strategy to replace the missing NA values with the
average steps in that interval across all the days

    # Calculate the number of rows with missing values
    missing_value_act <- activity[!complete.cases(activity), ]
    nrow(missing_value_act)

    ## [1] 2304

    # Loop through all the rows of activity, find the one with NA for steps.
    # For each identify the interval for that row
    # Then identify the avg steps for that interval in avg_steps_per_interval
    # Substitute the NA value with that value

    for (i in 1:nrow(activity)) {
        if(is.na(activity$steps[i])) {
            val <- avg_steps_per_interval$steps[which(avg_steps_per_interval$interval == activity$interval[i])]
            activity$steps[i] <- val 
        }
    }

    # Aggregate the steps per day with the imputed values
    steps_per_day_impute <- aggregate(steps ~ date, activity, sum)

    # Draw a histogram of the value 
    hist(steps_per_day_impute$steps, main = "Histogram of total number of steps per day (IMPUTED)", xlab = "Steps per day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-8-1.png)

    # Compute the mean and median of the imputed value
    # Calculate the mean and median of the total number of steps taken per day
    round(mean(steps_per_day_impute$steps))

    ## [1] 10766

    median(steps_per_day_impute$steps)

    ## [1] 10766.19

We note that the mean and the median has NOT changed because of the
imputed values

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

For this part the weekdays() function may be of some help here. Use the
dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels -
"weekday" and "weekend" indicating whether a given date is a weekday or
weekend day.

2.Make a panel plot containing a time series plot (i.e. type = "l") of
the 5-minute interval (x-axis) and the average number of steps taken,
averaged across all weekday days or weekend days (y-axis). See the
README file in the GitHub repository to see an example of what this plot
should look like using simulated data.

Create a function to determine if the date is a weekday

    week_day <- function(date_val) {
        wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
        if  (!(wd == 'zaterdag' || wd == 'zondag')) {
            x <- 'Weekday'
        } else {
            x <- 'Weekend'
        }
        x
    }

Apply the function to the dataset to create a new day type variable

    # Apply the week_day function and add a new column to activity dataset
    activity$day_type <- as.factor(sapply(activity$date, week_day))

    #load the ggplot library
    library(ggplot2)

    # Create the aggregated data frame by intervals and day_type
    steps_per_day_impute <- aggregate(steps ~ interval+day_type, activity, mean)

    # Create the plot
    plt <- ggplot(steps_per_day_impute, aes(interval, steps)) +
        geom_line(stat = "identity", aes(colour = day_type)) +
        theme_gray() +
        facet_grid(day_type ~ ., scales="fixed", space="fixed") +
        labs(x="Interval", y=expression("No of Steps")) +
        ggtitle("No of steps Per Interval by day type")
    print(plt)

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-11-1.png)

We do see some subtle differences between the average number of steps
between weekdays and weekends. For instance, it appears that the user
started a bit later on weekend mornings and tend to do smaller numbers
on weekend mornings.
