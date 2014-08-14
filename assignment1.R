#============================================#
# 	      REPRODUCIBLE RESERCH           #
#============================================#


#-----------------------------------#
#	    ASSIGNMENT 1	    #
#-----------------------------------#


# First, I set my working directory and I control if it exists.

setwd("C:/Documents and Settings/Sofia Cividini/Desktop")
file.exists("C:/Documents and Settings/Sofia Cividini/Desktop")
library(plyr)

# --------------------------  FIRST STEP  ------------------------------->


# I load and process the data.

activity <- read.csv("C:/Documents and Settings/Sofia Cividini/Desktop/activity.csv", 
header = TRUE, sep = ",")
activity <- edit(activity)

# I select only the columns of my interest, that is 'steps' and 'date'.

activity.1 <- activity[,1:2]
activity.1 <- edit(activity.1)


# --------------------------  SECOND STEP  ------------------------------->


# I calculate the sum of the total number of steps taken per day.

data.activity1 <- aggregate(.~date, FUN=sum, data=activity.1)
data.activity1 <- edit(data.activity1)

# I make the histogram of the total number of steps taken each day.

hist(data.activity1$steps, xlab = "Total number of steps per day",
main = "Total number of steps per day from October to November 2012", 
col.main="bisque4", col = "beige", ylim=c(0,30), border = "bisque3")

# I calculate the total mean and median (taken per day) on all the days.

steps.mean <- mean(data.activity1$steps)
steps.mean
steps.median <- median(data.activity1$steps)
steps.median


# --------------------------  THIRD STEP  ------------------------------->

# I calculate the total mean of the steps (taken per day) within each 5-minute
# interval on all the days.

data.activity4 <- aggregate(.~interval, FUN=mean, data=activity[,c(1,3)])
# I rename the column corresponding to the mean
names(data.activity4)[2] <- "mean"	
data.activity4 <- edit(data.activity4)

time <- c(1:288)
data.activity4 <- data.frame(data.activity4, time)
data.activity4 <- edit(data.activity4)

# I create the plot.

par(cex="0.75")
plot(mean ~ time, data=data.activity4, type="l", col="steelblue3", xaxt = "n",
ylab="average number of the steps averaged across all days", col.main="tomato3",
xlab="5-minute intervals", main="Average number of the steps averaged across all days
at 5-minute intervals from October to November 2012")
axis(1, at=c(1,61,121,181,241), labels=c("0","500","1000","1500","2000"))

# I look at the maximum number of steps, and which is the 5-minute interval that
# includes it.

max.steps <- max(data.activity4$mean)
max.steps

data.activity5 <- subset(data.activity4, mean==max.steps)
data.activity5


# --------------------------  FOURTH STEP  ------------------------------->


# I calculate and report the total number of missing value in the dataset.

summary(activity)


# I calculate the total mean of the number of steps taken per day.

data.activity2 <- aggregate(.~date, FUN=mean, data=activity.1)
# I rename the column corresponding to the mean
names(data.activity2)[2] <- "mean"	
data.activity2 <- edit(data.activity2)


data <- subset(data.activity2, date=="2012-10-02"|date=="2012-10-09"|
date=="2012-11-02"|date=="2012-11-05"|date=="2012-11-11"|date=="2012-11-12"|
date=="2012-11-15"|date=="2012-11-29")
data

# I recode NA 

data.1.ott <- activity[1:288,2:3]
steps <- c(0.4375000)
data.1.ott <- data.frame(steps,data.1.ott)

data.8.ott <- activity[2017:2304,2:3]
steps <- c(44.4826389)
data.8.ott <- data.frame(steps,data.8.ott)

data.1.nov <- activity[8929:9216,2:3]
steps <- c(36.8055556)
data.1.nov <- data.frame(steps,data.1.nov)

data.4.nov <- activity[9793:10080,2:3]
steps <- c(36.2465278)
data.4.nov <- data.frame(steps,data.4.nov)

data.9.nov <- activity[11233:11520,2:3]
steps <- c(43.7777778)
data.9.nov <- data.frame(steps,data.9.nov)

data.10.nov <- activity[11521:11808,2:3]
steps <- c(37.3784722)
data.10.nov <- data.frame(steps,data.10.nov)

data.14.nov <- activity[12673:12960,2:3]
steps <- c(0.1423611)
data.14.nov <- data.frame(steps,data.14.nov)

data.30.nov <- activity[17281:17568,2:3]
steps <- c(24.4687500)
data.30.nov <- data.frame(steps,data.30.nov)

data.NA <- rbind(data.1.ott,data.8.ott,data.1.nov,data.4.nov,data.9.nov,
data.10.nov,data.14.nov,data.30.nov)
row.names(data.NA) <- NULL
data.NA <- edit(data.NA)

activity.new <- activity[c(289:2016,2305:8928,9217:9792,10081:11232,
11809:12672,12961:17280), ]
row.names(activity.new) <- NULL
activity.new <- edit(activity.new)

activity.new2 <- rbind(data.NA,activity.new)
activity.new2 <- activity.new2[order(activity.new2$date), ]
activity.new2 <- edit(activity.new2)


# I calculate the sum of the total number of steps taken per day.

activity.new3 <- aggregate(.~date, FUN=sum, data=activity.new2)
activity.new3 <- edit(activity.new3)

# I make the histogram of the total number of steps taken each day.

hist(activity.new3$steps, xlab = "Total number of steps per day",
main = "Total number of steps per day from October to November 2012 
(manipulated missing values)", col.main="bisque4", col = "beige", 
ylim=c(0,40), border = "bisque3")

# I calculate the total mean and median (taken per day) on all the days.

steps.mean <- mean(activity.new3$steps)
steps.mean
steps.median <- median(activity.new3$steps)
steps.median


# --------------------------  FIFTH STEP  ------------------------------->


weekdays <- weekdays(as.Date(activity.new2$date))

data.week <- data.frame(activity.new2, weekdays)
data.week <- edit(data.week)

# I create the factor variable daytype.

daytype <- c(rep("weekday",1440), rep("weekend",576))
data.p <- data.week[1:16128, ]
data.p <- data.frame(data.p, daytype)

daytype <- c("weekday")
data.p1 <- data.week[16129:17568, ]
data.p1 <- data.frame(data.p1,daytype)

data.week1 <- rbind(data.p, data.p1)
row.names(data.week1) <- NULL
data.week1 <- edit(data.week1)


data.week2 <- aggregate(.~interval*daytype, FUN=mean, data=data.week1[,c(1,3,5)])
# I rename the column corresponding to the mean
names(data.week2)[3] <- "mean"	
data.week2 <- edit(data.week2)

time1 <- c(0:287)
data.week3 <- data.frame(data.week2, time1)
data.week3<- edit(data.week3)


# I create the panel plot
library(lattice)
library(grid)

xyplot(mean ~ time1|daytype, data=data.week3, type="l", layout=c(1,2), xaxt="n",
ylab="Average number of steps", xlab="5-minute interval", col="steelblue3",
main=list("Average number of steps by weekday and weekend at 5-minute intervals
from October to November 2012 (manipulated missing values)", cex=0.90, col="bisque4"),
scales=list(x=list(at=seq(0,287, by=60), labels=seq(0,2000, by=500))))


str(data.NA)
str(activity.new) 









