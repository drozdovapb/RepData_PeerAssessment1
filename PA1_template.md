# Peer Assessment 1

Instructions can be found at my [github repo]
(https://github.com/drozdovapb/RepData_PeerAssessment1)
or elsewhere

The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Preprocessing
Let's get the data!

```r
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile="activity.zip", method="curl")
    unzipped<-unzip("activity.zip")
    myDataSet<-read.csv(unzipped)
```
Now we got it :)

## The number of steps per day
Here we calculate the sum of steps for each day. Then we plot the histogram and calculate the mean and the median for this value.


```r
sumSteps <- tapply(myDataSet$steps, as.factor(myDataSet$date), sum, na.rm=TRUE)
hist(sumSteps, breaks=50, main="Steps per day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
meanSteps <- mean(sumSteps)
medianSteps <- median(sumSteps)
```
Mean number of steps per day is 9354.2295.
Median number of steps per day is 10395.

## Average daily activity pattern
Here we calculate mean number of steps for each period. Then we plot this pattern. There is some messy code intended to just make axis ticks readable.


```r
avgStepsInInt <- tapply(myDataSet$steps, as.factor(myDataSet$interval), mean, na.rm=TRUE)
subsInt<-1:288
plot(avgStepsInInt~subsInt, xaxt="n", type="l", ylab="average # steps", xlab="interval")
seqForAxLab<-seq(1,288,12)
axis(1, at=seqForAxLab, labels=names(avgStepsInInt)[seqForAxLab], las=2)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 
### Mini-conclusion
The daily activity pattern looks very logical. This person sleeps at night and walks much less then.

## Missing values

```r
naSum<-sum(is.na(myDataSet$steps))
```

The number of rows with NA values in 2304. That's pretty much.
How could we fill it in? For example, we can to fill it with the mean number for this interval. The easiest way to accomplish this task is a cycle. Unfortunately it's slow but I can't invent anything better right now.


```r
noNaSet<-myDataSet
for (i in 1:length(myDataSet$steps)) {
    if (is.na(myDataSet$steps[i]==TRUE)) {
        if (i%%288==0) noNaSet$steps[i]<-avgStepsInInt[288]
        else noNaSet$steps[i]<-avgStepsInInt[i%%288]
        }}
```

Now we repeat...


```r
sumStepsNoNa <- tapply(noNaSet$steps, as.factor(myDataSet$date), sum)
hist(sumStepsNoNa, breaks=50, main="Steps per day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

```r
meanStepsNoNa <- mean(sumStepsNoNa)
medianStepsNoNa <- median(sumStepsNoNa)
```

Mean number of steps per day is 1.0766 &times; 10<sup>4</sup>.
Median number of steps per day is 1.0766 &times; 10<sup>4</sup>.

### Mini-conclusion
Now mean and median number of steps per day coincide. It should mean that the distribution is symmetrical now... or that I miscoded something.

## Activity patterns on weekdays and weekends

Here we create a factor variable determining whether each day is a weekday or a weekend. Then we glue this variable to the existing dataset and split it into two.

```r
myWeekDays<-weekdays(as.Date(myDataSet$date))
factorDay<-vector(mode="character", length=length(myWeekDays))
    for (i in 1:length(myWeekDays)) {
        if (myWeekDays[i] == "Saturday") factorDay[i]<-"Weekend"
        else factorDay[i] <- "Weekday"
    }
factorDay<-as.factor(factorDay)
noNaSet[,4]<-factorDay; names(noNaSet)[4]<-"factorDay"
noNaWeek<-subset(noNaSet, factorDay=="Weekday")
noNaEnd<-subset(noNaSet, factorDay=="Weekend")
```

Then we calculate average steps per interval for each of the two sets.

```r
avgStepsInIntWeek <- tapply(noNaWeek$steps, as.factor(noNaWeek$interval), mean)
avgStepsInIntEnd <- tapply(noNaEnd$steps, as.factor(noNaEnd$interval), mean)
```

Then we create two plots.


```r
par(mfrow=c(2,1))
plot(avgStepsInIntWeek~subsInt, xaxt="n", type="l", ylab="average # steps", xlab="interval", main="weekdays")
axis(1, at=seqForAxLab, labels=names(avgStepsInInt)[seqForAxLab], las=2)

plot(avgStepsInIntEnd~subsInt, xaxt="n", type="l", ylab="average # steps", xlab="interval", main="weekends")
axis(1, at=seqForAxLab, labels=names(avgStepsInInt)[seqForAxLab], las=2)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9.png) 

### Mini-conclusion
These two plots differ in the "morning" part mostly. It looks like this person gets up later in the morning. Quite logical.
