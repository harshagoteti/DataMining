## Figuring out R's default working directory

getwd()

## You can use setwd() to change the directory

## Reading the dataset, which you have downloaded into the location, into a dataframe
SonjasData <- read.csv("./adult.data.simplified.csv")

## Looking at the first 25 rows
head(SonjasData,25)
SonjasData[1:25,]

## Looking at the last 10 rows
tail(SonjasData,10)
SonjasData[32552:32561,]

## What kinds of attributes do I have in this dataset?
attributes(SonjasData)
str(SonjasData)

## What are some basic stats for my attributes?
summary(SonjasData)

## Looking at just the age attribute
SonjasData$age # might produce more output than the cache can show
SonjasData$age[1:10] # limited the output to the first 10 lines

## Basic stats for the age attribute
summary(SonjasData$age)
mean(SonjasData$age)
median(SonjasData$age)
range(SonjasData$age)
quantile(SonjasData$age)
sd(SonjasData$age)

## Making pretty plots
hist(SonjasData$age)  # histogram
boxplot(SonjasData$age)  # boxplot
plot(SonjasData$age)   # dotplot (WHOA!!! Useless!)

table(SonjasData$age)   # preparing for a pie chart
pie(table(SonjasData$age))  # works better with fewer categories

## But missing values can affect mean, range, and sd.  What do I do?
## Manage them!
sd(SonjasData$age, na.rm = FALSE)   # this is the default with all values included
sd(SonjasData$age, na.rm = TRUE)   # missing values removed ... turns out there were no missing values, though.

## What about outliers?  The plots for age were ... um ... interesting
hist(SonjasData$age)
boxplot(SonjasData$age)  # obviously, there are some outliers; we will remove the people over 75

SonjasDataNoOutliers <- subset(SonjasData,age<75)   # use the subset() function to filter your data into a new dataframe
hist(SonjasDataNoOutliers$age)
boxplot(SonjasDataNoOutliers$age)   # HA! Awesome!  Cleaner data = better math!

