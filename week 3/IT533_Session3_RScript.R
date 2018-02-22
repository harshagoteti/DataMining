# Data Preprocessing and Correlation

library(nutshell)
library(lattice)
data(births2006.smpl)
data(oj.csv)
births <- births2006.smpl
str(births)
summary(births)

# Let's build a chi square test to see if education and plural births are dependent or indepent
# Remember that chi square tests work with categorical variables ONLY
# Remember that p > 0.05 means the variables are dependent; P , 0.05 means independence
library(MASS)
tbl=table(births$DMEDUC,births$DPLURAL)
tbl
chisq.test(tbl)
plot(tbl)

# Let's now see whether changes in two numerical variables (maternal age and estimated gestation) are related.
# We will use covariance for this
# The result indicates a positive linear relationship between the two variables.
age = births$MAGER
gest = births$ESTGEST
cov(age,gest)

# Producing a Pearson correlation on the entire raw dataset.
# This will generate an error because the dataset has missing values
# Note that -1 <= R == negative correlation, +1 >= R == positive correlation
#           anything around 0 == no correlation
na.rm=TRUE
cor(births$MAGER,births$APGAR5,use="all.obs",method=c("pearson"))

# Producing a Pearson correlation on only the complete tuples.
# This will generate an output that is true only for row with complete information
# As we will see, the output does not indicate a strong correlation.

cor(births$MAGER,births$APGAR5,use="complete.obs",method=c("pearson"))

# What happens if we compare gestation time and birthweight?
# Are these two attributes more or less related?

cor(births$ESTGEST,births$DBWT,use="complete.obs",method=c("pearson"))

# What happens if we compare gestation time and birthweight?
# What does the error message tell us?

cor(births$MAGER,births$DMETH_REC,use="complete.obs",method=c("pearson"))

# So, what kind of attribute is DMETH_REC in R?
is.integer(births$DMETH_REC)
is.null(births$DMETH_REC)
is.character(births$DMETH_REC)
is.numeric(births$DEMETH_REC)
is.factor(births$DMETH_REC)

# The correlation formula requires a numeric attribute, so we need to transform

births$DMETH_REC=as.numeric(births$DMETH_REC)
is.numeric(births$DMETH_REC)
summary(births$DMETH_REC)

# Let's rerun the correlation formula!
# Now it works!
cor(births$MAGER,births$DMETH_REC,use="complete.obs",method=c("pearson"))

# Let's aggregate data frame births, returning means
# for numeric variables
# This line still needs debugging
aggdata<-aggregate(births, by =list(births$MAGER,births$ESTGEST), FUN=mean, na.rm=TRUE)

# Producing a scatterplot
# Use abline to build a line of best fit

plot(births$MAGER,births$APGAR5,main="Scatterplot Maternal Age vs. Apgar",xlab="Maternal Age",ylab="Apgar Values")
plot(births$MAGER,births$DMETH_REC,main="Scatterplot Maternal Age vs. Delivery Method", xlab="Maternal Age",ylab="Delivery Method")

# Regression function
reg1<- lm(births$APGAR5~births$ESTGEST)
plot(births$MAGER~births$ESTGEST)
abline(reg1)
summary(births)
plot(births$MAGERL,births$WTGAIN,main="Scatterplot Maternal Age vs. Delivery Method", xlab="DPLURAL",ylab="DBWT")
reg2<- lm(births$MAGER~births$WTGAIN)
abline(reg2)

