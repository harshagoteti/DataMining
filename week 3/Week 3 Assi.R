library(nutshell)
library(lattice)
data(oj.csv)
str(oj)
summary(oj)
library(MASS)
tbl=table(oj$INCOME,oj$HVAL150)
tbl
chisq.test(tbl)
cov(oj$INCOME,oj$HVAL150)
cor(oj$INCOME,oj$HVAL150)
plot(oj$INCOME,oj$HVAL150)
summary(oj$INCOME,oj$HVAL150)
hist(oj$INCOME)
hist(oj$HVAL150)

age = oj$store
gest=oj$ETHNIC
tis=oj$SSTRVOL
cov(tis,age)
cov(gest,age)
plot(oj$store,oj$ETHNIC)
summary(gest)
summary(tis)
anyNA(oj)
plot(oj$store,oj$ETHNIC)
hist(gest)
# Producing a Pearson correlation on the entire raw dataset.
# This will generate an error because the dataset has missing values
# Note that -1 <= R == negative correlation, +1 >= R == positive correlation
#           anything around 0 == no correlation

cor(oj$store,oj$ETHNIC,use="complete.obs",method=c("pearson"))

# Producing a Pearson correlation on only the complete tuples.
# This will generate an output that is true only for row with complete information
# As we will see, the output does not indicate a strong correlation.

cor(oj$brand,oj$ETHNIC,use="complete.obs",method=c("pearson"))

# What happens if we compare gestation time and birthweight?
# Are these two attributes more or less related?

cor(births$ESTGEST,births$DBWT,use="complete.obs",method=c("pearson"))

# What happens if we compare gestation time and birthweight?
# What does the error message tell us?

cor(oj$store,oj$brand,use="complete.obs",method=c("pearson"))

# So, what kind of attribute is DMETH_REC in R?
is.integer(oj$brand)
is.null(oj$brand)
is.character(oj$brand)
is.numeric(oj$brand)
is.factor(oj$brand)
na.rm=TRUE
# The correlation formula requires a numeric attribute, so we need to transform

oj$brand=as.numeric(oj$brand)
is.numeric(oj$brand)
summary(oj$brand)

# Let's rerun the correlation formula!
# Now it works!
cor(oj$brand,oj$store,use="complete.obs",method=c("pearson"))

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
na.rm=TRUE
