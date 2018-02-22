Goteti<- read_excel("~/minute.xlsx")
library(nutshell)
library(lattice)
str(minute)
summary(minute)
cor(minute)
cor(Goteti$logmove,Goteti$AGE60,use="complete.obs",method=c("pearson"))
cor(Goteti$logmove,Goteti$brand,use="complete.obs",method=c("pearson"))
plot(logmove~SSTRDIST,data=minute)
boxplot(Goteti$logmove)
plot(SSTRVOL~SSTRDIST,data=minute)
plot(CPDIST5~SSTRDIST,data=minute)
plot(CPWVOL5~SSTRDIST,data=minute)
is.null(Goteti$logmove)
is.character(Goteti$logmove)
is.numeric(Goteti$logmove)
is.factor(Goteti$logmove)
is.numeric(Goteti)
Goteti$brand=as.numeric(Goteti$brand)
is.numeric(Goteti$brand)
summary(Goteti$brand)
barchart(Goteti$logmove)
m1=lm(brand~.,data=minute)
summary(m1)
m2=lm(SSTRDIST~.,data=minute)
summary(m2)
m3=lm(SSTRVOL~.,data=minute)
summary(m3)
m4=lm(CPDIST5~.,data=minute)
summary(m4)
m5=lm(CPWVOL5~.,data=minute)
summary(m5)
cor(minute)
n=length(Goteti$brand)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  m1=lm(brand~.,data=Goteti[train,])
  pred=predict(m1,newdat=Goteti[-train,])
  obs=Goteti$brand[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
n1=length(Goteti$logmove)
diff=dim(n1)
percdiff=dim(n1)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  m2=lm(logmove~.,data=Goteti[train,])
  pred=predict(m2,newdat=Goteti[-train,])
  obs=Goteti$brand[-train]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
filled.contour(volcano, color=terrain.colors, asp=1, plot.axes=contour(volcano, add=T))
persp(volcano, theta=25, phi=30, expand=0.5, col="blue")
parallelplot(~Goteti[1:4] | logmove, data=Goteti)
median(Goteti$logmove,na.rm=TRUE)
cor(Goteti)
me=mean(Goteti)
rmse=sqrt(mean(Goteti**2))
mape=100*(mean(percGoteti))
me   # mean error
rmse # root mean square error
mape 
data1=data[minus-2]
z<-lm(logmove~.,data=Goteti)
summary(z)

