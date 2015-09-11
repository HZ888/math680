
# Homework2 ZHANG Hao,260383434, BIOS601, Dr. Hanley, 2015-09-10
#Q14. a
rm(list=ls())

#getwd()
setwd("/Users/Hao/Downloads/Fall 2015/bios601/assignment/week1-1")
#C:\Users\Hao\Downloads\Fall 2015\bios601\assignment\week1-1

#########  JH DATA  

ds=read.csv("StepCounts20102011.csv", as.is=TRUE) ;

# not used, just for info
ds$y=as.numeric(substr(ds$Day,1,4))
ds$m=as.numeric(substr(ds$Day,6,7))
ds$d=as.numeric(substr(ds$Day,9,10))

 
ds$day=as.numeric( ceiling(difftime(as.Date(ds$Day),
   "2009-12-29", units="days")) )

head(ds);

ds$w=ceiling(ds$day/7)
ds$day.week=ds$day-7*(ds$w-1)
ds

#drop missing values in 2011 that does not make up a week

ds.keep<- ds[which(ds$day<=722),] 
ds.keep

dim(ds.keep)


ano<- aov(J~as.factor(w),data=ds.keep)
summary(ano)

#14.c
#pick 3 random days in 1 random week
#a)generate a random number
x1<-sample(1:104,1)
x1

newdata<-subset(ds.keep,w==x1)
newdata

x2<-sample(1:7,3)
x2

newdata.a<-subset(newdata,day.week%in%x2)
newdata.a

va<-var(newdata.a$J)
va

#b)generate a 1 random day in each of 3 random weeks
x3<-sample(1:104,1)
x4<-sample(1:7,3)

newdata.b<-subset(ds.keep,w==x3 & day.week%in%x4)
newdata.b
vb<-var(newdata.b$J)
vb
#c)generate 3 random days in each of 3 random weeks
x5<-sample(1:104,3)
x6<-sample(1:7,3)

new.data.c<-subset(ds.keep,w%in%x5 & day.week%in%x6)
new.data.c
vc<-var(new.data.c$J)
vc

