###Author: Troy Koser###
###5/14/2018###
###IDEAS Maymester 2. Programming###
###Scripts###
read.csv('wnv.csv')
data.2<-read.csv('wnv.csv')
head(data.2)
data.2$Year<-as.factor(data.2$Year)
ggplot(data=data.2)+
  geom_histogram(mapping=aes(x=Total))+
  labs(x='Frequency', y='Total Number of Cases')
#Histogram plot for toal number of WNV cases

data.2$Total.2<-log(data.2$Total)
ggplot(data=data.2)+
  geom_histogram(mapping=aes(x=Total.2))+
  labs(x='Frequency', y='Total Number of Cases')
#Histogram plot w/ total cases log transformed

data.2$Total.3<-log10(data.2$Total)
ggplot(data=data.2)+
  geom_histogram(mapping=aes(x=Total.3))+
  labs(x='Frequency', y='Total Number of Cases')
#Histogram plot w/ total cases log(10) transformed

data.2$CFR<-(data.2$Fatal/data.2$Total)
ggplot(data=data.2)+
  geom_histogram(mapping=aes(x=CFR))+
  labs(x='Frequency', y='CFR')
#CFR by state by year

data.2$Total.verify<-(data.2$EncephMen+data.2$Fever+data.2$Other)-data.2$Total
sum(data.2$EncephMen+data.2$Fever+data.2$Other)-sum(data.2$Total)
data.2$EncephMen+data.2$Fever+data.2$Other==data.2$Total
#Checking for Total to be verified


###Functions###
data.2$neuro<-data.2$EncephMen/data.2$Total
x<-data.2$neuro
mean<-function(x){s<-sum(x)
n<-length(x)
m<-s/n
return(m)
}
SE<-function(x){sd<-sd(x)
sqrt<-sqrt(length(x))
se<-sd/sqrt
return(se)
}
x.2<-data.2$neuro[which(data.2$State=='California')]
mean(x.2)
SE(x.2)
x.3<-data.2$neuro[which(data.2$State=='Colorado')]
mean(x.3)
SE(x.3)
x.4<-data.2$neuro[which(data.2$State=='New York')]
mean(x.4)
SE(x.4)
#Mean and Standard Error turned into functions to use on subsets of State neuro invasive rate data

data.2$Mean<-mean(data.2$neuro[which(data.2$State)])
Cal.Mean<-mean(x.2)
Cal.SE<-SE(x.2)
Col.Mean<-mean(x.3)
Col.SE<-SE(x.3)
NY.Mean<-mean(x.4)
NY.SE<-SE(x.4)
Means<-c('0.512343', '0.22358', '0.8148642')
Means<-as.numeric(Means)
ggplot(data=Means)+
  geom_bar(mapping=aes(x=Means))+
  labs(x=c("California", "Colorado", "New York"), y='Neuroinvasive Rate')
# Unfinished


###Loops###
Years<-seq(1999:2007)
Years
States.reporting<-function(t){
  length(unique(data.2$Total))
}
output<-c()
for(t in 1999:2007){
  data.2$States.reporting<-c(output, States.reporting)
  return(data.2$State.reporting)
}
plot(States.reporting, output, type='p')
#Unfinished