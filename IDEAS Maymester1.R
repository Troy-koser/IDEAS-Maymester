###Author: Troy Koser###
###5/14/2018###
###IDEAS Maymester 1 Data Visualization###
read.csv('cases.csv')
data<-read.csv('cases.csv')
head(data)
class(data$onset)
data$hospitalized[890]<-c('2015-02-20')
data<-data[-471,]

library(lubridate)
data$onset2<-ymd(data$onset)
data$hospitalized2<-ymd(data$hospitalized)
class(data$onset2)
day0.0<-min(data$onset2)
#no data value appears, recognizes 1740 objects...

day0<-min(na.omit(data$onset2))
data$epi.day<-as.numeric(data$onset2-day0)
data$epi.day
#as.numeric created numeric forms of the onset data values and day0 so that an operation could be applied to them.

##### Making a Plot#######
library(ggplot2)
ggplot(data=data)+
  geom_bar(mapping=aes(x=epi.day))+
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
##not including the + merges the function commands and produces just a line of code output instead of a built figure

ggplot(data=data)+
  geom_bar(mapping=aes(x=epi.day, position=fill))+
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplot(data=data)+
  geom_bar(mapping=aes(x=epi.day, fill=country))+
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")+
  coord_flip()+
  coord_polar()
#the positions of the data points became the new 'fill' function

#######Univariate Plot######
data$infectious.period<-data$hospitalized2-data$onset2
class(data$infectious.period)
data$infectious.period<-as.numeric(data$infectious.period, units="days")
ggplot(data=data)+
  geom_histogram(aes(x=infectious.period))+
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
data$infectious.period2<-ifelse(data$infectious.period<0,0,data$infectious.period)
ggplot(data=data)+
  geom_histogram(aes(x=infectious.period2))+
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
data$infectious.period3<-ifelse(data$infectious.period>0,0,data$infectious.period)
#Frequency Plot w/o nonsocimal/negative cases ^
ggplot(data=data)+
  geom_histogram(aes(x=infectious.period3))+
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#The 'ifelse' function now shows only the infectious period cases on the negative end of the scale or the presumably nonsocimal MERS cases.
ggplot(data=data)+
  geom_density(aes(x=infectious.period2))+
  labs(x='Infectious period', y='Frequency', title='Probability density for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#Density plot ^
ggplot(data=data)+
  geom_area(stat='bin', aes(x=infectious.period2))+
  labs(x='Infectious period', y='Frequency', title='Probability density for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#Area Plot ^
ggplot(data=data)+
  geom_dotplot(binwidth=1, aes(x=infectious.period2))+
  labs(x='Infectious period', y='Frequency', title='Probability density for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#Dot Plot ^
ggplot(data=data)+
  geom_bar(aes(x=infectious.period2))+
  labs(x='Infectious period', y='Frequency', title='Probability density for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#Bar Plot ^

######Multivariate Plots#######
data$infectious.period2<-as.numeric(data$infectious.period2, units="days")
data$epi.day<-as.numeric(data$epi.day, units="days")
ggplot(data=data, mapping=aes(x='epi.day', y='infectious.period2'))+
  geom_jitter(mapping=aes(x=epi.day, y=infectious.period2))+
  labs(x='Epidemic Day', y='Infectious Period', title='Probability density for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#Multivariate Jitter Plot w/ Infectious Period and Epidemic Days ^
ggplot(data=data, mapping=aes(x='epi.day', y='infectious.period2'))+
  geom_point(mapping=aes(x=epi.day, y=infectious.period2))+
  labs(x='Epidemic Day', y='Infectious Period', title='Probability density for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#Multivariate point plot with Infectious Period and Epidemic Days ^
ggplot(data=data, mapping=aes(x=epi.day, y=infectious.period2))+
  geom_point(mapping=aes(x=epi.day, y=infectious.period2))+
  geom_smooth(method="loess")+
  labs(x='Epidemic Day', y='Infectious Period', title='Probability density for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#Multivariate Plot with smooth curve^
ggplot(data=data, mapping=aes(x=epi.day, y=infectious.period2))+
  geom_point(mapping=aes(color=country))+
  facet_wrap(~country)
  geom_smooth(method="loess")+
  labs(x='Infectious period', y='epi.day', title='Probability density for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#Multivariate plot with smooth curve by country^
  
  
####Faceting######
ggplot(data=data, mapping=aes(x=epi.day, y=infectious.period2))+
  geom_point(mapping =aes(color=country))+facet_wrap(~country)+
  scale_y_continuous(limits =c(0, 50))+
  labs(x='Epidemic day', y='Infectious period',title='MERS infectious period (positive values only) over time', caption="Data from: https://git")
  
ggplot(data=subset(data, gender %in% c('M', 'F') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'South Korea', 'Qatar', 'UAE')), mapping=aes(x=epi.day, y=infectious.period2))+
    geom_point(mapping =aes(color=country))+
      facet_wrap(gender~country)+
    scale_y_continuous(limits =c(0, 50))+
    labs(x='Epidemic day', y='Infectious period',title='MERS infectious period (positive values only) over time', caption="Data from: https://git")
#infectious period by country

###More####
data$real.death<-
data$real.death<-ifelse(data$death~NA,data$death)
data$real.death<-ifelse(data$severity~'fatal', data$death)
data$real.death<-ifelse(data$outcome~'fatal', data$outcome)
ggplot(data=data, mapping=aes(x=epi.day, y=case.fatality.rate), fill=data$death)+
  