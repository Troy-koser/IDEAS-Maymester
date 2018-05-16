###Author: Troy Koser###
###05/14/2018###
###IDEAS Maymester Modeling###
library(tidyverse)
library(magrittr)
library(GGally)
ld<-read_csv('ld.prism.pop.csv')
#Exercise 4.1: Load in lyme data ^
head(ld)
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","size","cases"))
#Exercise 4.2: Ggpairs 4x4 summary plot w/ climate data and cases
library(dplyr)
ld.prism.pop %<>% 
  mutate(log10size=log10(size))
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","log10size","cases"))
ld.prism.pop %<>% 
  mutate(log10cases=log10(cases+1))
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","log10size","log10cases"))
#Exercise 4.3: Plots with log10 function applied to size and cases.  Cases needed to have 1 added because you can not take the log of 0.

###Simple Linear Model###
x<-ld.prism.pop %>% sample_n(100)
set.seed(222); ld.prism.pop %>% sample_n(100)
ldplot<-ggplot(data=x)
ldplot +
  geom_point(aes(prcp, avtemp))
#Exercise 4.4: Point plot of precipitation and average temp from 100 sample subsample of ld.prism.pop
ldplot +
  geom_point(aes(prcp, avtemp))+
  geom_smooth(aes(prcp, avtemp), method='lm')
#Exercise 4.5: Point plot of precipitation and average temp from 100 sample subsample of ld.prism.pop w/ line + 95% CI
ldModel<-lm(avtemp ~ prcp, data=x)
summary(ldModel)
#Exercise 4.6: Generate model and summary of model for prcp and avtemp.
summary(ldModel)$coefficients[2,1]
#Slope of the line = 0.0028289
summary(ldModel)$coefficients[2,4]
#P value = 0.005195569
#Exercise 4.7: The p=value extracted is significantly different from 0.

###Modelr Package###
ld.prism.pop %>% group_by(year) %>% summarize(total=sum(size)) %>%
  ggplot(.)+geom_point(aes(x=year,y=total))
#Exercise 4.8: Generated a point plot with total of the sizes for each year.
by.state<-ld.prism.pop %>% group_by(state) 
View(by.state)
#Exercise 4.9: Grouped the data set by state
by.state %<>% nest
by.state
#Exercise 4.10: Nested by state grouping
by.state$data[[10]]
#Exercise 4.11: Called for the county data nested in the Georgia set (10th) line
LGmodel <- function(by.state){
  lm(size ~ year, data = by.state)
}
LGmodel
#Exercise 4.12: Create function that uses a data frame for a model
models <- purrr::map(by.state$data, LGmodel)
by.state %<>% mutate(model = map(data, LGmodel))
#Exercise 4.13: Added column with lgmodel data
library(modelr)
by.state %<>% mutate(resids = map2(data, model, add_residuals))
#Exercise 4.14: Residuals seem to be a duplicate of the data column of counties.Structure: list of counties
sum_resids <- function(x){
  sum(abs(x$resid))
}
by.state %<>% mutate(totalResid = map(resids,sum_resids))
#Exercise 4.15: Created column with total resids added together for each state and each year (absolute value added)
get_slope <- function(model){
  model$coefficients[2]
}
by.state %<>% mutate(slope = purrr::map(model, get_slope))
#Exercise 4.16: Ran model for each state and created a new column with coefficients for each state.
slopes <- unnest(by.state, slope)
totalResid <- unnest(by.state, totalResid)
slopes %>% ggplot(aes(state,slope))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Exercise 4.17: Plot the coefficients for each state
totalResid %>% ggplot(aes(state,totalResid))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Exercise 4.18: Plot the totalResids for each state.
by.state2<-ld.prism.pop %>% group_by(state)
by.state2 %<>% nest
LGmodel <- function(by.state2){
  lm(size ~ year, data = by.state2)
}
models <- purrr::map(by.state2$data, LGmodel)
by.state2 %<>% mutate(model = map(data, LGmodel))
by.state2 %<>% mutate(resids = map2(data, model, add_residuals))
sum_resids <- function(x){
  sum(abs(x$resid))
}
by.state2 %<>% mutate(totalResid = map(resids,sum_resids))
get_slope <- function(model){
  model$coefficients[2]
}
by.state2 %<>% mutate(slope = purrr::map(model, get_slope))
slopes <- unnest(by.state, slope)
totalResid <- unnest(by.state, totalResid)
slopes %>% ggplot(aes(state,slope))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
totalResid %>% ggplot(aes(state,totalResid))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Exercise 4.19: Slope and Total Resids of each state but with by.state2
runCor <- function(df){
  suppressWarnings(cor.test(df$cases,df$prcp,method="spearman")$estimate)
}
by.state2 %<>% mutate(spCor = purrr::map(data, runCor))
spCors <- unnest(by.state2,spCor)
spCors %<>% arrange(desc(spCor))
spCors$state <- factor(spCors$state, levels=unique(spCors$state))
ggplot(spCors,aes(state,spCor))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#Exercise 4.20: spearman correlation coefficient plotted for each state for lyme cases and precipitation data.  By state, how good a predicting variable is precipitation for lyme case numbers