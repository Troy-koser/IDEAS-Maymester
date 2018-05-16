library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)
library(readr)
library(tidyr)
###Author: Troy Koser###
###05/15/2018###
###IDEAS Maymester Data Wrangling###
ld<-read_csv('lyme.csv')
pop<-read_csv('pop.csv')
prism<-read_csv('climate.csv')
#Exercise 3.1: Load data appropriately labeled/ in tibbles w/ read_csv command ^


#Exercise 3.2: Data has redundant rows and extra data values that are conferred in other rows.  Additionally, many cells do not have data values 'NA'.


pop %<>% select(fips,starts_with("pop2"))   #Changed the data titles to start with 'pop2###'
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit #Reformatted the columns to now become rows of data by county/year, also added size column for pop data
pop %<>% mutate(year=str_replace_all(str_year,"pop","")) #Created new column for the 'year'
pop %<>% mutate(year=as.integer(year)) #changed year class to integer(?)
pop %<>% mutate(fips=str_replace_all(fips,"^0","")) #cleaned-up FIPS numbers
pop %<>% mutate(fips=as.integer(fips)) #Made the FIPS number as class integer
#Exercise 3.3: use the tidyverse package to clean-up pop data^

ld %<>% gather(starts_with("Cases2"),key="str_year",value="size") %>% na.omit
ld %<>% mutate(STCODE=as.integer(STCODE))
ld %<>% mutate(CTYCODE=as.integer(CTYCODE))
ld$STCODE.2<-ld$STCODE*1000
ld$fips <- formatC(ld$fips, width = 5)
get_sum<-function(STCODE.2,CTYCODE)
  {return(sum(STCODE.2,CTYCODE))}
ld %>% rowwise %>% 
  mutate(fips=get_sum(STCODE.2,CTYCODE))
ld %<>% rename(state=STNAME,county=CTYNAME)
#Function for creating fips column arithmatic way^

ld<-read_csv('lyme.csv')
ld %<>% gather(starts_with("Cases"),key="str_year",value="cases") #Created a new column 'str_year' with the cases columns as individual rows.
ld %<>% mutate(year=str_replace_all(str_year,"Cases","")) #Removed 'cases'
ld %<>% mutate(year=as.integer(year)) #changed the year data column to integer
ld %<>% rename(state=STNAME,county=CTYNAME) #renamed the FIPS columns
fips.builder<-function(st,ct){
  if (str_length(ct)==3){
    fips<-paste(as.character(st),as.character(ct),sep="") %>% as.integer
  }
  else if (str_length(ct)==2){
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer
  }
  else {
    3
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer
  }
  return(fips)
} #made a 3-layered ifelse function for padding the fips
ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE)) # takes about 10 seconds, applied fips function row wise
ld %<>% select(-c(STCODE,CTYCODE,str_year)) #got rid of deleterious columns
#Exercise 3.4: Function for creating fips/tidy data the function/logical way ^

ld.prism<- inner_join(ld,prism, by=c('fips'='fips', 'year'='year'))
#Exercise 3.5: Join ld and prism with matching fips^

ld.prism.pop<-inner_join(ld.prism, pop, by=c('fips'='fips', 'year'='year'))
#Exercise 3.6: Join ld.prism and pop with matching fips^

###Summary Information###
ld.prism.pop <- ld %>% ungroup %>% group_by(year) %>%
  summarize(total=sum(cases)) %>% arrange(desc(total))
#Worst year is 2009
ld.prism.pop<- ld %>% ungroup %>% group_by(state) %>%
  summarize(average=mean(cases)) %>% arrange(desc(average))
#Connecticut, Massachusetts, then New Jersey are most affected
#Exercise 3.7: Summarizing Data

###Saving data frames###
tmp<- tempfile()
save(ld.prism.pop, file="ld.prism.pop.csv")
write_csv(ld.prism.pop, 'ld.prism.pop.csv')
#Exercise 3.8: Save ld file

###Visuals!###
county_map <- map_data("county")
state_map <- map_data("state")
ag.fips <- group_by(ld.prism.pop,fips) #Grouped the large data frame by fips
ld.16y<-summarize(ag.fips,all.cases=sum(cases)) #Created new data frame with 16 years of case totals by fips
ld.16y<-left_join(select(ld.prism.pop,c(state,county,fips)),ld.16y) #joined large data frame columns giving priority to the large data set
ld.16y<-distinct(ld.16y) #Retained only unique values in this data set
ld.16y %<>% rename(region=state,subregion=county) #Renamed the state/country columns
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","") #Got rid of the 'county' title
ld.16y$region<-tolower(ld.16y$region) #regional ranking
ld.16y$subregion<-tolower(ld.16y$subregion) #subregional ranking
ld.16y %<>% mutate(log10cases=log10(1+all.cases)) #log transformed
map.ld.16y<-left_join(county_map,ld.16y) #creates a new map vector with county map joined with ld totals
ggplot(map.ld.16y)+geom_point(aes(long,lat,color=log10cases),size=0.1) +
  scale_colour_gradientn(colours=rev(rainbow(4))) #maps!
#Exercise 3.9: Making the map!


pop %<>% mutate(str_year=str_replace_all(str_year,"pop20104","pop2014")) #Created new column for the 'year'
pop %<>% mutate(year=str_replace_all(year,"20104","2014")) #Created new column for the 'year'
