library("data.table")
library("lubridate")
library("magrittr")
library("readr")
library(dplyr)
library(stringr)
#getwd()
# Read in the data. 
#dat <- fread("../../data/dqcdm-temporal-summary/dqcdm_temporal_summary_subset_2.txt")

dat <- fread("data/dqcdm-temporal-summary/dqcdm_temporal_summary.txt")

save(dat,file='data/mega.RData')
load(file='data/mega.RData')
str(dat)
# Light munging. 
dat[ , prevalence := as.double(prevalence)]
dat[ , time_period := paste0(time_period, "01")]
dat[ , time_period := ymd(time_period)]

#library(dplyr)
tta<-as.data.frame(table(dat$domain_id))


ttb<- dat %>% select(domain_id, concept_id) %>% distinct()

ttc<-as.data.frame(table(ttb$domain_id))
ttc
tte<- dat %>% group_by(domain_id) %>% group_by(time_period) %>% summarize(count=n())
  

ttd=as.data.frame(table(dat$time_period))
ttd
# Exploratory data analysis.  
# ggplot(subset(dat, concept_id==312664), aes(x=time_period , y=prevalence)) + 
#   geom_line(aes(group=1)) + 
#   geom_point() + 
#   facet_wrap(~ source_name) + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   labs(title="Influenza by Database")
# 
# ggplot(subset(dat, concept_id==312664), 
#        aes(x=time_period , y=prevalence, colour=source_name)) + 
#   geom_line(aes(group=1)) + 
#   geom_point() + 
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
#   labs(title="Influenza by Database")

library(sqldf)

obs = dat %>%  filter(domain_id=='Observation')
str_sub(obs$time_period,1,4)
#it is prevalence , so it will not work, new re-run is needed
#tte = obs %>% group_by(concept_id,str_sub(time_period,1,4)) %>% summarize(total=sum(obs$))