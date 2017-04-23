######################################################
#Name: 02_data_clean.R
#Date: April 2017
#Purpose: Cleanup and format scraped data, then output to CSV
######################################################

rm(list=ls())

options(scipen=999)

library(tidyverse)
library(lubridate)

path<-'/Users/walkerag/Documents/deadspin_trends/'

######################
#LOAD RAW FILES
######################

f<-seq(100,80500,by=100)
meta<-NULL
tags<-NULL
for(i in f){
  m<-readRDS(paste0(path,"deadspin_meta_all_",i,".rds"))
  t<-readRDS(paste0(path,"deadspin_tags_all_",i,".rds"))
  meta<-rbind(meta,m)
  tags<-rbind(tags,t)
}
rm(m)
rm(t)

head(meta)
head(tags)

######################
#PREP META
######################

#Make everything character fields to start (besides ID)
meta$w<-as.character(meta$w)
meta$p<-as.character(meta$p)
meta$h<-as.character(meta$h)
meta$d<-as.character(meta$d)
meta$url<-as.character(meta$url)

#Remove the tag urls
meta<-meta[!grepl("/tag/",meta$url),]

#Remove if NA headline
meta<-meta[meta$h!="NA",]

#Remove if NA page count
meta<-meta[meta$p!="NA",]

#Format Date
meta$Date<-substr(meta$d,9,nchar(meta$d))
meta$Date <- mdy_hm(meta$Date)

#Only 2005 and after
meta<-meta[year(meta$Date)>=2005,]

#Get page views in numeric form
meta$p_last<-substring(meta$p,nchar(meta$p))
meta$p_first<-substring(meta$p,1,nchar(meta$p)-1)
meta$page_count<-0
meta[meta$p_last=="K","page_count"]<-as.numeric(meta[meta$p_last=="K","p_first"])*1000
meta[meta$p_last=="M","page_count"]<-as.numeric(meta[meta$p_last=="M","p_first"])*1000000
meta[!(meta$p_last %in% c('K','M','A')),"page_count"]<-as.numeric(meta[!(meta$p_last %in% c('K','M','A')),"p"])

#Dedupe articles based on date/headline
meta<-meta %>% group_by(h, Date) %>% mutate(row=row_number())
meta<-meta[meta$row==1,]

#Check meta unique on ID
length(unique(meta$id))==dim(meta)[1]

#Writer stats
writer_totals<-meta %>% group_by(w) %>% summarise(
  total=n()
  ,median(page_count)
  ,mean(page_count)
  ,sum(page_count)
)

#View(writer_totals)

#Standardize writer names where appropriate
meta[meta$w=='Leitch',"w"]<-"Will Leitch"
meta[meta$w=='rickchand',"w"]<-"Rick Chandler"
meta[meta$w=='DAULERIO',"w"]<-"A.J. Daulerio"
meta[meta$w=='sussman',"w"]<-"Matt Sussman"

#Keep needed columns
meta<-subset(meta,select=-c(d,p_first,p_last,row))

######################
#PREP TAGS
######################

#Keep needed columns
tags<-subset(tags,select=c(data.urlname,ID))
names(tags)<-c('Tag','id')

#Format tags
tags$tag_formatted<-toupper(tags$Tag)
tags$tag_formatted<-gsub("-", " ", tags$tag_formatted)

######################
#SAVE DATA
######################

write.csv(meta, file = paste0(path,"deadspin_meta_clean.csv"),row.names=FALSE)
write.csv(tags, file = paste0(path,"deadspin_tags_clean.csv"),row.names=FALSE)