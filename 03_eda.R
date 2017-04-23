######################################################
#Name: 03_analysis.R
#Date: April 2017
#Purpose: Analyze and plot cleaned up Deadspin data
######################################################

rm(list=ls())

options(scipen=999)

library(lubridate)
library(tidyverse)
library(extrafont)
library(scales)
library(reshape)
library(ggrepel)
library(stringr)
library(readability)
library(syllable)

path<-'/Users/walkerag/Documents/deadspin/'

######################
#READ AND FORMAT
######################

meta<-read.csv(file = paste0(path,"deadspin_meta_clean.csv"),stringsAsFactors=FALSE)
tags<-read.csv(file = paste0(path,"deadspin_tags_clean.csv"),stringsAsFactors=FALSE)

meta$page_count<-as.numeric(meta$page_count)

#Format date, create a few date variables
meta$Date <- ymd_hms(meta$Date)
meta$Hour<-hour(meta$Date)
meta$Minute<-minute(meta$Date)
meta$HourDecimal <- meta$Hour + (meta$Minute/60)
meta$Year<-year(meta$Date)
meta$Month<-month(meta$Date)
meta$Weekday<-weekdays(meta$Date)

#Replace NA writer
#These are often articles with bylines like "Deadspin Staff" that weren't picked up properly by scraper
meta[is.na(meta$w),"w"]<-"Unknown"

######################
#ANALYSIS
######################

#Common color codes
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

summary(meta$page_count)
sum(as.numeric(meta$page_count))

View(meta[order(meta$page_count,decreasing=TRUE),])

meta$YrMonth <- floor_date(meta$Date, unit="month")
meta$label<-paste0(months(meta$Date)," ",meta$Year)
time<-meta %>% group_by(label, YrMonth) %>% summarise(
  total = n()
  ,mean_views=mean(page_count)
  ,median_views=median(page_count)
)

#TOTAL ARTICLES
ggplot(data = time,
       aes(x=YrMonth, y=total)) +
  geom_line(color="#C77CFF",lwd=1.7) +
  xlab("Month-Year") +
  ylab("Total Posts") +
  ggtitle("Pulled Deadspin Posts, by Month") +
  theme(text = element_text(size = 19,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))
  ) + 
  scale_x_datetime(labels = date_format("%b-%Y"),date_breaks="2 year") 

#AVERAGE VIEWS
ggplot(data = time,
       aes(x=YrMonth, y=mean_views)) +
  geom_line(color="cyan4",lwd=1.7) +
  xlab("Month-Year") +
  ylab("Average Views Per Post") +
  ggtitle("Average Views Per Deadspin Post, by Month") +
  theme(text = element_text(size = 19,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))) + 
  geom_text(data=filter(time, label %in% c('October 2010','August 2014'))
            , aes(label=label,family="Trebuchet MS")
            ,vjust=-1,size=6) +
  scale_x_datetime(labels = date_format("%b-%Y"),date_breaks="2 year") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)
                     ,limits=c(0,150000),breaks=seq(0,150000,by=15000))

#MEDIAN VIEWS
ggplot(data = time,
       aes(x=YrMonth, y=median_views)) +
  geom_line(color="dodgerblue1",lwd=1.7) +
  xlab("Month-Year") +
  ylab("Median Views Per Post") +
  ggtitle("Median Views Per Deadspin Post, by Month") +
  theme(text = element_text(size = 19,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))
  ) + 
  scale_x_datetime(labels = date_format("%b-%Y"),date_breaks="2 year") +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)
                     ,limits=c(0,70000),breaks=c(0,10000,20000,30000,40000,50000,60000,70000))

######################
#WRITER CHARTS
######################

#Writer stats
writer_totals<-meta %>% group_by(w) %>% summarise(
  total=n()
  ,first=min(Date)
  ,last=max(Date)
)

#Remove unknown authorship
writer_totals<-writer_totals[writer_totals$w!="Unknown" & writer_totals$total>500,]

#Need to sort and treat as factor to get gantt chart to render correctly
writer_totals$writer<-paste0(writer_totals$w," (",writer_totals$total," posts)")
writer_totals<-writer_totals[order(writer_totals$first,decreasing=TRUE),]
writer_totals$writer<-factor(writer_totals$writer, levels = writer_totals$writer)

#Melt the data so every date, whether start or end, has its own row
writer_totals<-subset(writer_totals,select=c(writer,first,last))
writer.melt <- melt(data.frame(writer_totals), id="writer")

#GANTT CHART
ggplot(writer.melt, aes(x=value, y=writer)) +
  geom_line(size = 6,colour="springgreen3") +
  ggtitle("Deadspin Writer Residences (Min. 500 Posts)") +
  xlab("Date Span") + 
  ylab("") +
  theme(legend.position="none") +
  scale_x_datetime(labels = date_format("%Y"),date_breaks="1 year") + 
  theme(text = element_text(size = 19,family="Trebuchet MS")
  ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
  ,axis.title.x=element_text(margin=margin(10,0,0,0))
  ) 


#########################
#TAG CHARTS
#########################

#Get median page views by month, calculate diff
meta<-meta %>% group_by(YrMonth) %>% mutate(median_views = median(page_count))
meta$median_difference<-meta$page_count-meta$median_views

meta[meta$median_difference==max(meta$median_difference),]
meta[meta$median_difference==min(meta$median_difference),]

#Merge tags to meta data
meta_short<-subset(meta,select=c(id,median_difference,Date,h))
comb<-merge(tags,meta_short,by=c("id"))
rm(meta_short)

#Only keep past '08
comb<-comb[comb$Date>='2008-01-01',]

#Summarize by tag
tag_summ<-comb %>% group_by(tag_formatted) %>% summarize(
  articles = n()
  ,sum_diff=sum(median_difference)
  ,median_diff=median(median_difference)
)

head(tag_summ)

#Min. 100 articles
tag_summ<-tag_summ[tag_summ$articles>=100,]

#TOP TAGS
top<-tag_summ[order(tag_summ$median_diff,decreasing=TRUE),][1:40,]

ggplot(data = top,
       aes(x=articles, y=median_diff)) +
  geom_point(color="#00BA38",size=4) +
  geom_text_repel(data=top, aes(label=tag_formatted)
                  ,color="gray18"
                  ,size=7
                  ,segment.color = '#cccccc'
                  ,point.padding = unit(1.2, 'lines')
                  # Width of the line segments.
                  ,segment.size = 0.5
                  ) +
  xlab("Total Posts") +
  ylab("Median Views Above Expected") +
  ggtitle("Top 40 Tags by Median Views Above Expected (Min. 100 Posts)") +
  scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)
                     ,limits=c(0,1200),breaks=seq(0,1200,by=200)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)
                     ,limits=c(-6000,125000),breaks=seq(0,125000,by=25000)) +
  theme(text = element_text(size = 35,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))
        ) 

#BOTTOM TAGS
bottom<-tag_summ[order(tag_summ$median_diff),][1:40,]
head(bottom)
ggplot(data = bottom,
       aes(x=articles, y=median_diff)) +
  geom_point(color="#F8766D",size=4) +
  geom_text_repel(data=bottom, aes(label=tag_formatted)
                  ,color="gray18"
                  ,size=7
                  ,segment.color = '#cccccc'
                  ,point.padding = unit(1.2, 'lines')
                  # Width of the line segments.
                  ,segment.size = 0.5
  ) +
  xlab("Total Posts") +
  ylab("Median Views Above Expected") +
  ggtitle("Bottom 40 Tags by Median Views Above Expected (Min. 100 Posts)") +
  scale_x_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)
                     ,limits=c(0,2400),breaks=seq(0,2400,by=200)) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE)
                     ,limits=c(-40000,1000),breaks=seq(-40000,0,by=10000)) +
  theme(text = element_text(size = 35,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))
  ) 


#########################
#READING GRADE LEVEL
#########################

#Readability by year
read_dat<-with(meta, readability(h, grouping.var=Year))
head(read_dat)

read_dat<-read_dat[order(read_dat$Year),]
ggplot(data = read_dat,
       aes(x=Year, y=Average_Grade_Level)) +
  geom_line(color="violetred1",lwd=1.7) +
  geom_point(color="violetred1",size=3) +
  xlab("Year") +
  ylab("Grade Level") +
  ggtitle("Average Estimated Headline Grade Level, by Year") +
  theme(text = element_text(size = 19,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))
  ) +
  scale_x_continuous(breaks=seq(2005,2017,by=1)) +
  scale_y_continuous(breaks=seq(5,9,by=0.5))

#Readability by article
scores<-with(meta, readability(h, grouping.var=id))
#Remove infinite values
scores<-scores[scores$Average_Grade_Level<1000,]
#Merge to get url
text<-merge(scores,meta,by=c("id"))
text %>% group_by(Year) %>% summarize(avg=mean(Average_Grade_Level))
head(scores)
tail(scores)
meta[meta$id==51384,]
meta[meta$id==73727,]

#########################
#HEADLINE MATCHES
#########################

meta$h_char<-nchar(meta$h)
View(meta[which.max(meta$h_char),])

meta$h_char<-nchar(meta$h)
View(meta[which.min(meta$h_char),])

#Match a string
meta$h_low<-tolower(meta$h)
meta$match_count<-apply(meta[,"h_low"],1,function(x) str_count(x,"!"))
sum(meta$match_count)
View(meta[which.max(meta$match_count),])

