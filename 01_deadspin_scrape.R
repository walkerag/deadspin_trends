######################################################
#Name: 01_deadspin_scrape.R
#Date: April 2017
#Purpose: Scrape deadspin.com by retrieving/following links using RSelenium package
######################################################

rm(list=ls())
gc()

library(RSelenium)
library(V8)
library(dplyr)

tags_all<-NULL
meta_all<-NULL
pulled<-NULL
pulled_id<-NULL

#Initial seed URLs
waiting<-c('http://adequateman.deadspin.com/im-already-old-and-its-glorious-1794195104'
           ,'http://adequateman.deadspin.com/the-evil-new-toy-fad-that-must-be-destroyed-1793990461'
           ,"http://deadspin.com/aaron-hernandez-found-not-guilty-of-double-homicide-1794338462"
           ,"http://deadspin.com/worlds-worst-columnist-under-police-investigation-for-r-1794336304"
           ,'http://deadspin.com/your-annual-reminder-that-jim-nantz-is-a-deeply-strange-1794042941'
           ,'http://deadspin.com/tony-romo-a-career-in-takes-1793988332'
           ,'http://deadspin.com/the-las-vegas-raiders-are-screwed-1793686172'
           ,'http://deadspin.com/cristiano-ronaldo-isnt-the-greatest-player-alive-and-t-1736251535'
           ,'http://deadspin.com/david-villa-chips-the-goalkeeper-from-65-yards-1794346889'
           ,'http://thestacks.deadspin.com/the-gospel-according-to-bill-hicks-1794051362'
           ,'http://thestacks.deadspin.com/mark-aguirre-was-never-going-to-fail-1793156512'
           ,'http://screengrabber.deadspin.com/dale-scott-immobilized-taken-off-field-on-stretcher-af-1794347263'
           ,'http://deadspin.com/white-sox-outfield-features-muchas-garcias-1794347147'
           ,'http://deadspin.com/deadspin-up-all-night-up-so-tight-1794342392'
           ,'http://deadspin.com/phil-jackson-says-carmelo-anthony-would-be-better-off-1794338482'
           ,'http://deadspin.com/this-ncaa-gymnast-is-doing-a-syrian-refugee-crisis-insp-1794340798'
           ,'http://deadspin.com/enjoy-a-seasons-worth-of-amazing-giannis-antetokounmpo-1794337359'
           ,'http://deadspin.com/do-you-have-what-it-takes-to-be-a-shot-caller-1793098491'
           ,'http://deadspin.com/tom-brady-s-expensive-vegan-kibble-is-bland-and-revolti-1794257647'
           ,'http://deadspin.com/we-have-made-food-porn-for-you-the-hungry-people-1787657561'
           ,'http://deadspin.com/liam-mcgeary-enjoys-violence-1788510428'
           ,'http://theconcourse.deadspin.com/hb2-is-over-but-the-north-carolina-legislature-continu-1794301327'
           ,'http://theconcourse.deadspin.com/trump-conspiracy-tweetstorms-are-the-infowars-of-the-le-1793957969'
           ,'http://theconcourse.deadspin.com/the-roman-empire-was-brought-down-by-structural-rot-1793960166'
           ,'http://theconcourse.deadspin.com/my-offer-for-the-stonyfield-yogurt-company-1793905436'
           ,'http://theconcourse.deadspin.com/at-angola-death-row-is-psychological-torture-1793819095'
           ,'http://deadspin.com/humorless-dickhole-business-writer-very-upset-about-mar-1793537062')

path<-'/Users/walkerag/Documents/deadspin_trends/'

#######################
#END USER INPUT
######################

#Leaving rsDriver runs with Chrome
rD<-rsDriver()
remDr <- rD[["client"]]
remDr$open()

#Set counter
id<-1

#Run if link available
while(length(waiting)>0){
  
  #Select first link from vector of all available links
  url<-waiting[1]
  print(url)
  waiting<-waiting[-1]
  pulled<-append(pulled,url)
  
  #Check if article URL has a numeric string longer than 6 characters (usually an article ID)
  numeric_id <- regmatches(url, gregexpr("[[:digit:]]+", url))
  numeric_id<-as.numeric(unlist(numeric_id))
  numeric_id<-unique(numeric_id[nchar(numeric_id)>=6])
  
  #Check if article's ID string matches an already pulled ID
  match<-sum(numeric_id %in% pulled_id)
  
  if(match>0){
    print("Matching ID already pulled")
  }else{
    
    #Wait a bit
    Sys.sleep(1.25)
    
    #Go to page, get source, then get HTML
    remDr$navigate(url)
    page_source<-remDr$getPageSource()
    pg <- read_html(page_source[[1]])
    
    #LINKS
    
    #Get links
    links<-html_nodes(pg,"[href*='deadspin.com/']") %>% html_nodes(xpath='@href') %>% html_text()
    
    #Remove various non-article/tag urls
    links<-links[!grepl("/amp$",links)]
    links<-links[!grepl("replies$",links)]
    links<-links[!grepl("/rss$",links)]
    links<-links[!grepl("tips@",links)]
    links<-links[!grepl("#_ga",links)]
    links<-links[!grepl("comment=",links)]
    links<-links[!grepl("comments$",links)]
    links<-links[!grepl("taboola$",links)]
    
    #Add http if needed
    links[!grepl("^http:",links)]<-paste0('http:',links[!grepl("^http:",links)])
    
    #Append links to waitlist
    waiting<-append(waiting,links)
    
    #Make sure waitlist links are distinct
    waiting<-unique(waiting)
    
    #TAGS
    
    #Get tags
    t<-html_nodes(pg, "div.post-taglist li a")
    tags<-bind_rows(lapply(xml_attrs(t), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
    if(length(tags)>0){
      tags$ID<-id
      tags_all<-rbind(tags_all,tags)
    }
    
    #METADATA
    
    #Get article writer
    w<-html_nodes(pg, "div.meta__byline a") %>% html_nodes(xpath='text()')
    w<-as.character(w)
    
    #Get article headline
    h<-html_nodes(pg, "h1.headline a") %>% html_nodes(xpath='text()')
    h<-as.character(h)
    
    #Get article date
    d<-html_nodes(pg, "a.js_publish_time") %>% html_nodes(xpath='@title')
    d<-as.character(d)
    
    #Get article pageview count
    p<-html_nodes(pg, "span.view-count")  %>% html_nodes(xpath='text()')
    p<-as.character(p)
    
    #Check for missing values
    w<-ifelse(length(w)==0,'NA',w)
    h<-ifelse(length(h)==0,'NA',h)
    d<-ifelse(length(d)==0,'NA',d)
    p<-ifelse(length(p)==0,'NA',p)
    
    #Save writer, headline, date, and pageviews, URL ID, link
    meta_all<-rbind(meta_all,data.frame(w,p,h,d,id,url))
    
    #Cleanup
    rm(tags)
    rm(w)
    rm(p)
    rm(h)
    rm(d)
    rm(url)
    rm(links)
    rm(page_source)
    rm(pg)
    rm(t)
    
    #Save to file every 100 URLs
    if(id %% 100==0){
      #Save everything
      saveRDS(tags_all, file = paste0(path,"deadspin_tags_all_",id,".rds"))
      saveRDS(meta_all, file = paste0(path,"deadspin_meta_all_",id,".rds"))
      saveRDS(waiting, file = paste0(path,"deadspin_waiting.rds"))
      saveRDS(pulled, file = paste0(path,"deadspin_pulled.rds"))
      tags_all<-NULL
      meta_all<-NULL
    }
    
    #Increment counter
    id<-id+1
    print(id)
    
    #Remove links already used from waiting
    waiting<-setdiff(waiting,pulled)
    print(length(waiting))
    
    #Collect numeric IDs of all pulled articles
    pulled_id <- regmatches(pulled, gregexpr("[[:digit:]]+", pulled))
    pulled_id<-as.numeric(unlist(pulled_id))
    pulled_id<-unique(pulled_id[nchar(pulled_id)>=6])
    
  }
  
} 

print("No more links!")

remDr$close()
# stop the selenium server
rD[["server"]]$stop() 

