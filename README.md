# deadspin_trends

Analyzing post trends from the sports blog Deadspin using R

Blog post: https://www.arandomwalker.com/blog/2017/

## Code Overview

01_deadspin_scrape.R: Uses RSelenium package to scrape Deadspin article metadata    
02_data_clean.R: Clean-up the scraped files, produce the output CSVs in ./data.    
deadspin_meta_clean.csv includes article headline, writer, date, url, and page count     
deadspin_tags_clean.csv includes tags associated with article    
03_eda.R: Analyzed cleaned up data, create plots for blog post



