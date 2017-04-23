# Deadspin Trends

Analyzing viral content using R and data from the sports blog Deadspin.com

Blog post: https://www.arandomwalker.com/blog/2017/4/21/analyzing-deadspin

## Code Overview

01_deadspin_scrape.R: Uses RSelenium package to scrape Deadspin article metadata    
02_data_clean.R: Clean-ups the scraped files, produce the output CSVs in ./data:    
	deadspin_meta_clean.csv includes article headline, writer, date, url, and page count     
	deadspin_tags_clean.csv includes tags associated with article    
03_eda.R: Exploratory analysis plus plots for blog post



