#--------------------------------------------------------------------------------#
# SCHOOL OUTBREAK CHANGES.R
# AUTHOR: ARYN TOOMBS
# DATE: 2020-10-16
# REQUIRED LIBRARIES: TIDYVERSE
# DATA SOURCE: GOVERNMENT OF ALBERTA SCHOOL OUTBREAK STATUS
# DATA WEBSITE: https://www.alberta.ca/schools/covid-19-school-status-map.htm
#--------------------------------------------------------------------------------#
# MIT License
#
# Copyright (c) 2020 Aryn Toombs
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#  
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#--------------------------------------------------------------------------------#

#LOAD LIBRARIES
library(tidyverse)

#--------------------------------------------------------------------------------#

#GATHER TODAY'S DATE
today_d <- format(Sys.time(), "%Y-%m-%d")

#READ IN CSV OF YESTERDAY (OR SECOND OLDEST) SCHOOL OUTBREAK LIST
covid_data_yesterday <- read_csv("covid19dataexport-schools 2020-10-14.csv") %>%
  #DESLECT X1 AS THIS IS NOT USEFUL
  select(-X1) %>%
  #RENAME PERTINANT COLUMN NAMES FOR EASIER PROCESSING
  rename(r_name = "Region name") %>%
  rename(s_name = "Schools details") %>%
  rename(s_status = "School status")

#READ IN CSV OF TODAY (OR NEWEST) SCHOOL OUTBREAK LIST
covid_data_today <- read_csv("covid19dataexport-schools 2020-10-15.csv") %>%
  select(-X1) %>%
  rename(r_name = "Region name") %>%
  rename(s_name = "Schools details") %>%
  rename(s_status = "School status")

#FILTER A DATAFRAME OF ALL OF YESTERDAY'S OUTBREAKS
outbreak_yesterday <- covid_data_yesterday %>%
  filter(str_detect(s_status,"Outbreak"))

#FILTER A DATAFRAME OF ALL OF TODAY'S OUTBREAKS
outbreak_today <- covid_data_today %>%
  filter(str_detect(s_status,"Outbreak"))

#FILTER A DATAFRAME OF ALL OF YESTERDAY'S WATCHES
watch_yesterday <- covid_data_yesterday %>%
  filter(str_detect(s_status,"Watch"))

#FILTER A DATAFRAME OF ALL OF TODAY'S WATCHES
watch_today <- covid_data_today %>%
  filter(str_detect(s_status,"Watch"))

#FIND OUT WHICH OUTBREAKS HAVE BEEN ADDED BY USING AN ANTI-JOIN TO REMOVE 
#YESTERDAY'S OUTBREAKS FROM TODAY'S OUTBREAKS
outbreak_change_add <- outbreak_today %>% 
  anti_join(outbreak_yesterday) %>%
  #ADD A CHANGE STATUS COLUMN AND POPULATE WITH 'ADD'
  mutate(ar_status = "add")

#FIND OUT WHICH WATCHES HAVE BEEN ADDED BY USING AN ANTI-JOIN TO REMOVE 
#YESTERDAY'S WATCHES FROM TODAY'S OUTBREAKS
watch_change_add <- watch_today %>% 
  anti_join(watch_yesterday) %>%
  mutate(ar_status = "add")

#FIND WHICH OUTBREAKS HAVE BEEN REMOVED BY USING AN ANTI-JOIN TO REMOVE
#TODAY'S OUTBREAKS, AND THEN REMOVE OUTBREAKS THAT HAVE BEEN UPGRADED
#TO WATCH STATUS -- FOR A COMPLETE REMOVE LIST WITH DUPLICATES REMOVE
#SECOND ANTI-JOIN
outbreak_change_remove <- outbreak_yesterday %>% 
  anti_join(outbreak_today) %>%
  anti_join(watch_change_add, by="s_name", keep=FALSE) %>%
  #ADD A CHANGE STATUS COLUMN AND POPULATE WITH 'REMOVE'
  mutate(ar_status = "remove")

#REPEAT FOR WATCHES
watch_change_remove <- watch_yesterday %>% 
  anti_join(watch_today) %>%
  anti_join(outbreak_change_add, by="s_name", keep=FALSE) %>%
  mutate(ar_status = "remove")

#FULL JOIN ALL DATA FRAMES BACK TOGETHER FOR A FULL LIST
all_changes <- watch_change_add %>% 
  full_join(watch_change_remove) %>%
  full_join(outbreak_change_add) %>%
  full_join(outbreak_change_remove) %>%
  mutate(change_date = as.Date(today_d))

#EXPORT TO A CSV FILE FOR FURTHER USE OR DISTRIBUTION
#PASTE TOGETHER A FILE NAME WITH A CURRENT DATE
write.csv(all_changes,paste("covid19dataexport-schools-changes ",today_d,".csv",sep=""), row.names = FALSE)


  