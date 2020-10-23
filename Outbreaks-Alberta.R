#--------------------------------------------------------------------------------#
# OUTBREAKS-ALBERTA.R
# AUTHOR: ARYN TOOMBS
# DATE: 2020-10-16
# REQUIRED LIBRARIES: TIDYVERSE, READXL, ZOO, LUBRIDATE, HRBRTHEMES, SCALES
# DATA SOURCE: GOVERNMENT OF ALBERTA, SUPPORT OUR STUDENTS, OTHERS
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

library(tidyverse)        #FOR WORKING WITH DATA
library(readxl)           #READ IN EXCEL DATA
library(zoo)              #NEEDED TO MUTATE DATES
library(lubridate)        #MAKES WORKING WITH DATES EASIER
library(hrbrthemes)       #THEME FOR CHART
library(scales)           #MAKES WORKING WITH THE DATE SCALES FOR THE CHART EASIER

#GATHER TODAY's DATE
todayd <- format(Sys.time(), "%Y-%m-%d")

#GATHER THE OUTBREAKS AND MUTATE TO TURN DATE INTS INTO DATE VALUES
Outbreaks_Alberta <- read_excel("Data/Outbreaks-Alberta.xlsx", 
                                col_types = c("text", "text", "text", "numeric",
                                              "text", "text", "date", "date")) %>%
                     filter(is.na(NAME) != TRUE) %>%
                     mutate(END_DATE = ifelse(is.na(END_DATE),as.Date(todayd),as.Date(END_DATE)))

#GATHER THE LONG SCHOOL DATA
Covid_Schools_Alberta <- read_excel("Data/Covid-Schools-Alberta.xlsx", 
                                    col_types = c("date", "text", "text", 
                                                  "skip", "text", "skip", "skip", "skip", 
                                                  "skip", "skip", "numeric", "numeric", 
                                                  "skip"))

#GENERATE DATE SEQUENCE UP TO TODAY
Date_Sequence <- seq(from = as.Date("2020-09-01"),
                                    to = as.Date(todayd),
                                    by="day")

OB_Schools_Levels <- data.frame(START = c("Outbreak","Outbreak 2"),
                                END = c("Outbreak Ended","Outbreak Ended 2"))

#CREATE A NULL VARIABLE TO PUT ALL OF THE SCHOOL OUTBREAK DATA INTO
Outbreak_Schools <- NULL

#FOR ALL OF THE OUTBREAKS, INCLUDING REPEATING OUTBREAKS
for(i in 1:length(OB_Schools_Levels$START)) {
  
  #CREATE A HOLDING DATA FRAME FOR THE LEVEL OF OUTBREAK (IE FIRST, SECOND, ETC.)
  Outbreak_School_Level <- Covid_Schools_Alberta %>% 
    filter(TYPE == OB_Schools_Levels$START[i] | TYPE == OB_Schools_Levels$END[i]) %>%
    
    #TRANSFORM OUR DATE INTO A CHRACTER FORMAT TO PRESERVE Y-M-D FORMAT
    mutate(DATE_CHR = as.character(DATE)) %>%
    #CAST OUT THE DATES SO THAT THE FORMAT MATCHES THE DATA FOR ALL OTHER OUTBREAKS
    pivot_wider(id_cols=c(LOCATION,SCHOOL), names_from=TYPE, values_from=DATE_CHR) %>%
    rename(START_DATE = OB_Schools_Levels$START[i])

  #IF THE LEVEL OF OUTBREAK HAS HAD ANY END, WE DON'T NEED TO ADD A END_DATE COLUMN
  #BUT IF THERE HASN'T BEEN AN OUTBREAK END YET WE NEED TO MUTATE THE DATA AS THERE
  #WILL BE NO OUTBREAK ENDED .x. VALUE AS A COLUMN WHEN THE LONG DATA IS CAST INTO WIDE
  if (OB_Schools_Levels$END[i] %in% colnames(Outbreak_School_Level)) {
    Outbreak_School_Level <- Outbreak_School_Level %>%
      rename(END_DATE = OB_Schools_Levels$END[i])
  } else {
    Outbreak_School_Level <- Outbreak_School_Level %>%
      mutate(END_DATE = NA)
  }
  
  #CLEAN UP THE DATA TO MATCH THE OTHER OUTBREAK FORMAT
  Outbreak_School_Level <- Outbreak_School_Level %>%
    #ADD A COUNT OF THE OUTBREAK (FOR REPEATED LOCATIONS)
    mutate(OUTBREAK_NUM = i) %>%
    #RENAME SCHOOL TO NAME TO MATCH FINAL FORMAT
    rename(NAME = SCHOOL) %>%
    #AS ZONES ARE NOT RECORDED (YET), USE OTHER AS A PLACEHOLDER FOR NOW
    mutate(ZONE = "OTHER") %>%
    #RENAME LOCATION TO MUNI TO MATCH FINAL FORMAT
    rename(MUNI = LOCATION) %>%
    #GIVE THE TYPE AND SHORT_TYPE SCHOOL TO DIFFERENTIATE FROM OTHER OUTBREAK TYPES
    mutate(TYPE = "School") %>%
    mutate(SHORT_TYPE = "School") %>%
    #PROCESS THE DATES INTO A NICE EASY TO USE DATE FORMAT, AND FILL EMPTY HOLES
    #WITH TODAY'S DATE
    mutate(START_DATE = as.Date(START_DATE)) %>%
    mutate(END_DATE = ifelse(is.na(END_DATE),as.Date(todayd),as.Date(END_DATE))) %>%
    mutate(END_DATE = as.Date(END_DATE))
  
  if(is.null(Outbreak_Schools)) {
    Outbreak_Schools <- Outbreak_School_Level
  } else {
    Outbreak_Schools <- Outbreak_Schools %>% full_join(Outbreak_School_Level)
  }
}



#CREATE AN EMPTY DATA FRAME TO PUT DATA INTO
Outbreak_Dates <- data.frame(ZONE = NA, MUNI=NA, NAME=NA, SHORT_TYPE=NA, DATE=NA)

#FOR EACH OF OUR SEQUENCE DATES, SELECT THE OUTBREAKS THAT ARE CURRENT FOR THAT DATE
#THEN JOIN THOSE OUTBREAKS WITH THE CURRENT DATE AND ADD THAT TO THE TOTAL LIST
#FOR FURTHER PROCESSING LATER - PURPOSELY DUPLICATES DATA FOR EACH DAY AS SOME
#INEFFICENCY HERE WILL MAKE DOING DATA CALCULATIONS OF DIFFERENT SORTS EASIER LATER
for(i in 1:length(Date_Sequence)) {
  Daily_Outbreak <- Outbreaks_Alberta %>% 
                      filter(Date_Sequence[i] >= START_DATE) %>%
                      filter(Date_Sequence[i] <= END_DATE) %>%
                      select(-START_DATE, -END_DATE) %>%
                      mutate(DATE = Date_Sequence[i])
  
  Daily_School_Outbreak <- Outbreak_Schools %>%
                      filter(Date_Sequence[i] >= START_DATE) %>%
                      filter(Date_Sequence[i] <= END_DATE) %>%
                      select(-START_DATE, -END_DATE) %>%
                      mutate(DATE = Date_Sequence[i])
                      
  #DON'T ADD IF THERE ARE NO OUTBREAKS
  if(length(Daily_Outbreak$DATE) > 0) {
    Outbreak_Dates <- Outbreak_Dates %>% full_join(Daily_Outbreak)
  }
  
  #DON'T ADD IF THERE ARE NO SCHOOL OUTBREAKS
  if(length(Daily_School_Outbreak$DATE > 0)) {
    Outbreak_Dates <- Outbreak_Dates %>% full_join(Daily_School_Outbreak)
  }
  
}

#GET RID OF ANY VALUES WITHOUT A PROPER NAME
Outbreak_Dates <- Outbreak_Dates %>% filter(is.na(NAME) != TRUE)

#SUMMARISE THE DAILY COUNTS AND PERCENTAGES OF TOTAL DAILY SUM
Outbreak_Counts <- Outbreak_Dates %>% 
                        group_by(DATE, SHORT_TYPE) %>%
                        summarise(COUNT = n()) %>%
                        group_by(DATE) %>%
                        mutate(T_DAILY_SUM = sum(COUNT)) %>%
                        mutate(DAILY_PERC = COUNT/T_DAILY_SUM)

#PULL THE LAST DAY OF DATA AND FACTOR THE TYPE BASED ON THE COUNTS
Outbreak_End_Day <- Outbreak_Counts %>% filter(DATE == Outbreak_Counts$DATE[length(Outbreak_Counts$DATE)]) %>%
  mutate(SHORT_TYPE = fct_reorder(SHORT_TYPE, COUNT, .fun=max, desc=TRUE)) %>%
  arrange(SHORT_TYPE)

corder <- levels(Outbreak_End_Day$SHORT_TYPE) #GATHER LEVELS

#REAPPLY THE FACTOR LEVELS IN REVERSE ORDER FOR BETTER CHART DISPLAY
Outbreak_Counts$SHORT_TYPE <- factor(Outbreak_Counts$SHORT_TYPE, levels=rev(corder))
Outbreak_End_Day$SHORT_TYPE <- factor(Outbreak_End_Day$SHORT_TYPE, levels=rev(corder))


#BUILD THE TWO LINE CAPTION TEXT
capt_text <- paste("Data Source: Government of Alberta, Support Our Students (supportourstudents.ca)","\nChart created by Aryn Toombs on ",todayd,sep="")
#SET THE RIGHT FONT FOR GEOM_TEXT
update_geom_font_defaults(family=font_rc)

#HOW MUCH WE WANT TO ADJUST THE ANNOTATION TEXT UP OR DOWN
v_adj <- 25

#CREATE THE CHART
Outbreak_Counts_Area <- Outbreak_Counts %>% ggplot(aes(x=DATE, y=COUNT)) +
  #ANNOTATE BY ADDING 14 AND 30 DAY RECTANGLES
  annotate("rect",fill="#AAAAAA",color=NA,alpha=0.1,
           xmin=as.Date(todayd)-30,
           xmax=as.Date(todayd),
           ymin = 0,
           ymax = Inf) +
  annotate("rect",fill="#AAAAAA",color=NA,alpha=0.1,
           xmin=as.Date(todayd)-14,
           xmax=as.Date(todayd),
           ymin = 0,
           ymax = Inf) +
  
  #CREATE THE AREA CHART
  geom_area(aes(color=SHORT_TYPE, fill=SHORT_TYPE)) +
  
  #ANNOTATE THE TEXT AND LINES OVER TOP OF THE AREA CHART
  geom_vline(aes(xintercept=as.Date(todayd)-30), color="#999999") +
  geom_vline(aes(xintercept=as.Date(todayd)-14), color="#999999") +
  geom_text(data=Outbreak_End_Day,aes(x=as.Date(todayd)-13.5, label="Previous 2 weeks", max(Outbreak_End_Day$T_DAILY_SUM) + v_adj),hjust="left", size=3, color="#CCCCCC",key_glyph="blank")+
  geom_text(data=Outbreak_End_Day,aes(x=as.Date(todayd)-29.5, label="Previous 30 days", max(Outbreak_End_Day$T_DAILY_SUM) + v_adj),hjust="left", size=3, color="#CCCCCC",key_glyph="blank")+
  
  #PUT END NUMBERS ON CHART
  geom_text(data=Outbreak_End_Day,
            aes(x=as.Date(Outbreak_End_Day$DATE[length(Outbreak_End_Day$DATE)])+0.05, 
                y=cumsum(COUNT), 
                label=paste(format(COUNT, digits=0,nsmall=0)," (",format(DAILY_PERC*100, digits=0,nsmall=0),"%)",sep=""), 
                color=SHORT_TYPE),
            size=2.5,
            hjust="left",
            key_glyph="blank",
  ) +
  #SET COLOR AND SIZE SCALES - EXPAND X TO INCLUDE TEXT SO NOT CUTOFF
  scale_fill_brewer(palette="Set3",direction=1) +
  scale_color_brewer(palette="Set3",direction=1) +
  #MAKE THE SCALE LOOK NICER, AND EXPAND TO GIVE ROOM FOR THE END NUMBERS
  scale_x_date(breaks = breaks_pretty(10), expand=expansion(add=3)) +
  
  #SET THEME AND LABELS
  theme_ft_rc() +
  #SET THE LABELS
  labs(x="Date", y="Current total",
       title = "Covid-19 Outbreaks in Alberta",
       subtitle ="Current publicly reported outbreaks from September 1 in Alberta",
       caption=capt_text) +
  #TURN OFF VERTICAL GRID LINES
  theme(legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle=90),
        legend.position="top")

#OUTPUT AND SAVE
Outbreak_Counts_Area
ggsave(paste("graphics/covid-all-outbreaks-",todayd,".png",sep=""),width=9, height=9)
