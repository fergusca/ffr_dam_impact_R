####################
## PROCESS DATA TO CALCULATE PROPORTION DISCHARGE AT DAM FROM THE BACKBONE RIVER MAX DISCHARGE
## REVISED DATASET BASED ON DISCUSSIONS WITH JEFF AND GUENTHER 7/20/2022
##  PROPORTION DISCHARGE ESTIMATES ARE >1 - WILL REACH OUT TO GUENTHER OT SEE IF CAN HELP TROUBLESHOOT
## 7/31/2022
####################

remove(list=ls())

library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggpubr) # to compare means http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/

###############
## LOAD DATA
# Need to read in using read_csv to specify discharge is double not integer
dat_org<- read_csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/GRAND_FFR_proportion_v2.csv",
                   col_types = cols(
                     DIS_AVG_LS = col_double(),
                     MAX_DIS__1 = col_double()
                   ))

# LOAD MAX BACKBONE RIVER DISCHARGE (BB_MAX_ALL)
bb_max<-read_csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/BB_MAX_ALL.csv")
str(bb_max)
names(bb_max)
head(bb_max)

bb_max_mod <- bb_max %>%
  rename(BB_ID=...1, MAXBB_DIS_CMS=DIS_AV_CMS) %>%
  filter(!row_number()%in% c(1,2))%>%
  select(BB_ID,MAXBB_DIS_CMS)
names(bb_max_mod)
head(bb_max_mod)

# REMOVE BB_MAX OBSERVATIONS WITH DUPLICATED BB_ID AND MAXBB_DIS_CMS VALUES
#  DROPS FROM 4358175 to 4357789 - drops 386 obs - issue because of how loaded FFR data using continent mask
bb_max_mod<-bb_max_mod%>%
  distinct(BB_ID,MAXBB_DIS_CMS,.keep_all=TRUE)
df %>% distinct(x, y, .keep_all = TRUE)

# MERGE BB_MAX WITH PROCESSED GRanD_FFR DATA
test<-left_join(dat_org,bb_max_mod, by="BB_ID")
length(unique(test$BB_ID))

# Look at duplicates
dupl<-test%>%
  filter(duplicated(GRAND_ID))%>%
  select(GRAND_ID)

# SELECT OBSERVATIONS WITH DUPLICATED GRAND_IDs - Some observations have two max BB discharge values 
# probably because loaded by continent and some backbone rivers cross continents - some have higher values than others
test2<-test%>%
  filter(GRAND_ID%in%dupl$GRAND_ID)%>%
  select(GRAND_ID,GOID,BB_ID,DIS_AV_CMS,MAXBB_DIS_CMS)

# REMOVE DUPLICATED VALUES AND RETAIN LARGEST MAXBB_DIS_CMS VALUE
# BY FIRST SORTING BASED ON GRAND_ID AND MAXBB_DIS_CMS (in descending order - drop last, retain first)
# https://stackoverflow.com/questions/12805964/remove-duplicates-keeping-entry-with-largest-absolute-value
#https://www.datanovia.com/en/lessons/reorder-data-frame-rows-in-r/
test3<-test%>%
  group_by(GRAND_ID)%>%
  arrange(desc(MAXBB_DIS_CMS))%>%
  filter(!duplicated(GRAND_ID))
# n = 7301 - missing 2 obs (BUT OKAY BECAUSE THESE ARE DUPLICATES GRAND_ID=5034 and 5466)

# CALCULATE PROPORTION DISCHARGE BASED ON MAX BB RIVER DISCHARGE
# FIRST CONVERT GRAND DISCHARGE DIS_AVG_LS INTO CMS by multiplying by 0.001
# Then divide discharge at dam by max bb discharge

dat<-test3%>%
  rename(DIS_BASIN_PROP=DIS_AV_PROP,MAXBAS_DIS_LS_GG=MAX_DIS__1)%>% #MAXBAS_DIS_AV=MAX_DIS_AV,MAXBAS_ERO_YL=MAX_ERO_YL,MAXBAS_UPLAND=MAX_UPLAND,
  mutate(MAXBB_DIS_CMS=as.numeric(MAXBB_DIS_CMS))%>%
  mutate(DIS_BB_PROP=DIS_AVG_LS_CMS/MAXBB_DIS_CMS)%>%
  select(!c(MAX_DIS_AV,MAX_ERO_YL,MAX_UPLAND))

summary(dat$DIS_BB_PROP)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000    0.2036    0.4715    0.8665    0.7963 1773.0379 

# SOME OBSERVATIONS HAVE PROPORTION DISCHARGES GREATER THAN 1
high<-dat%>%
  filter(DIS_BB_PROP>1) %>%
  select(GRAND_ID,RES_NAME,DAM_NAME,ALT_NAME, RIVER, ALT_RIVER,MAIN_BASIN,SUB_BASIN,
         GOID,LONG_DD,LAT_DD,CONTINENT,COUNTRY,BB_ID,
         DIS_AVG_LS_CMS,MAXBB_DIS_CMS,DIS_BB_PROP)

# There are 56 observations the proportion discharge greater than 1 - some are close to 1, but others are quite large
# Will email Guenther to see if he can help figure out what may be going on

################
## WRITE FILES FOR GUENTHER TO LOOK AT
# PROCESSED GRAND_FFR_BBID_PROP DATA
write.csv(dat,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/GRAND_BBMAX_PROP_EF.csv",row.names = FALSE)

# OBSERVATIONS WITH HIGH PROPORTION DISCHARGE
write.csv(high,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/GRAND_BBMAX_PROP_HIGH.csv",row.names = FALSE)


