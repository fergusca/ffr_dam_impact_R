####################
## CHECK ON DATA LOADING AND PROCESSING
##  River discharge measures seem low compared to what one would expect based on communication with Gunther
##  Need to see if there were some errors when loading the data or if more steps are needed to get discharge estimates at the dam
##
## 3/28/2022
## 4/10/2022
####################

remove(list=ls())
library(dplyr)
library(ggplot2)
library(GGally)
library(ggpubr)
library(explore)
library(tidyr)
library(reshape2)
library(tidyverse)

#install.packages('Rcpp')
library(Rcpp)

library(sf)

library(kableExtra)

############
## LOAD PROCESSED DATA 
##  Merged FFR and GRanD data n = 7303 observations
#dat <- read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/GRAND_FFR.csv")
# n = 759 obs
#dat <- read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/data/GRAND_FFR_proportion.csv")
#africa <- read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/data/GRAND_FFR_AFRICA.csv")

# Summed basin metrics
#basin <-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/data/BASIN_SUM_ALL.csv")
# Original Grand
grand<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/GRANDv13.csv")
# Processed GRANDFFR - OLD - don't use
dat_v2<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/GRAND_FFR_v2.csv")
#Processed GRANDFFR duplicates because MAX BAS had multiple observations for some basin IDs based onhow read data by continent - OLD DON"T USE
dat_d<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/GRANDFFR_MAX_dupl.csv")

# OLD GRAND_FFR PROPORTION dataset - Cleaned up version 4/10/22 n = 7303
dat<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/GRAND_FFR_proportion.csv")

# USE THIS ONE - GRAND_FFR_V2 PROPORTION DATASET - CLEANED GUENTHER WITH CORRECT MAX BASIN DISCHARGE 4/18/22 n = 7303
#dat <- read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/GRAND_FFR_proportion_v2.csv")
# Need to read in using read_csv to specify discharge is double not integer
dat<- read_csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/GRAND_FFR_proportion_v2.csv",
               col_types = cols(
                 DIS_AVG_LS = col_double(),
                 MAX_DIS__1 = col_double()
               ))


# EXAMINE DUPLICATED GOIDS
length(unique(grand$GRAND_ID)) #7321
length(unique(dat_v2$GRAND_ID)) # 7301
length(unique(dat_v2$GOID)) # 7238
length(unique(dat_d$GRAND_ID))
length(unique(dat_d$GOID))
length(unique(dat$GRAND_ID)) # 7301
length(unique(dat$GOID)) #7238
length(unique(dat$BAS_ID)) #1409

# RETAIN DUPLICATED BASIN ID n = 6603
# https://stackoverflow.com/questions/28244123/find-duplicated-elements-with-dplyr
dupl<-dat_d %>%
  filter(BAS_ID %in% unique(.[["BAS_ID"]][duplicated(.[["BAS_ID"]])]))%>%
  #filter(BAS_ID==2441863)%>%
  select(BAS_ID, CONTINENT, GOID,GRAND_ID, DIS_AVG_LS, DIS_AV_CMS, MAX_DIS_AV_CMS)

# RETAIN DUPLICATED GRAND_ID n = 156
# https://stackoverflow.com/questions/28244123/find-duplicated-elements-with-dplyr
dupl_grand<-dat_d %>%
  filter(GRAND_ID %in% unique(.[["GRAND_ID"]][duplicated(.[["GRAND_ID"]])]))%>%
  #filter(BAS_ID==2441863)%>%
  select(BAS_ID, CONTINENT, GOID,GRAND_ID, DIS_AVG_LS, DIS_AV_CMS, MAX_DIS_AV_CMS)

# DUPLICATED GRAND_ID in cleaned up dataset with proportions
dup_grand <- dat %>%
  filter(GRAND_ID %in% unique(.[["GRAND_ID"]][duplicated(.[["GRAND_ID"]])]))%>%
  select(BAS_ID, CONTINENT, GOID,GRAND_ID,DIS_AVG_LS, DIS_AVG__1, DIS_AV_CMS, MAX_DIS__1,DIS_AV_PROP)


#######
## REMOVE TWO GRAND_ID DUPLICATE ROWS
dat <- dat %>%
  distinct(GRAND_ID, .keep_all = TRUE)

#############
# READ PROCESSED DATA WITH PROPORTIONS
# n = 7301
#dat<-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/GRAND_FFR_proportion.csv")

# Check to see that GOIDs are correct after Guenther corrected
check<-dat%>%
  filter(GOID=="2338856")

summary(dat$DIS_AVG_LS)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.001     0.832     3.812   115.305    24.654 19533.678 
summary(dat$DIS_AV_PROP)
#Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#0.0000000 0.0006365 0.0087137 0.1171723 0.1003223 1.4000000 

# Look at dams with very high proportions of discharge n = 32
hi_prop <- dat%>%
  filter(DIS_AV_PROP>1)%>%
  select(GRAND_ID, GOID, BAS_ID, CONTINENT, DIS_AVG_LS, DIS_AVG__1, DIS_AV_CMS, MAX_DIS__1,DIS_AV_PROP) # DIS_AVG_LS_CMS MAX_DIS_AV_CMS,DIS_AV_CMS

# CONVERT FROM LS TO CMS
dat <- dat %>%
  mutate(MAX_DIS__1_CMS = MAX_DIS__1 *0.001,
         DIS_AV_PROP_ffr = DIS_AV_CMS/MAX_DIS__1_CMS)%>%
  rename(DIS_AV_PROP_grnd = DIS_AV_PROP)%>%
  mutate (DIS_AV_PCT_grnd = DIS_AV_PROP_grnd*100,
          DIS_AV_PCT_ffr = DIS_AV_PROP_ffr*100)
# CREATE NEW PROPORTION DISCHARGE CALCULATED FROM FFR DISCHARGE/MAX BASIN DISCHARGE
#dat <- dat %>%
#  mutate(DIS_AV_PROP_ffr = DIS_AV_CMS/MAX_DIS_AV_CMS)%>%
#  rename(DIS_AV_PROP_grnd = DIS_AV_PROP)%>%
#  mutate(DIS_AV_PCT_grnd = DIS_AV_PROP_grnd*100,
#         DIS_AV_PCT_ffr = DIS_AV_PROP_ffr*100)

summary(dat$DIS_AV_PROP_ffr)
summary(dat$DIS_AV_PROP_grnd)
hist(dat$DIS_AV_PCT_grnd)
hist(dat$DIS_AV_PCT_ffr)

# LOOK AT ONE-ONE PLOT OF PERCENT DISCHARGE USING FFR VS GRAND DISCHARGE MEASURES
plot(dat$DIS_AV_PCT_grnd~dat$DIS_AV_PCT_ffr)

## PRINT 1:1 plot
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/plot_discharge_compare.tiff",
     width=4, height=4, units="in", res=100)
plot(dat$DIS_AV_PCT_grnd~dat$DIS_AV_PCT_ffr)
dev.off()
# SEVERAL observations have greater FFR discharge than at the dam site - as one might expect based on Guenther's email


# Look at sites where discharge is equal to basin max discharge 
hi_pct <- dat%>%
  filter(DIS_AV_PCT_grnd>99.9)%>%
  select(GRAND_ID, GOID, BAS_ID, CONTINENT, DIS_AVG_LS_CMS, DIS_AVG_LS_CMS, 
         DIS_AV_PCT_grnd, DIS_AV_PCT_ffr, MAX_DIS__1_CMS, MAIN_HYDRO)

#########
## SUMMARIZE DISCHARGE MEASURES
#https://thatdatatho.com/easily-create-descriptive-summary-statistic-tables-r-studio/
# Save table https://stackoverflow.com/questions/60815583/how-to-save-table1-package-output-table-to-doc-format-r
library(table1)
table1::table1(~DIS_AV_CMS + DIS_AVG_LS_CMS+DIS_AV_PCT_ffr+DIS_AV_PCT_grnd+DOR+MAX_DIS__1_CMS, data=dat)

table1::label(dat$DIS_AV_CMS) <- "Discharge_CMS_ffr"
table1::label(dat$DIS_AVG_LS_CMS) <- "Discharge_CMS_grnd"
table1::label(dat$DIS_AV_PCT_ffr) <- "% Discharge_ffr"
table1::label(dat$DIS_AV_PCT_grnd) <- "% Discharge_grnd"
table1::label(dat$DOR) <- "Deg of regulation"
table1::lable(dat$SED) <- "Sediment trapping"
table1::label(dat$BB_VOL_TCM) <- "Backbone river vol"
table1::label(dat$MAX_DIS__1_CMS)<-"Max basin discharge cms"
table1::table1(~DIS_AVG_LS_CMS+DIS_AV_PCT_grnd+DOR+SED+BB_VOL_TCM | MAIN_HYDRO, data=dat) #DIS_AV_CMS + DIS_AV_PCT_ffr+

table(dat$INCLUDE)
# 1 
#7301 






dat_red<-dat%>%
  select(c(CONTINENT,MAIN_HYDRO,DIS_AVG_LS_CMS,DIS_AV_CMS,DIS_AV_PCT_grnd,DIS_AV_PCT_ffr,DOF,DOR,CSI,BB_VOL_TCM))
summary(dat_red)

melted <- melt(dat_red, id.vars=c("CONTINENT","MAIN_HYDRO"))
melted %>%
  group_by(CONTINENT,MAIN_HYDRO)%>%
  summarise(mean=mean(value),sd=sd(value),min=min(value),max=max(value))


summarise(melted, mean=mean(value),sd=sd(value),min=min(value),max=max(value))

#melted <- melt(data, id.vars=c("sex", "treatment"))
## This part is new:
library(dplyr)
grouped <- group_by(melted, sex, treatment)
summarise(grouped, mean=mean(value), sd=sd(value))


dat %>%
  summarise(mean_dis_cms = mean(DIS_AVG_LS_CMS))
mtcars %>%
  group_by(cyl) %>%
  summarise(qs = quantile(disp, c(0.25, 0.75)), prob = c(0.25, 0.75))






# SUBSET OF duplicated BASIN_ID SITES WITH DIFFERENT SUMMED DISCHARGE FOR THEIR BASIN n = 22 obs (or 11 sites)
#uniq <- dat_d %>%
#  distinct(BAS_ID, MAX_DIS_AV_CMS, .keep_all = TRUE)%>%
#  filter(BAS_ID %in% unique(.[["BAS_ID"]][duplicated(.[["BAS_ID"]])]))%>%
#  select(GRAND_ID, GOID, BAS_ID, CONTINENT, MAX_DIS_AV_CMS)
#describe(uniq)



## PROCESS DATA
# SELECTED QAQC OBSERVATIONS INC==1
#dat<- org %>%
#  filter(INC==1)

# ORDER MAIN DAM PURPOSE WITH HYDROELECTRIC LISTED IF PRIMARY OR SECONDARY USE - "Other expanded" = Other, Fisheries, Recreation
dat$MAIN_HYDRO<-ordered(dat$MAIN_HYDRO, levels=c("Hydroelectricity","Flood control","Water supply",
                                                 "Irrigation","Navigation","Other expanded", ""))
test <- dat%>%
  mutate(DIS_AV_PCT = DIS_AV_PROP*100)
summary(test$DIS_AV_PCT)
check<-test%>%
  filter(DIS_AV_PCT>100) %>%
  select(CONTINENT, BAS_ID, MAIN_HYDRO,DIS_AV_PROP, DIS_AV_CMS,SUM_DIS_AV_CMS)

# CHECK IF BASIN SUMS ARE SAME AS WHAT IS IN THE PROCESSED DATA
basin_check <- basin%>%
  filter(BAS_ID==2441863) # THIS SITE has 160% discharge bc basin sum is very small
# THERE ARE TWO OBSERVATIONS - not sure which one to use or should we sum them?
# I think when we calculated basin sums - we did it by continent
#  but some basins may overlap continents and then would have multiple records?

#####################
## REPROCESS DATA - remove basin sum from original processed data
d<- dat %>%
  select(-c(SUM_LENGTHKM,SUM_VOLUMETCM,SUM_UPLANDSKM,SUM_DIS_AV_CMS,SUM_ERO_YLD_TON,DIS_AV_PROP))

# REJOIN PROCESSED GRAND-FFR DATA AND BASIN SUMS AND SEE IF THERE ARE DUPLICATE BASINS
dat2<-d %>%
  left_join(basin, by="BAS_ID") #n = 7385 vs 7302

# RETAIN DUPLICATED GOID n = 606 (keeps all obs)
dup_goid<- dat2 %>%
  filter(GOID %in% unique(.[["GOID"]][duplicated(.[["GOID"]])]))%>%
  #filter(BAS_ID==2441863)%>%
  select(BAS_ID, CONTINENT, GOID, DIS_AV_CMS, SUM_DIS_AV_CMS)

# RETAIN OBS WITH DISTINCT GOID AND SUM_DIS_AV_CMS
uniq <- dup_goid %>%
  distinct(GOID, SUM_DIS_AV_CMS, .keep_all = TRUE)%>%
  filter(GOID %in% unique(.[["GOID"]][duplicated(.[["GOID"]])]))
describe(uniq) # There are 82 distinct GOID - but two different summed discharge 
 # values when they should be the samefor some reason?




# RETAIN DUPLICATED BASIN ID n = 6389
# https://stackoverflow.com/questions/28244123/find-duplicated-elements-with-dplyr
dupl<-dat2 %>%
  filter(BAS_ID %in% unique(.[["BAS_ID"]][duplicated(.[["BAS_ID"]])]))%>%
  #filter(BAS_ID==2441863)%>%
  select(BAS_ID, CONTINENT, GOID, DIS_AV_CMS, SUM_DIS_AV_CMS)

# REMOVE DUPLICATES n = 5773
dupl2<-test%>%
  filter(duplicated(.[c("BAS_ID","SUM_DIS_AV_CMS.x")]))%>%
  #filter(BAS_ID==2441863)%>%
  select(BAS_ID, CONTINENT, GOID, DIS_AV_CMS, SUM_DIS_AV_CMS.x, SUM_DIS_AV_CMS.y)


# SUBSET OF duplicated BASIN_ID SITES WITH DIFFERENT SUMMED DISCHARGE FOR THEIR BASIN n = 22 obs (or 11 sites)
uniq <- dupl %>%
  distinct(BAS_ID, SUM_DIS_AV_CMS.y, .keep_all = TRUE)%>%
  filter(BAS_ID %in% unique(.[["BAS_ID"]][duplicated(.[["BAS_ID"]])]))
describe(uniq)

# Only duplicated elements
#############
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

table(dat$CONTINENT, dat$MAIN_HYDRO)
summary(dat$DIS_AV_CMS)
names(dat)
dat.explore<-describe(dat)

## EXPORT COLUMN NAMES
colnames<-as.data.frame(names(dat))
write.csv(colnames,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/column_names.csv")


#####################
## PROBLEM - DISCHARGE VALUES DO NOT MAKE SENSE AND DOF SHOULD BE 100 wherever there is a dam
# Check discharge reported in GRanD vs discharge from FFR
dat$DIS_AVG_LS_CMS <- dat$DIS_AVG_LS*0.001

# SEE WHICH OBSV HAVE DIFFERENT DAM DISCHARGE COMPARED TO THEIR LINKED RIVER REACH DISCHARGE
dat$dis_check<-dat$DIS_AVG_LS_CMS-dat$DIS_AV_CMS

# SELECT OBSERVATIONS WITH ABSOLUTE DIFFERENCES >50 n = 1254 out of 7302
diff_dis <- dat%>%
  filter(abs(dis_check)>10)%>%
  select(GRAND_ID,GOID,BAS_ID,COUNTRY,CONTINENT,LONG_DD,LAT_DD,DIS_AVG_LS_CMS,DIS_AV_CMS,dis_check)

table(diff_dis$CONTINENT)
#Africa          Asia     Australia        Europe North America 
#   37           408            43           261           354 
#South America 
#151 
# There appear to be misalignments across continents

# CREATE A SPATIAL OBJECT SO CAN LOOK AT IN QGIS
p.sf <- st_as_sf(diff_dis, coords = c("LONG_DD", "LAT_DD"), crs = 4326) 
p.sf  

st_write(p.sf, "C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/geospatial_data/discharge_wrong.shp", driver="ESRI Shapefile")  # create to a shapefile 
#C:\Users\Owner\Dropbox\z_EmiFergus\a_Earth_Analytics_UC\a_Capstone_project\geospatial_data

#########################
## SUMMARY STATISTICS
summary(dat$DIS_AV_CMS)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000    0.0615    0.4140   51.7688    3.1235 3078.4520 

summary(dat$DIS_AV_PROP)
#Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000000 0.0000005 0.0000096 0.0098708 0.0004724 1.0000000 

###################
## DIS_AV_CMS
# Kable - html table
#https://cran.r-project.org/web/packages/kableExtra/vignettes/awesome_table_in_html.html
dat%>%
  drop_na(DIS_AV_CMS) %>%
  group_by(MAIN_HYDRO, CONTINENT)%>%
  summarise(
    n = n(),
    mean = mean(DIS_AV_CMS),
    min = min(DIS_AV_CMS),
    max = max(DIS_AV_CMS),
    mean_prop = mean(DIS_AV_PROP),
    min_prop = min(DIS_AV_PROP),
    max_prop = max(DIS_AV_PROP)) %>%
  kbl(caption = "Average discharge cms")%>%
  kable_classic(full_width = T, html_font= "Cambria")


#MAIN_HYDRO             n mean_discms min_discms max_discms mean_propdiscms   min_propdis max_propdis
#1 "Hydroelectricity"   112     271.         0.042    3078.          0.00587  0.00000000375     0.153  
#2 "Flood control"        3       2.50       0.021       7.36        0.0180   0.00000957        0.0537 
#3 "Water supply"       133       3.48       0.005     151.          0.00671  0.0000000179      0.222  
#4 "Irrigation"         357      22.1        0        2577.          0.0131   0                 1      
#5 "Other expanded"       8       0.722      0.041       2.61        0.000211 0.000000296       0.00123
#6 ""                   146       4.02       0          88.2         0.00835  0                 0.418  

# HISTOGRAMS OF DAM TYPES
p<-dat %>%
  ggplot(aes(x=DIS_AV_CMS, color=MAIN_HYDRO, fill=MAIN_HYDRO))+
  geom_histogram(alpha=0.6, binwidth=1)+
  scale_x_log10()+
  #scale_fill_viridis_b(discrete=TRUE)+
  #scale_color_viridis_b(discrete=TRUE)+
  #theme_ipsum()+
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size=8))+
  xlab("Dishcarge cm/s")+
  ylab("")+
  facet_wrap(~MAIN_HYDRO)
p

p <- data %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot( aes(x=value, color=text, fill=text)) +
  geom_histogram(alpha=0.6, binwidth = 5) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~text)
