####################
## CHECK ON DATA LAODING AND PROCESSING
##  River discharge measures seem low compared to what one would expect based on communication with Gunther
##  Need to see if there were some errors when loading the data or if more steps are needed to get discharge estimates at the dam
##
## 3/28/2022
####################

library(dplyr)
library(ggplot2)
library(GGally)
library(ggpubr)
library(explore)
library(tidyr)

library(sf)

library(kableExtra)

############
## LOAD PROCESSED DATA 
##  Merged FFR and GRanD data n = 7303 observations
#dat <- read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/GRAND_FFR.csv")
# n = 759 obs
dat <- read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/data/GRAND_FFR_proportion.csv")
africa <- read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/data/GRAND_FFR_AFRICA.csv")

# Summed basin metrics
basin <-read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/data/BASIN_SUM_ALL.csv")

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
