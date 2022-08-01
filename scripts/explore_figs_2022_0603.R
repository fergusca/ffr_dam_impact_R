##########################
## DAM PROJECT
## EXPLORATORY FIGURES AND ANALYSES
##
## 6/3/2022
## 7/20/2022
##########################

# OBJECTIVES: Examine whether hydroelectric dams have different relationships with rivers around the world
# Possible questions: 1) Are hydroelectric dams on larger rivers? DO they disrupt more flow?
# Distribution of discharge (raw) and proportion discharge by dam type and by continent
# Distribution of CAP_MCM: reservoir capacity by dam types and by continent
# DOR_PC: Degree of regulation % (residence time of water in the reservoir
# CATCH_SKM: Area of upstream catchment draining into the reservoir in km2 (from HydroSHEDS)

# Trends in hydroelectric dam construction?
# Barchart of number of hydroelectric dams built by year
# After meeting with Jeff and others - 
# added code to reclassify dams as primary hydroelectric, multipurpose hydroelectric
# Disaggregated Other expanded into individual components
# Distribution of dams bar chart - scale by continent and reach level so each panel adds up to 100%

###############
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





# OTHER EXPLORATION OF DATA TRYING TO FIGURE OUT WHY number of observations was off
test4<-test%>%
  filter(!BB_ID %in% test3$BB_ID)%>%
  select(GRAND_ID,GOID,BB_ID,MAXBB_DIS_CMS,DIS_AV_CMS)
  
# Duplicated GRAND obs 5034 and 5466
grand_dupl<-dat_org%>%
  filter(GRAND_ID==5034|GRAND_ID==5466)%>%
  select(GRAND_ID,GOID,BB_ID,DIS_AV_CMS,CONTINENT)


# LOAD MODIFIED DATA WITH MAX BACKBONE RIVER DISCHARGE (DIS_BB_PROP)
bb_max_prop<-read_csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/GRAND_FFR_BBMAX_proportion.csv")
str(bb_max_prop)
names(bb_max_prop)

# MAKE "-99" into NA
#https://stackoverflow.com/questions/11036989/replace-all-0-values-to-na
bb_max<-bb_max%>%
  mutate(across(where(is.numeric),~na_if(.,-99)))
summary(bb_max$YEAR_) 
summary(bb_max$DAM_HGT_M)

summary(bb_max$DIS_BB_PROP)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#0.0000    0.2036    0.4715    0.8699    0.7966 1773.0379 
# Some dams have greater individual discharge than maximum discharge on the river backbone
# Not sure why...Can we cap the max to not be less than the discharge at the dam?

test<- bb_max%>%
  filter(DIS_BB_PROP>1)%>%
  select(GOID,RIVER,MAIN_BASIN,BB_ID,BB_NAME,DIS_AVG_LS_CMS,MAX_DIS_AV_CMS,DIS_BB_PROP)

# PROCESS BB_MAX TO MERGE WITH ORIGINAL PROCESSED DATA
bb_max<- bb_max%>%
  rename(MAX_DIS_AV_LS_BB = MAX_DIS_AV,
         MAX_DIS_AV_CMS_BB=MAX_DIS_AV_CMS)%>%
  select(GOID,MAX_DIS_AV_LS_BB,MAX_DIS_AV_CMS_BB,DIS_BB_PROP)


# CREATE NEW CLASS FOR HYDROELECTRIC - MAIN AND MULTIPURPOSE_HYDRO
dat <- dat_org%>%
  mutate(MAIN_MULTI=recode(MAIN_HYDEX, Hydro_flood="Multipurpose-hydro", Hydro_irrig="Multipurpose-hydro", 
         Hydro_navig="Multipurpose-hydro",Hydro_other="Multipurpose-hydro",Hydro_water="Multipurpose-hydro"))

dat<- dat%>%
  mutate(MAIN_MULTI=case_when(MAIN_MULTI=="Flood control" ~ "Flood control",
                              MAIN_MULTI=="Hydroelectricity" ~ "Hydroelectricity",
                              MAIN_MULTI=="Irrigation" ~ "Irrigation",
                              MAIN_MULTI=="Multipurpose-hydro" ~ "Multipurpose-hydro",
                              MAIN_MULTI=="Navigation" ~ "Navigation",
                              MAIN_MULTI=="Water supply" ~ "Water supply",
                              MAIN_MULTI=="Other expanded" & MAIN_USE=="Other" ~ "Other",
                              MAIN_MULTI=="Other expanded" & MAIN_USE=="Fisheries" ~ "Fisheries",
                              MAIN_MULTI=="Other expanded" & MAIN_USE=="Recreation" ~ "Recreation"))
table(dat$MAIN_MULTI)

# GRAB VARIABLE NAMES
#var_names<-names(dat)
#write.csv(var_names,"C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/var_names.csv")

# REDUCED DATASET TO VARIABLES TO USE
#dat<-dat_org %>%
#  select(GOID,GRAND_ID, YEAR_, DAM_HGT_M, DAM_LEN_M,AREA_SKM,CAP_MCM,DEPTH_M,DOR_PC,ELEV_MASL,CATCH_SKM,
#         MULTI_DAMS,TIMELINE,
#         LONG_DD,LAT_DD,INCLUDE,
#         MAIN_USE, MAIN_HYDRO, 
#         CONTINENT,COUNTRY,DIS_AVG_LS,DIS_AVG_LS_CMS,MAX_DIS__1,DIS_AV_PROP
#         )

######################
## SUMMARY STATS
table(dat$MAIN_HYDRO)
#Flood control Hydroelectricity       Irrigation       Navigation 
#406             2476             1527               33 
#Other expanded     Water supply 
#512              789 

table(dat$MAIN_MULTI)
#Fisheries      Flood control   Hydroelectricity         Irrigation Multipurpose-hydro         Navigation 
#14                406               1817               1527                659                 33 
#Other         Recreation       Water supply 
#205                293                789 

######################
## PROCESS DATA
# REMOVE SPACES IN CONTINENT NAMES
#test<-dat
#test$CONTINENT <- str_replace_all(test$CONTINENT, c(" "="."))
table(dat$CONTINENT)
#Africa          Asia     Australia        Europe North America 
#758          2108           289          1517          2117 
#South America 
#514 
# ORDER MAIN_HYDRO CLASSES
dat$MAIN_HYDRO<- factor(dat$MAIN_HYDRO,levels=c("Hydroelectricity", "Irrigation", "Flood control",
                        "Water supply","Navigation","Other expanded"))
table(dat$MAIN_HYDRO)

dat_org$MAIN_HYDRO<- factor(dat_org$MAIN_HYDRO,levels=c("Hydroelectricity", "Irrigation", "Flood control",
                                                "Water supply","Navigation","Other expanded"))

# REVISED CLASSES
dat$MAIN_MULTI<-factor(dat$MAIN_MULTI, levels=c("Hydroelectricity","Multipurpose-hydro", "Irrigation", 
                                                "Flood control","Water supply","Navigation",
                                                "Fisheries","Recreation","Other"))
################
## MAKE LONG FORMAT DATASET TO PLOT PROPORTION (Number of dams) by dam type and continent
# https://rpubs.com/m_dev/tables_and_plots
dam_prop <- dat %>% 
  group_by(CONTINENT)%>%
  count(CONTINENT,MAIN_HYDRO) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))

dam_perc <- dat %>% 
  group_by(CONTINENT) %>%
  count(CONTINENT,MAIN_HYDRO) %>% 
  mutate(perc = prop.table(n)*100) %>%      # mutate count(n) into perc
  select(-n) #%>%                            # remove the count...
  #spread(MAIN_HYDRO, perc)                        # to spread perc by subgroup

dam_count<-dat %>%
  group_by(CONTINENT)%>%
  count(CONTINENT)

dam_ord_perc<-dat_org %>%
  group_by(CONTINENT)%>%
  count(CONTINENT,MAIN_HYDRO,RIV_ORD)%>%
  mutate(perc = prop.table(n)*100)%>%
  select(-n)

dam_ord_perc$MAIN_HYDRO<- factor(dam_ord_perc$MAIN_HYDRO,levels=c("Hydroelectricity", "Irrigation", "Flood control",
                                                        "Water supply","Navigation","Other expanded"))
# Subset RIV_ORD to most prevalent levels
dam_ord_perc<- dam_ord_perc%>%
  filter(RIV_ORD>2 & RIV_ORD<9)

############
# EXPANDED DAM CLASS AND SCALE DISTRIBUTIONS BY CONTINENT AND DISCHARGE ORDER
dam_perc_rev<-dat %>%
  group_by(CONTINENT)%>%
  count(CONTINENT, MAIN_MULTI)%>%
  mutate(perc = prop.table(n)*100)%>%
  select(-n)

dam_ord_perc_rev<-dat %>%
  group_by(CONTINENT,RIV_ORD)%>%
  count(CONTINENT, MAIN_MULTI,RIV_ORD)%>%
  mutate(perc = prop.table(n)*100)%>%
  select(-n)

# COUNT
dam_ord_rev<-dat %>%
  group_by(CONTINENT,RIV_ORD)%>%
  count(CONTINENT,RIV_ORD) #, MAIN_MULTI

# Subset RIV_ORD to most prevalent levels
dam_ord_perc_rev<- dam_ord_perc_rev%>%
  filter(RIV_ORD<9)


#####################
## PLOT SPECIFICATIONS
# Dam type color palette
# https://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=6
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# SIX CLASSES
dam_col <- c("#d73027","#fc8d59","#fee090", "#e0f3f8", "#91bfdb","#4575b4" ,"#999999") #)

# EXPANDED CLASSES
dam_col_exp <- c("#d73027","#f46d43","#fdae61","#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4","#999999")

# Specify panel to be white with outlines and no gridlines
# https://stackoverflow.com/questions/14185754/remove-strip-background-keep-panel-border

########################
## Barchart of dam types by continent
# COUNTS
g<- ggplot(dat, aes(MAIN_HYDRO, fill=MAIN_HYDRO))+
  geom_bar()+ 
  scale_fill_manual(values=dam_col)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  facet_wrap(~dat$CONTINENT, nrow=3)+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))

g

# PERCENT
gg_perc<-ggplot(dam_perc, aes(x= MAIN_HYDRO, y=perc, fill=MAIN_HYDRO))+
  geom_bar(stat='identity', position="dodge")+ # alpha=2/3 - makes translucent
  scale_fill_manual(values=dam_col)+
  facet_wrap(~CONTINENT)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))+
  labs(x='Main use', y="% dams", title="Distribution of dam types by continent")
gg_perc

# PERCENT BY RIVER REACH LEVEL
gg_reach_perc<-ggplot(dam_ord_perc, aes(x=MAIN_HYDRO, y=perc,fill=MAIN_HYDRO))+
  geom_bar(stat='identity', position='dodge') +
  scale_fill_manual(values=dam_col)+
  facet_grid(vars(RIV_ORD),vars(CONTINENT))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))+
  labs(x='Main use', y="% dams", title="Distribution of dam types by continent and reach level")

gg_reach_perc

#############
## EXPANDED DAM CLASSES
# PERCENT BY CONTINENT
# Labels for number of dams by continent
dat_text<-data.frame(
  label = c("n=758","n=2108","n=289","n=1517","n=2117","n=514"),
  CONTINENT = c("Africa","Asia","Australia","Europe","North America","South America"),
  MAIN_MULTI = c("Fisheries","Fisheries","Fisheries","Fisheries","Fisheries","Fisheries"),
  y = c(40,40,40,40,40,40)
)

gg_perc_exp<-ggplot(dam_perc_rev, aes(x= MAIN_MULTI, y=perc, fill=MAIN_MULTI))+
  geom_bar(stat='identity', position="dodge")+ # alpha=2/3 - makes translucent
  scale_fill_manual(values=dam_col_exp)+
  geom_text(dat_text, mapping =aes(x=MAIN_MULTI,y=y,label=label))+
  facet_wrap(~CONTINENT)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))+
  labs(x='Main use', y="% dams", title="Distribution of dam types by continent")
  

gg_perc_exp

# PERCENT BY RIVER REACH LEVEL
# Get the number of observations by continent and river order
#https://stackoverflow.com/questions/13239843/annotate-ggplot2-facets-with-number-of-observations-per-facet
data_text_ord <- dat %>%
  filter(RIV_ORD<9)%>%
  group_by(CONTINENT,RIV_ORD)%>%
  count(CONTINENT,RIV_ORD)%>%
  mutate(n = paste("n=",n))%>%
  mutate(MAIN_MULTI="Fisheries")%>%
  mutate(y=80)

gg_reach_exp_perc<-ggplot(dam_ord_perc_rev, aes(x=MAIN_MULTI, y=perc,fill=MAIN_MULTI))+
  geom_bar(stat='identity', position='dodge') +
  scale_fill_manual(values=dam_col_exp)+
  geom_text(data_text_ord, mapping =aes(x=MAIN_MULTI,y=y,label=n),size=3)+ # add text indicating number of observations per plot
  facet_grid(vars(RIV_ORD),vars(CONTINENT))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))+
  theme(legend.position = "bottom")+
  labs(x='Main use', y="% dams", title="Distribution of dam types by continent and reach level")

gg_reach_exp_perc

theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
      legend.position = "right")
################
## LOT OF ASIAN DAMS DO NOT HAVE A USE LISTED n = 980 out of 2108
asia_na<- dat%>%
  filter(CONTINENT=="Asia")%>%
  filter(is.na(MAIN_USE))


##############
## BOXPLOTS OF DISCHARGE BY DAM USE AND CONTINENT
discharge<-ggplot(dat, aes(x=MAIN_HYDRO, y=DIS_AVG_LS_CMS, fill=MAIN_HYDRO))+
  geom_boxplot() + #geom_jitter(width=0.1, alpha=0.1)+
  scale_y_continuous(trans = 'log10')+
  scale_fill_manual(values=dam_col)+
  facet_wrap(~CONTINENT)+
  labs(x='', y="Discharge cms", title="Mean discharge at the dam by dam type and continent")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.position = "right")

discharge

### PROPORTION DISCHARGE

prop_discharge<-ggplot(dat, aes(x=MAIN_HYDRO, y=DIS_AV_PROP*100, fill=MAIN_HYDRO))+
  geom_boxplot() + #geom_jitter(width=0.1, alpha=0.1)+
  scale_y_continuous(limits=c(0, 120))+#scale_y_continuous(trans = 'log10')+
  scale_fill_manual(values=dam_col)+
  facet_wrap(~CONTINENT)+
  stat_compare_means(label.y = 110, label.x=2, size=3)+
  labs(x='', y="% discharge", title="Percent discharge of main river intercepted by dam")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.position = "none")

prop_discharge

## PROPORTION DISCHARGE BY CONTINENT AND RIVER REACH LEVEL
#= Reduced reach levels
dat_red<-dat_org%>%
  filter(RIV_ORD>2 & RIV_ORD<8)

dat_red$MAIN_HYDRO<- factor(dat_red$MAIN_HYDRO,levels=c("Hydroelectricity", "Irrigation", "Flood control",
                                                                  "Water supply","Navigation","Other expanded"))

prop_discharge_reach<-ggplot(dat_red, aes(x=MAIN_HYDRO, y=DIS_AV_PROP*100, fill=MAIN_HYDRO))+
  geom_boxplot() + #geom_jitter(width=0.1, alpha=0.1)+
  scale_y_continuous(limits=c(0, 100))+#scale_y_continuous(trans = 'log10')+
  scale_fill_manual(values=dam_col)+
  facet_grid(vars(RIV_ORD),vars(CONTINENT))+
  #stat_compare_means(label.y = 110, label.x=2, size=3)+
  labs(x='', y="% discharge", title="Percent discharge of main river intercepted by dam")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.position = "none")

prop_discharge_reach

# Compare against mean
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/76-add-p-values-and-significance-levels-to-ggplots/
prop_discharge_mean<-ggplot(dat, aes(x=MAIN_HYDRO, y=DIS_AV_PROP*100, fill=MAIN_HYDRO))+
  geom_boxplot() + #geom_jitter(width=0.1, alpha=0.1)+
  #scale_y_continuous(trans = 'log10')+
  scale_fill_manual(values=dam_col)+
  facet_wrap(~CONTINENT)+
  geom_hline(yintercept = mean(dat$DIS_AV_PROP*100),linetype=2)+
  stat_compare_means(label.y = 120, size=3)+
  stat_compare_means(label="p.signif",method="t.test", ref.group=".all.")+  
  labs(x='', y="% discharge", title="Percent discharge of main river intercepted by dam")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.position = "none")

prop_discharge_mean

# Line plot
#perc_disch <- ggline(dat, x="MAIN_HYDRO", y="DIS_AV_PROP", add="mean_se",
#                     color="MAIN_HYDRO", palette="dam_col")+
#  stat_compare_means(label="p.signif",method = "t.test",ref.group = ".all.")
#perc_disch

####################
### DAM ATTRIBUTE BOXPLOTS BY DAM TYPE
# Drop obs missing dam attribute from 7303 to 6822
dat_proc<- dat_org%>%
  filter(DAM_HGT_M>0)

# from 7303 to 5868
dat_proc2 <- dat_org%>%
  filter(DAM_LEN_M>0)

# Dam height
ht_mean<-ggplot(dat_proc, aes(x=MAIN_HYDRO, y=DAM_HGT_M, fill=MAIN_HYDRO))+
  geom_boxplot() + #geom_jitter(width=0.1, alpha=0.1)+
  #scale_y_continuous(trans = 'log10')+
  scale_fill_manual(values=dam_col)+
  facet_wrap(~CONTINENT)+
  geom_hline(yintercept = mean(dat_proc$DAM_HGT_M),linetype=2)+
  #stat_compare_means(label.y = 370)+
  #stat_compare_means(label="p.signif",method="t.test", ref.group=".all.")+  
  labs(x='Main use', y="Dam height (m)", title="")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.position = "right")

ht_mean

# Length
length_mean<-ggplot(dat_proc2, aes(x=MAIN_HYDRO, y=DAM_LEN_M, fill=MAIN_HYDRO))+
  geom_boxplot() + #geom_jitter(width=0.1, alpha=0.1)+
  scale_y_continuous(trans = 'log10')+
  scale_fill_manual(values=dam_col)+
  facet_wrap(~CONTINENT)+
  geom_hline(yintercept = mean(dat_proc2$DAM_LEN_M),linetype=2)+
  #stat_compare_means(label.y = 10)+
  #stat_compare_means(label="p.signif",method="t.test", ref.group=".all.")+  
  labs(x='Main use', y="Dam length (m)", title="")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.position = "right")

length_mean

####################
## RESERVOIR ATTRIBUTES
# Drop observations missing reservoir area (from 7303 to 7276)
dat_proc3<-dat_org%>%
  filter(AREA_SKM>0)

# Drop observations missing reservoir capacity info (from 7303 to 7295)
dat_proc4<- dat_org%>%
  filter(CAP_MCM>0)

summary(dat_proc4$AREA_SKM)
summary(dat_proc4$DOR_PC)
summary(dat_proc4$CATCH_SKM)

# RESERVOIR AREA
res_area_mean<-ggplot(dat_proc3, aes(x=MAIN_HYDRO, y=AREA_SKM, fill=MAIN_HYDRO))+
  geom_boxplot() + #geom_jitter(width=0.1, alpha=0.1)+
  scale_y_continuous(trans = 'log10')+
  scale_fill_manual(values=dam_col)+
  facet_wrap(~CONTINENT)+
  geom_hline(yintercept = mean(dat_proc3$AREA_SKM),linetype=2)+
  #stat_compare_means(label.y = 10)+
  #stat_compare_means(label="p.signif",method="t.test", ref.group=".all.")+  
  labs(x='Main use', y="Reservoir area (skm)", title="")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.position = "right")

res_area_mean

# RESERVOIR CAPACITY
res_cap_mean<-ggplot(dat_proc4, aes(x=MAIN_HYDRO, y=CAP_MCM, fill=MAIN_HYDRO))+
  geom_boxplot() + #geom_jitter(width=0.1, alpha=0.1)+
  scale_y_continuous(trans = 'log10')+
  scale_fill_manual(values=dam_col)+
  #geom_hline(yintercept = mean(dat_proc4$CAP_MCM),linetype=2)+
  facet_wrap(~CONTINENT)+
  #stat_compare_means(label.y = 10)+
  #stat_compare_means(label="p.signif",method="t.test", ref.group=".all.")+  
  labs(x='', y="Reservoir capacity (mcm)", title="")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.position = "right")

res_cap_mean

# RESERVOIR DOR
res_dor_mean<-ggplot(dat_proc4, aes(x=MAIN_HYDRO, y=DOR_PC, fill=MAIN_HYDRO))+
  geom_boxplot() + #geom_jitter(width=0.1, alpha=0.1)+
  scale_y_continuous(trans = 'log10')+
  scale_fill_manual(values=dam_col)+
  facet_wrap(~CONTINENT)+
  #geom_hline(yintercept = mean(dat_proc4$DOR_PC),linetype=2)+
  #stat_compare_means(label.y = 10)+
  #stat_compare_means(label="p.signif",method="t.test", ref.group=".all.")+  
  labs(x='', y="Reservoir % degree of regulation", title="")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.position = "right")

res_dor_mean

# RESERVOIR DOR  by REACH LEVEL
#Subset RIV_ORD
dat_proc4_red<-dat_proc4%>%
  filter(RIV_ORD>2 & RIV_ORD<8)
res_dor_level<-ggplot(dat_proc4_red, aes(x=MAIN_HYDRO, y=DOR_PC, fill=MAIN_HYDRO))+
  geom_boxplot() + #geom_jitter(width=0.1, alpha=0.1)+
  scale_y_continuous(trans = 'log10')+
  scale_fill_manual(values=dam_col)+
  facet_grid(vars(RIV_ORD),vars(CONTINENT))+
  #facet_wrap(~RIV_ORD)+
  #geom_hline(yintercept = mean(dat_proc4$DOR_PC),linetype=2)+
  #stat_compare_means(label.y = 10)+
  #stat_compare_means(label="p.signif",method="t.test", ref.group=".all.")+  
  labs(x='', y="Reservoir degree of regulation", title="Proportion of reservoir storage capacity by total annual discharge")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.position = "none")

res_dor_level


# UPSTREAM CATCHMENT
res_catch_mean<-ggplot(dat_proc4, aes(x=MAIN_HYDRO, y=CATCH_SKM, fill=MAIN_HYDRO))+
  geom_boxplot() + #geom_jitter(width=0.1, alpha=0.1)+
  scale_y_continuous(trans = 'log10')+
  scale_fill_manual(values=dam_col)+
  facet_wrap(~CONTINENT)+
  #geom_hline(yintercept = mean(dat_proc4$DOR_PC),linetype=2)+
  #stat_compare_means(label.y = 10)+
  #stat_compare_means(label="p.signif",method="t.test", ref.group=".all.")+  
  labs(x='', y="Reservoirupstream catch (skm)", title="")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))+
  theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1),
        legend.position = "right")

res_catch_mean


####################################
## DAM TIMELINE
# Drop observations before 1900 n = 6800
dat_proc5<- dat%>%
  filter(YEAR_>1900)

# Creating column with decade
dat_decade <- dat_proc5%>%
  mutate(DECADE=floor(YEAR_/10)*10)#%>% # Create column for decade
  #filter(!is.na(MAIN_HYDRO))%>%
  #filter(MAIN_HYDRO==c("Hydroelectricity","Irrigation","Flood control","Water supply","Navigation"))

# make data into long format
dam_yr <- dat_proc5 %>% 
  group_by(CONTINENT)%>%
  count(CONTINENT,MAIN_HYDRO,YEAR_) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))

yr_dam <- ggplot(dam_yr, aes(x=YEAR_,y = n, group=MAIN_HYDRO, color=MAIN_HYDRO))+
  geom_line(size=1)+
  scale_color_manual(values=dam_col)+
  facet_wrap(~CONTINENT)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

yr_dam


# DECADE
dam_dec <- dat_decade %>% 
  group_by(CONTINENT)%>%
  count(CONTINENT,MAIN_HYDRO,DECADE) %>%            # group_by() & summarise(n = n()) are implicit
  mutate(prop = prop.table(n))

decade_dam <- ggplot(dam_dec, aes(x=DECADE,y = n, group=MAIN_HYDRO, color=MAIN_HYDRO))+
  geom_line(size=1)+
  scale_color_manual(values=dam_col)+
  facet_wrap(~CONTINENT)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA))

decade_dam


####################
## PRINT PLOTS

# BARCHARTS OF NUMBER OF DAMS BY CONTINENT
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/Barchart_numb_dam_pct.tiff",
     width=7, height=5, units="in", res=200)
gg_perc
dev.off()

#   by reach level
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/Barchart_numb_dam_reach_pct.tiff",
     width=7.5, height=5, units="in", res=200)
gg_reach_perc
dev.off()

# BOXPLOT OF DISCHARGE
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/Boxplot_discharge_cms.tiff",
     width=7, height=5, units="in", res=200)
discharge
dev.off()

tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/Boxplot_discharge_pct.tiff",
     width=7, height=5, units="in", res=200)
prop_discharge
dev.off()

# global
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/Boxplot_discharge_pct_globe.tiff",
     width=4, height=4, units="in", res=200)
prop_discharge_mean
dev.off()

# by reach level
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/Boxplot_discharge_pct_reach.tiff",
     width=7, height=7, units="in", res=200)
prop_discharge_reach
dev.off()

# BOXPLOTS OF DAM ATTRIBUTES
# STORAGE CAPACITY
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/Boxplot_stor_cap.tiff",
     width=7, height=5, units="in", res=200)
res_cap_mean
dev.off()

# DEGREE OF REGULATION (CAPACITY and DISCHARGE)
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/Boxplot_dor.tiff",
     width=7, height=5, units="in", res=200)
res_dor_mean
dev.off()

# by reach level
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/Boxplot_dor_reach.tiff",
     width=7, height=7, units="in", res=200)
res_dor_level
dev.off()


# UPSTREAM CATCHMENT AREA
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/Boxplot_upstrm_catch.tiff",
     width=7, height=5, units="in", res=200)
res_catch_mean
dev.off()

# DAM CONSTRUCTION OVER TIME
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/Dam_year.tiff",
     width=7, height=5, units="in", res=200)
yr_dam
dev.off()

# DECADE
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/Dam_decade.tiff",
     width=7, height=5, units="in", res=200)
decade_dam
dev.off()


###################
## SCATTERPLOT
# Change point shapes and colors
ggplot(dat, aes(x=CATCH_SKM, y=DIS_AVG_LS_CMS, shape=MAIN_HYDRO, color=MAIN_HYDRO)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

# Correlations
cor(dat$CATCH_SKM, dat$DIS_AVG_LS_CMS,  method = "pearson", use = "complete.obs")
#[1] 0.6097554
cor(dat$CATCH_SKM, dat$MAX_DIS__1,  method = "pearson", use = "complete.obs")
#[1] 0.05594227

# CORRELATION - % discharge and river order
ggplot(dat_org, aes(x=RIV_ORD, y=DIS_AV_PROP*100, shape=MAIN_HYDRO, color=MAIN_HYDRO)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)


##############################
## REVISED PLOTS AFTER MEETING 7/20/2022
# BARCHARTS OF NUMBER OF DAMS BY CONTINENT
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/Barchart_numb_dam_pct_expand.tiff",
     width=7, height=5, units="in", res=200)
gg_perc_exp
dev.off()

#   by reach level
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/Barchart_numb_dam_reach_pct_expand.tiff",
     width=7.5, height=5, units="in", res=200)
gg_reach_exp_perc
dev.off()

