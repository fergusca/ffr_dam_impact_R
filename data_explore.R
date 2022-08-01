####################
## DATA EXPLORATION FOR DAM PROJECT
## 12/12/21
####################

library(dplyr)
library(ggplot2)
library(GGally)
library(ggpubr)

############
## LOAD PROCESSED DATA 
##  Merged FFR and GRanD data n = 7303 observations
dat <- read.csv("C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/Data_processing/GRAND_FFR.csv")

## PROCESS DATA
# SELECTED QAQC OBSERVATIONS INC==1
#dat<- org %>%
#  filter(INC==1)

# ORDER MAIN DAM PURPOSE WITH HYDROELECTRIC LISTED IF PRIMARY OR SECONDARY USE - "Other expanded" = Other, Fisheries, Recreation
dat$MAIN_HYDRO<-ordered(dat$MAIN_HYDRO, levels=c("Hydroelectricity","Flood control","Water supply",
                                                 "Irrigation","Navigation","Other expanded", ""))

#############
## Set font for plot
windowsFonts(RMN=windowsFont("Times New Roman")) #RMN = Times New Roman

table(dat$CONTINENT, dat$MAIN_HYDRO)

##############
## BOXPLOTS RIVER FLOW BY DAM PURPOSE
flow_bxplt<-ggplot(dat, aes(x=factor(MAIN_HYDRO), y = DIS_AV_CMS))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(MAIN_HYDRO),y=DIS_AV_CMS),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,20000),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  #scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+
  #geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(label="p.format",size=2.5,family="RMN",label.x=2)+ # KRUSKAL WALLIS GRP MEAN TEST 
  #facet_wrap(~ECOREG_rev,ncol=5)+
  #facet_wrap(Lake_Origin_mod~ECOREG_rev, ncol=5)+
  facet_wrap(~CONTINENT)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_blank(),#element_text(family="RMN"),
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    #x=expression("HydrAP rank"),
    y=expression("Mean discharge cubic m/s"))

flow_bxplt


flow_global_bxplt<-ggplot(dat, aes(x=factor(MAIN_HYDRO), y = DIS_AV_CMS))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(MAIN_HYDRO),y=DIS_AV_CMS),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,20000),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  #scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+
  geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(label="p.format",size=2.5,family="RMN",label.x=2)+ # KRUSKAL WALLIS GRP MEAN TEST 
  #facet_wrap(~ECOREG_rev,ncol=5)+
  #facet_wrap(Lake_Origin_mod~ECOREG_rev, ncol=5)+
  #facet_wrap(~CONTINENT)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_blank(),#element_text(family="RMN"),
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    #x=expression("HydrAP rank"),
    y=expression("Mean discharge cubic m/s"))

flow_global_bxplt

#########################
## PRINT BOXPLOT
tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/boxplot_flow_continent.tiff",
     width=7.5, height=5, units="in", res=300)
  flow_bxplt
dev.off()


tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Earth_Analytics_UC/a_Capstone_project/ffr_dam_impact_R/Routput/boxplot_flow_globalt.tiff",
     width=7.5, height=5, units="in", res=300)
flow_global_bxplt
dev.off()

#tiff(filename="C:/Users/Owner/Dropbox/z_EmiFergus/a_Work_computer/a_Water_Level/Analysis/a_Lake_managed_class/Routput/Pop_Estimates/HydAP_v9_lk_origin_0712_bargraph.tiff",
#     width=7.5, height=5, units="in", res=300)
#grid.arrange(p_lo_07, p_lo_12, legend, ncol=3, widths=c(3.0, 2.8, 0.9))
#dev.off()

####################
## RIVER ORDER
ggplot(dat, aes(x=RIV_ORD, y=DIS_AV_CMS, group=MAIN_HYDRO)) +
  geom_line(aes(linetype=MAIN_HYDRO, color=MAIN_HYDRO))+
  geom_point(aes(shape=MAIN_HYDRO,color=MAIN_HYDRO))+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_blank(),#element_text(family="RMN"),
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="right")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  #ggtitle("CPL")+
  labs(#title="\nAn example",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    x=expression("River order"),
    y=expression("Mean discharge cubic m/s"))

########################
## BACKBONE RIVER
bbflow_bxplt<-ggplot(dat, aes(x=factor(MAIN_HYDRO), y = BB_DIS_ORD))+#,fill=factor(YEAR))+
  geom_boxplot(aes(fill=factor(MAIN_HYDRO),y=BB_DIS_ORD),outlier.shape=NA)+#,stat="identity")+
  scale_y_continuous(trans="log10",limits=c(NA,20000),labels=function(x) format(x,scientific = FALSE))+ #,  breaks=c(25,100,500,1000,2000,3500)) +
  #scale_fill_manual(values = c("#4575b4","#74add1","#abd9e9","#e0f3f8","#fee090","#fdae61","#f46d43","#d73027"),na.value="gray")+
  #geom_jitter(width=0.1, alpha=0.3,)+ #aes(shape=factor(OUTLET_DAMS_rev))
  stat_compare_means(label="p.format",size=2.5,family="RMN",label.x=2)+ # KRUSKAL WALLIS GRP MEAN TEST 
  #facet_wrap(~ECOREG_rev,ncol=5)+
  #facet_wrap(Lake_Origin_mod~ECOREG_rev, ncol=5)+
  facet_wrap(~CONTINENT)+
  theme_bw(base_size=12)+
  theme(plot.title = element_text(family = "RMN",face="plain",size=14, hjust=0.5), 
        axis.text.x = element_text(family = "RMN", angle=45, hjust=1),
        axis.text.y = element_text(family = "RMN"),
        axis.title.y=element_text(family="RMN"),
        axis.title.x= element_blank(),#element_text(family="RMN"),
        #axis.ticks.x=element_blank(),
        panel.grid.major =  element_line(colour = NA), 
        panel.grid.minor=element_line(colour = NA),
        strip.text = element_text(family = "RMN"),
        text = element_text(family = "RMN", size=12),
        #plot.caption = element_text(family="RMN"),
        # panel.spacing = unit(c(1,1,0,4), "lines"),
        legend.position="none")+#c(0.90,0.25),#"right" # 1st = x-axis (0-1), 2nd = y-axis (0-1)
  ggtitle("Backbone")+
  labs(#title="\nAn example",
    #subtitle="using font Decima WE",
    #caption="Notes: probably compliant with the new logo font ARPA-FVG\n",
    #x=expression("HydrAP rank"),
    y=expression("Mean discharge cubic m/s"))

bbflow_bxplt
