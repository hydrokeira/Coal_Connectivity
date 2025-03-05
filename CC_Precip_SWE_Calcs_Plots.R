require(lubridate)
require(ggpubr)
require(dataRetrieval)
require(EGRET)
require(EflowStats)
require(ggplot2)
require(rcartocolor)
require(data.table)
require(dplyr)
require(forcats)

#for average Q, precip, SWE plot with 2021 as dashed line - make sure to change wd
setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek/Data")

#read in long term Coal Creek discharge (this has interpolations in it)
#q<-read.csv("CoalCreekDischarge.csv")

#read in just WY 2021 discharge from USGS
qUSGS<-readNWISDaily("09111250", "00060", "2020-04-01", "2023-11-14")
qUSGS$WYday<-get_waterYearDay(qUSGS$Date)
qUSGS$norm_date<-as.Date(paste0("2000-", format(as.Date(qUSGS$Date), "%m-%d")))

# qUSGS_cont<-data.frame(seq(min(qUSGS$Date), max(qUSGS$Date), by=1))
# colnames(qUSGS_cont)[1]<-"Date"
# 
# qUSGS_cont<-merge(qUSGS_cont, qUSGS, by="Date", all.x = T)

# qUSGS_cont<-qUSGS_cont %>%
#   mutate(Q = case_when(
#     WYday < 184 ~ NA,
#     .default = Q
#   ))

qavg<-readNWISDaily("09111250", "00060", "1990-04-01", "2023-11-14")
qavg$WYday<-get_waterYearDay(qavg$Date)
qavg$norm_date<-as.Date(paste0("2000-", format(as.Date(qavg$Date), "%m-%d")))

qavg<-aggregate(Q~norm_date, qavg, FUN=mean)


# qavg_cont<-data.frame(seq(1,366,1))
# colnames(qavg_cont)[1]<-"WYday"
# qavg_cont<-merge(qavg_cont, qavg, by="WYday", all.x = TRUE)

# qavg_cont<-qavg_cont %>%
#   mutate(Q = case_when(
#     WYday < 184 ~ NA,
#     .default = Q
#   ))


setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek/Summer")

TM<-read.csv("SFA_Syn.csv")
dates<-unique(TM$Sampling.date)
dates<-as.data.frame(as.Date(dates, "%m/%d/%y"))
colnames(dates)<-"Date"
dates<-as.data.frame(dates[complete.cases(dates$Date),])
colnames(dates)<-"Date"
dates<-merge(dates, qUSGS, by="Date")

avg_col<-c("Average"="black")

p1<-ggplot()+
  geom_line(qavg, mapping=aes(norm_date, Q, col="Average"), lwd=0.7)+
  geom_line(qUSGS, mapping=aes(norm_date, Q, col=as.character(year(Date))), lwd=0.7, alpha=0.7)+theme_bw()+
  #geom_point(dates, mapping=aes(Day, Q, col=as.character(year(Date))), size=3)+
  labs(col="Water Year", x="", y=expression("Discharge (m"^3*"/s)"), lty="Water Year", tag="a")+
  theme(text = element_text(size=20, family = "Times"), legend.position = "null")+
  scale_color_manual(values = c("#7F3C8D", "#11A579", "#3969AC", "#F2B701", "black"))+
  scale_y_continuous(labels = seq(0,8,1), breaks = seq(0,8,1), limits = c(0,8))

p1

###calculate some discharge stats###

avg_stats<-qavg %>%
  summarise(max=max(Q), lowflow=quantile(Q, 0.05))

avg_max<-qavg %>%
  slice_max(Q)

qavg %>%
  filter(Q < quantile(Q, 0.05))

annual_stats<-qavg %>%
  group_by(as.character(year(Date))) %>%
  summarise(max=max(Q, na.rm = T), lowflow=quantile(Q, 0.05, na.rm = T))

annual_stats<-subset(annual_stats, annual_stats$`as.character(year(Date))`!=2014)

sd(annual_stats$lowflow)

annual_stats$max_avg<-avg_stats$max
annual_stats$low_avg<-avg_stats$lowflow

annual_stats<-annual_stats[-5,]

annual_stats$diff_max<-((annual_stats$max-annual_stats$max_avg)/annual_stats$max_avg)*100

annual_stats$diff_lowflow<-((annual_stats$lowflow-annual_stats$low_avg)/annual_stats$low_avg)*100

qUSGS_low<- qavg %>%
  group_by(as.character(year(Date))) %>%
  filter(Q < quantile(Q, 0.05, na.rm = T)) %>%
  summarise(min_Date=min(Date, na.rm = T))

qUSGS_low$doy<-yday(as.Date(qUSGS_low$min_Date))

qUSGS_low<-subset(qUSGS_low, qUSGS_low$`as.character(year(Date))`!=2014)

qUSGS_low_Date$avg_doy<-245

qUSGS_low_Date$diff<-qUSGS_low_Date$doy-qUSGS_low_Date$avg_doy

qUSGS_max<- qavg %>%
  group_by(as.character(year(Date))) %>%
  filter(Q == max(Q, na.rm = T)) %>%
  summarise(min_Date=min(Date, na.rm = T))

qUSGS_max<-subset(qUSGS_max, qUSGS_max$`as.character(year(Date))`!=2014)

qUSGS_max$doy<-yday(as.Date(qUSGS_max$min_Date))

sd(qUSGS_max$doy)

qUSGS_max$max_avg<-avg_max$Day

qUSGS_max$diff<-qUSGS_max$Day-qUSGS_max$max_avg

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek/Data")

#read in snotel file
snotel<-read.delim("ButteSNOTEL_2024.txt", sep = ",", skip = 63)
#select columns of interest and rename
snotel<-snotel[,c(1:3)]
names(snotel)<-c("Date", "SWE", "Precip")

#format snotel columns
snotel$Date<-as.Date(snotel$Date) #convert to date format
snotel$WYday<-get_waterYearDay(snotel$Date)
snotel$norm_date<-as.Date(paste0("2000-", format(as.Date(snotel$Date), "%m-%d"))) #get water year day
snotel$WY<-get_waterYear(snotel$Date)

snotel<-subset(snotel, snotel$WY %in% c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"))

precip_sum<-snotel %>% 
  group_by(WY) %>%
  summarise(precip_sum=max(Precip)*25.4, swe_sum=max(SWE)*25.4)

#aggregate by water year day
snotelAgg<-aggregate(snotel, by=list(snotel$WYday), FUN=mean)

snotel<-subset(snotel, snotel$WY %in% c("2020", "2021", "2022", "2023"))

snotel<-snotel %>%
  mutate(norm_date=fct_reorder(as.factor(norm_date), WYday))

snotelAgg<-snotelAgg %>%
  mutate(norm_date=fct_reorder(as.factor(norm_date), WYday))

p2<-ggplot()+
  geom_line(snotelAgg, mapping=aes(WYday, SWE*25.4, col="Average"), lwd=0.7, linetype="dashed")+
  geom_line(snotelAgg, mapping=aes(WYday, Precip*25.4, col="Average"), lwd=0.7)+
  geom_line(snotel, mapping=aes(WYday, SWE*25.4, col=as.character(WY)),lwd=0.7, linetype="dashed", alpha=0.7)+
  geom_line(snotel, mapping=aes(WYday, Precip*25.4, col=as.character(WY)),lwd=0.7, alpha=0.7)+
  theme_bw()+theme(text = element_text(size=20, family = "Times"))+
  labs(col="Water Year", x="", y="Precipitation (mm)", lty="Water Year", tag = "b")+
  scale_color_manual(values = c("#7F3C8D", "#11A579", "#3969AC", "#F2B701", "black"))+
  scale_x_continuous(labels = c("Oct", "Jan", "Apr", "Jul", "Oct"), breaks = c(1, 93, 184, 275, 365))+
  scale_y_continuous(breaks = seq(0,700,100), labels = seq(0,700,100))

p2

pdf("CC_discharge_SWE_2020_2023_Horizontal.pdf", width = 14, height = 6)

ggarrange(p1, p2, nrow=1, align = "h", widths = c(0.435, 0.565))

dev.off()

####calculate some snotel metrics####

max(snotelAgg$Precip)*25.4

snotelAgg$lag<-shift(snotelAgg$SWE, 1, type = "lag")
snotelAgg$diff<-snotelAgg$SWE-snotelAgg$lag

sum(snotelAgg$diff[which(snotelAgg$diff > 0)])*25.4
  
precip_sum<-snotel %>% 
  group_by(WY) %>%
  summarise(precip_sum=max(Precip)*25.4)

sd(precip_sum$swe_sum)

precip_sum$precip_avg<-618

precip_sum$diff<-((precip_sum$precip_sum-precip_sum$precip_avg)/precip_sum$precip_avg)*100

WYs<-unique(snotel$WY)

snow_list<-list()

i=3

for (i in 1:length(WYs)) {
  
  sno_one<-subset(snotel, snotel$WY==WYs[i])
  
  sno_one$lag<-shift(sno_one$SWE, 1, type = "lag")
  sno_one$diff<-sno_one$SWE-sno_one$lag
  
  ggplot(sno_one, aes(Date, SWE))+geom_line()
  
  snow_list[[i]]<-sum(sno_one$diff[which(sno_one$diff > 0)])*25.4
  
}

snow<-data.frame(do.call(rbind, snow_list))
snow$avg<-380

sd(snow$do.call.rbind..snow_list.)
snow$WY<-WYs

all<-merge(snow, precip_sum)

all$snow_frac<-all$do.call.rbind..snow_list./all$precip_sum
all$total_rain<-all$precip_sum-all$do.call.rbind..snow_list.

sd(all$total_rain)

snow$diff<-((snow$do.call.rbind..snow_list.-snow$avg)/snow$avg)*100

peakSWE<-snotel %>%
  group_by(as.character(WY)) %>%
  filter(SWE==max(SWE))

avg_peak<-snotel %>%
  group_by(as.character(WY)) %>%
  filter(SWE==max(SWE)) %>%
  summarise(min_day=min(Date))

avg_peak$day<-yday(avg_peak$min_day)

sd(avg_peak$day)

peakSWE$SWE<-peakSWE$SWE*25.4

peakSWE$avg<-359

peakSWE$diff<-((peakSWE$SWE-peakSWE$avg)/peakSWE$avg)*100

peakSWE$Day<-yday(peakSWE$Date)
peakSWE$avg_day<-93

peakSWE$day_diff<-peakSWE$Day-peakSWE$avg_day

avg_zero_day<-"2018-06-08"

snotel_zero<-subset(snotel, snotel$SWE==0)

snotel_zero_avg<-subset(snotelAgg, snotelAgg$SWE==0)

SDD<-snotel_zero %>%
  group_by(as.character(year(Date))) %>%
  summarise(min_day=min(Date))

SDD$average<-avg_zero_day

SDD$day<-yday(SDD$min_day)

SDD<-subset(SDD, SDD$`as.character(year(Date))`!=2014)

sd(SDD$day)

SDD$avg_day<-yday(SDD$average)

SDD$Diff<-SDD$day-SDD$avg_day

tot<-bind_cols(precip_sum, snow)

tot$rain<-tot$precip_sum-tot$do.call.rbind..snow_list.

tot$rain_avg<-252

tot$diff_rain<-((tot$rain-tot$rain_avg)/tot$rain_avg)*100

tot$snow_frac<-tot$do.call.rbind..snow_list./tot$precip_sum

tot$avg_snow_frac<-367/619

tot$diff_snow_frac<-((tot$snow_frac-tot$avg_snow_frac)/tot$avg_snow_frac)*100
