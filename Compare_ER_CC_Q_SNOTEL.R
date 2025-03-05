##ER, Coal Creek, SNOTEL POR comparison
CC<-readNWISDaily("09111250", "00060", "1990-04-01", "2023-11-14")
CC$WYday<-get_waterYearDay(CC$Date)
colnames(CC)[2]<-"CC_Q"

ER<-readNWISDaily("09112200", "00060", "1990-04-01", "2023-11-14")
ER$WYday<-get_waterYearDay(ER$Date)
colnames(ER)[2]<-"ER_Q"

ER_avg<-ER %>%
  group_by(WYday) %>%
  summarise(mean_q=mean(ER_Q))

ER_short<-ER %>%
  filter(get_waterYear(ER$Date) %in% seq(2015,2023,1))

ER_bold<-ER %>%
  filter(get_waterYear(ER$Date) %in% seq(2020,2023,1))

ER_avg_short<-ER_short %>%
  group_by(WYday) %>%
  summarise(mean_q=mean(ER_Q))

ER_avg_bold<-ER_bold %>%
  group_by(WYday) %>%
  summarise(mean_q=mean(ER_Q))




CC_ER<-left_join(CC[1:2], ER[1:2])
CC_ER$WYday<-get_waterYearDay(CC_ER$Date)

pdf("CC_ER_Compare.pdf", width = 8, height = 8)

ggplot(CC_ER, aes(CC_Q, ER_Q))+geom_point()+
  labs(y=expression("East River Discharge (m"^3*"/s)"),x=expression("Coal Creek Discharge (m"^3*"/s)"))+
  theme_classic()+theme(text = element_text(size=20))

dev.off()

summary(lm(ER_Q~CC_Q, CC_ER))

p1<-ggplot()+geom_line(ER, mapping=aes(WYday, ER_Q, group=get_waterYear(Date)), alpha=0.3)+
  geom_line(ER_short, mapping=aes(WYday, ER_Q, group=get_waterYear(Date)), alpha=0.7, col="blue")+
  geom_line(ER_bold, mapping=aes(WYday, ER_Q, group=get_waterYear(Date)), alpha=0.7, col="blue", lwd=1.5)+
  theme_classic()+theme(text = element_text(size=20))+
  labs(x="Day of Water Year", y=expression("East River Discharge (m"^3*"/s)"), tag="a")

p2<-ggplot()+geom_line(ER_avg, mapping=aes(WYday, mean_q))+
  geom_line(ER_avg_short, mapping=aes(WYday, mean_q),col="blue")+
  geom_line(ER_avg_bold, mapping=aes(WYday, mean_q),col="blue", lwd=1.5)+
  theme_classic()+theme(text = element_text(size=20))+
  labs(x="Day of Water Year", y=expression("East River Discharge (m"^3*"/s)"), tag="b")

p2

ER_peak<-ER %>%
  group_by(get_waterYear(Date)) %>%
  summarise(peakflow=mean(ER_Q)) %>%
  filter(`get_waterYear(Date)` != 2024)

ER_peak_short<-ER_short %>%
  group_by(get_waterYear(Date)) %>%
  summarise(peakflow=mean(ER_Q))

wilcox.test(ER_peak$peakflow, ER_peak_short$peakflow)

#format snotel columns
snotel<-read.delim("ButteSNOTEL_2024.txt", sep = ",", skip = 63)
#select columns of interest and rename
snotel<-snotel[,c(1:6)]
names(snotel)<-c("Date", "SWE", "Precip", "MaxT", "MinT", "AvgT")

snotel$Date<-as.Date(snotel$Date) #convert to date format
snotel$WYday<-get_waterYearDay(snotel$Date)
snotel$WY<-get_waterYear(snotel$Date)

snotel_short<-subset(snotel, snotel$WY %in% c("2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023"))

snotel_bold<-subset(snotel, snotel$WY %in% c("2020", "2021", "2022", "2023"))

precip_sum<-snotel %>% 
  group_by(WY) %>%
  summarise(precip_sum=max(Precip, na.rm=T)*25.4, swe_sum=max(SWE, na.rm=T)*25.4, avg_min=mean(MinT, na.rm=T),
            avg_max=mean(MaxT, na.rm=T), avg_avg=mean(AvgT, na.rm=T)) %>%
  filter(WY!=2024)

precip_sum_short<-snotel_short %>% 
  group_by(WY) %>%
  summarise(precip_sum=max(Precip, na.rm=T)*25.4, swe_sum=max(SWE, na.rm=T)*25.4, avg_min=mean(MinT, na.rm=T),
            avg_max=mean(MaxT, na.rm=T), avg_avg=mean(AvgT, na.rm=T))


wilcox.test(precip_sum$swe_sum, precip_sum_short$swe_sum)

mean(precip_sum$avg_avg[6:43])

mean(precip_sum_short$avg_avg)

snotel_avg<-snotel %>%
  group_by(WYday) %>%
  summarise(mean_swe=mean(SWE), mean_precip=mean(Precip))

snotel_avg_short<-snotel_short %>%
  group_by(WYday) %>%
  summarise(mean_swe=mean(SWE), mean_precip=mean(Precip))

snotel_avg_bold<-snotel_bold %>%
  group_by(WYday) %>%
  summarise(mean_swe=mean(SWE), mean_precip=mean(Precip)) %>%
  filter(WYday != 366)

p3<-ggplot()+geom_line(snotel, mapping=aes(WYday, SWE*25.4, group=WY), alpha=0.3)+
  geom_line(snotel_short, mapping=aes(WYday, SWE*25.4, group=WY), alpha=0.7, col="blue")+
  geom_line(snotel_bold, mapping=aes(WYday, SWE*25.4, group=WY), alpha=0.7, col="blue", lwd=1.5)+
  theme_classic()+theme(text = element_text(size=20))+
  labs(x="Day of Water Year", y="Snow Water Equivalent (mm)", tag="c")

p5<-ggplot()+geom_line(snotel, mapping=aes(WYday, Precip*25.4, group=WY), alpha=0.3)+
  geom_line(snotel_short, mapping=aes(WYday, Precip*25.4, group=WY), alpha=0.7, col="blue")+
  geom_line(snotel_bold, mapping=aes(WYday, Precip*25.4, group=WY), alpha=0.7, col="blue", lwd=1.5)+
  theme_classic()+theme(text = element_text(size=20))+
  labs(x="Day of Water Year", y="Precipitation Accumulation (mm)", tag="e")

p4<-ggplot()+geom_line(snotel_avg, mapping=aes(WYday, mean_swe*25.4))+
  geom_line(snotel_avg_short, mapping=aes(WYday, mean_swe*25.4),col="blue")+
  geom_line(snotel_avg_bold, mapping=aes(WYday, mean_swe*25.4), col="blue", lwd=1.5)+
  theme_classic()+theme(text = element_text(size=20))+
  labs(x="Day of Water Year", y="Snow Water Equivalent (mm)", tag="d")

p6<-ggplot()+geom_line(snotel_avg, mapping=aes(WYday, mean_precip*25.4))+
  geom_line(snotel_avg_short, mapping=aes(WYday, mean_precip*25.4), col="blue")+
  geom_line(snotel_avg_bold, mapping=aes(WYday, mean_precip*25.4), col="blue", lwd=1.5)+
  theme_classic()+theme(text = element_text(size=20))+
  labs(x="Day of Water Year", y="Precipitation Accumulation (mm)", tag="f")

pdf("POR_Climate_Comparison.pdf", width = 10, height = 14)

ggarrange(p1, p2, p3, p4, p5, p6, nrow=3, ncol = 2)

dev.off()
