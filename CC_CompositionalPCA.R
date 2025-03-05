#install.packages("robCompositions")
require(robCompositions)
require(ggplot2)
require(ggrepel)
require(dplyr)
require(data.table)
require(wesanderson)
require(gridExtra)
require(ggpmisc)
require(lemon)
require(reshape2)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek/Summer")

TM<-read.csv("TraceMetal_CleanName.csv")

TM$Date<-as.Date(TM$Date, "%m/%d/%y")
TM<-TM[,-c(1:3)]
TM<-TM[,c(1:3, seq(5,73,2))]

anion<-read.csv("Anion_CleanName.csv")
anion<-anion[,-c(1,9)]
anion$Date<-as.Date(anion$Date, "%m/%d/%y")
colnames(anion)[1]<-"Name"

all<-merge(TM, anion, by=c("Name", "Date"))

all[,c(3:43)]<-suppressWarnings(lapply(all[,c(3:43)], as.numeric))

springs<-all[all$Name %like% c("SPRING|SFCON"),]

gw_springs<-subset(springs, springs$Name %in% c("SPRING8","SPRING5","UPPERSPRING","LOWERSPRING"))

gw_springs<-subset(springs, springs$Name %in% c("SPRING8"))

meadow_spring<-subset(springs, springs$Name=="MEADOWSPRING")

#Elk<-subset(all, all$Name=="ELK")

# springs_avg<-data.frame(t(as.data.frame(c("springs_avg", "2022-07-03", sapply(springs[3:43], mean)))))
# colnames(springs_avg)<-colnames(springs)
# springs_avg$Date<-as.Date(springs_avg$Date)
# springs_avg[3:43]<-lapply(springs_avg[3:43], as.numeric)

SM<-read.csv("CoalCreekStreamMeter.csv")
colnames(SM)[1]<-"Name"

all<-merge(all, SM, by="Name")
all<-subset(all, all$Stream.Meter > 9000)
all<-all[all$Name != "ELK",]
#all<-all[all$Name != "COAL15",]

#all<-bind_rows(all, springs)
# 
# Ohio<-read.csv("OhioCreek.csv")
# 
# Ohio<-Ohio[-1,c(1,2,seq(3,73,2),75:79)]
# 
# Ohio$Date<-as.Date(Ohio$Sampling.date, "%m/%d/%y")
# 
# colnames(Ohio)[1]<-"Name"
# 
# Ohio[,c(3:43)]<-suppressWarnings(lapply(Ohio[,c(3:43)], as.numeric))
# 
# Ohio<-Ohio[,-2]

#piez
piez<-suppressWarnings(read.csv("Piez_2022.csv"))

piez<-piez[,c(1,2,seq(3,73,2),75:79)]

piez<-piez[2,]

piez$Date<-as.Date(piez$Sampling.date, "%m/%d/%y")

colnames(piez)[1]<-"Name"

piez[,c(3:43)]<-suppressWarnings(lapply(piez[,c(3:43)], as.numeric))

piez<-piez[,-2]

all<-bind_rows(all, piez)
all<-bind_rows(all, meadow_spring)
all<-bind_rows(all, gw_springs)

all_noNA<-all

all_noNA<-all_noNA[,c(colnames(all_noNA)[colSums(is.na(all_noNA)) == 0])]

all_noNA<-subset(all_noNA, !c(all_noNA$Name %like% c("CC7.")))
all_noNA<-subset(all_noNA, !c(all_noNA$Name=="CC9"))

table(all_noNA$Name)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/Manuscripts/CoalCreek/Connectivity/DataSubmission")

write.csv(all_noNA, "CoalCreekGeochem_ESSDIVE.csv")

# all_noNA$clean_name<-ifelse(all_noNA$Name %in% c(gw_springs$Name), "GW Spring", 
#                             ifelse(all_noNA$Name %in% c(meadow_spring$Name), "Meadow Spring",
#                                    ifelse(all_noNA$Name %like% "Piez", "Piezometer",
#                                           ifelse(all_noNA$Name=="ELK", "Elk", all_noNA$Name))))


#all_noNA$spring_name<-ifelse(all_noNA$Name %like% c("SPRING|SFCON|Piez|ELK"), all_noNA$Name, "stream")

all_noNA$clean_name<-factor(all_noNA$Name, levels = c("CC5","UPSTREAM", "CC6", "CC7", "CC8", "DOWNSTREAM", 
                                                      "CC9.5", "UPSTREAMELK","COAL15"))

all_noNA<-all_noNA %>%
  mutate(clean_name = case_when(
    clean_name=="UPSTREAMELK"~"COAL20",
    .default = clean_name
  ))

colnames(all_noNA)

remove_these<-all_noNA[!is.na(all_noNA$clean_name)&month(all_noNA$Date)==9,]

all_noNA<-setdiff(all_noNA, remove_these)

all_noNA_forplot<-all_noNA
all_noNA_forplot<-all_noNA_forplot[,-24]
colnames(all_noNA_forplot)[c(21:23)]<-c("Fl", "Cl", "SO4")

#convert molar to mg/L
all_noNA_forplot$Fl<-all_noNA_forplot$Fl*19
all_noNA_forplot$Cl<-all_noNA_forplot$Cl*35.45
all_noNA_forplot$SO4<-all_noNA_forplot$SO4*96.07

all_noNA_melt<-melt(all_noNA_forplot, id.vars=c("Name", "Date", "clean_name"))
all_noNA_melt<-all_noNA_melt[complete.cases(all_noNA_melt),]

pal<-wes_palette("Zissou1", n=8, type="continuous")

pdf("Solute_Concs_All_CoalCreek.pdf", width = 18, height = 12)

ggplot(all_noNA_melt, aes(Date, value))+geom_line(aes(col=clean_name))+geom_point(aes(col=clean_name))+
  facet_wrap(~variable, scales = "free_y")+theme_classic()+
  scale_color_manual(values=pal, limits= c("UPSTREAM", "CC6", "CC7", "CC8", "DOWNSTREAM", 
                                               "CC9.5", "COAL20","COAL15"))+
  labs(x= "Date", y="Concentration (ppb)", col="Stream Site")+
  theme(text = element_text(size=20, family="Times"), axis.text.x = element_text(angle = 45, hjust=1))

dev.off()

all_noNA_melt_3solutes<-subset(all_noNA_melt, all_noNA_melt$variable %in% c("Mg", "Mn", "Eu"))

all_noNA_melt_3solutes$variable<-factor(all_noNA_melt_3solutes$variable, levels = c("Mn", "Eu", "Mg"))

solute_stats<-all_noNA_melt_3solutes %>%
  dplyr::group_by(variable, clean_name) %>%
  summarise(mean_val=mean(value), sd_value=sd(value), cv=sd_value/mean_val)

all_noNA_melt_3solutes<-all_noNA_melt_3solutes %>%
  mutate(variable = case_when(
    variable=="Mn"~"Mn - Reach Inflow",
    variable=="Eu"~"Eu - Quick Flow GW",
    variable=="Mg"~"Mg - Slow Flow GW",
    .default = variable
  ))

all_noNA_melt_3solutes$variable<-factor(all_noNA_melt_3solutes$variable, 
                                        levels = c("Mn - Reach Inflow", "Eu - Quick Flow GW", "Mg - Slow Flow GW"))

pdf("Solute_Concs_3solutes_CoalCreek.pdf", width = 16, height = 4.5)

ggplot(all_noNA_melt_3solutes, aes(Date, value))+geom_line(aes(col=clean_name))+geom_point(aes(col=clean_name))+
  facet_wrap(~variable, scales = "free_y")+theme_classic()+
  scale_color_manual(values=pal, limits= c("UPSTREAM", "CC6", "CC7", "CC8", "DOWNSTREAM", 
                                           "CC9.5", "COAL20","COAL15"))+
  labs(x= "Date", y="Concentration (ppb)", col="Stream Site")+
  theme(text = element_text(size=20, family="Times"))

dev.off()

#all_noNA<-all_noNA[complete.cases(all_noNA$clean_name),]

molar<-read.csv("MolarMass.csv")

vect<-molar$Molar_Mass

toconvert<-all_noNA[,c(3:20)]

toconvert_mat<-as.matrix(toconvert)

molar_const<-sweep(toconvert_mat,2,vect,FUN="/")

cont_comp<-cbind(all_noNA[c(1,2, 21:23)], molar_const)

cont_comp$sum<-rowSums(cont_comp[3:23])

cont_prop<-cont_comp[3:23]/cont_comp$sum

cont_prop<-cbind(all_noNA[c(1,2,25)], cont_prop)

cont_prop$check<-rowSums(cont_prop[4:24])

colnames(cont_prop)[c(4:6)]<-c("Fl","Cl","SO4")

set.seed(124)

pca_coda<-pcaCoDa(
  cont_prop[,c(4:24)],
  method = "robust",
  mult_comp = NULL,
  external = NULL,
  solve = "eigen"
)

plot(pca_coda)

summary(pca_coda)

loadings<-as.data.frame(pca_coda$loadings)
loads_abs<-data.frame(lapply(loadings, abs))
rownames(loads_abs)<-rownames(loadings)

pc_loadings<-as.data.frame(pca_coda$scores)

pc_loadings<-cbind(pc_loadings, all_noNA[,c(1,2,25)])

pc_loadings$emma<-ifelse(!is.na(pc_loadings$clean_name), "stream",
                                 ifelse(pc_loadings$Name %like% "MEADOWSPRING", "Meadow Spring",
                                        ifelse(pc_loadings$Name %like% "Piez", "Piezometer",
                                               ifelse(pc_loadings$Name %like% "ELK", "Elk", "GW Spring"))))

pc_loadings$emma<-factor(pc_loadings$emma, levels = c("stream","GW Spring", "Meadow Spring", "Elk", "Piezometer"))

pc_loadings$Date<-ifelse(is.na(pc_loadings$clean_name), NA, pc_loadings$Date)

pc_loadings$Date<-as.Date(pc_loadings$Date, origin = "1970-01-01")

pc_loadings_stream<-subset(pc_loadings, pc_loadings$emma=="stream")
pc_loadings_EM<-subset(pc_loadings, pc_loadings$emma!="stream")

pc_loadings_EM$emma<-c("Reach Inflow", "Fractured Hillslope (quick flow)", "GW Spring (slow flow)")

pc_loadings_EM$emma<-factor(pc_loadings_EM$emma, levels = c("Reach Inflow","Fractured Hillslope (quick flow)",
                                                              "GW Spring (slow flow)"))


#pc_loadings$clean_name2<-ifelse(is.na(pc_loadings$clean_name), "End Member", pc_loadings$clean_name)
pal<-wes_palette("Zissou1", n=8, type="continuous")
  
p1<-ggplot()+
    geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.2*10),linewidth=0.6, col="grey50")+
    geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.2*10, col="grey50"), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
    geom_point(pc_loadings_stream, mapping = aes(Comp.1, Comp.2, col=clean_name), size=4)+
    scale_color_manual(values=pal,
                     limits= c("UPSTREAM", "CC6", "CC7", "CC8", "DOWNSTREAM", 
                               "CC9.5", "COAL20","COAL15"))+
    geom_point(pc_loadings_EM, mapping = aes(Comp.1, Comp.2, shape=emma, fill=emma), size=7)+
    scale_fill_manual(values=c("#9ecae1", "#4292c6","#08306b"))+
    scale_shape_manual(values=c(24, 23, 22))+
    theme_classic()+
    geom_text_repel(data = loadings,aes(Comp.1*10,Comp.2*10,label=rownames(loadings), family="Times"), 
                   size=6.5, max.overlaps = 20)+
    labs(x= "PC1 37% variation", y="PC2 21% variation", shape="End Member",col="Stream Site   ", fill="End Member", tag="a")+
    theme(text = element_text(size=20, family="Times"))
    
p1

p1_leg<-g_legend(p1)

p2<-ggplot()+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.2*10),linewidth=0.6, col="grey50")+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.2*10, col="grey50"), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  geom_point(pc_loadings_stream, mapping = aes(Comp.1, Comp.2, col=as.character(month(Date))), size=4)+
  scale_color_manual(values = carto_pal(4, "TealRose"), limits=c("6", "7", "8"))+
  geom_point(pc_loadings_EM, mapping = aes(Comp.1, Comp.2, shape=emma, fill=emma), size=7)+
  scale_fill_manual(values=c("#9ecae1", "#4292c6","#08306b"))+
  scale_shape_manual(values=c(24, 23, 22))+
  theme_classic()+
  geom_text_repel(data = loadings,aes(Comp.1*10,Comp.2*10,label=rownames(loadings), family="Times"), 
                 size=6.5, max.overlaps = 20)+
  labs(x= "PC1 37% variation", y="", col="Sample Month", tag="b", shape="End Member", fill="End Member")+
  theme(text = element_text(size=20, family="Times"))

p2

p2_leg<-g_legend(p2)

hlay<-rbind(c(1,1,2,2,3),
            c(1,1,2,2,3),
            c(1,1,2,2,3))

leg_item<-ggarrange(p1_leg, p2_leg, ncol = 1, heights = c(1.1,.4), align = "v")

leg_item

p3<-ggarrange(p1, p2, leg_item, ncol = 3, widths = c(1,1,.7))

p3


pdf("PCA_CompCoalCreek_Loadings.pdf", width = 23, height = 7)

ggarrange(p1, p2)

dev.off()

write.csv(pc_loadings, "PC_CC_Metals_March2024.csv")

###EMMA####
stream_df<-subset(pc_loadings, pc_loadings$emma=="stream")

GW_spring<-subset(pc_loadings, pc_loadings$Name=="SPRING8")

Meadow_spring<-subset(pc_loadings, pc_loadings$Name=="MEADOWSPRING")

piezometer<-subset(pc_loadings, pc_loadings$Name=="CC-6 Piez KJ 7/3/2022")

#bind soil mean and gw mean into one dataframe
pcadf_sources<-rbind(piezometer, Meadow_spring, GW_spring)
rownames(pcadf_sources)<-c("piezometer", "fracture_hillslope", "spring") #rename columns

#create dataframe wuth length of pcadf_stream df and 3 columns
fraction_df<-matrix(nrow=nrow(stream_df), ncol = 3)

#open loop to calculate prop. source contribution for each stream sample
for (i in 1:nrow(stream_df)) {
  
  coeff<-matrix(c(pcadf_sources[1,1], pcadf_sources[2,1], pcadf_sources[3,1],
                  pcadf_sources[1,2], pcadf_sources[2,2], pcadf_sources[3,2],
                  1,1,1),
                nrow = 3, byrow = T)
  
  stream<-matrix(c(stream_df[i,1], stream_df[i,2], 1), nrow = 3, byrow = T)
  
  fraction_df[i,]<-solve(coeff, stream)
  
}

#append all results from above loop into one dataframe
fraction_df<-as.data.frame(cbind(fraction_df, stream_df[,c("Name", "Date")]))

colnames(fraction_df)<-c("Piezometer", "Meadow Spring", "GW Spring", "Site", "Date") #rename columns

#set columns with fractions as numeric
fraction_df[,c(1:3)]<-sapply(fraction_df[,c(1:3)], as.numeric)

fraction_df_noneg<-fraction_df[rowSums(fraction_df[,c(1:3)] > 0) == 3, ]

fraction_df_noneg<-subset(fraction_df_noneg, fraction_df_noneg$Date < as.Date("2021-08-05"))

fraction_df_melt<-reshape2::melt(fraction_df_noneg, id.vars=c("Site", "Date"))

fraction_df_melt<-fraction_df_melt %>%
  mutate(Site = case_when(
    Site=="UPSTREAMELK"~"COAL20",
    .default = Site
  ))

fraction_df_melt$Site<-factor(fraction_df_melt$Site, levels = c("UPSTREAM", "CC6", "CC7", "CC8", 
                                                                "DOWNSTREAM", "COAL20", "COAL15"))

fraction_df_melt<-fraction_df_melt[complete.cases(fraction_df_melt$Site),]

fraction_df_melt <- fraction_df_melt %>%
  mutate(variable = case_when(
    variable=="GW Spring"~"GW Spring (slow flow)",
    variable=="Meadow Spring"~"Fractured Hillslope (quick flow)",
    variable=="Piezometer"~"Reach Inflow",
    .default = variable
    
  ))

fraction_df_melt$variable<-factor(fraction_df_melt$variable, levels = c("Reach Inflow",
                                                                        "Fractured Hillslope (quick flow)",
                                                                        "GW Spring (slow flow)"))

#setdiff(fraction_df, fraction_df_noneg)

pdf("EM_Mixing_CoalCreek_Piez_Meadow_GW.pdf", width = 15, height = 10, family = "Times")

p4<-ggplot(fraction_df_melt, aes(x=Date, y=value))+
  geom_area(aes(fill=variable))+facet_wrap(~Site, ncol = 4)+
  theme_classic()+labs(x="Date", y="Proportional Contribution", fill="End Member", col="End Member", tag = "c")+
  theme(text = element_text(size=20, family = "Times"), legend.position = c(0.871, 0.25))+
  scale_fill_manual(values=c("#9ecae1", "#4292c6", "#08306b"))

p4

dev.off()

pdf("EndMemberMixing_PCA_2021.pdf", width = 16, height = 13)

grid.arrange(p3, p4, nrow = 2, heights=c(0.4, 0.6))

dev.off()

