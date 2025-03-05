require(robCompositions)
require(ggplot2)
require(ggrepel)
require(dplyr)
require(rcartocolor)
require(ggpubr)
require(dataRetrieval)
require(EGRET)

setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/Coal_Creek/Summer")

TM1<-read.csv("SFA_Syn.csv")

TM1<-TM1[-1,]

colnames(TM1)[c(1,2)]<-c("Name", "Date")

TM2<-read.csv("SFA_Syn2023.csv")

TM2<-TM2[-1,]

colnames(TM2)[c(1,2)]<-c("Name", "Date")

TM<-bind_rows(TM1, TM2)

TM$Date<-as.Date(TM$Date, "%m/%d/%y")
TM<-TM[,c(1,2,seq(3,73,2),75:79)]

TM<-subset(TM, year(TM$Date) > 2019)

TM[,c(3:43)]<-lapply(TM[,c(3:43)], as.numeric)

TM[TM==0]<-NA

all<-TM

colnames(all)

na_count <- sapply(TM[,c(3:43)], function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

all_noNA<-all[,c(colnames(all)[colSums(is.na(all)) == 0])]

all_noNA$Name<-factor(all_noNA$Name, levels = c("CC-5", "CC-6", "CC-7", "CC-8", "CC-9", "COAL-20", "COAL-15"))

all_noNA<-all_noNA %>%
  mutate(Name=case_when(
    Name=="CC-5"~"CC5",
    Name=="CC-6"~"CC6",
    Name=="CC-7"~"CC7",
    Name=="CC-8"~"CC8",
    Name=="CC-9"~"CC9",
    Name=="COAL-20"~"COAL20",
    Name=="COAL-15"~"COAL15"
  ))


all_noNA<-all_noNA[,colnames(all_noNA) !="Co"]

all_noNA<-all_noNA[,colnames(all_noNA) !="P"]

colnames(all_noNA)

# TM_melt<-melt(all_noNA, id.vars=c("Name", "Date", "clean_name"))
# 
# TM_melt$value<-as.numeric(TM_melt$value)
# 
# ggplot(TM_melt, aes(Date, value))+geom_point()+facet_wrap(~variable, scales = "free_y")+theme_bw()+
#   theme(text = element_text(size=20))

molar<-read.csv("MolarMass_SFA.csv")

molar<-molar[molar$Consituent!="Co",]
molar<-molar[molar$Consituent!="Ni",]

vect<-molar$Molar_Mass

toconvert<-all_noNA[,c(3:17)]

toconvert_mat<-as.matrix(toconvert)

molar_const<-sweep(toconvert_mat,2,vect,FUN="/")

cont_comp<-cbind(all_noNA[c(1,2,18:20)], molar_const)

cont_comp$sum<-rowSums(cont_comp[3:20])

cont_prop<-cont_comp[3:20]/cont_comp$sum

cont_prop<-cbind(all_noNA[c(1,2)], cont_prop)

cont_prop$check<-rowSums(cont_prop[3:20])

colnames(cont_prop)[c(3,4,5)]<-c("Fl", "Cl", "SO4")

set.seed(124)

pca_coda<-pcaCoDa(
  cont_prop[,c(3:20)],
  method = "classic",
  mult_comp = NULL,
  external = NULL,
  solve = "eigen"
)

plot(pca_coda)

summary(pca_coda)

loadings<-as.data.frame(pca_coda$loadings)

pc_loadings<-as.data.frame(pca_coda$scores)

#the columns from all_noNA are like site name and date and such
pc_loadings<-cbind(pc_loadings, all_noNA[,c(1,2)])

pc_loadings$samp_time<-ifelse(month(pc_loadings$Date) < 7, "peak flow",
                              ifelse(month(pc_loadings$Date) == 10, "low flow", "falling limb"))

#pc_loadings$location<-ifelse(pc_loadings$Name %in% c("CC6", "CC7", "CC8"), "fracture-influenced",
#                             ifelse(pc_loadings$Name=="COAL15", "Elk-influenced", "non-fractured"))

pc_loadings$Name<-factor(pc_loadings$Name, levels = c("CC5", "CC6", "CC7", "CC8", "CC9", "COAL20", "COAL15"))

#CoalQ<-readNWISDaily("09111250", "00060")

#add in Q here
#pc_loadings<-merge(pc_loadings, CoalQ[,c(1,2)], by="Date")

#color by year

p1<-ggplot()+geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.2*10),size=0.6, col="grey50")+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.2*10), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6, col="grey50")+
  geom_point(pc_loadings, mapping = aes(Comp.1, Comp.2, col=as.character(year(Date))), size=4)+theme_classic()+
  geom_text_repel(data = loadings,aes(Comp.1*10,Comp.2*10,label=rownames(loadings), family="Times"), size=6.5)+
  labs(x= "PC1 33% variation", y="PC2 22% variation", col="Year", tag="a")+
  theme(text = element_text(size=20, family="Times"), legend.position = "null")+
  scale_color_manual(values = carto_pal(5, "Bold"))


p1

# p1<-ggplot()+geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.2*10),size=0.6, col="grey50")+
#   geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.2*10), 
#                arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6, col="grey50")+
#   geom_point(pc_loadings, mapping = aes(Comp.1, Comp.2, col=as.character(year(Date)), alpha=as.character(year(Date))), size=4)+theme_classic()+
#   geom_text_repel(data = loadings,aes(Comp.1*10,Comp.2*10,label=rownames(loadings), family="Times"), size=6.5)+
#   labs(x= "PC1 33% variation", y="PC2 22% variation", col="Year")+
#   theme(text = element_text(size=20, family="Times"), legend.position = "null")+
#   scale_color_manual(values = carto_pal(5, "Bold"))+scale_alpha_manual(values=c(1,1,1,1))
# 
# 
# p1

p2<-ggplot()+geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.3*10),size=0.6)+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.3*10), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  geom_point(pc_loadings, mapping = aes(Comp.1, Comp.3, col=as.character(year(Date))), size=4)+theme_classic()+
  geom_text_repel(data = loadings,aes(Comp.1*10,Comp.3*10,label=rownames(loadings), family="Times"), size=6.5)+
  labs(x= "PC1 33% variation", y="PC3 18% variation", col="Year", tag="b")+
  theme(text = element_text(size=20, family="Times"), legend.position = "null")+
  scale_color_manual(values = carto_pal(5, "Bold"))

p2

p3<-ggplot()+geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.2*10, yend=Comp.3*10),size=0.6)+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.2*10, yend=Comp.3*10), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  geom_point(pc_loadings, mapping = aes(Comp.2, Comp.3, col=as.character(year(Date))), size=4)+theme_classic()+
  geom_text_repel(data = loadings,aes(Comp.2*10,Comp.3*10,label=rownames(loadings), family="Times"), size=6.5)+
  labs(x= "PC2 22% variation", y="PC3 18% variation", col="Year", tag = "c")+
  theme(text = element_text(size=20, family="Times"))+
  scale_color_manual(values = carto_pal(5, "Bold"))

p3

#color by month

pc_loadings$samp_time<-factor(pc_loadings$samp_time, levels = c("peak flow", "falling limb", "low flow"))

p4<-ggplot()+geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.2*10),size=0.6, col="grey50")+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.2*10), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6, col="grey50")+
  geom_point(pc_loadings, mapping = aes(Comp.1, Comp.2, col=samp_time), size=4)+theme_classic()+
  geom_text_repel(data = loadings,aes(Comp.1*10,Comp.2*10,label=rownames(loadings), family="Times"), size=6.5)+
  labs(x= "PC1 33% variation", y="PC2 22% variation", col="Flow Period", tag="d")+
  theme(text = element_text(size=20, family="Times"), legend.position = "null")+
  scale_color_manual(values = carto_pal(4, "TealRose"))


p4

p5<-ggplot()+geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.3*10),size=0.6)+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.3*10), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  geom_point(pc_loadings, mapping = aes(Comp.1, Comp.3, col=samp_time), size=4)+theme_classic()+
  geom_text_repel(data = loadings,aes(Comp.1*10,Comp.3*10,label=rownames(loadings), family="Times"), size=6.5)+
  labs(x= "PC1 33% variation", y="PC3 18% variation", col="Period", tag = "e")+
  theme(text = element_text(size=20, family="Times"), legend.position = "null")+
  scale_color_manual(values = carto_pal(4, "TealRose"))

p5

p6<-ggplot()+geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.2*10, yend=Comp.3*10),size=0.6)+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.2*10, yend=Comp.3*10), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  geom_point(pc_loadings, mapping = aes(Comp.2, Comp.3, col=samp_time), size=4)+theme_classic()+
  geom_text_repel(data = loadings,aes(Comp.2*10,Comp.3*10,label=rownames(loadings), family="Times"), size=6.5)+
  labs(x= "PC2 22% variation", y="PC3 18% variation", col="Flow Period", tag = "f")+
  theme(text = element_text(size=20, family="Times"))+
  scale_color_manual(values = carto_pal(4, "TealRose"))

p6

#color by site

pal<-wes_palette("Zissou1", n=7, type="continuous")

p7<-ggplot()+geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.2*10),size=0.6, col="grey50")+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.2*10), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6, col="grey50")+
  geom_point(pc_loadings, mapping = aes(Comp.1, Comp.2, col=Name), size=4)+theme_classic()+
  geom_text_repel(data = loadings,aes(Comp.1*10,Comp.2*10,label=rownames(loadings), family="Times"), size=6.5)+
  labs(x= "PC1 33% variation", y="PC2 22% variation", col="Stream Site", tag="g")+
  theme(text = element_text(size=20, family="Times"), legend.position = "null")+
  scale_color_manual(values = pal)


p7

p8<-ggplot()+geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.3*10),size=0.6)+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.1*10, yend=Comp.3*10), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  geom_point(pc_loadings, mapping = aes(Comp.1, Comp.3, col=Name), size=4)+theme_classic()+
  geom_text_repel(data = loadings,aes(Comp.1*10,Comp.3*10,label=rownames(loadings), family="Times"), size=6.5)+
  labs(x= "PC1 33% variation", y="PC3 18% variation", col="Site", tag = "h")+
  theme(text = element_text(size=20, family="Times"), legend.position = "null")+
  scale_color_manual(values = pal)

p8

p9<-ggplot()+geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.2*10, yend=Comp.3*10),size=0.6)+
  geom_segment(loadings, mapping=aes(x=0, y=0, xend=Comp.2*10, yend=Comp.3*10), 
               arrow = arrow(angle=22.5,length = unit(0.35,"cm"),type = "closed"), lty="blank", size=0.6)+
  geom_point(pc_loadings, mapping = aes(Comp.2, Comp.3, col=Name), size=4)+theme_classic()+
  geom_text_repel(data = loadings,aes(Comp.2*10,Comp.3*10,label=rownames(loadings), family="Times"), size=6.5)+
  labs(x= "PC2 22% variation", y="PC3 18% variation", col="Stream Site", tag = "i")+
  theme(text = element_text(size=20, family="Times"))+
  scale_color_manual(values = pal)

p9


pdf("PCA_array_Compositional.pdf", width = 24, height = 18)

ggarrange(p1, p2, p3,
          p4, p5, p6,
          p7, p8, p9,
          nrow = 3, ncol = 3, align = "hv")
dev.off()

pdf("SFA_PCA_Comp_all3.pdf", width = 7, height = 15)
ggarrange(p1, p4, p7, nrow = 3, align = "v")
dev.off()

