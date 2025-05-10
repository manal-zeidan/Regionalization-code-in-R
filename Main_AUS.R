library(SpatialExtremes)
library(ggplot2)
library(maps)
library(mapdata)
library(sp)
library(tidyverse)
library(ggthemes)
library(ggpubr)
###############################
source("functions.R")
DATA=monthly_maxima_rainfall_AUS$rainfall
lon.lat=cbind(monthly_maxima_rainfall_AUS$Longitude,monthly_maxima_rainfall_AUS$Latitude)
COORD=lonlat.to.planar(lon.lat, miles =FALSE)
DATA_Frechet=data2frechet(DATA)  ############ transform the data to Frechet

Co.data<-extremal_concurrence_prob(DATA_Frechet)   #### calculate the extremal concurrence probability matrix


k=2
locations_clustering<-spectral_clustering(Co.data,k)  ####### Apply the spectral clustering to regionalize the locations k=2
ff <- as.data.frame(cbind(monthly_maxima_rainfall_AUS$Longitude,monthly_maxima_rainfall_AUS$Latitude,locations_clustering)) 

aus<-maps::map("worldHires", "Australia", fill=T,col = 0,plot=F, ylim=c(-45,-5))
ss.d<-fortify(aus)
ss.d<- ss.d[ss.d[,1]>=140,]
ss.d<- ss.d[ss.d[,1]<=160,]

AUS_SP_2_clusters<- ggplot() + 
  geom_line()+
  geom_polygon(data = ss.d, aes(x = long, y = lat, group = group),fill="gray90", colour = "gray90")+
  geom_point(data =ff, mapping = aes(x = V1,y = V2,colour=factor(locations_clustering)))+
  scale_color_manual(values=c("indianred3", "dodgerblue3"))+
  scale_x_continuous('Longitude',limits = c(min(ss.d$long)-1,max(ss.d$long)-4), expand = c(0, 0),breaks=waiver()) +
  scale_y_continuous('Latitude', limits = c(min(ss.d$lat)-1,max(ss.d$lat)+1), expand = c(0, 0))+ 
  coord_equal() +
  theme_bw()+
  ggtitle("No. of clusters k=2")+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(legend.position="none")+
  labs(colour = "Clusters") 

AUS_SP_2_clusters
###################################################################################################
k=3
locations_clustering<-spectral_clustering(Co.data,k)  ####### Apply the spectral clustering to regionalize the locations k=3
ff <- as.data.frame(cbind(monthly_maxima_rainfall_AUS$Longitude,monthly_maxima_rainfall_AUS$Latitude,locations_clustering)) 

AUS_SP_3_clusters<- ggplot() + 
  geom_line()+
  geom_polygon(data = ss.d, aes(x = long, y = lat, group = group),fill="gray90", colour = "gray90")+
  geom_point(data =ff, mapping = aes(x = V1,y = V2,colour=factor(locations_clustering)))+
  scale_color_manual(values=c("indianred3", "dodgerblue3","pink1"))+
  scale_x_continuous('Longitude',limits = c(min(ss.d$long)-1,max(ss.d$long)-4), expand = c(0, 0),breaks=waiver()) +
  scale_y_continuous('Latitude', limits = c(min(ss.d$lat)-1,max(ss.d$lat)+1), expand = c(0, 0))+ 
  coord_equal() +
  theme_bw()+
  ggtitle("No. of clusters k=3")+
  theme(plot.title = element_text(hjust = 0.5))+
  #theme(legend.position="none")+
  labs(colour = "Clusters") 

AUS_SP_3_clusters
######################################################################################################
k=4
locations_clustering<-spectral_clustering(Co.data,k)  ####### Apply the spectral clustering to regionalize the locations k=4
ff <- as.data.frame(cbind(monthly_maxima_rainfall_AUS$Longitude,monthly_maxima_rainfall_AUS$Latitude,locations_clustering)) 

AUS_SP_4_clusters<- ggplot() + 
  geom_line()+
  geom_polygon(data = ss.d, aes(x = long, y = lat, group = group),fill="gray90", colour = "gray90")+
  geom_point(data =ff, mapping = aes(x = V1,y = V2,colour=factor(locations_clustering)))+
  scale_color_manual(values=c("indianred3", "dodgerblue3","pink1","olivedrab4"))+
  scale_x_continuous('Longitude',limits = c(min(ss.d$long)-1,max(ss.d$long)-4), expand = c(0, 0),breaks=waiver()) +
  scale_y_continuous('Latitude', limits = c(min(ss.d$lat)-1,max(ss.d$lat)+1), expand = c(0, 0))+ 
  coord_equal() +
  theme_bw()+
  ggtitle("No. of clusters k=4")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(colour = "Clusters") 

AUS_SP_4_clusters

ggarrange(AUS_SP_2_clusters,AUS_SP_3_clusters,AUS_SP_4_clusters, ncol =3 , nrow =1)+
  theme(plot.margin = unit(c(0,1,0,1),"cm"))

