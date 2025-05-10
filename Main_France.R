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
DATA=Weekly_maxima_Prec_France$Prec
lon.lat=cbind(Weekly_maxima_Prec_France$Longitude,Weekly_maxima_Prec_France$Latitude)
COORD=lonlat.to.planar(lon.lat, miles =FALSE)
DATA_Frechet=data2frechet(DATA)  ############ transform the data to Frechet

Co.data<-extremal_concurrence_prob(DATA_Frechet)   #### calculate the extremal concurrence probability matrix


k=2
locations_clustering<-spectral_clustering(Co.data,k)  ####### Apply the spectral clustering to regionalize the locations k=2
ff <- as.data.frame(cbind(Weekly_maxima_Prec_France$Longitude,Weekly_maxima_Prec_France$Latitude,locations_clustering)) 

france_map <- map_data("france")
FRA_SP_2_clusters<- ggplot() + 
  geom_line()+
  geom_polygon(data = france_map, aes(x = long, y = lat, group = group),fill="gray90", colour = "gray90")+
  geom_point(data =ff, mapping = aes(x = V1,y = V2,colour=factor(locations_clustering)))+
  scale_color_manual(values=c("indianred3", "dodgerblue3"))+
  scale_x_continuous('Longitude',limits = c(-5, 10), expand = c(0, 0),breaks=waiver()) +
  scale_y_continuous('Latitude', limits = c(41, 52), expand = c(0, 0)) +
  coord_equal() +
  theme_bw()+
  ggtitle("No. of clusters k=2")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(colour = "Clusters") 
FRA_SP_2_clusters


###################################################################################################
k=3
locations_clustering<-spectral_clustering(Co.data,k)  ####### Apply the spectral clustering to regionalize the locations k=2
ff <- as.data.frame(cbind(Weekly_maxima_Prec_France$Longitude,Weekly_maxima_Prec_France$Latitude,locations_clustering)) 

FRA_SP_3_clusters<- ggplot() + 
  geom_line()+
  geom_polygon(data = france_map, aes(x = long, y = lat, group = group),fill="gray90", colour = "gray90")+
  geom_point(data =ff, mapping = aes(x = V1,y = V2,colour=factor(locations_clustering)))+
  scale_color_manual(values=c("indianred3", "dodgerblue3","pink1"))+
  scale_x_continuous('Longitude',limits = c(-5, 10), expand = c(0, 0),breaks=waiver()) +
  scale_y_continuous('Latitude', limits = c(41, 52), expand = c(0, 0)) +
  coord_equal() +
  theme_bw()+
  ggtitle("No. of clusters k=3")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(colour = "Clusters") 
FRA_SP_3_clusters
######################################################################################################
k=4
locations_clustering<-spectral_clustering(Co.data,k)  ####### Apply the spectral clustering to regionalize the locations k=2
ff <- as.data.frame(cbind(Weekly_maxima_Prec_France$Longitude,Weekly_maxima_Prec_France$Latitude,locations_clustering)) 

FRA_SP_4_clusters<- ggplot() + 
  geom_line()+
  geom_polygon(data = france_map, aes(x = long, y = lat, group = group),fill="gray90", colour = "gray90")+
  geom_point(data =ff, mapping = aes(x = V1,y = V2,colour=factor(locations_clustering)))+
  scale_color_manual(values=c("indianred3", "dodgerblue3","pink1","olivedrab4"))+
  scale_x_continuous('Longitude',limits = c(-5, 10), expand = c(0, 0),breaks=waiver()) +
  scale_y_continuous('Latitude', limits = c(41, 52), expand = c(0, 0)) +
  coord_equal() +
  theme_bw()+
  ggtitle("No. of clusters k=4")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(colour = "Clusters") 
FRA_SP_4_clusters


ggarrange(FRA_SP_2_clusters,FRA_SP_3_clusters,FRA_SP_4_clusters, ncol =2 , nrow =2)+
  theme(plot.margin = unit(c(0,1,0,1),"cm"))

