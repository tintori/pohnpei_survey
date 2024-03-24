# 2024.03.24
# Figures from Tuc's first exploration into the data

library("ggplot2")
library(sf)

##These shape files are downloaded from https://islandatlas.org/data-repo/ The specific shape files are: 50-m elevation contours, Municipalities [lines], prominent peaks.

pohnpei <- st_read("phn_contours_50m.shp")
municipality <- st_read("phn_municipality_poly.shp")
peaks <- st_read("phn_peaks.shp")
##There are a lot of peaks here, so this is to include the most well known peaks, as well as the sokehs ridge’s highest point. 
peaks =subset(peaks, peaks$Elev_m_Src> 0 & peaks$Elev_m_2 >250)

tropicalis = subset(SpeciesDistribution, SpeciesDistribution$C.tropicalis >0)
briggsae = subset(SpeciesDistribution, SpeciesDistribution$C.briggsae >0)
sp0384 =subset(SpeciesDistribution, SpeciesDistribution$C.sp.0384B >0)
sp0079 =subset(SpeciesDistribution, SpeciesDistribution$C.sp.0079A >0)
sp0619 =subset(SpeciesDistribution, SpeciesDistribution$C.sp.0619B >0)
sp0578 =subset(SpeciesDistribution, SpeciesDistribution$C.sp.0578D >0)
sp0059 =subset(SpeciesDistribution, SpeciesDistribution$C.sp.0059B >0)

ggplot(data = municipality)+ 
    geom_sf(linewidth = 0.5, alpha = 0)+   
    geom_sf_text(aes(label = Name),size = 5)+
    geom_sf(data = pohnpei, linewidth = 0.1)+
    geom_sf(data=peaks, shape = 24)+
    geom_sf_text(data= peaks, aes(label = Tag),size = 3)+
    coord_sf(default_crs = sf::st_crs(4326))+
    geom_point(data = sp0384,aes(longitude,latitude), color = "#1686AD", size = 1)+
    geom_point(data = tropicalis,aes(longitude,latitude), size = 1,color = "#BC242F")+
    geom_point(data = briggsae,aes(longitude,latitude), size = 1,color = "#66a61e")+
    geom_point(data = sp0079,aes(longitude,latitude),size = 1, color = "#F9A003")+
    geom_point(data = sp0619,aes(longitude,latitude),size = 1, color = "#777CB2")+
    geom_point(data = sp0578,aes(longitude,latitude),size = 1, color = "#1b9e77")+
    geom_point(data = sp0059,aes(longitude,latitude), size = 1,color = "#d95f02")+
    xlab("Longitude") + ylab("Latitude")+theme_light()
