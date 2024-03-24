# 2024.03.24
# Figures from Sophie's first exploration into the data

require(tidyverse)
require(RColorBrewer)
source("00_functions.R")

# Load species data
species_table = read.csv2("01_data_in/SplateData - Sheet1.csv", 
                          sep = ",", stringsAsFactors = F)
for(tmp.col in c("Altitude", "Latitude", "Longitude", "SubstrateTemp", "AmbientTemp", "Humidity")){
    species_table[,tmp.col] = as.numeric(species_table[,tmp.col])
}

table(species_table$Species)
species=c("Csp59B", "Csp79A", "Csp619B", "Csp578D", "Csp384B", "Csp116H", 
          "C. parvicauda", "C. tropicalis", "C. briggsae")
species_table %>%
    filter(Species%in%species) %>%
    select(Sokehs.Ridge.Experiment) %>% table
# haaaaha 482 from sokehs ridge, 536 from not.

species_table %>%
    filter(Species%in%species) %>%
    mutate(experiment = case_when(Sokehs.Ridge.Experiment=="Yes" ~ "Sokehs Ridge Exp (482 isolates)",
                                  .default = "Not Sokehs Ridge Exp (536 isolates)")) %>%
    ggplot(aes(x=Species, y=Altitude))+
    geom_jitter(width = .2, height=0, alpha=.5)+
    facet_wrap(~experiment, ncol=1)+
    theme_sophie
ggsave("02_plots_out/01_All_altitude.pdf", 
       device="pdf", width = 7, height = 7)

species_table %>%
    filter(Species%in%species, Sokehs.Ridge.Experiment!="Yes") %>%
    ggplot(aes(x=Longitude, y=Latitude, color=Species))+
    scale_color_manual(values = c('#4daf4a','#e41a1c','#377eb8','#f781bf','#984ea3','#ffff33','#a65628','#ff7f00','#999999'))+
    #   geom_point()+
    geom_jitter(width = .002, height=.002, alpha=.75, size=1)+
    theme_sophie
ggsave("02_plots_out/02_All_lat_long.pdf", 
       device="pdf", width = 9, height = 7)
ggsave("02_plots_out/02_All_lat_long.png", 
       device="png", width = 9, height = 7)

ggplot() +
    geom_point(data=species_table %>%
                   filter(Species%in%species, Sokehs.Ridge.Experiment!="Yes") %>%
                   select(Longitude, Latitude),
               aes(x=Longitude, y=Latitude),
               color="#000000", size=2)+
    geom_point(data=species_table %>%
                   filter(Species%in%species, Sokehs.Ridge.Experiment!="Yes") %>%
                   select(Longitude, Latitude),
               aes(x=Longitude, y=Latitude),
               color="#ffffff", size=1.75)+
    geom_point(data=species_table %>%
                   filter(Species%in%species, Sokehs.Ridge.Experiment!="Yes"),
               aes(x=Longitude, y=Latitude, color=Species), size=1.6)+
    scale_color_manual(values = c('#4daf4a','#e41a1c','#377eb8','#f781bf','#984ea3','#ffff33','#a65628','#ff7f00','#999999'))+
    #    geom_jitter(width = .002, height=.002, alpha=.75, size=1)+
    facet_wrap(~Species)+
    theme_sophie
ggsave("02_plots_out/03_All_lat_long_by_species.pdf", 
       device="pdf", width = 7, height = 6)
ggsave("02_plots_out/03_All_lat_long_by_species.png", 
       device="png", width = 7, height = 6)

# whoa what is that one location that has every single species
species_table %>% 
    filter(Latitude>6.9185, Latitude<6.920, Longitude>158.27, Longitude<158.28) %>%
    filter(Species %in% species) %>%
    select(Splate, Species, QG.number, CollectionDate, Latitude, Longitude, Altitude, SubstrateClass, SubstrateNotes)

species_table %>% filter(Species=="Caenorhabditis parvicauda")
species_table %>% filter(Altitude>188.87287, Altitude<188.87289)

save.image("05_image_freeze.RData")



#~#~#~# Check out just the sokehs ridge data
#
#

SR_raw = species_table %>%
    filter(Sokehs.Ridge.Experiment=="Yes", Species %in% species) %>%
    select(Splate, Species, QG.number, Cplate, SubstrateNotes) %>% 
    mutate(rep = substr(SubstrateNotes, 3,3),
           collet = substr(SubstrateNotes, 4,4),
           rownum = substr(SubstrateNotes, 5,5),
           material = case_when(nchar(SubstrateNotes)==7 & substr(SubstrateNotes,6,7)=="LL" ~ "Leaf Litter",
                                .default = "Fig"),
           experiment = case_when(collet=="R" ~ "temporal",
                                  .default = "spatial"))

SR_raw %>% head
SR_raw %>% select(rep) %>% table(useNA = "ifany")
SR_raw %>% select(rep, material) %>% table(useNA = "ifany")
SR_raw %>% select(collet) %>% table(useNA = "ifany")
SR_raw %>% select(rownum) %>% table(useNA = "ifany")
for(tmp.col in c("QG.number", "rep", "rownum")){SR_raw[,tmp.col] = as.numeric(SR_raw[,tmp.col])}
# Ok, everything looks good and this table is nice


#~#~# Some plots

# Rotting progression

# number of circles
SR_raw %>% filter(experiment=="temporal") %>%
    ggplot(aes(x=rownum, fill=Species))+
    geom_histogram()+
    facet_wrap(~rep, ncol = 1)+
    theme_sophie+
    scale_fill_manual(values = c('#4daf4a','#984ea3','#ff7f00'))+
    xlab("Yellow to black")
ggsave("02_plots_out/04_temporal_histo.pdf", device = "pdf", width = 7, height = 5)
ggsave("02_plots_out/04_temporal_histo.png", device = "png", width = 7, height = 5)


# Spatial diversity

# jitter
ggplot()+
    geom_point(data = data.frame(collet=rep(c("A", "B", "C", "D", "E"), 15), 
                                 rownum=rep(rep(c(1:5), each=5), 3),
                                 rep=rep(1:3, each=25)) %>% 
                   mutate(uniquename = paste(collet, rownum, rep, sep = "_"))%>%
                   filter(uniquename != "B_2_2"),
               aes(x=collet, y=rownum), color="#cf9517", alpha=.15, size=8)+
    geom_jitter(data = SR_raw %>% filter(experiment=="spatial", material=="Fig"),
                aes(x=collet, y=rownum, color=Species),
                width = .15, height = .15, alpha=.85, size=1)+
    facet_wrap(~rep, ncol = 1)+
    labs(x=NULL, y=NULL, color=NULL)+
    scale_color_manual(values = c('#4daf4a','#984ea3'))+
    theme_sophie
ggsave("02_plots_out/05_spatial_jitter.pdf", device = "pdf", width = 4, height = 7)
ggsave("02_plots_out/05_spatial_jitter.png", device = "png", width = 4, height = 7)






