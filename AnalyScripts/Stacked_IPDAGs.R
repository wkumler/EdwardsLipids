# Piecharts.R

# Code designed to make Figure 2 of the poster

# Input: Clean_Complete.csv
# Output: Stacked.png, 

# Startup things ----
library(dplyr)
library(ggplot2)
source("AnalyScripts/SimpleODV.R")

LOBdata <- read.csv("Data/Clean_Complete.csv", stringsAsFactors = F)

IPDAGs <- filter(LOBdata, lipid_class=="IP_DAG")

# Choose only relevant IP-DAGs ----
all.IPDAGs <- unique(IPDAGs$species)
big.9.names <- c("MGDG", "DGDG", "SQDG", "PG", "PE", "PC", "DGTS_DGTA")
#BLL represents less than 1% of all stations
#WaxEsters and DAGs not proper membrane lipids

big.9.data <- IPDAGs %>%
  filter(species%in%big.9.names) %>%
  group_by(species, Station) %>%
  summarize(total_intensity=sum(intensity))

# Handle DGCC/S_DGCC separately
true.DGCC <- IPDAGs %>%
  filter(species=="DGCC"|species=="S_DGCC") %>%
  group_by(Station) %>%
  summarize(total_intensity=sum(intensity)) %>%
  cbind(species="DGCC")

#Put together
rel.IPDAGs <- bind_rows(big.9.data, true.DGCC)




# Begin graphing ----
stations <- unique(LOBdata$Station)

rel.IPDAGs %>% group_by(Station) %>% 
  ggplot(aes(x=Station, y=total_intensity)) + 
  geom_bar(aes(fill=species), stat = "identity")

#Okay, but normalize to 100% ----
norm.rel.IPDAGs <- rel.IPDAGs %>%
  group_by(Station) %>%
  mutate(proportion=(total_intensity/sum(total_intensity))*100)

#Aaaaand replot.
norm.rel.IPDAGs %>% group_by(Station) %>%
  ggplot(aes(x=Station, y=proportion)) + 
  geom_bar(aes(fill=species), stat = "identity")

#Make it pretty
norm.rel.IPDAGs$Station <- factor(norm.rel.IPDAGs$Station,
                                  levels = c(1,2,4,6,11,7,8,9,10)
                                  #Insert paste("Station", c(...)) here later
                                  )
classify.MvP <- function(station.number) {
  if(station.number %in% c(1,2,4,6,11)){
    return("Monterey")
  } else {
    return("Point Reyes")
  }
}
norm.rel.IPDAGs <- mutate(norm.rel.IPDAGs, "Location"=classify.MvP(Station))

norm.rel.IPDAGs %>% group_by(Station) %>%
  ggplot(aes(x=Station, y=proportion)) + 
  geom_bar(aes(fill=species), stat = "identity") +
  facet_wrap(~Location, scales = "free_x")

ggsave(filename = "Stacked_IPDAGs.png", device = "png", path = "Images")