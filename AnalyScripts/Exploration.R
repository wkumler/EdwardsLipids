#Exploration.R

library(dplyr)
library(ggplot2)
library(MBA)
library(reshape2)
source("AnalyScripts/SimpleODV.R")

LOBdata <- read.csv("Data/Clean_Complete.csv", stringsAsFactors = F)

# Data structure ----
lipid.classes <- unique(LOBdata$lipid_class)
lp <- list()
for(i in 1:length(lipid.classes)) {
  lp[[i]] <- NA
  data_i <- filter(LOBdata, lipid_class==lipid.classes[i])
  lp[[i]][1] <- paste(length(unique(data_i$species)), "species")
  lp[[i]][2] <- paste(length(unique(data_i$compound_name)), "compounds")
}
names(lp) <- lipid.classes
data.frame(lp)


# Plotting the data via ggplot ----
lipids.grouped <- LOBdata %>% 
  group_by(Orbi_num, lipid_class, species) %>% 
  summarize("Total intensity"=sum(intensity))

ggplot(data = lipids.grouped, aes(x=Orbi_num, y=`Total intensity`, color=species)) +geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~lipid_class, scales = "free_y") +
  ggtitle("All lipids and their values")
ggsave("AllLipids.png", device = "png", path = "Images/")


ODV()



#Irrelevant ones (only one of each type)
irr <- c("FFA","hapCER", "hGSL", "plastoquinone_9OH", "PUA", "sterol", "TAG", "DNPPE")

IPDAG.data <- filter(lipids.grouped, lipid_class=="IP_DAG")
IPDAG.data <- filter(IPDAG.data, Orbi_num!="Orbi_1287")
ggplot(data = IPDAG.data, aes(x=Orbi_num, y=`Total intensity`, color=species)) +geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~species, scales = "free_y") + 
  ggtitle("Only lipids with 1+ compounds per species")
ggsave("InterestLipids.png", device = "png", path = "Images/")


