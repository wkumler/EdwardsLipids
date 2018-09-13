#Exploration.R

library(dplyr)
library(ggplot2)
library(MBA)
library(reshape2)

LOBdata <- read.csv("Data/Clean_Complete.csv", stringsAsFactors = F)

#####Looking at structure of the IP-DAGs#####
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

#####Looking at the data via ggplot#####
sdata <- LOBdata %>% 
  group_by(Orbi_num, lipid_class, species) %>% 
  summarize("Total intensity"=sum(intensity))

ggplot(data = sdata, aes(x=Orbi_num, y=`Total intensity`, color=species)) +geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~lipid_class, scales = "free_y") +
  ggtitle("All lipids and their values")
ggsave("AllLipids.png", device = "png", path = "Images/")


#Irrelevant ones (only one of each type)
irr <- c("FFA","hapCER", "hGSL", "plastoquinone_9OH", "PUA", "sterol", "TAG", "DNPPE")

rdata <- filter(sdata, lipid_class=="IP_DAG")
rdata <- filter(rdata, Orbi_num!="Orbi_1287")
ggplot(data = rdata, aes(x=Orbi_num, y=`Total intensity`, color=species)) +geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~species, scales = "free_y") + 
  ggtitle("Only lipids with 1+ compounds per species")
ggsave("InterestLipids.png", device = "png", path = "Images/")


