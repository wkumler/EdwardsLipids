#Exploration.R

setwd("AnalyScripts")

library(dplyr)
library(ggplot2)
library(MBA)
library(reshape2)

LOBdata <- read.csv("../Data/Clean_Complete.csv", stringsAsFactors = F)

#####Looking at structure of the IP-DAGs#####
classes <- unique(LOBdata$lipid_class)
lp <- list()
for(i in 1:length(classes)) {
  lp[[i]] <- NA
  data_i <- filter(LOBdata, lipid_class==classes[i])
  lp[[i]][1] <- paste(length(unique(data_i$species)), "species")
  lp[[i]][2] <- paste(length(unique(data_i$compound_name)), "compounds")
}
names(lp) <- classes
data.frame(lp)

#####Looking at the data via ggplot#####
sdata <- LOBdata %>% 
  group_by(Orbi_num, lipid_class, species) %>% 
  summarize("Total intensity"=sum(intensity))

ggplot(data = sdata, aes(x=Orbi_num, y=`Total intensity`, color=species)) +geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~lipid_class, scales = "free_y") +
  ggtitle("All lipids and their values")
ggsave("AllLipids.png", device = "png", path = "../Images/")


#Irrelevant ones (only one of each type)
irr <- c("FFA","hapCER", "hGSL", "plastoquinone_9OH", "PUA", "sterol", "TAG", "DNPPE")

rdata <- filter(sdata, lipid_class=="IP_DAG")
rdata <- filter(rdata, Orbi_num!="Orbi_1287")
ggplot(data = rdata, aes(x=Orbi_num, y=`Total intensity`, color=species)) +geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~species, scales = "free_y") + 
  ggtitle("Only lipids with 1+ compounds per species")
ggsave("InterestLipids.png", device = "png", path = "../Images/")

#####Doing ODV on them#####
ODV <- function(spec) {
  long_samples_i <- LOBdata %>% 
    filter(species==spec) %>% 
    group_by(Station, Depth) %>%
    summarize("total" = sum(intensity))
  
  #Interpolating and redrawing
  surf_i <- mba.surf(long_samples_i, no.X = 300, no.Y = 300, extend = T)
  dimnames(surf_i$xyz.est$z) <- list(surf_i$xyz.est$x, surf_i$xyz.est$y)
  surf_i <- melt(surf_i$xyz.est$z, varnames = c('Station', 'Depth'), value.name = 'total')
  
  gp <- ggplot(data = surf_i, aes(x = Station, y = Depth)) +
    geom_raster(aes(fill = total)) +
    scale_fill_gradientn(colours = rev(rainbow(5))) +
    coord_cartesian(expand = 0) +
    geom_point(data = long_samples_i, alpha = 0.2, aes(x=Station, y=Depth)) +
    geom_contour(aes(z = total), binwidth = max(surf_i$total)/8, colour = "black", alpha = 0.2) +
    ylim(c(50,0)) + ggtitle(spec) + xlim(1, 11) +
    theme(axis.text.x = element_text(angle = 330, hjust = 0), axis.title.x = element_blank())
  return(gp)
}

for(i in unique(LOBdata$species)) {
  print(i)
  ggsave(ODV(i), filename = paste0("../Images/ODVplots/", i, "_ODV.png"))
}

#####Correlations#####
