#===========================
# ODV Script
#
# Inputs: Data frame, in long format. 
#   Columns: Stations (x axis), Depth (y axis), intensity (z axis, aka color)
# Outputs: Contour image
#
#===========================

# Packages ----
library(dplyr)
library(ggplot2)
library(MBA)
library(reshape2)

# Define the function ----
ODV <- function(data) {
  #Group and sum all samples with the same station and depth
  long_samples_i <- data %>% 
    group_by(Station, Depth) %>%
    summarize("total" = sum(intensity))
  
  #Interpolating via MBA
  surf_i <- mba.surf(long_samples_i, no.X = 300, no.Y = 300, extend = T)
  dimnames(surf_i$xyz.est$z) <- list(surf_i$xyz.est$x, surf_i$xyz.est$y)
  surf_i <- melt(surf_i$xyz.est$z, varnames = c('Station', 'Depth'), 
                 value.name = 'total')
  
  #Drawing
  gp <- ggplot(data = surf_i, aes(x = Station, y = Depth)) +
    geom_raster(aes(fill = total)) +
    scale_fill_gradientn(colours = rev(rainbow(5))) +
    geom_point(data = long_samples_i, 
               alpha = 0.2, 
               aes(x=Station, y=Depth)) +
    geom_contour(aes(z = total), 
                 binwidth = max(surf_i$total)/8, 
                 colour = "black", 
                 alpha = 0.2) +
    scale_y_reverse() +
    scale_x_continuous(breaks=long_samples_i$Station,
                     labels=paste("Station", long_samples_i$Station)) +
    theme(#axis.text.x = element_text(angle = 330, hjust = 0), 
          axis.title.x = element_blank())
  return(gp)
}

# Load the data ----
LOBdata <- read.csv("Data/Clean_Complete.csv", stringsAsFactors = F)


#Apply the function ----
ODV(LOBdata)

# PG_data <- LOBdata %>% 
#   filter(species == "PG")
# ODV(PG_data)
# 
# shal_PG_data <- PG_data %>%
#   filter(Depth < 10)
# ODV(shal_PG_data)
