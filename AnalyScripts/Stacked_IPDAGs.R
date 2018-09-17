# Stacked_IPDAGs.R

# Code designed to make Figure 2 of the poster

# Input: Clean_Complete.csv
# Output: Stacked_IPDAGs.png, 

# Startup things ----
library(dplyr)
library(ggplot2)
library(MBA)
library(reshape2)
library(grid)
library(gridExtra)

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

# Okay, but normalize to 100% ----
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
ordered.IPDAGs <- norm.rel.IPDAGs
specs <- c("DGTS_DGTA", "DGCC", "MGDG", "DGDG", "SQDG", 
           "PC", "PE", "PG")
ordered.IPDAGs$species <- factor(ordered.IPDAGs$species, levels = specs)

proper.colors <- c("darkgoldenrod1", "darkgoldenrod3", "#41ae76", "#238b45", 
                    "#006d2c", "#3690c0", "#0570b0", "#045a8d")
# proper.colors <- c("#238b45", "#006d2c", "#3690c0", "#0570b0", "#045a8d",
#                    "mediumpurple3", "#6E56AA", "mediumpurple4")

stacked_gp <- ordered.IPDAGs %>% 
  group_by(Station) %>%
  ggplot(aes(x=Station, y=proportion, fill=species)) + 
  geom_bar(stat = "identity") +
  geom_bar(color="black", stat = "identity", show.legend = F) + #second call to
    #geom_bar to draw outlines without outlining legend boxes
  facet_wrap(~Location, scales = "free_x") + #hella clever, thanks internet
  #guides(fill = guide_legend(nrow = 1)) +
  ylab("% of total IP-DAG species") +
  xlab("Station Number") + 
  theme_bw() +
  theme(legend.title=element_blank(), legend.position = "bottom",
      axis.text = element_text(size = 18, face = "bold", color="black"),
      axis.title = element_text(size = 18, face = "bold"),
      #strip.background =element_rect(fill=c("#F8766D", "#00BFC4")),
      legend.text = element_text(size = 16)) +
  scale_fill_manual(breaks = specs,
                      labels = paste0(" ", specs, "   "),
                      values = proper.colors)

#Save progress
stacked_gp
ggsave(filename = "Stacked_IPDAGs.png", plot = stacked_gp, device = "png", 
       path = "Images", width = 8, height = 8, units = "in")

# Add ODV plots ----
ODV <- function(data, title, x.axis="n") {
  #Group and sum all samples with the same station and depth
  long_samples_i <- data %>% 
    group_by(Station, Depth) %>%
    summarize("total" = sum(intensity))
  
  #Interpolating via MBA
  surf_i <- mba.surf(long_samples_i, no.X = 300, no.Y = 300, extend = T)
  dimnames(surf_i$xyz.est$z) <- list(surf_i$xyz.est$x, surf_i$xyz.est$y)
  surf_i <- melt(surf_i$xyz.est$z, varnames = c('Station', 'Depth'), 
                 value.name = 'total')
  if(x.axis=="y"){
    x.axis
  }
  
  #Drawing
  gp <- ggplot(data = surf_i, aes(x = Station, y = Depth)) +
    geom_raster(aes(fill = total)) +
    scale_fill_gradientn(colours = rev(rainbow(5))) +
    geom_point(data = long_samples_i, 
               alpha = 0.2, 
               aes(x=Station, y=Depth),
               cex = 2) +
    geom_contour(aes(z = total), 
                 binwidth = max(surf_i$total)/8, 
                 colour = "black", 
                 alpha = 0.2, 
                 lwd = 1) +
    scale_y_reverse() +
    scale_x_continuous(breaks=long_samples_i$Station,
                       labels=paste("St. ", long_samples_i$Station)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 315, hjust = 0), 
      axis.title.x = element_blank(), 
      legend.title=element_blank(), 
      axis.text = element_text(size = 18, color="black"),
      axis.title = element_text(size = 18, face = "bold"),
      legend.text = element_text(size = 18, face = "bold"),
      legend.position = "none", 
      plot.title = element_text(size = 18),
      plot.margin = unit(c(0.5,0.5,0,0), "in")) +
    ggtitle(title)
    
  
  if(x.axis=="y"){
    return(gp)
  } else {
    gp <- gp +theme(axis.ticks = element_blank(), axis.text.x = element_blank())
    return(gp)
  }
  
}

# And plot the whole thing ----

SQDG_df <- filter(IPDAGs, species=="SQDG")
SQDG_gp <- ODV(SQDG_df, title = "SQDG intensity", x.axis = "n")
PG_df <- filter(IPDAGs, species=="PG")
PG_gp <- ODV(PG_df, x.axis = "y", title = "PG intensity")

layout_matrix_vert <- cbind(1, c(2,2,2,3,3,3,3))
gpfinal <- grid.arrange(stacked_gp, SQDG_gp, PG_gp, layout_matrix = layout_matrix_vert)
ggsave(filename = "Stacked_IPDAG_complete.png", plot = gpfinal, device = "png",
       path = "Images", width = 14, height = 8, units = "in")




# Try a horizontal format? ----
# stacked_gp_horiz <- norm.rel.IPDAGs %>% 
#   group_by(Station) %>%
#   ggplot(aes(x=Station, y=proportion, fill=species)) + 
#   geom_bar(stat = "identity") +
#   geom_bar(color="black", stat = "identity", show.legend = F) + #second call to
#   #geom_bar to draw outlines without outlining legend boxes
#   facet_wrap(~Location, scales = "free_x") + #hella clever, thanks internet
#   #guides(fill = guide_legend(nrow = 1)) +
#   ylab("IP-DAG %") +
#   xlab("Station Number") + 
#   theme_bw() +
#   theme(legend.title=element_blank(), legend.position = "right",
#         axis.text = element_text(size = 18, face = "bold", color="black"),
#         axis.title = element_text(size = 18, face = "bold"),
#         #strip.background =element_rect(fill=c("#F8766D", "#00BFC4")),
#         legend.text = element_text(size = 16)) +
#   scale_fill_discrete(breaks = c(legend.order),
#                       labels = paste0(" ", legend.order, "   "))
# 
# SQDG_df <- filter(IPDAGs, species=="SQDG")
# SQDG_gp <- ODV(SQDG_df, title = "SQDG intensity", x.axis = "y")
# PG_df <- filter(IPDAGs, species=="PG")
# PG_gp <- ODV(PG_df, x.axis = "y", title = "PG intensity")
# 
# 
# layout_matrix_horiz <- rbind(1, c(2,3))
# gpfinal <- grid.arrange(stacked_gp_horiz, SQDG_gp, PG_gp, layout_matrix = layout_matrix_horiz)
# ggsave(filename = "Stacked_IPDAG_complete_horiz.png", plot = gpfinal, device = "png",
#        path = "Images", width = 14, height = 8, units = "in")
# 
