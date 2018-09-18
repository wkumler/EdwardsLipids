# Stacked_IPDAGs.R

# Code designed to make Figure 3 of the poster and do stats

# Input: Clean_Complete.csv
# Output: SQDPG barplot, 

# Startup things ----
library(dplyr)
library(ggplot2)

LOBdata <- read.csv("Data/Clean_Complete.csv", stringsAsFactors = F)

classify.MvP <- function(station.number) {
  if(station.number %in% c(1,2,4,6,11)){
    return("Monterey")
  } else {
    return("Point Reyes")
  }
}
locs <- sapply(LOBdata$Station, classify.MvP)
LOBdata$Location <- locs


# Grab SQDG and PG values ----
SQDG_df_samples <- LOBdata %>% 
  filter(species=="SQDG") %>% 
  group_by(Orbi_num, Location) %>%
  summarize("Total SQDG"=sum(intensity))
SQDG_Mont_samp <- filter(SQDG_df_samples, Location=="Monterey") %>% pull(`Total SQDG`)
SQDG_PtR_samp <- filter(SQDG_df_samples, Location=="Point Reyes") %>% pull(`Total SQDG`)

PG_df_samples <- LOBdata %>% 
  filter(species=="PG") %>% 
  group_by(Orbi_num, Location) %>%
  summarize("Total PG"=sum(intensity))
PG_Mont_samp <- filter(PG_df_samples, Location=="Monterey") %>% pull(`Total PG`)
PG_PtR_samp <- filter(PG_df_samples, Location=="Point Reyes") %>% pull(`Total PG`)

SQDPG_Mont_samp <- SQDG_Mont_samp/PG_Mont_samp
SQDPG_PtR_samp <- SQDG_PtR_samp/PG_PtR_samp

wilcox.test(SQDPG_Mont_samp, SQDG_PtR_samp)
boxplot(log(SQDPG_Mont_samp), log(SQDG_PtR_samp))




SQDG_stations <- LOBdata %>% 
  filter(species=="SQDG") %>% 
  group_by(Station) %>%
  summarize("Total SQDG"=sum(intensity)) %>%
  pull(`Total SQDG`)

PG_stations <- LOBdata %>%
  filter(species=="PG") %>% 
  group_by(Station) %>%
  summarize("Total PG"=sum(intensity)) %>%
  pull(`Total PG`)

SQDPG <- SQDG_stations/PG_stations
loc = c(rep("Monterey", 4), rep("Point Reyes", 4), "Monterey")

df <- data.frame(x=sort(unique(LOBdata$Station)), y=SQDPG, loc=loc)

ggplot(data = df, aes(x=as.factor(x), y=y)) + 
  geom_bar(stat = "identity") + 
  facet_wrap(~loc, scales = "free_x") +
  ylab("SQDG/PG Ratio") +
  xlab("Station Number") +
  theme(legend.title=element_blank(), 
        axis.text = element_text(size = 24, face = "bold", color="black"),
        axis.title = element_text(size = 24, face = "bold"))

ggsave(filename = "SQDPG Barplot.png", device = "png", path = "Images",
       width = 10, height = 4, units = "in")
