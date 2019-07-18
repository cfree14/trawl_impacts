
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
inputdir <- "data/saup/raw"
outputdir <- "data/saup/processed"

# Read data
data <- read.csv(file.path(inputdir, "SeaAroundUs_all.csv"), as.is=T)


# Inspect data
################################################################################

# Inspect data
colnames(data)
sort(unique(data$area_name))
sort(unique(data$area_type))
sort(unique(data$functional_group))
sort(unique(data$commercial_group))
sort(unique(data$fishing_entity)) # EEZs
sort(unique(data$fishing_sector))
sort(unique(data$gear_type))

# Format data
################################################################################

# Format data
lme_data <- data %>% 
  group_by(area_name, year, scientific_name, common_name, reporting_status) %>% 
  summarize(tl_mt=sum(tonnes)) %>% 
  ungroup() %>% 
  spread(key="reporting_status", value="tl_mt") %>% 
  rename(lme=area_name, species=scientific_name, comm_name=common_name, 
         reported_mt=Reported, unreported_mt=Unreported) %>% 
  mutate(total_mt=rowSums(cbind(reported_mt, unreported_mt), na.rm = T)) %>% 
  arrange(lme, species, year)

# Export data
saveRDS(lme_data, file.path(outputdir, "saup_lme_catch_data.Rds"))


# Plot data
################################################################################

# Plot
rep_mt <- lme_data %>% 
  group_by(year) %>% 
  summarise(tl_mt=sum(reported_mt, na.rm=T)/1e6)
plot(tl_mt ~ year, rep_mt, type="l", bty="n", ylim=c(0,80), xlab="", ylab="Reported landings (MMT)")





