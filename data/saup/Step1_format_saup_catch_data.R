
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
sort(unique(data$catch_type))


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



# Percent of each stock caught by trawl fisheries
################################################################################

# Trawl gears
trawl_gears <- c("beam trawl", "bottom trawl", "otter trawl", "pelagic trawl", "shrimp trawl")

# Average over last X years
n_yrs <- 5

# Percent of both reported and total catch coming from trawl fisheries (mean last 5 years)
stats <- data %>% 
  # Add column for trawl/non-trawl
  mutate(trawl=ifelse(gear_type %in% trawl_gears, "trawl", "non-trawl")) %>% 
  # Calculate reported/total catch by trawl/non-trawl in each year
  group_by(area_name, scientific_name, common_name, trawl, year) %>% 
  summarize(reported_mt=sum(tonnes[reporting_status=="Reported"], na.rm=T),
            total_mt=sum(tonnes, na.rm=T)) %>% 
  # Reduce to last 5 years
  filter(year %in% (max(year)-n_yrs+1): max(year)) %>% 
  # Calculate stock-level stats
  group_by(area_name, scientific_name, common_name, year) %>% 
  summarize(trawl_mt_rep=sum(reported_mt[trawl=="trawl"], na.rm=T),
            total_mt_rep=sum(reported_mt, na.rm=T),
            trawl_perc_rep=trawl_mt_rep/total_mt_rep,
            trawl_mt_tot=sum(reported_mt[trawl=="trawl"], na.rm=T),
            total_mt_tot=sum(reported_mt, na.rm=T),
            trawl_perc_tot=trawl_mt_tot/total_mt_tot) %>% 
  # Calculate mean
  group_by(area_name, scientific_name, common_name) %>% 
  summarize(trawl_perc_rep=mean(trawl_perc_rep, na.rm=T)*100,
            trawl_perc_tot=mean(trawl_perc_tot, na.rm=T)*100) %>% 
  # Add stockid column
  mutate(stockid=paste(area_name, scientific_name, sep="-"))

# Export
write.csv(stats, file=file.path(outputdir, "saup_trawl_percent_key.csv"), row.names=F)





