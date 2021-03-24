
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
saupdir <- "data/saup/processed"
datadir <- "approach2/data"

# Read RAM data
data_ram <- read.csv(file=file.path(datadir, "RAM_stock_catch_time_series.csv"), as.is=T)
stocks_ram <- read.csv(file=file.path(datadir, "RAM_stock_metadata.csv"), as.is=T)

# Read SAUP data
load(file.path(saupdir, "saup_lme_catch_data_to_use.Rdata"))


# Format SAUP LME names to match RAM LME names
################################################################################

# Check RAM LMEs against SAUP LMEs
ram_lmes <- sort(unique(stocks_ram$lme))
saup_lmes <- sort(unique(stocks$lme))
ram_lmes[!ram_lmes%in%saup_lmes]

# Fix stocks
stocks_saup <- stocks %>% 
  # SAUP = RAM
  mutate(lme=recode(lme,
                    "Newfoundland-Labrador Shelf"="Labrador - Newfoundland",
                    "Sea of Japan / East Sea"="Sea of Japan",
                    "Southwest Australian Shelf"="South West Australian Shelf",
                    "East-Central Australian Shelf"="East Central Australian Shelf")) %>% 
  # Correct stock id
  rename(stockid_orig=stockid) %>% 
  mutate(stockid=paste(lme, species, sep="-")) %>% 
  ungroup() %>% 
  # Remove outdated catch statistics
  select(-c(rep_mt_nyr, rep_mt_avg, rep_mt_max))

# Check RAM LMEs against SAUP LMEs
ram_lmes <- sort(unique(stocks_ram$lme))
saup_lmes <- sort(unique(stocks_saup$lme))
ram_lmes[!ram_lmes%in%saup_lmes]

# Fix data
data_saup <- data %>% 
  ungroup() %>% 
 # Add important things
  rename(stockid_orig=stockid) %>% 
  left_join(stocks_saup %>% select(stockid_orig, stockid), by="stockid_orig") %>% 
  select(stockid, everything())

# Remove RAM catch
################################################################################

# Calculate total RAM catch
data_ram_tot <- data_ram %>% 
  left_join(stocks_ram %>% select(stockid, species), by="stockid") %>% 
  group_by(lme, species, year) %>% 
  summarize(ram_mt=sum(catch)) %>% 
  ungroup() %>% 
  mutate(stockid=paste(lme, species, sep="-")) %>% 
  select(stockid, everything())

# Build data
data_final <- data_saup %>% 
  # Add RAM catch
  left_join(data_ram_tot %>% select(stockid, year, ram_mt), by=c("stockid", "year")) %>% 
  mutate(ram_mt=ifelse(is.na(ram_mt), 0, ram_mt)) %>% 
  # Adjust SAUP catch
  mutate(reported_adj_mt=pmax(0, reported_mt-ram_mt),
         total_adj_mt=pmax(0, total_mt-ram_mt))


# Export data
##############################

data_saup <- data_final 
stocks_saup <- stocks_saup
save(data_saup, stocks_saup, file=file.path(datadir, "SAUP_stock_data.Rdata"))


