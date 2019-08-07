
# Clear workspace
rm(list = ls())
options(scipen = 999)

# Setup
################################################################################

# Packages
library(taxize)
library(tidyverse)
library(seaaroundus)
library(datalimited2)

# Directories
plotdir <- "figures"
inputdir <- "data/saup/processed"
outputdir <- "data/assessments/raw"
outputdir1 <- "data/assessments/processed"

# Load input data
load(file.path(inputdir, "saup_lme_catch_data_to_use.Rdata"))
rm(data)
stock_key <- stocks
rm(stocks)

# Read habitat key and trawl percentage key
hab_key <- read.csv(file.path(inputdir, "SAUP_species_taxonomy_resilience.csv"), as.is=T)
ptrawl_key <- read.csv(file.path(inputdir, "saup_trawl_percent_key.csv"), as.is=T)

# Merge files
################################################################################

# Read data
files <- list.files(outputdir)

# Loop through and merge
data_merged <- purrr::map_df(files, function(x) {
  
  # Read file
  output <- readRDS(file.path(outputdir, x))
  stockid <- gsub(".Rds", "", x)
  
  # Extract data
  preds <- output$preds_pt
  rks <- output$id_rk_ests
  
  # Format rk data
  rks1 <- rks %>% 
    mutate(stock=stockid) %>% 
    select(stock, r, k)
  
  # Merge data
  preds1 <- preds %>% 
    left_join(rks1, by="stock") %>% 
    select(stock, r, k, everything())
  
})

# Format files
################################################################################

# Shape parameter
p <- 0.2

# Build data (time series file)
data <- data_merged %>% 
  select(-c(r, k)) %>% 
  rename(stockid=stock, catch_mt=catch, biomass_mt=b)

# Build stock key (stock meta-data file)
ref_key <- data_merged %>% 
  # Rename stockid column
  rename(stockid=stock) %>% 
  # Isolate r/K and get terminal B/BMSY
  group_by(stockid, r, k) %>%
  summarize(year_last=max(year),
            biomass_mt_last=b[year==year_last],
            er_last=er[year==year_last],
            bbmsy_last=bbmsy[year==year_last],
            uumsy_last=uumsy[year==year_last],
            status_last=cut(bbmsy_last, breaks=c(0,0.8,1.2,999), labels=c("Overfished", "Fully fished", "Underfished"))) %>% 
  # Calculate other MSY reference points
  mutate(msy=r*k*(1/(p+1))^(1+1/p),
         bmsy=k*(1/(p+1))^(1/p),
         fmsy=r/p*(1-1/(p+1))) %>% 
  # Rename MSY reference points
  rename(k_mt=k, msy_mt=msy, bmsy_mt=bmsy) %>% 
  select(stockid, year_last, biomass_mt_last, 
         er_last, bbmsy_last, uumsy_last, status_last, 
         r, k_mt, everything())

# Add MSY ref point key to stock key
stocks <- stock_key %>% 
  ungroup() %>% 
  left_join(ref_key, by="stockid") %>% 
  left_join(select(hab_key, species_saup, habitat), by="species_saup") %>% 
  left_join(select(ptrawl_key, stockid, trawl_perc_rep, trawl_perc_tot), by="stockid") %>% 
  select(stockid:resilience, habitat, trawl_perc_rep, trawl_perc_tot, everything())


# Export data
################################################################################

# Export
save(stocks, data, file=file.path(outputdir1, "SAUP_LME_rhcmsy_results.Rdata"))

