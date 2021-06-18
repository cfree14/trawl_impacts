
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
plotdir <- "approach3/figures"
datadir <- "approach3/data"
outputdir <- "approach3/output/raw"
outputdir1 <- "approach3/output/processed"

# Load input data
load(file.path(datadir, "SAUP_stock_data.Rdata"))
stock_key <- stocks_saup
rm(data_saup, stocks_saup)


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
  ungroup() %>% 
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
  select(stockid:resilience, everything())


# Export data
################################################################################

# Export
save(stocks, data, file=file.path(outputdir1, "SAUP_LME_rhcmsy_results.Rdata"))

