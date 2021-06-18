
# Clear workspace
rm(list = ls())
options(scipen = 999)

# Setup
################################################################################

# Packages
library(taxize)
library(tidyverse)
# library(seaaroundus)
library(datalimited2)

# Directories
inputdir <- "approach3/data"
outputdir <- "approach3/output/raw"
outputdir1 <- "approach3/output/processed"

# Id stocks to do
# load(file.path(outputdir1, "SAUP_LME_rhcmsy_results.Rdata"))
# stocks_todo <- filter(stocks, is.na(r))

# Read data
load(file.path(inputdir, "SAUP_stock_data.Rdata"))
stocks <- stocks_saup 
data <- data_saup
rm(data_saup, stocks_saup)

# Read RH-CMSY
source("/Users/cfree/Dropbox/Chris/Rutgers/projects/rh_cmsy/code/functions/rh_cmsy_new.R")


# Apply RH-cMSY
################################################################################

# Setup parallel
library(doParallel)
ncores <- detectCores()
registerDoParallel(cores=ncores)

# Loop through stocks and apply RH-cMSY
i <- 1
# 3 and 4 = no viable trajectories
stocks$error <- ""
foreach(i=1:nrow(stocks)) %dopar% {
  
  # Build data
  # stock, year, catch
  # stock <- stocks$stockid[i]
  stock <- stocks$stockid[i]
  sdata <- data %>% 
    ungroup() %>% 
    # Reduce to stock of interest
    filter(stockid==stock) %>% 
    # Reduce columns
    select(stockid, year, total_adj_mt) %>% 
    # Use ADJUSTED total catch
    rename(stock=stockid, catch=total_adj_mt)
  
  # Build key
  # species, family, resilience, id_fixed
  key <- stocks %>% 
    ungroup() %>% 
    filter(stockid==stock) %>% 
    select(species, family, resilience) %>% 
    mutate(id_fixed=T)
  
  # Run RH-cMSY
  output <- try(rh_cmsy(data=sdata, key=key, npairs=10000))
  # plot_rh_cmsy(output)
  
  # If it didn't fail...
  if(!inherits(output, "try-error")){
    # Reduce size of output
    output1 <- output[c("preds_pt", "id_rk_ests")]
    # Export output
    outfile <- paste0(stock, ".Rds")
    saveRDS(output, file=file.path(outputdir, outfile))
  # If it doesn't work, record error
  }else{
    stocks$error[i] <- trimws(strsplit(output[[1]], "\n")[[1]][2])
  }
  
}

