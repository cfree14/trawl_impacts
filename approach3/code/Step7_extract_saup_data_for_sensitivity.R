
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

# Number of samples
n_use <- 100

# Loop through and merge
x <- files[1]
data_merged <- purrr::map_df(files, function(x) {
  
  # Read file
  output <- readRDS(file.path(outputdir, x))
  stockid <- gsub(".Rds", "", x)
  
  # Indexes to sample
  index_use <- sample(1:ncol(output$bbmsy_v), size=pmin(n_use, ncol(output$bbmsy_v)))
  
  # Status viable (last year)
  nyr <- nrow(output$bbmsy_v)
  b_v <- output$b_v[nyr, ]
  er_v <- output$er_v[nyr, ]
  bbmsy_v <- output$bbmsy_v[nyr, ]
  uumsy_v <- output$uumsy_v[nyr, ]
  
  # MSY viable
  p <- 0.2
  r_v <- output$id_rk_v$r
  k_v <- output$id_rk_v$k
  msy_v <- r_v*k_v*(1/(p+1))^(1+1/p)
  
  # Assemble
  out <- tibble(stockid=stockid,
                iteration=1:length(bbmsy_v),
                r=r_v,
                k=k_v,
                msy_mt=msy_v,
                biomass_mt=b_v,
                er=er_v,
                bbmsy=bbmsy_v, 
                uumsy=uumsy_v)
  
})


# Export data
################################################################################

# Export
saveRDS(data_merged, file=file.path(outputdir1, "SAUP_stock_status_posterior.rds"))
# write.csv(data_merged, file=file.path(outputdir1, "SAUP_stock_status_posterior.csv"), row.names=F)

