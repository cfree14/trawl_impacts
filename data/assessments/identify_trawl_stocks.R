
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
inputdir <- "data/saup/processed"
outputdir <- "data/assessments/raw"
outputdir1 <- "data/assessments/processed"

# Id stocks to do
# load(file.path(outputdir1, "SAUP_LME_rhcmsy_results.Rdata"))
# stocks_todo <- filter(stocks, is.na(r))

# Read data
load(file.path(inputdir, "saup_lme_catch_data_to_use.Rdata"))