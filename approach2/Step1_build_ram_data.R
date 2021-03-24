
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "approach2/data"

# Read RAM Legacy Database v4.491 (with model fits)
load("/Users/cfree/Dropbox/Chris/UCSB/data/ramldb/RAM v4.491 Files (1-14-20)/RAM v4.491/DB Files With Model Fit Data/R Data/DBdata[mdl][v4.491].RData")


# Build stock key
################################################################################

# Build stock key
stock_key <- stock %>% 
  select(-c(tsn, inmyersdb, myersstockid)) %>% 
  # Add area name
  left_join(select(area, areaid, country, areaname), by="areaid") %>% 
  # Rename columns
  rename(species=scientificname, comm_name=commonname, area=areaname) %>% 
  # Format columns
  mutate(comm_name=freeR::sentcase(comm_name),
         species=gsub("spp.", "spp", species),
         species=recode(species, 
                        "Chrysophrys auratus"="Pagrus auratus",
                        "Clupea pallasii"="Clupea pallasii pallasii",
                        "Epinephelus flavolimbatus"="Hyporthodus flavolimbatus",
                        "Epinephelus niveatus"="Hyporthodus niveatus",
                        "Etrumeus teres"="Etrumeus sadina",
                        "Loligo bleekeri"="Heterololigo bleekeri",
                        "Loligo pealeii"="Doryteuthis pealeii",
                        "Merluccius gayi"="Merluccius gayi gayi",
                        "Mullus barbatus"="Mullus barbatus barbatus",
                        "Neoplatycephalus richardsoni"="Platycephalus richardsoni",
                        "Psetta maxima"="Scophthalmus maximus",
                        "Tetrapturus albidus"="Kajikia albida",
                        "Sardinops melanostictus"="Sardinops sagax",
                        "Clupea bentincki"="Strangomera bentincki",
                        "Raja binoculata"="Beringraja binoculata",
                        "Raja rhina"="Beringraja rhina",
                        "Theragra chalcogramma"="Gadus chalcogrammus")) %>% 
  # Rearrange columns
  select(stockid, stocklong, country, region, area, areaid, species, comm_name)


# Get MSY and recent U/UMSY value
################################################################################

# Get MSY
stock_msy <- bioparams_values_views %>% 
  select(stockid, MSYbest) %>% 
  # Add source
  left_join(bioparams_sources_views %>% select(stockid, MSYbest), by="stockid") %>% 
  # Add units
  left_join(bioparams_units_views %>% select(stockid, MSYbest), by="stockid") %>% 
  # Add notes
  left_join(bioparams_notes_views %>% select(stockid, MSYbest), by="stockid") %>% 
  # Clean up
  rename(msy=MSYbest.x, msy_source=MSYbest.y, msy_units=MSYbest.x.x, msy_code=MSYbest.y.y) %>% 
  # Arrange
  select(stockid, msy_source, msy_code, msy_units, msy, everything())

# Get U/UMSY
stock_uumsy <- timeseries_values_views %>% 
  # Reduce to U/UMSY time series
  select(stockid, year, BdivBmsypref, UdivUmsypref) %>% 
  rename(bbmsy=BdivBmsypref, uumsy=UdivUmsypref) %>% 
  filter(!is.na(uumsy) & year<=2014) %>% 
  # Reduce to current
  group_by(stockid) %>% 
  arrange(desc(year)) %>% 
  slice(1) %>% 
  # Add source
  left_join(timeseries_sources_views %>% select(stockid, BdivBmsypref, UdivUmsypref)) %>% 
  rename(bbmsy_source=BdivBmsypref, uumsy_source=UdivUmsypref) %>% 
  # Add note
  left_join(timeseries_ids_views %>% select(stockid, BdivBmsypref, UdivUmsypref)) %>% 
  rename(bbmsy_notes=BdivBmsypref, uumsy_notes=UdivUmsypref)

# Expand stock key 
stocks1 <- stock_key %>% 
  # Add MSY info
  left_join(stock_msy, by="stockid") %>% 
  # Add U/UMSY info
  left_join(stock_uumsy, by="stockid") %>% 
  # Reduce to stocks with requirements
  filter(!is.na(msy) & !is.na(uumsy))

# Inspect
freeR::complete(stocks1)


# Build catch time series
################################################################################

# Build catch time series
catch_ts <- timeseries_values_views %>% 
  # Columns
  select(stockid, year, TC, TL) %>% 
  rename(tc=TC, tl=TL) %>% 
  # Reduce to stocks of interest 
  filter(stockid %in% stocks1$stockid) %>% 
  # Add units
  left_join(timeseries_units_views %>% select(stockid, TC, TL), by="stockid") %>% 
  rename(tc_units=TC, tl_units=TL) %>% 
  # ID catch to use
  mutate(catch=ifelse(!is.na(tc) & tc_units=="MT", tc, tl),
         catch_type=ifelse(!is.na(tc) & tc_units=="MT", "TC", "TL"),
         catch_units=ifelse(!is.na(tc) & tc_units=="MT", tc_units, tl_units)) %>% 
  # Reduce
  filter(!is.na(catch)) %>% 
  select(stockid, year, catch_type, catch_units, catch)


# Expand catch time series
################################################################################

# Year cutoff
year2_req <- 2000

# Extend catch time series to end of SAUP data (2014)
catch_ts_exp <- expand.grid(stockid=sort(unique(catch_ts$stockid)),
                            year=1950:2014) %>% 
  arrange(stockid, year) %>% 
  # Add real data
  left_join(catch_ts) %>% 
  # Mark real data before extrapolation
  mutate(catch_est=ifelse(!is.na(catch), "reported", "extrapolated")) %>% 
  # Fill gaps based on nearest real data
  group_by(stockid) %>% 
  fill(catch_type, catch_units, catch, .direction = "downup") %>% 
  # Remove extrapolated early data
  mutate(year1=min(year[catch_est=="reported"])) %>% 
  filter(year >= year1) %>% 
  # Remove stocks with data ending beofr cutoff
  mutate(year2=max(year[catch_est=="reported"])) %>% 
  filter(year2 >= year2_req) %>% 
  # Finalize
  ungroup() %>% 
  select(-year1) %>% 
  select(stockid, year, catch_est, everything())

# Inspect data
table(catch_ts_exp$stockid)
table(catch_ts_exp$catch_est)
table(catch_ts_exp$catch_type)
table(catch_ts_exp$catch_units)


# Build LME key
################################################################################

# I'm doing this late so that I only have to figure out the LMEs for the target stocks

# First build
if(F){
  
  # Read RAM boundary key (assessid/stockid)
  lme_key1 <- readxl::read_excel("/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/ram_boundaries/data/ramldb_v3.8_stock_boundary_table_v2_formatted.xlsx")
  
  # Read RAM boundary key (assessid/LME)
  lme_key2 <- readxl::read_excel("/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/ram_boundaries/data/ramldb_v3.8_stock_boundary_centroids_areas_fixed.xlsx")
  
  # Build area-to-LME key
  lme_key <- lme_key1 %>% 
    select(assessid, stockid, country, region, area, area_code) %>% 
    left_join(lme_key2 %>% select(assessid, lme_name), by="assessid") %>% 
    select(stockid, lme_name) %>% 
    unique()
  
  # Add LME to stocks
  stocks1_temp <- stocks1 %>% 
    left_join(lme_key) %>% 
    select(stockid:region, lme_name, everything())
  
  # Export data
  write.csv(stocks1_temp, file=file.path(datadir, "RAM_stock_lme_key_incomplete.csv"), row.names = F)
  
}

# Build final data
################################################################################

# Read LME key
lme_key <- readxl::read_excel(file.path(datadir, "RAM_stock_lme_key.xlsx")) %>% 
  select(stockid, lme_name) %>% 
  rename(lme=lme_name)

# Add recent catch to stock key
stocks2 <- stocks1 %>% 
  # Remove stocks without any catch time series
  filter(stockid %in% catch_ts_exp$stockid) %>% 
  # Add catch in last year of assessment
  left_join(catch_ts, by=c("stockid", "year")) %>% 
  # Add large marine ecoregion
  left_join(lme_key) %>% 
  # Arrange
  select(stockid:region, lme, everything())

# Add ecosystem to catch time series
catch_ts_exp2 <- catch_ts_exp %>% 
  left_join(stocks2 %>% select(stockid, lme), by="stockid") %>% 
  select(lme, everything())


# Export data
################################################################################

# Export
write.csv(stocks2, file=file.path(datadir, "RAM_stock_metadata.csv"), row.names=F)
write.csv(catch_ts_exp2, file=file.path(datadir, "RAM_stock_catch_time_series.csv"), row.names=F)




