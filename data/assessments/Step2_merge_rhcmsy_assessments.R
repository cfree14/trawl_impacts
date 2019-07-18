
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
  left_join(ref_key, by="stockid")


# Export data
################################################################################

# Export
save(stocks, data, file=file.path(outputdir1, "SAUP_LME_rhcmsy_results.Rdata"))


# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  # panel.grid.major = element_blank(), 
                  # panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Terminal B/BMSY
g <- ggplot(stocks, aes(x=bbmsy_last, fill=status_last)) +
  geom_histogram(binwidth=0.05, boundary=0) +
  labs(x=expression("B/B"["MSY"]), y="Frequency") +
  scale_fill_discrete(name="Status") +
  theme_bw() + my_theme
g
ggsave(g, filename=file.path(plotdir, "figure_saup_stock_status.png"), width=6.5, height=3.5, units="in", dpi=600)



# Terminal U/UMSY
g <- ggplot(stocks, aes(x=uumsy_last)) +
  geom_histogram(binwidth=0.05) +
  labs(x=expression("F/F"["MSY"]), y="Frequency") +
  theme_bw()
g
  
  




