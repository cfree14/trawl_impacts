

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(TMB)
library(TMBhelper)
library(tidyverse)
library(splink)

# Directories
datadir <- "approach2/data"
plotdir <- "approach2/figures"


# Fit models
################################################################################

# Get RAM data
data <- splink::ram

# Prep RAM data
data_prepped <- splink::prep_data(data)

# Inspect a sample of prepped data
splink::plot_raw(data_prepped, n=20)

# Use simple NLS fit to identify problematic stocks
output <- splink::fit_sp_nls(data = data_prepped, p=0.2)

# Inspect fits
hist(output$r, breaks=seq(0,10,0.2))
hist(output$k, xlim=c(0,10), breaks=seq(0,10000,0.2))

# Problem stocks
problem_stocks <- output %>%
  filter(is.na(r) | is.na(k) | r>3 | k>10) %>%
  pull(stockid)

problem_stocks <- c(problem_stocks, "SARDMEDGSA6", "SNOWCRABBS")

# Remove problem stocks
data_final <- data_prepped %>%
  filter(!stockid %in% problem_stocks)

# Fit to data
output <- splink::fit_sp(data = data_final, p=0.2)

# Extract results
results <- splink::get_results(output)

# Plot results
splink::plot_results(results)

# Plot fits
# splink::plot_fits(output, plotdir, "AppendixA_RAM_sp_fits.pdf")

# Maximum biomass key
max_tb <- data_final %>% 
  group_by(stockid) %>% 
  summarize(biomass_max=max(biomass)) %>% 
  ungroup()

# Extra results formatting
results_out <-   results %>% 
  # Spread
  select(stockid, param, est) %>% 
  spread(key = "param", value="est") %>% 
  # Add maximum biomass
  left_join(max_tb) %>% 
  rename(B0_scalar=B0) %>% 
  # Calculate reference points
  mutate(p=0.2, 
         B0=B0_scalar*biomass_max,
         bmsy=B0* (1 / (p+1)) ^ (1/p),
         fmsy=r / p * (1-1/(p+1)), 
         msy=bmsy*fmsy)



# Export data
################################################################################

# Export data
save(output, results, file=file.path(datadir, "RAM_sp_model_fits.Rdata"))
write.csv(results_out, file.path(datadir, "RAM_sp_model_fits.csv"), row.names=F)


