
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
library(ggplus)

# Directories
inputdir <- "data/saup/processed"
outputdir <- "data/assessments/raw"
outputdir1 <- "data/assessments/processed"
plotdir <- "figures"

# Id stocks to do
load(file.path(outputdir1, "SAUP_LME_rhcmsy_results.Rdata"))
stocks_todo <- filter(stocks, is.na(r))

# Read data
load(file.path(inputdir, "saup_lme_catch_data_to_use.Rdata"))


# Calculate r priors
################################################################################

# Build species key
key <- stocks %>% 
  ungroup() %>% 
  select(species, family, resilience) %>% 
  mutate(id_fixed=T) %>% 
  unique()

# Calculate r priors for all species
r_priors <- purrr::map_df(1:nrow(key), function(x){calc_r_priors(key[x,])})

# Identify and format FishLife r priors
r_priors_fl <- r_priors %>% 
  filter(use=="FishLife") %>% 
  unique() %>% 
  # Calculate CV
  mutate(cv=sqrt(exp(ln_r_sd)-1),
         sd=exp(ln_r_sd)) %>% 
  # Arrange by species
  arrange(ln_r_sd)



# Build densities for plotting
################################################################################

# Build densities
dens <- purrr::map_df(1:nrow(r_priors_fl), function(x){
  spp <- r_priors_fl$species[x]
  vals <- seq(0, 1.25, 0.0025)
  dens <- dlnorm(x=vals, meanlog=r_priors_fl$ln_r_mu[x], sdlog=r_priors_fl$ln_r_sd[x])
  df <- data.frame(species=spp, r=vals, density=dens, stringsAsFactors = F)
})

# Format densities
dens1 <- dens %>% 
  # Add distribution properties
  left_join(select(r_priors_fl, species, ln_r_sd, ln_r_mu), by="species") %>% 
  arrange(ln_r_sd, species, density)


# Plot r priors
################################################################################

# Plot one
dens2 <- filter(dens1, species=="Gadus morhua")
g <- ggplot(dens2, aes(x=r, y=density)) +
  geom_area(fill="grey60") +
  facet_wrap(~ species, ncol=3, scale="free") +
  labs(x="Intrinsic growth rate, r", y="") +
  theme_bw()
g

# Plot all
g <- ggplot(dens1, aes(x=r, y=density)) +
  geom_area(fill="grey60") +
  labs(x="Intrinsic growth rate, r", y="") +
  theme_bw() + theme(axis.text.y = element_blank())

# Creat multi-page PDF
pdf(file.path(plotdir, "SAUP_species_r_priors.pdf"), width=8.5, height=11)
g_multi <- facet_multiple(plot=g, facets="species", ncol = 5, nrow = 6, scales="free")
dev.off()



# Toy example to understand log-normal distribution
################################################################################









