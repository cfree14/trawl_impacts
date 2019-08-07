
# Clear workspace
rm(list = ls())
options(scipen = 999)

# Setup
################################################################################

# Packages
library(taxize)
library(tidyverse)
library(seaaroundus)
library(rfishbase)

# Directories
inputdir <- "data/saup/raw"
outputdir <- "data/saup/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file.path(file.path(outputdir, "saup_lme_catch_data.Rds")))


# Build species key
################################################################################

# Species with 3-word names
spp_3words <- c("Diplodus sargus sargus",
                "Sarda chiliensis lineolata", 
                "Osmerus mordax mordax",
                "Mullus barbatus barbatus", 
                "Scomberesox saurus saurus",
                "Trachyscorpia cristulata echinata",
                "Diplodus argenteus argenteus",
                "Diplodus cervinus cervinus",
                "Merluccius gayi gayi",
                "Oncorhynchus masou masou",
                "Patagonotothen brevicauda brevicauda",
                "Emmelichthys nitidus nitidus",
                "Hoplostethus mediterraneus mediterraneus",
                "Clupea pallasii pallasii", 
                "Salvelinus malma malma", 
                "Salvelinus alpinus alpinus")

# Build species key
spp_key <- data_orig %>% 
  # Unique species
  ungroup() %>% 
  select(comm_name, species) %>% 
  rename(sci_name=species) %>% 
  unique() %>% 
  # Determine level: species or not-species
  mutate(taxa_level=ifelse(sapply(strsplit(sci_name, " "), length)==2, "species", "not species")) %>% 
  # Then correct a few exceptions
  mutate(taxa_level=ifelse(sci_name %in% spp_3words, "species", taxa_level))

# Any duplicated common names? Yes.
anyDuplicated(spp_key$comm_name)

# Check species and not-species designations
spp <- spp_key$sci_name[spp_key$taxa_level=="species"]
notspp <- spp_key$sci_name[spp_key$taxa_level=="not species"]
# freeR::check_names(spp)
# freeR::suggest_names(spp)

# Build species key w/ family and resilience
spp_key1 <- spp_key %>% 
  # Reduce to species
  filter(taxa_level=="species") %>% 
  # Match names to FishBase
  rename(species_orig=sci_name) %>% 
  mutate(species=plyr::revalue(species_orig, c("Abudefduf luridus"="Similiparma lurida",
                                         "Clupea bentincki"="Strangomera bentincki",
                                         "Dasyatis akajei"="Hemitrygon akajei", 
                                         "Dasyatis centroura"="Bathytoshia centroura",
                                         "Dasyatis guttata"="Hypanus guttatus",
                                         "Dasyatis longa"="Hypanus longus",
                                         "Diplodus argenteus argenteus"="Diplodus argenteus",
                                         "Diplodus cervinus cervinus"="Diplodus cervinus",
                                         "Diplodus sargus sargus"="Diplodus sargus",
                                         "Gadus ogac"="Gadus macrocephalus",
                                         "Gymnothorax rueppellii"="Gymnothorax rueppelliae",
                                         "Hoplostethus mediterraneus mediterraneus"="Hoplostethus mediterraneus",
                                         "Lepidonotothen nudifrons"="Lindbergichthys nudifrons",
                                         "Liza aurata"="Chelon auratus",
                                         "Liza haematocheila"="Planiliza haematocheila",
                                         "Liza ramada"="Chelon ramada",
                                         "Liza saliens"="Chelon saliens",
                                         "Manta birostris"="Mobula birostris", 
                                         "Moolgarda seheli"="Crenimugil seheli",
                                         "Oncorhynchus masou masou"="Oncorhynchus masou",
                                         "Osmerus mordax mordax"="Osmerus mordax", 
                                         "Pomadasys corvinaeformis"="Haemulopsis corvinaeformis",
                                         "Pseudopentaceros richardsoni"="Pentaceros richardsoni",
                                         "Pseudopentaceros wheeleri"="Pentaceros wheeleri", 
                                         "Pterothrissus belloci"="Nemoossis belloci",
                                         "Raja stellulata"="Beringraja stellulata",
                                         "Rhinobatos percellens"="Pseudobatos percellens", 
                                         "Saccostrea cuccullata"="Saccostrea cucullata",
                                         "Salvelinus alpinus alpinus"="Salvelinus alpinus",
                                         "Salvelinus malma malma"="Salvelinus malma",
                                         "Sarda chiliensis lineolata"="Sarda lineolata",
                                         "Scomberesox saurus saurus"="Scomberesox saurus",
                                         "Theragra chalcogramma"="Gadus chalcogrammus",
                                         "Trigloporus lastoviza"="Chelidonichthys lastoviza",
                                         "Valamugil engeli"="Osteomugil engeli")))

# Get resilience and taxa info  
spp_res <- datalimited2::resilience(spp_key1$species)
spp_taxa <- freeR::taxa(spp_key1$species)

# Get FishBase habitat info
fish <- spp_taxa$sciname[spp_taxa$type=="fish"]
spp_fb <- species(fish, fields=c("Species", "DemersPelag"), server="fishbase") %>% 
  rename(species=Species, habitat=DemersPelag)

# Get SeaLifeBase habitat info
invs <- spp_taxa$sciname[spp_taxa$type=="invert"]
spp_slb <- species(invs, fields=c("Species", "DemersPelag"), server="sealifebase") %>% 
  rename(species=Species, habitat=DemersPelag)

# Merge FB/SLB habitat info
spp_habs <- rbind(spp_fb, spp_slb) %>% 
  filter(!is.na(species))

# Build key
spp_key2 <- spp_key1 %>% 
  left_join(select(spp_taxa, -species), by=c("species"="sciname")) %>% 
  left_join(spp_res, by="species") %>% 
  left_join(spp_habs, by="species") %>% 
  select(type:genus, species, species_orig, comm_name, resilience, habitat) %>% 
  rename(species_saup=species_orig)

# Inspect and export
freeR::complete(spp_key2)
write.csv(spp_key2, file=file.path(outputdir, "SAUP_species_taxonomy_resilience.csv"))


# Stocks to use
################################################################################

# Number of required years
nyr_req <- 20
max_mt_req <- 1000

# Identify stocks to use
stocks <- data_orig %>%
  # Calculate reported catch statistics
  ungroup() %>% 
  group_by(lme, species, comm_name) %>% 
  summarize(rep_mt_nyr=sum(!is.na(reported_mt)), 
            rep_mt_avg=mean(reported_mt, na.rm=T),
            rep_mt_max=max(reported_mt, na.rm=T)) %>% 
  # Add taxanomic level
  left_join(select(spp_key, sci_name, taxa_level), by=c("species"="sci_name")) %>% 
  # Filters
  # 1) Species-specific
  # 2) More than XX years of data
  # 3) More than XXXX mt of catch
  filter(taxa_level=="species", 
         rep_mt_nyr>=nyr_req,
         rep_mt_max>=max_mt_req) %>% 
  # Add stock id
  mutate(stockid=paste(lme, species, sep="-")) %>% 
  # Add family, resilience, and corrected scientific name
  rename(species_saup=species) %>% 
  left_join(select(spp_key2, species_saup, species, family, resilience), by="species_saup") %>% 
  # Arrange columns (remove taxa level)
  select(stockid, lme, species_saup, species, comm_name, family, resilience, everything()) %>% 
  select(-taxa_level)
  
# Subset data to use
data_use <- data_orig %>% 
  ungroup() %>% 
  # Add stockid and subset
  mutate(stockid=paste(lme, species, sep="-")) %>% 
  filter(stockid %in% stocks$stockid)

# Gap fill total data
data_use_tot <- data_use %>% 
  select(stockid, year, total_mt) %>% 
  # Fill gaps with zeros
  spread(key="year", value="total_mt", fill=0) %>% 
  gather(key="year", value="total_mt", 2:ncol(.)) %>% 
  # Arrange by year and trim leading zeros
  arrange(stockid, year) %>% 
  group_by(stockid) %>% 
  filter(year>=min(year[total_mt>0]))

# Gap fill reported data
data_use_rep <- data_use %>% 
  select(stockid, year, reported_mt) %>% 
  # Fill gaps with zeros
  spread(key="year", value="reported_mt", fill=0) %>% 
  gather(key="year", value="reported_mt", 2:ncol(.)) %>% 
  # Arrange by year and trim leading zeros
  arrange(stockid, year) %>% 
  group_by(stockid) %>% 
  filter(year>=min(year[reported_mt>0]))

# Merge gap-filled reported and total data and add unreported catch column
data <- data_use_tot %>% 
  # Add reported catch
  left_join(data_use_rep, by=c("stockid", "year")) %>% 
  # Calculate unreported catch
  mutate(unreported_mt=total_mt-reported_mt) %>% 
  # Arrange columns
  select(stockid, year, reported_mt, unreported_mt, total_mt) %>% 
  mutate(year=as.numeric(year))

# Export
save(data, stocks, file=file.path(outputdir, "saup_lme_catch_data_to_use.Rdata"))


# Proportion of global catch
################################################################################

# Total reported / reconstructed catch by year
tl_mt <- data_orig %>% 
  group_by(year) %>% 
  summarize(rep_mt=sum(reported_mt, na.rm=T),
            tot_mt=sum(total_mt, na.rm=T))

# Used reported / reconstructed catch by year
tl_mt_use <- data %>% 
  group_by(year) %>% 
  summarize(rep_mt_use=sum(reported_mt, na.rm=T),
            tot_mt_use=sum(total_mt, na.rm=T))  

# Merge
tl_mt_stats <- tl_mt_use %>% 
  left_join(tl_mt, by="year") %>% 
  mutate(rep_prop=rep_mt_use/rep_mt,
         tot_prop=tot_mt_use/tot_mt)

# Percentages
tl_mt_stats$rep_prop[tl_mt_stats$year==2014]*100


# Proportion of EEZ catch
################################################################################

# # Packages
# library(sf)
# 
# # Read LMEs
# lmes <- sf::st_read(dsn="/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/lme/lmes/", layer="LME66")
# 
# # Theme
# my_theme <- theme(axis.text=element_text(size=10),
#                   axis.title=element_text(size=10),
#                   legend.text=element_text(size=8),
#                   legend.title=element_text(size=10), 
#                   strip.text=element_text(size=10),
#                   plot.title=element_text(size=10),
#                   legend.position = "bottom",
#                   # panel.grid.major = element_blank(), 
#                   panel.grid.major = element_line(colour = 'transparent'),
#                   panel.grid.minor = element_blank(),
#                   panel.background = element_blank(), 
#                   axis.line = element_line(colour = "black"))
# 
# # World layer
# world <- rnaturalearth::ne_countries(scale = "large", type = "countries", returnclass = "sf")
# 
# # Plot LMEs
# g <- ggplot() +
#   geom_sf(data=world, fill="grey80", lwd=0.05, color="white") +
#   geom_sf(data=lmes, mapping=aes(fill=Shape_Area)) +
#   theme_bw() + my_theme
# 
# # Export
# ggsave(plot=g, file.path(plotdir, "figure_lme_coverage.png"), width=5.5, height=4.5, units="in", dpi=600)















