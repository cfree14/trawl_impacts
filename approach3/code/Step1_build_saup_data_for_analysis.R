
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
inputdir <- "data/saup/processed"
outputdir <- "approach3/data"
plotdir <- "approach3/figures"

# Read data
data_orig <- readRDS(file.path(file.path(inputdir, "saup_lme_catch_data.Rds")))

# Inspect SAUP data
################################################################################

# Annual catch
stats_orig <- data_orig %>% 
  group_by(year) %>% 
  summarize(reported_mt=sum(reported_mt, na.rm = T),
            unreported_mt=sum(unreported_mt, na.rm = T)) %>% 
  ungroup %>% 
  gather(key="catch_type", value="catch_mt", 2:3) %>% 
  mutate(catch_type=recode_factor(catch_type,
                           "unreported_mt"="Unreported",
                           "reported_mt"="Reported"))

# Plot annual catch
ggplot(stats_orig, mapping=aes(x=year, y=catch_mt/1e6, fill=catch_type)) +
  geom_bar(stat="identity") +
  labs(x="Year", y="Landings (millions of mt)") +
  scale_fill_discrete(name="") +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  theme_bw()


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
spp_res <- freeR::resilience(spp_key1$species)
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


# Build non-species key
################################################################################

# Family names
all_fish <- freeR::all_fish()
families <- sort(unique(all_fish$family))
gen_key <- all_fish %>% 
  group_by(genus) %>% 
  summarize(families=paste(sort(unique(family)), collapse=","))

# Get resilience values for all fish
# This takes forever to run so export and read results
if(F){
  res_all <- freeR::resilience(all_fish$sciname)
  write.csv(res_all, file=file.path(outputdir, "FB_SLB_resilience_values_all_species.csv"), row.names=F)
}else{
  res_all <- read.csv(file=file.path(outputdir, "FB_SLB_resilience_values_all_species.csv"), as.is=T)
  
}

# Resilience by genus
res_gen <- res_all %>% 
  # Add taxanomic info
  left_join(all_fish %>% select(-species), by=c("species"="sciname")) %>% 
  # Summarize by genus
  group_by(family, genus, resilience) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Find most common
  group_by(family, genus) %>% 
  arrange(family, genus, desc(n)) %>% 
  slice(1) %>% 
  # Rename
  rename(resilience_gen=resilience)

# Resilience by family
res_fam <- res_all %>% 
  # Add taxanomic info
  left_join(all_fish %>% select(-species), by=c("species"="sciname")) %>% 
  # Summarize by family
  group_by(family, resilience) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Find most common
  group_by(family) %>% 
  arrange(family, desc(n)) %>% 
  slice(1) %>% 
  # Rename
  rename(resilience_fam=resilience)

# Resilience by order
res_ord <- res_all %>% 
  # Add taxanomic info
  left_join(all_fish %>% select(-species), by=c("species"="sciname")) %>% 
  # Summarize by family
  group_by(order, resilience) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Find most common
  group_by(order) %>% 
  arrange(order, desc(n)) %>% 
  slice(1) %>% 
  # Rename
  rename(resilience_ord=resilience)
  

# Build non-species key
nonspp_key <- spp_key %>% 
  # Reduce to species
  filter(taxa_level!="species") %>% 
  # Add family
  mutate(family=ifelse(sci_name %in% families, sci_name, NA)) %>% 
  # Add family based on genus
  left_join(gen_key, by=c("sci_name"="genus")) %>% 
  # Merge
  mutate(family=ifelse(!is.na(family), family, families),
         family=recode(family, 
                       "Cardiidae,Tridacnidae"="Cardiidae",
                       "Hippolytidae,Palaemonidae"="Palaemonidae",
                       "Mytilidae,Pyramidellidae"="Mytilidae")) %>% 
  select(-families) %>% 
  # Assign resilience value based on genus, family, order
  left_join(res_gen %>% select(-n), by=c("family", "sci_name"="genus")) %>% 
  left_join(res_fam %>% select(-n), by="family") %>% 
  left_join(res_ord %>% select(-n), by=c("sci_name"="order")) %>% 
  # Finalize resilience
  mutate(resilience=ifelse(!is.na(resilience_gen), resilience_gen, 
                           ifelse(!is.na(resilience_fam), resilience_fam, resilience_ord))) %>% 
  # Simplify
  rename(species_saup=sci_name) %>% 
  select(species_saup, comm_name, taxa_level, family, resilience)
  

# Merge species, family, resilience keys
################################################################################

# Format species-spefic family-resilience key for merge
spp_key3 <- spp_key2 %>% 
  mutate(taxa_level="species") %>% 
  select(species_saup, species, comm_name, taxa_level, family, resilience)

# Merge species-speficand not species-specific family-resilience key
spp_res_family <- bind_rows(spp_key3, nonspp_key)



# Build stocks key
################################################################################

# Build stock key
stocks <- data_orig %>%
  # Calculate reported catch statistics
  ungroup() %>% 
  group_by(lme, species, comm_name) %>% 
  summarize(rep_mt_nyr=sum(!is.na(reported_mt)), 
            rep_mt_avg=mean(reported_mt, na.rm=T),
            rep_mt_max=max(reported_mt, na.rm=T)) %>% 
  ungroup() %>%
  # Add family, resilience, and corrected scientific name
  rename(species_saup=species) %>% 
  left_join(select(spp_res_family, species_saup, species, taxa_level, family, resilience), by="species_saup") %>% 
  # Add stock id
  mutate(stockid=ifelse(taxa_level=="species", paste(lme, species, sep="-"), paste(lme, species_saup, sep="-"))) %>% 
  # Arrange columns (remove taxa level)
  select(stockid, lme, species_saup, species, comm_name, taxa_level, family, resilience, everything()) %>% 
  # Filter unusable
  filter(rep_mt_nyr>=10)

# Inspect completeness
freeR::complete(stocks)


# Build final data
################################################################################

# Reduce to data to use
data_use <- data_orig %>% 
  # Add taxanomic level
  left_join(select(spp_key, sci_name, taxa_level), by=c("species"="sci_name")) %>% 
  # Add family, resilience, and corrected scientific name
  rename(species_saup=species) %>% 
  left_join(select(spp_key2, species_saup, species, family, resilience), by="species_saup") %>% 
  # Add stock id
  mutate(stockid=ifelse(taxa_level=="species", paste(lme, species, sep="-"), paste(lme, species_saup, sep="-"))) %>% 
  # Reduce to stock ids of interest
  filter(stockid %in% stocks$stockid)

# Format total data
data_tot <- data_use %>%
  # Simplify
  select(stockid, year, total_mt) %>% 
  # Fill gaps with zeros
  spread(key="year", value="total_mt", fill=0) %>% 
  gather(key="year", value="total_mt", 2:ncol(.))

# Inspect
freeR::complete(data_tot)

# Format reported data
data_rep <- data_use %>%
  # Simplify
  select(stockid, year, reported_mt) %>% 
  # Fill gaps with zeros
  spread(key="year", value="reported_mt", fill=0) %>% 
  gather(key="year", value="reported_mt", 2:ncol(.))

# Inspect
freeR::complete(data_rep)

# Merge gap-filled reported and total data and add unreported catch column
data <- data_tot %>% 
  # Add reported catch
  left_join(data_rep, by=c("stockid", "year")) %>% 
  # Calculate unreported catch
  mutate(unreported_mt=total_mt-reported_mt) %>% 
  # Arrange columns
  select(stockid, year, reported_mt, unreported_mt, total_mt) %>% 
  mutate(year=as.numeric(year))

# Inspect
freeR::complete(data)

# Export
save(data, stocks, file=file.path(outputdir, "saup_lme_catch_data_to_use.Rdata"))

# Annual catch
stats <- data %>% 
  group_by(year) %>% 
  summarize(reported_mt=sum(reported_mt, na.rm = T),
            unreported_mt=sum(unreported_mt, na.rm = T)) %>% 
  ungroup %>% 
  gather(key="catch_type", value="catch_mt", 2:3) %>% 
  mutate(catch_type=recode_factor(catch_type,
                                  "unreported_mt"="Unreported",
                                  "reported_mt"="Reported"))

# Plot annual catch
ggplot(stats, mapping=aes(x=year, y=catch_mt/1e6, fill=catch_type)) +
  geom_bar(stat="identity") +
  labs(x="Year", y="Landings (millions of mt)") +
  scale_fill_discrete(name="") +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  theme_bw()

