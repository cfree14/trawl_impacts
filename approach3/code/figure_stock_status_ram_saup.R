
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "approach3/data"
outputdir <- "approach3/output/processed"
plotdir <- "approach3/figures"

# Read RAM data
stocks_ram <- read.csv(file=file.path(datadir, "RAM_stock_metadata.csv"), as.is=T)

# Read SAUP data
load(file.path(outputdir, "SAUP_LME_rhcmsy_results.Rdata"))
stocks_saup <- stocks
rm(data, stocks)

# Merge stock data
################################################################################

# RAM
stocks_ram1 <- stocks_ram %>% 
  # Add source
  mutate(source="RAM") %>% 
  # Rename columns
  select(-msy_mt) %>% 
  rename(r=r_est, k_mt=k_mt_est, msy_mt=msy_mt_est) %>% 
  # Arrange
  select(source, stockid, lme, comm_name, species, year, biomass_mt, er, bbmsy, uumsy, msy_mt, bmsy_mt, umsy, r, k_mt)

# SAUP
stocks_saup1 <- stocks_saup %>% 
  # Add source and year
  mutate(source="SAUP") %>% 
  # Rename columns
  rename(year=year_last, bbmsy=bbmsy_last, uumsy=uumsy_last, umsy=fmsy, er=er_last, biomass_mt=biomass_mt_last) %>%
  # Arrange
  select(source, stockid, lme, comm_name, species, year, biomass_mt, er, bbmsy, uumsy, msy_mt, bmsy_mt, umsy, r, k_mt)

# Merge
stocks <- bind_rows(stocks_ram1, stocks_saup1)

table(stocks$source)

# Export
write.csv(stocks, file=file.path(outputdir, "RAM_SAUP_terminal_status.csv"), row.names = F)


# Kobe plot
################################################################################

# Plot
g <- ggplot(stocks, aes(x=pmin(bbmsy,4), y=pmin(uumsy,4), size=msy_mt/1e6, fill=source)) +
  # Lines
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  # Points
  geom_point(pch=21, alpha=0.5) +
  # Labels
  labs(x=expression("B/B"["MSY"]), y=expression("U/U"["MSY"])) +
  scale_x_continuous(limits=c(0, NA)) +
  scale_size_continuous(name="MSY\n(millions of mt)") +
  scale_fill_discrete(name="Source") +
  # Theme
  theme_bw() + 
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        plot.title=element_text(size=9),
        # Gridlines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

# Export data
ggsave(g, filename=file.path(plotdir, "figure_ram_saup_status.png"), 
       width=6, height=4.5, units="in", dpi=600)

