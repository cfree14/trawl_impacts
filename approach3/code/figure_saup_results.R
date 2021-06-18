
# Clear workspace
rm(list = ls())

# Turn off scientific notation
options(scipen=999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "approach3/output/processed"
plotdir <- "approach3/figures"

# Read data
load(file.path(datadir, "SAUP_LME_rhcmsy_results.Rdata"))


# Kobe plot
################################################################################

# Plot
g <- ggplot(stocks, aes(x=pmin(bbmsy_last,4), y=pmin(uumsy_last,4), size=msy_mt/1e6)) +
  # Lines
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  # Points
  geom_point(pch=21) +
  # Labels
  labs(x=expression("B/B"["MSY"]), y=expression("U/U"["MSY"]), title="Status of SAUP stocks used in the analysis") +
  scale_size_continuous(name="MSY\n(millions of mt)") +
  scale_x_continuous(limits=c(0, NA)) +
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
ggsave(g, filename=file.path(plotdir, "figure_saup_status.png"), 
       width=6, height=4.5, units="in", dpi=600)

