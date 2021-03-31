

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
plotdir <- "approach2/figures"

# Read RAM data
data_ram <- read.csv(file=file.path(datadir, "RAM_stock_catch_time_series.csv"), as.is=T)
stocks_ram <- read.csv(file=file.path(datadir, "RAM_stock_metadata.csv"), as.is=T)

# Read RAUP data
load(file.path(datadir, "SAUP_stock_data.Rdata"))


# Merge catch data
################################################################################

# Format RAM data
data_ram1 <- data_ram %>% 
  mutate(source="RAM") %>% 
  select(source, lme, stockid, year, catch_est, catch_type, catch_units, catch)

# Format SAUP data
data_saup1 <- data_saup %>% 
  # Add columns
  mutate(source="SAUP", 
         catch_est="reported",
         catch_type="TL",
         catch_units="MT") %>% 
  # Add LME
  left_join(stocks_saup %>% select(stockid, lme)) %>% 
  # Rename
  rename(catch=reported_adj_mt) %>% 
  select(source, lme, stockid, year, catch_est, catch_type, catch_units, catch)

# Merge data
data <- bind_rows(data_ram1, data_saup1) %>%
  filter(!is.na(catch))

# Inspect
freeR::complete(data)

# Plot
################################################################################

# Stats
stats <- data %>% 
  group_by(source, year) %>% 
  summarize(catch_mt=sum(catch)) %>% 
  ungroup() %>% 
  group_by(year) %>% 
  mutate(catch_prop=catch_mt/sum(catch_mt)) %>% 
  ungroup()

# Theme
my_theme <-   theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    plot.title=element_text(size=9),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"))

# Plot data
g1 <- ggplot(stats, aes(x=year, y=catch_mt/1e6, fill=source)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Catch (millions of mt)") +
  scale_fill_discrete(name="Source") + 
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.2,0.8))
g1

# Plot data
g2 <- ggplot(stats, aes(x=year, y=catch_prop, fill=source)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Catch proportion") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position="none")
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, ncol=1, heights=c(0.7,0.3))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "figure_ram_saup_catch_ts.png"), 
       width=6.5, height=4.5, units="in", dpi=600)



