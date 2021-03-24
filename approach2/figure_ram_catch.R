
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

# Read data
data <- read.csv(file=file.path(datadir, "RAM_stock_catch_time_series.csv"), as.is=T)
stocks <- read.csv(file=file.path(datadir, "RAM_stock_metadata.csv"), as.is=T)


# Kobe plot
################################################################################

# Plot
g <- ggplot(stocks, aes(x=pmin(bbmsy,4), y=pmin(uumsy,4), size=msy/1e6, fill=year)) +
  # Lines
  geom_hline(yintercept = 1) +
  geom_vline(xintercept = 1) +
  # Points
  geom_point(pch=21) +
  # Labels
  labs(x=expression("B/B"["MSY"]), y=expression("U/U"["MSY"]), title="Status of RAM stocks used in the analysis") +
  scale_size_continuous(name="MSY\n(millions of mt)") +
  scale_fill_gradientn(name="Status year", colors=rev(RColorBrewer::brewer.pal(9, "YlOrRd")), breaks=seq(2000, 2014, 2)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
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
ggsave(g, filename=file.path(plotdir, "figure_ram_status.png"), 
       width=6, height=4.5, units="in", dpi=600)


# Catch coverage
################################################################################

# Stats
stats <- data %>% 
  filter(catch_est=="reported") %>% 
  group_by(stockid) %>% 
  summarize(nyr=n(),
            yr1=min(year), 
            yr2=max(year)) %>% 
  ungroup() %>% 
  arrange(yr1)

# Order data
data_plot <- data %>% 
  mutate(stockid=factor(stockid, levels=stats$stockid),
         catch_est=recode_factor(catch_est, 
                                 "reported"="Reported",
                                 "extrapolated"="Extrapolated"))
  
# Plot
g <- ggplot(data_plot, aes(x=year, y=stockid, fill=catch_est)) +
  # Lines
  geom_tile() +
  # Labels
  labs(x="Year", y="", title="Catch coverage for RAM stocks used in the analysis") +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  scale_fill_manual(name="Catch type", values = c("grey80", "grey30")) +
  # Vertical line
  geom_vline(xintercept = 2000, linetype="dotted", color="black") +
  # Theme
  theme_bw() +
  theme(axis.text=element_text(size=5),
        axis.text.y = element_blank(),
        axis.title=element_text(size=7),
        plot.title=element_text(size=9),
        # Gridlines
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g

# Export data
ggsave(g, filename=file.path(plotdir, "figure_ram_catch_coverage.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

# Catch time series
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=5),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=7),
                   plot.title=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position="bottom",
                   legend.key.size = unit(0.25, "cm"))

# Plot data
g <- ggplot(data, aes(x=year, y=catch/1e6, fill=lme)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Catch (millions of mt)", title="Catch from RAM Legacy Database stocks used in the analysis") +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + my_theme
g

# Export data
ggsave(g, filename=file.path(plotdir, "figure_ram_catch.png"), 
       width=6.5, height=4.5, units="in", dpi=600)
