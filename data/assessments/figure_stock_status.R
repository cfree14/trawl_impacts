
# Clear workspace
rm(list = ls())
options(scipen = 999)

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data/assessments/processed"
lmedir <- "/Users/cfree/Dropbox/Chris/Rutgers/projects/productivity/data/lme/lmes"

# Read LME shapefile
lmes <- sf::st_read(dsn=lmedir, layer="LME66")

# Load input data
load(file=file.path(datadir, "SAUP_LME_rhcmsy_results.Rdata"))

# Add status to data
data <- data %>% 
  mutate(status=cut(bbmsy, breaks=c(0, 0.8, 1.2, 999), labels=c("Overfished", "Fully fished", "Underfished")), 
         status=factor(status, levels=c("Underfished", "Fully fished", "Overfished")))


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

# B/BMSY terminal
g <- ggplot(stocks, aes(x=bbmsy_last, fill=status_last)) +
  geom_histogram(binwidth=0.05, boundary=0) +
  labs(x=expression("B/B"["MSY"]), y="Frequency") +
  scale_fill_discrete(name="Status") +
  theme_bw() + my_theme
g

ggsave(g, filename=file.path(plotdir, "figure_saup_stock_status.png"), width=6.5, height=3.5, units="in", dpi=600)

# B/BMSY through time
cols <- hue_pal()(3)
g <- ggplot(data, aes(x=year, fill=status)) +
  geom_bar() + theme_bw() +
  labs(x="", y="Number of stocks") +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_fill_manual(name="Status", values=rev(cols))
g

ggsave(g, filename=file.path(plotdir, "figure_saup_stock_status_overtime.png"), width=6.5, height=3.5, units="in", dpi=600)

# U/UMSY terminal
g <- ggplot(stocks, aes(x=uumsy_last)) +
  geom_histogram(binwidth=0.05) +
  labs(x=expression("F/F"["MSY"]), y="Frequency") +
  theme_bw()
g

# B/BMSY mean by LME
lme <- stocks %>% 
  filter(year_last==2014) %>% 
  group_by(lme) %>% 
  summarize(nstock=n(),
            bbmsy_avg=mean(bbmsy_last, na.rm=T))


# Plot mean status by LME
g <- ggplot(lmes) +
  geom_sf()
g



