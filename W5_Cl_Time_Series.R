# File Header -------------------------------------------------------------

# File Name: W5_Cl_Time_Series.R
# Created: 15 Jun 2021 by Tyler Weiglein
# Last Modified: 15 Jun 2021 by Tyler Weiglein

# Purpose: To look at chloride (Cl-) time series for Watershed 5 (W5, 
# harvested 1983-1984) to see if there is a change in stream water Cl- 
# concentration or load after harvest.

# Notes: 
# 1) 


# Preliminaries -----------------------------------------------------------

# Clear console and environment

cat("\014") 
rm(list = ls(all.names = TRUE))

# Load package(s)

library(tidyverse)
library(lubridate)

# Set working directory

setwd("/Users/tweiglein/Documents/Graduate School/Virginia Tech - Ph.D./Research/Data Analysis/R Code/HBEF_TT_Project")

# Read in stream water concentration data

source("read_stream_conc_data.R")
source("read_daily_discharge_data.R")

stream_conc_data <- as_tibble(as.list(read_stream_conc_data()))
Q_daily <- as_tibble(as.list(read_daily_discharge_data()))


# Data Processing ---------------------------------------------------------

# Specify W5 area

W5_area_ha <- 21.9  # [ha] (https://hubbardbrook.org/watersheds/watershed-5)
W5_area_m2 <- W5_area_ha * 10^4  # [m^2]

# Select W5 data

W5_conc_data <- stream_conc_data %>% 
  filter(site == "W5")

W5_Q_daily <- Q_daily %>% 
  filter(WS == 5)

# Match Q index to concentration index

Q_i <- match(W5_conc_data$date, W5_Q_daily$DATE)

# Add discharge and daily load columns to W5_conc_data

W5_conc_data <- W5_conc_data %>% 
  mutate(Q = W5_Q_daily$Streamflow[Q_i],  # [mm/d]
         Cl_L = Cl * (Q / 1000 * W5_area_m2) * 1000)  # [mg/d]

# Create date_time column with proper formatting

# W5_conc_data <- W5_conc_data %>% 
#   mutate(date_time = ymd_hm(paste(substr(datetime, 1, 11), timeEST), tz = "EST"))


# Data Plotting -----------------------------------------------------------

start_date <- as.Date("1975-01-01")
end_date <- as.Date("1995-01-01")

harvest_start <- as.Date("1983-10-01")
harvest_end <- as.Date("1984-05-31")

# Plot of Cl concentration

Cl_conc_plot <- ggplot() +
  geom_point(data = filter(W5_conc_data,
                           date >= start_date,
                           date < end_date),
             aes(x = date, y = Cl)) +
  geom_vline(xintercept = harvest_start) +
  geom_vline(xintercept = harvest_end) +
  xlab("Time") +
  ylab("Chloride Concentration (mg/L)") +
  theme_bw()

# Plot of Cl load

Cl_load_plot <- ggplot() +
  geom_point(data = filter(W5_conc_data,
                           date >= start_date,
                           date < end_date),
             aes(x = date, y = Cl_L)) +
  geom_vline(xintercept = harvest_start) +
  geom_vline(xintercept = harvest_end) +
  xlab("Time") +
  ylab("Daily Chloride Load (mg/d)") +
  ylim(0, 10^6)
  theme_bw()
