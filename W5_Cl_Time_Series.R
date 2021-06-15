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

stream_conc_data <- as_tibble(as.list(read_stream_conc_data()))


# Data Processing ---------------------------------------------------------

# Select W5 data

W5_conc_data <- stream_conc_data %>% 
  filter(site == "W5")

# Create date_time column with proper formatting

W5_conc_data <- W5_conc_data %>% 
  mutate(date_time = ymd_hm(paste(substr(datetime, 1, 11), timeEST), tz = "EST"))


# Data Plotting -----------------------------------------------------------

Cl_conc_plot <- ggplot() +
  geom_point(data = filter(W5_conc_data,
                           date_time >= as.POSIXct("1975-01-01", tz = "EST"),
                           date_time < as.POSIXct("1995-01-01", tz = "EST")),
             aes(x = date_time, y = Cl)) +
  geom_vline(xintercept = as.POSIXct("1983-10-01", tz = "EST")) +
  geom_vline(xintercept = as.POSIXct("1984-05-31", tz = "EST")) +
  xlab("Time") +
  ylab("Chloride Concentration (mg/L)") +
  theme_bw()

# Plot of Cl load
