library(tidyverse)
library(ggrepel)
library(rlang)
library(here)
library(data.table)
library(logitr)
library(cowplot)
library(janitor)
library(conjointTools)
options(dplyr.width = Inf) # Option to preview all columns in a data frame
source('functions.R')

# Define global leg mode vars:
none  <- 'None'
car   <- 'Car'
taxi  <- 'Uber/Taxi'
bus   <- 'Bus'
metro <- 'Metro'
walk  <- 'Walk'