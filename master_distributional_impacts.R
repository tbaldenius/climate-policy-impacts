### This is the master file. Code may be run from here to receive given results and graphic output.

### Setup

rm(list = ls()) # Clear environment

setwd(" --- Insert the directory where all scripts are contained here --- ")

library(tidyverse) # Check if packages are installed on your device (see ?installed.packages)
library(matrixStats)
library(MetricsWeighted)

Sys.setlocale("LC_ALL", "German") # Allow for display of 'Umlaute', ignore possible warning 
options(encoding = "UTF-8") 
theme_set(theme_bw(base_size = 14)) # Set theme parameter for graphic output

### Global variables (set to default values used in final analysis)

keep_electric_cars = TRUE # True: Include electric vehicles
remove_na_cars = FALSE # True: Exclude cars of which some of the relevant characteristics are missing
test_anzauto = FALSE # True: Exclude households that do not own a car
test_income = FALSE # True: Exclude housholds exceeding the maximum reported income (9000 Euro/month)
test_morecars = FALSE # True: Include households that own more than three cars and impute average efficiency for those whose characteristics were not reported (a maximum of three cars' characterisitcs are reported in the survey)
morecars_fl = 0 # If 'test_morecars' is True, set the distance travelled yearly of cars whose characteristics were not reported to 'morecars_fl'
use_eq_scale = 2 # Equivalence income: 1: Use total household income, 2: Use new OECD scale, 3: Use square root of HH members

### Load data

# Mobilitaetspanel (MOP)
dat_TANK <- read_delim('TANK18.csv', delim=";", col_types = cols(GEWICHT = col_number()))
dat_HH <- read_delim('HH17.csv', delim=";")

#Mobilitaet in Deutschland (MiD)
dat_MiD_Auto <- read_delim('MiD2017_Autos.csv',delim = ";", locale=locale(decimal_mark = ","))
dat_MiD_HH <- read_delim('MiD2017_Haushalte.csv', delim=";", locale=locale(decimal_mark = ","))
dat_MiD_Wege <- read_delim('MiD2017_Wege.csv', delim = ";", locale=locale(decimal_mark = ","))
dat_MiD_P <- read_delim('MiD2017_Personen.csv', delim = ";", locale=locale(decimal_mark = ","))

### Main analysis

source("run_analysis.R")

### Produce plots (you may need to create a graphs folder in your working directory)

source("create_plots.R")