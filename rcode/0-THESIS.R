rm(list=ls())
library("zoo")
library("tseries")
library("forecast")

# Define functions for Bodie, Munk, CRRA Utility. Simply formulas.
source("/home/ravshan/Dropbox/research/mathesis/rcode/FUNCTIONS.R")

# Import datasets for wages, stock prices, house prices
source("/home/ravshan/Dropbox/research/mathesis/rcode/DATA.R")

# Calculate main parameters
source("/home/ravshan/Dropbox/research/mathesis/rcode/PARAMETERS.R")

# Derive human capital from estimated wage series
source("/home/ravshan/Dropbox/research/mathesis/rcode/HUMCAPITAL.R")

# Perform a Monte Carlo simulation for each investment option
source("/home/ravshan/Dropbox/research/mathesis/rcode/FINCAPITAL.R")



# Calculations
#source("/home/ravshan/Dropbox/research/mathesis/rcode/CALCULATIONS.R.R")