rm(list=ls())
library("zoo")
library("tseries")
library("forecast")
options(scipen = 999)

# Define functions for Bodie, Munk, CRRA Utility. Simply formulas.
source("1-FUNCTIONS.R")

# Import datasets for wages, stock prices, house prices
source("2-DATA.R")

# Do calculations
source("3-CALCULATIONS.R")

# Calculate main parameters
source("4-PARAMETERS.R")

# Derive human capital from estimated wage series
source("5-HUMCAPITAL.R")

# Perform a Monte Carlo simulation for each investment option
source("6-FINCAPITAL.R")

# Perform a benchmarking of the results
source("7-BENCHMARK.R")

# Access the graphs
source("8-GRAPHS.R")

