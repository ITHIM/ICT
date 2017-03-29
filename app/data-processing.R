library(stringr)
# idata <- read.csv("data/csv/mmets_regional_var.csv", as.is = T)
# Temporarily reading as an rds file
idata <- readRDS("data/csv/mmets_regional.rds")

sdata <- readRDS("data/csv/ICT_aggr_regional.rds")

# Remove the code which discards baseline info from the aggregate summary file
baselineSummary <- subset(sdata, MS == 0)
# # Temporarily remove baseline summary
# Remove baseline rows
sdata <-subset(sdata, MS != 0)

# sdata <- sdata[-1,]
# sdata[is.na(sdata)] <- 0

# Create a lookup table for mode of transport
tp_mode <- data.frame (mode = c("Walk", "Bicycle", "Ebike", "Car Driver", "Car Passenger", "Bus", "Train", "Other"), code = c(1, 2, 2.5, c(3:7)))

# # Read Health Calculations
# yll <- read.csv("data/csv/yll_agg_u.csv", header = T, as.is = T)
# death <- read.csv("data/csv/death_agg_u.csv", header = T, as.is = T)
# yll_red <- read.csv("data/csv/yll_red_agg_u.csv", header = T, as.is = T)


yll <- readRDS("data/csv/ylls.rds")
death <- readRDS("data/csv/deaths.rds")
yllReduction <- readRDS("data/csv/yll_reduction.rds")
# Round death and yll reductions numbers to two decimal places
death[,4:ncol(death)] <- round(death[,4:ncol(death)], 2)
yllReduction[,4:ncol(yllReduction)] <- round(yllReduction[,4:ncol(yllReduction)], 2)


# Read Car Miles data
carMiles <- readRDS("data/csv/carMiles_regional.rds")
carMiles[is.na(carMiles)] <- 0

milesCycled <- readRDS("data/csv/milesCycled.pers_regional.rds")
milesCycled[is.na(milesCycled)] <- 0

# Read CO2 data
# co2data <- read.csv("data/csv/co2.csv", header = T, as.is = T)
co2data <- readRDS("data/csv/co2.rds")
# Replace NAs with 0
co2data[is.na(co2data)] <- 0

# Read directProbCasesAboveGivenPerc which main role is to store info of every case in which Observed > DP

directProbCasesAboveGivenPerc <- readRDS("data/csv/dp_cases_above_given.rds")

# Trip time data - prev TripTotalTime is now read on the fly as separate files

# Trip data - prev tripsdf_regional is now read on the fly as separate files

