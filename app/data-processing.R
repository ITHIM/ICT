library(stringr)

idata <- read.csv("data/csv/mmets_var.csv", as.is = T)

sdata <- read.csv("data/csv/ICT_aggr.csv", header = T, check.names=FALSE)

baselineSummary <- sdata[1,]
# Temporarily remove baseline summary
sdata <- sdata[-1,]
sdata[is.na(sdata)] <- 0

# Create a lookup table for mode of transport
tp_mode <- data.frame (mode = c("Walk", "Bicycle", "Ebike", "Car Driver", "Car Passenger", "Bus", "Train", "Other"), code = c(1, 2, 2.5, c(3:7)))

# # Read Health Calculations
yll <- read.csv("data/csv/yll_agg_u.csv", header = T, as.is = T)
death <- read.csv("data/csv/death_agg_u.csv", header = T, as.is = T)
yll_red <- read.csv("data/csv/yll_red_agg_u.csv", header = T, as.is = T)

#Read Car Miles data
carMiles <- read.csv("data/csv/carMiles_var1.csv", header = T, as.is = T)
carMiles[is.na(carMiles)] <- 0

milesCycled <- read.csv("data/csv/milesCycled.pers_var1.csv", header = T, as.is = T)
milesCycled[is.na(milesCycled)] <- 0

# #Read Trip data
tripData <- read.csv("data/csv/tripsdf.csv", header = T, as.is = T)

#Read CO2 data
co2data <- read.csv("data/csv/co2.csv", header = T, as.is = T)