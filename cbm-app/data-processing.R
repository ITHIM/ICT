library(stringr)

tdata <- read.csv("data/baseline_filtered.csv", as.is = T)
# Turning off this line of code
#idata <- read.csv("data/bl_with_people_no_trip_filtered.csv", as.is = T)

# idata <- read.csv("data/idataSubset.csv", as.is = T)
# 
# tdata$age_group <- as.character(tdata$age_group)
# tdata$age_group <- str_trim(tdata$age_group)
# idata$age_group <- as.character(idata$age_group)
# idata$age_group <- str_trim(idata$age_group)

# idata <- read.csv("data/PADf1.csv", as.is = T)

idata <- read.csv("data/csv/mmets_var.csv", as.is = T)


msharedtata <- read.csv("data/BD_share1split.csv", as.is = T)

fasterTripData <- read.csv("data/BreakDowns/BD_mode-fastertrips.csv")
slowerTripData <- read.csv("data/BreakDowns/BD_mode-slowertrips.csv")

# sdata <- read.csv("data/CBM_aggr_II.csv", header = T, check.names=FALSE)
# sdata <- subset(sdata, TDR == 1 | TDR == -1)

# sdata <- read.csv("data/ICT_aggr.csv", header = T, check.names=FALSE)

sdata <- read.csv("data/csv/ICT_aggr.csv", header = T, check.names=FALSE)

baselineSummary <- sdata[1,]
# Temporarily remove baseline summary
sdata <- sdata[-1,]
sdata[is.na(sdata)] <- 0

#scenariosIdata <- read.csv("data/b_eng_mmet.csv", as.is = T)

scenariosTimeTravelIdata <- read.csv("data/triptim_filtered.csv", as.is = T)

# scenariosTripTimeTravelIdata <- read.csv("data/bd_triptime.csv", as.is = T)

scenariosTripTimeTravelIdata <- read.csv("data/bd_triptime_changed.csv", as.is = T)

# Create a lookup table for mode of transport
tp_mode <- data.frame (mode = c("Walk", "Bicycle", "Ebike", "Car Driver", "Car Passenger", "Bus", "Train", "Other"), code = c(1, 2, 2.5, c(3:7)))

# # Read Health Calculations
# yll <- read.csv("data/yll_eng_agg.csv", header = T, as.is = T)
# yll_red <- read.csv("data/yll_red_eng_agg.csv", header = T, as.is = T)

# Read Health Calculations

#yll <- read.csv("data/health/yll_agg.csv", header = T, as.is = T)
#yll_red <- read.csv("data/health/yll_red_agg.csv", header = T, as.is = T)

#yll <- read.csv("data/health/yll_agg_reduced_scenarios.csv", header = T, as.is = T)
#yll_red <- read.csv("data/health/yll_red_agg_reduced_scenarios.csv", header = T, as.is = T)

yll <- read.csv("data/csv/yll_agg.csv", header = T, as.is = T)
yll_red <- read.csv("data/csv/yll_red_agg.csv", header = T, as.is = T)

# #Read Car Miles data
# carMiles <- read.csv("data/carMilesSubset.csv", header = T, as.is = T)
# # Remove all rows where baseline rows don't have any car miles
# carMiles <- subset(carMiles, !is.na(baseline))

#Read Car Miles data
carMiles <- read.csv("data/csv/carMiles_var.csv", header = T, as.is = T)
carMiles[is.na(carMiles)] <- 0
# Remove all rows where baseline rows don't have any car miles
#carMiles <- subset(carMiles, !is.na(baseline))

milesCycled <- read.csv("data/csv/milesCycled.pers_var.csv", header = T, as.is = T)
milesCycled[is.na(milesCycled)] <- 0

# #Read Trip data
# tripData <- read.csv("data/bl_ag_sc_modes.csv", header = T, as.is = T)
tripData <- read.csv("data/csv/tripsdf.csv", header = T, as.is = T)

#Read CO2 data
co2data <- read.csv("data/csv/co2.csv", header = T, as.is = T)