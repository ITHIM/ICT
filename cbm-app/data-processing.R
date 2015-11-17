library(stringr)

tdata <- read.csv("data/baseline_filtered.csv", as.is = T)
# Turning off this line of code
#idata <- read.csv("data/bl_with_people_no_trip_filtered.csv", as.is = T)

idata <- read.csv("data/idataSubset.csv", as.is = T)

tdata$age_group <- as.character(tdata$age_group)
tdata$age_group <- str_trim(tdata$age_group)
idata$age_group <- as.character(idata$age_group)
idata$age_group <- str_trim(idata$age_group)
msharedtata <- read.csv("data/BD_share1split.csv", as.is = T)

fasterTripData <- read.csv("data/BreakDowns/BD_mode-fastertrips.csv")
slowerTripData <- read.csv("data/BreakDowns/BD_mode-slowertrips.csv")

sdata <- read.csv("data/CBM_aggr_II.csv", header = T, check.names=FALSE)
baselineSummary <- sdata[1,]
# Temporarily remove baseline summary
sdata <- sdata[-1,]
sdata[is.na(sdata)] <- 0

scenariosIdata <- read.csv("data/b_eng_mmet.csv", as.is = T)

scenariosTimeTravelIdata <- read.csv("data/triptim_filtered.csv", as.is = T)

# scenariosTripTimeTravelIdata <- read.csv("data/bd_triptime.csv", as.is = T)

scenariosTripTimeTravelIdata <- read.csv("data/bd_triptime_changed.csv", as.is = T)

# Create a lookup table for mode of transport
tp_mode <- data.frame (mode = c("Walk", "Bicycle", "Car Driver", "Car Passenger", "Bus", "Train", "Other"), code = c(1:7))

# Read Health Calculations
yll <- read.csv("data/yll_eng_agg.csv", header = T, as.is = T)
yll_red <- read.csv("data/yll_red_eng_agg.csv", header = T, as.is = T)

#Read Car Miles data
carMiles <- read.csv("data/carMilesSubset.csv", header = T, as.is = T)
# Remove all rows where baseline rows don't have any car miles
carMiles <- subset(carMiles, !is.na(baseline))

#Read Trip data
tripData <- read.csv("data/bl_ag_sc_modes.csv", header = T, as.is = T)