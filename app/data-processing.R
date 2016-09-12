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


# #Read Car Miles data
carMiles <- readRDS("data/csv/carMiles_regional.rds")
carMiles[is.na(carMiles)] <- 0

milesCycled <- readRDS("data/csv/milesCycled.pers_regional.rds")
milesCycled[is.na(milesCycled)] <- 0

# #Read Trip data
#tripData <- read.csv("data/csv/tripsdf.csv", header = T, as.is = T)

# Read updated Trip data
# tripMode <- read.csv("data/csv/tripsdf_regional.csv", header = T, as.is = T)
# Read trip data as an rds file
tripMode <- readRDS("data/csv/tripsdf_regional.rds")

# Rename MainMode_Reduced columns to baseline
tripMode$baseline <- tripMode$MainMode_Reduced
tripMode$MainMode_Reduced <- NULL
tripMode$Cycled <- NULL



# # names(tripTime)[names(tripTime)=="MainMode_Reduced"] <- "baseline"
# 
# # "Walk", "Bicycle", "Ebike", "Car Driver", "Car Passenger", "Bus", "Train", "Other"
# # Reduce the number of modes to 4
# # walk, bicycle, car, others
# lookup <- data.frame(mode=c(1.0,2.0,2.5,3.0,4.0,5.0,6.0,7.0),red_mode=c(1.0,2.0,2.0,3.0,3.0,4.0,4.0,4.0))
# 
# # Replace number of modes in each of the scenarios and the baseline to 4
# for (i in 7:ncol(tripMode)){
#   tripMode[,i] <- lookup$red_mode[match(tripMode[,i], lookup$mode)]
# }

# if (!file.exists(readRDS("data/csv/TripTotalTime1_regional.rds"))){
#   
#   library(RCurl)
#   myfile <- getURL('https://github.com/ITHIM/ICT/releases/download/v0.1-beta/TripTotalTime1_regional.rds', ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
#   
#   tripTime <- readRDS(url("https://github.com/ITHIM/ICT/releases/download/v0.1-beta/TripTotalTime1_regional.rds"))
#   library(RCurl)
#   #
#   f = CFILE("TripTotalTime1_regional.rds", mode="wb")
#   curlPerform(url = "https://github.com/ITHIM/ICT/releases/download/v0.1-beta/TripTotalTime1_regional.rds", writedata = f@ref)
#   close(f)
#   
# }
  
# con <- gzfile("https://github.com/ITHIM/ICT/releases/download/v0.1-beta/TripTotalTime1_regional.rds")
# url1 <- "https://github.com/ITHIM/ICT/releases/download/v0.1-beta/TripTotalTime1_regional.rds"
# 
# fn <- tempfile()
# 
# con <- download.file(url1,tempfile(),mode="wb",quiet = T)
# 
# con <- gzcon(url(paste("github.com/ITHIM/ICT/releases/download/v0.1-beta/TripTotalTime1_regional.rds", sep="")))
# td <- readRDS(con)
# close(con)

# Comment out trip dataset
# Read trip time
# tripTime <- readRDS("data/csv/TripTotalTime1_regional.rds")

# Get row numbers with NA
temp <- data.frame(rn = which( is.na(tripMode$MainMode_Reduced), arr.ind=TRUE))

tripMode$X <- c(1:nrow(tripMode))

# tripTime$X <- c(1:nrow(tripTime))

# Remove all rows with NA in them
tripMode <- (subset(tripMode, !(X %in% temp$rn) ))

# tripTime <- (subset(tripTime, !(X %in% temp$rn) ))

rm(temp)

# # # "Walk", "Bicycle", "Ebike", "Car Driver", "Car Passenger", "Bus", "Train", "Other"
# # # Reduce the number of modes to 4
# # # walk, bicycle, car, others
# # lookup <- data.frame(mode=c(1.0,2.0,2.5,3.0,4.0,5.0,6.0,7.0),red_mode=c(1.0,2.0,2.0,3.0,3.0,4.0,4.0,4.0))
# # 
# # # Replace number of modes in each of the scenarios and the baseline to 4
# # for (i in 7:31){
# #   tripMode[,i] <- lookup$red_mode[match(tripMode[,i], lookup$mode)]
# # }
# 
# 
# 
names(tripMode)[names(tripMode)=="MainMode_Reduced"] <- "baseline"

#Read CO2 data
# co2data <- read.csv("data/csv/co2.csv", header = T, as.is = T)
co2data <- readRDS("data/csv/co2.rds")
# Replace NAs with 0
co2data[is.na(co2data)] <- 0

# Read directProbCasesAboveGivenPerc which main role is to store info of every case in which Observed > DP

directProbCasesAboveGivenPerc <- readRDS("data/csv/dp_cases_above_given.rds")