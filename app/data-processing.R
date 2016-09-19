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

# Aggregate trips used in "Journey Time" tab



TripTotalTimeFreq <- function(tripTime, tripMode){
  # all possible age_group + 'All'
  
  aaAgeGroups <- c(sort(unique(tripTime[, c("age_group")])), 'All')
  
  # all possible Sex + 3 (all)
  
  aaSexs <- c(sort(unique(tripTime[, c("Sex_B01ID")])), '3')
  
  # all possible Ethnicity + 'All'
  
  aaEthnicities <- c(sort(unique(tripTime[, c("EthGroupTS_B02ID")])), 'All')
  
  # all possible SES + 'All'
  
  aaSESs <- c(sort(unique(tripTime[, c("NSSec_B03ID")])), 'All')
  
  # all possible scenarios - not very elegant way
  
  aaScenarios <- colnames(tripTime)[10:length(colnames(tripTime))-1]
  
  # all possible regions
  
  aaRegions <- sort(unique(tripTime[, c("HHoldGOR_B02ID")]))
  
  outputScenariosFreq <- data.frame(freq = character(), umode = numeric(), region = numeric(), stringsAsFactors = FALSE)
  
  outputFilteredScenariosFreq <- data.frame(freq = character(),
                                            umode = numeric(),
                                            region = numeric(),
                                            agegroup = character(),
                                            gender = character(),
                                            ethnicity = character(),
                                            ses = character(),
                                            stringsAsFactors = FALSE)
  
  for (aaRegion in aaRegions){
    
    # select data for region
    
    tripTimeRegion <- subset(tripTime, HHoldGOR_B02ID == aaRegion)
    tripModeRegion <- subset(tripMode, HHoldGOR_B02ID == aaRegion)
    
    # store freqs for scenario baseline for particular region
    
    tempScenarioFreqRegion <- data.frame(freq = character(), umode = numeric(), region = numeric(), stringsAsFactors = FALSE)
    
    # store freqs for filtered scenario for particular region
    
    tempScenarioFilteredFreqRegion <- data.frame(freq = character(),
                                                 umode = numeric(),
                                                 region = numeric(),
                                                 agegroup = character(),
                                                 gender = character(),
                                                 ethnicity = character(),
                                                 ses = character(),
                                                 stringsAsFactors = FALSE)
    
    for (aaScenario in aaScenarios){
      
      print(aaScenario)
      
      # scenario baseline shouldn't be calculated every time, only once for every scenario to reduce number of operations
      
      tempScenarioFreq <- data.frame(stringsAsFactors = FALSE)
      
      columnName <- aaScenario
      
      data <- tripTimeRegion
      
      # Get row numbers with NA
      temp <- data.frame(rn = tripTimeRegion[,c("X")])
      
      locatTripModeData <- tripModeRegion[,c("X","baseline", columnName)]
      
      # "Walk", "Bicycle", "Ebike", "Car Driver", "Car Passenger", "Bus", "Train", "Other"
      # Reduce the number of modes to 4
      # walk, bicycle, car, others
      lookup <- data.frame(mode=c(1.0,2.0,2.5,3.0,4.0,5.0,6.0,7.0),red_mode=c(1.0,2.0,2.0,3.0,3.0,4.0,4.0,4.0))
      
      # Replace number of modes in each of the scenarios and the baseline to 4
      locatTripModeData[["baseline"]] <- lookup$red_mode[match(locatTripModeData[["baseline"]], lookup$mode)]
      
      # Replace number of modes in each of the scenarios and the baseline to 4
      locatTripModeData[[columnName]] <- lookup$red_mode[match(locatTripModeData[[columnName]], lookup$mode)]
      
      # Remove all rows with NA in them
      locatTripModeData <- (subset(locatTripModeData, (X %in% temp$rn) ))
      
      localtripData <- data[,c("X","TripTotalTime1", columnName)]
      
      localtripData <- data.frame(rn = localtripData$X, diff = ((localtripData[[columnName]] - localtripData$TripTotalTime1) / localtripData$TripTotalTime1 ) * 100)
      
      locatTripModeData <- subset(locatTripModeData, (X %in% localtripData$rn) )
      
      names(locatTripModeData)[names(locatTripModeData)=="X"] <- "rn"
      localtripData <- inner_join(localtripData, locatTripModeData, by = "rn")
      
      localtripData <- subset(localtripData, localtripData$baseline != localtripData[[columnName]])
      
      scTripTimeTraveldata <- localtripData
      
      # calc freqs
      
      total_col <- "baseline"
      
      umode <- sort(unique(scTripTimeTraveldata[,total_col]))
      # Exclude bicycle mode
      umode <- umode[umode != 2]
      
      for (i in 1:length(umode)){
        
        ldata <- subset(scTripTimeTraveldata, scTripTimeTraveldata[,total_col] == umode[i])
        
        bc <- as.data.frame(table (cut (ldata$diff, breaks = c(-100, -50, -20, 0, 20, 50, 100, Inf))), stringsAsFactors = FALSE)
        
        bc$Freq <- round(bc$Freq  / nrow(scTripTimeTraveldata) * 100, digits = 1)
        
        # change column name - use scenario name
        
        colnames(bc) <- c('freq', aaScenario)
        
        # add column with umode
        
        bc[,c('umode')] <- umode[i]
        
        tempScenarioFreq <- bind_rows(tempScenarioFreq, bc)
        
      }
      
      # for filtered data
      
      tempScenarioFilteredFreq <- data.frame(stringsAsFactors = FALSE)
      
      for (aaAgeGroup in aaAgeGroups){
  
        for (aaSex in aaSexs){
  
          for (aaEthnicity in aaEthnicities){
  
            for (aaSES in aaSESs){
  
              data <- tripTimeRegion
  
              if (aaAgeGroup != 'All'){
                data <- subset(data, age_group == aaAgeGroup)
              }
              if (aaSex != 3)
                data <- subset(data, Sex_B01ID %in% aaSex)
  
              if (aaSES != "All"){
                data <- subset(data, NSSec_B03ID %in% aaSES)
              }
  
              if (aaEthnicity != "All"){
                data <- subset(data, EthGroupTS_B02ID %in% aaEthnicity)
              }
              data[is.na(data)] <- 0
  
              locatTripModeData <- tripModeRegion[,c("X","baseline", columnName)]
  
              # "Walk", "Bicycle", "Ebike", "Car Driver", "Car Passenger", "Bus", "Train", "Other"
              # Reduce the number of modes to 4
              # walk, bicycle, car, others
              lookup <- data.frame(mode=c(1.0,2.0,2.5,3.0,4.0,5.0,6.0,7.0),red_mode=c(1.0,2.0,2.0,3.0,3.0,4.0,4.0,4.0))
  
              # Replace number of modes in each of the scenarios and the baseline to 4
              locatTripModeData[["baseline"]] <- lookup$red_mode[match(locatTripModeData[["baseline"]], lookup$mode)]
  
              # Replace number of modes in each of the scenarios and the baseline to 4
              locatTripModeData[[columnName]] <- lookup$red_mode[match(locatTripModeData[[columnName]], lookup$mode)]
  
              # Get row numbers with NA
              #temp <- data.frame(rn = which(sessionData$tripTime[,c("X")] %in% data$X))
  
              # Get row numbers which fulfil selected conditions
  
              temp <- tripTimeRegion[,c("X")] %in% data$X
  
              selectedRows <- tripTimeRegion[temp, ]
  
              # Remove all rows with NA in them
              locatTripModeData <- (subset(locatTripModeData, (X %in% selectedRows$X) ))
  
              localtripData <- data[,c("X","TripTotalTime1", columnName)]
  
              localtripData <- data.frame(rn = localtripData$X, diff = ((localtripData[[columnName]] - localtripData$TripTotalTime1) / localtripData$TripTotalTime1 ) * 100)
  
              #localtripData <- subset(localtripData, diff <= 200 & diff >= -200 )
  
              locatTripModeData <- subset(locatTripModeData, (X %in% localtripData$rn) )
  
              names(locatTripModeData)[names(locatTripModeData)=="X"] <- "rn"
              localtripData <- inner_join(localtripData, locatTripModeData, by = "rn")
  
              localtripData <- subset(localtripData, localtripData$baseline != localtripData[[columnName]])
              scFilteredTripTimeTraveldata <- localtripData
  
              # calc freqs
  
              total_col <- "baseline"
  
              umode <- sort(unique(scFilteredTripTimeTraveldata[,total_col]))
              # Exclude bicycle mode
              umode <- umode[umode != 2]
  
              for (i in 1:length(umode)){
                ldata <- subset(scFilteredTripTimeTraveldata, scFilteredTripTimeTraveldata[,total_col] == umode[i])
  
                bc <- as.data.frame(table (cut (ldata$diff, breaks = c(-100, -50, -20, 0, 20, 50, 100, Inf))), stringsAsFactors = FALSE)
  
                bc$Freq <- round(bc$Freq  / nrow(scFilteredTripTimeTraveldata) * 100, digits = 1)
  
                # change column name - use scenario name
                
                colnames(bc) <- c('freq', aaScenario)
                
                # add column with umode
                
                bc[,c('umode')] <- umode[i]
                bc[,c('agegroup')] <- aaAgeGroup
                bc[,c('gender')] <- aaSex
                bc[,c('ethnicity')] <- aaEthnicity
                bc[,c('ses')] <- aaSES
                
                tempScenarioFilteredFreq <- bind_rows(tempScenarioFilteredFreq, bc)
              }
  
            }
  
          }
  
        }
  
      }
      
      # for scenario baseline - add region column, add scenario values
      
      tempScenarioFreq[,c('region')] <- aaRegion
      
      tempScenarioFreqRegion <- full_join(tempScenarioFreqRegion, tempScenarioFreq, by=c('freq', 'umode', 'region'))
      
      # for filtered scenario - add region column, add scenario values
      
      tempScenarioFilteredFreq[,c('region')] <- aaRegion
      
      tempScenarioFilteredFreqRegion <- full_join(tempScenarioFilteredFreqRegion, tempScenarioFilteredFreq,
                                                  by=c('freq', 'umode', 'region', 'agegroup', 'gender', 'ethnicity', 'ses'))
      
    }
    
    # for scenario baseline - add region to regions
    
    outputScenariosFreq <- bind_rows(outputScenariosFreq, tempScenarioFreqRegion)
    
    # for filtered scenario baseline - add region to regions
    
    outputFilteredScenariosFreq <- bind_rows(outputFilteredScenariosFreq, tempScenarioFilteredFreqRegion)
    
    gc()
    
  }
  
  return(list("outputScenariosFreq" = outputScenariosFreq, "outputFilteredScenariosFreq" = outputFilteredScenariosFreq))
}


# testTripTotalTimeFreq <- TripTotalTimeFreq(tripTime, tripMode)

# rm(tripTime)
# gc()