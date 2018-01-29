createPhyActTable <- function(data){
  m <- matrix(nrow = 3, ncol = 2)
  colnames(m) <- c("val","Freq")
  m[1,1] = 0
  m[1,2] = nrow(data[data$total_mmet < 8.75,]) # sum(data < 8.75)
  m[2,1] = 8.75
  m[2,2] = nrow(data[data$total_mmet >= 8.75,]) # sum(data >= 8.75)
  m[3,1] = 17.5
  m[3,2] = nrow(data[data$total_mmet >= 17.5,]) # sum(data >= 17.5)
  m <- as.data.frame(m)
  m
}

lookup_table <- function(data, ag){
  
  var <- ag$V2[match(data, ag$V1)]
  var
}

format_function <- "#! function() {
        var fraction  = this.y / this.series.yAxis.max * 100;
        if (fraction > 20){
              return Math.round(this.y);
        }else{
            return null;
        }
      }!#"

getSeriesName <- function( EQ, EB){
  nEQ <- "Off"
  if (EQ == 1)
    nEQ <- "On"
  
  nEB <- "Off"
  if (EB == 1)
    nEB <- "On"
  # cat(EQ, " : ", EB, "\n")
  paste("EQ:", nEQ, "& EB:", nEB, sep = " ")
  
}

# since tripMode is precalculated this function is not used
# appendMissingFrequencies <- function( df1, df2){
#   missingModes <- setdiff(df1[,2], df2[,1])
#   if (nrow(df2) < 8){
#     for (i in (1:length(missingModes))){
#       df2 = rbind(df2,c(missingModes[i], 0))
#     }
#   }
#   df2
# }


#(tripTime, tripMode, "MS64_ebik1_eq1", 2)

getModeSpecificTrips <- function(data1, data2, columnName, mn){
  
  data1[is.na(data1)] <- 0
  
  temp <- data.frame(rn = data1$X)
  
  locatTripModeData <- data2[,c("X","baseline", columnName)]
  
  locatTripModeData <- (subset(locatTripModeData, (X %in% temp$rn) ))
  
  localtripData <- data1[,c("X","baseline", columnName)]
  
  localtripData <- data.frame(rn = localtripData$X, diff = ((localtripData[[columnName]] - localtripData$baseline) / localtripData$baseline ) * 100)
  
  localtripData <- subset(localtripData, diff <= 200 & diff >= -200 )
  
  locatTripModeData <- subset(locatTripModeData, (X %in% localtripData$rn) )
  
  names(locatTripModeData)[names(locatTripModeData)=="X"] <- "rn"
  localtripData <- dplyr::inner_join(localtripData, locatTripModeData, by = "rn")
  
  wtrips <- subset(localtripData, baseline == mn)
  
  wtrips
  
  
}

generateUniqueMS <- function(region){
  
  # default values
  
  uniqueMS <- data.frame(labels=c("5%", "10%", "25%", "50%", "75%", "100%"),
                         values=c(0.05, 0.10, 0.25, 0.50, 0.75, 1.00), stringsAsFactors=F)
  
  # check if there are cases observed > DP for selected region
  
  DPCasesRegion <- unique(directProbCasesAboveGivenPerc[directProbCasesAboveGivenPerc$region == region, 'MS'])
  
  # remove observed > DP cases
  
  withoutRemovedObservedDPCases <- uniqueMS[uniqueMS$values %in% setdiff(uniqueMS$values, DPCasesRegion), ]
  
  output <- structure(withoutRemovedObservedDPCases$values, names=withoutRemovedObservedDPCases$labels)
  
}

generateRegionsList <- function(region = -1){
  
  regions <- c("England"	 = 0,
               "North East" =	1,
               "North West" = 2,
               "Yorkshire and the Humber" = 3,
               "East Midlands"	 = 4,
               "West Midlands"	= 5,
               "East of England"	= 6,
               "London" = 7,
               "South East" =	8,
               "South West" =	9)
  
  output <- regions[regions %in% setdiff(regions, region)]
  
}