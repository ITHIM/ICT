# "Physical Activity"

pd <- idata$total_mmet
bMETdata <- NULL
scMETdata <- NULL
scFilteredMETdata <- NULL
scMETdataAltRegFull <- NULL
scMETdataAltRegFiltered <- NULL

# not used

scTimeTraveldata <- NULL
scFilteredTimeTraveldata <- NULL
scFilteredTripTimeTraveldata <- NULL
scTripTimeTraveldata <- NULL
scFilteredTripModeTraveldata <- NULL

# "Health"

ftdata <- NULL
swdata <- NULL
bd <- NULL
pdl <- NULL
bldata <- NULL
scYllData <- NULL
scYllReductionData <- NULL

scYllData1 <- NULL
scYllReductionData1 <- NULL

# "Mode Share"

msBaseline <- NULL
msScenario <- NULL
msAltRegionScenario <- NULL
tdBaseline <- NULL
tdScenario <- NULL
tdAltRegionScenario <- NULL

# "Miles Cycled"

scMilesCycledData <- NULL
scMilesCycledFilteredData <- NULL
scMilesCycledDataAltRegFull <- NULL
scMilesCycledDataAltRegFiltered <- NULL

blMilesCycledData <- NULL
blMilesCycledFilteredData <- NULL

# "Car Miles"

scCarMilesData <- NULL
scCarMilesFilteredData <- NULL
scCarMilesDataAltRegFull <- NULL
scCarMilesDataAltRegFiltered <- NULL

blCarMilesData <- NULL
blCarMilesFilteredData <- NULL

# "CO2"

scCO2Data <- NULL
scCO2FilteredData <- NULL
scCO2DataAltRegionFull <- NULL
scCO2DataAltRegionFiltered  <- NULL

blCO2Data <- NULL
blCO2FilteredData <- NULL

# available regions

regions <- generateRegionsList()

# name of the "main" selected region

nameOfTheSelectedRegion <- NULL

# name of the alternative region

scenarioAltRegion <- NULL

# name of the alternative region - only for "Health" tab

scenarioAltRegionHealth <- NULL

# directory with precalculated data for "Mode Share"

tripsdfRegionalInputData <- 'data/csv/tripsdf_regional/'

# directory with precalculated data for "Journey Time"

tripTotalTimePrecalculated <- 'data/csv/TripTotalTime1_regional/'

# server part

shinyServer(function(input, output, session){
  
  # Create a session specific placeholder to contain all region specific datasets
  sessionData <- NULL
  
  observe({
    input$inRegions
    
    if (!is.na(input$inRegions)){
      
      withProgress(message = 'Loading data', value = 0, {
        # Number of times we'll go through the loop
        n <- 10
        
        for (i in 1:n) {
          if (i == 1)
            setMSValues()
          if(i == 2)
            sessionData$sdata <<- subset(sdata, Region == input$inRegions)
          if(i == 3)
            sessionData$idata <<- subset(idata, HHoldGOR_B02ID == input$inRegions)
          if(i == 4)
            sessionData$co2data <<- subset(co2data, HHoldGOR_B02ID == input$inRegions)
          if(i == 5)
            sessionData$yll <<- subset(yll, regions == input$inRegions)
          if(i == 6)
            sessionData$yllReduction <<- subset(yllReduction, regions == input$inRegions)
          if(i == 7)
            sessionData$death <<- subset(death, regions == input$inRegions)
          if(i == 8)
            sessionData$baselineSummary <<- subset(baselineSummary, Region == input$inRegions)
          if(i == 9)
            sessionData$milesCycled <<- subset(milesCycled, HHoldGOR_B02ID == input$inRegions)
          if(i == 10)
            sessionData$carMiles <<- subset(carMiles, HHoldGOR_B02ID == input$inRegions)
          
          # Increment the progress bar, and update the detail text.
          incProgress(1/n, detail = paste(i, "out of", n))
        
        }
        
      })
      
      # regenerate list with MS/DP for selected region (filtering out cases when observed # of cyclists > DP)
      
      updateSelectInput(session, inputId = "inRegionSelected", choices = generateRegionsList(input$inRegions))
      
      # only for "Health" tab - regenerate list with MS/DP for selected region setting alternative region == main selected region
      
      updateSelectInput(session, inputId = "inRegionSelectedHealth", choices = generateRegionsList(), selected = input$inRegions)
      
      # set name of the selected main region
      
      nameOfTheSelectedRegion <<- names(regions[regions == as.integer(input$inRegions)])
      
      # set name of the alternative region in "Health" tab
      
      scenarioAltRegionHealth <<- names(regions[regions == as.integer(input$inRegions)])
      
    }
  })
  
  setMSValues <- reactive({
    
    updateSelectInput(session, inputId = "inGMS", choices =  generateUniqueMS(input$inRegions))
    updateSelectInput(session, inputId = "inGMS", choices =  generateUniqueMS(input$inRegions))
    updateSelectInput(session, inputId = "inGMS",  choices =  generateUniqueMS(input$inRegions))
    updateSelectInput(session, inputId = "inGMS", choices =  generateUniqueMS(input$inRegions))
    updateSelectInput(session, inputId = "inHealthMS",  choices =  generateUniqueMS(input$inRegions))
    updateSelectInput(session, inputId = "inCMMS",  choices =  generateUniqueMS(input$inRegions))
    updateSelectInput(session, inputId = "inCO2MS", choices =  generateUniqueMS(input$inRegions))
    
  })
  
  # observe Comparision with alternative region -> add warning if observed # cyclist in pop is > DP/MS value
  
  observe({
    
    # observe both, in case if selected region (this on the top of list) should show warning
    input$inRegionSelected
    input$inRegionSwitch
    
    # TODO: assuming that input$inGMS, input$inGEB, input$inGEQ are synced amongst tabs. In the future inputs should be unified
    casesOfDP <- subset(directProbCasesAboveGivenPerc, MS == as.numeric(input$inGMS) & ebikes == as.numeric(input$inGEB) & equity == as.numeric(input$inGEQ) & region == as.numeric(input$inRegionSelected))
    
    if (input$inRegionSwitch == "Region" & nrow(casesOfDP) > 0){
      
      show('region-switch-warning')
      shinyjs::html('region-switch-warning', 'Warning: in selected alternative region number of observed cyclists in a population is greater than "Select % of Population who are Regular Cyclists"')
      
    } else {
      
      # hide <p> with warning + remove content
      
      hide('region-switch-warning')
      shinyjs::html('region-switch-warning', '')
      
    }
    
    # set name of the alternative region
    
    scenarioAltRegion <<- names(regions[regions == as.integer(input$inRegionSelected)])
    
  })
  
  # observe alternative region in "Health" tab
  
  observe({
    
    # observe alternatvie region in "Health"
    
    input$inRegionSelectedHealth
    
    # set list with MS/DP values for selected alternative region
    
    #updateSelectInput(session, inputId = "inHealthMS1", choices =  generateUniqueMS(input$inRegionSelectedHealth))
    
    # inputs only available in "Health"
    
    casesOfDP <- subset(directProbCasesAboveGivenPerc, MS == as.numeric(input$inHealthMS1) & ebikes == as.numeric(input$inHealthEB1) & equity == as.numeric(input$inHealthEQ1) & region == as.numeric(input$inRegionSelectedHealth))
    
    if (nrow(casesOfDP) > 0){
      
      show('region-health-switch-warning')
      shinyjs::html('region-health-switch-warning', 'Warning: in selected alternative region number of observed cyclists in a population is greater than "Select % of Population who are Regular Cyclists"')
      
    } else {
      
      # hide <p> with warning + remove content
      
      hide('region-health-switch-warning')
      shinyjs::html('region-health-switch-warning', '')
      
    }
    
    # set name of the alternative region
    
    scenarioAltRegionHealth <<- names(regions[regions == as.integer(input$inRegionSelectedHealth)])
    
  })
  
  # observe inRegionSelected for tabs which don't use precalculated data, but subsetting happens on-the-fly
  
  observe({
    
    input$inRegionSelected
    
    if (!is.na(input$inRegionSelected)){
    
      # to save time subset data used in the selected tab only
      
      # "Miles Cycled"
      
      if (input$conditionedPanels == 3) {
        
        sessionData$RegionmilesCycled <<- subset(milesCycled, HHoldGOR_B02ID == input$inRegionSelected)
        
      # "Physical Activity"
        
      } else if (input$conditionedPanels == 4){
        
        sessionData$Regionidata <<- subset(idata, HHoldGOR_B02ID == input$inRegionSelected)
        
      # "Car Miles"
        
      } else if (input$conditionedPanels == 6){
        
        sessionData$RegioncarMiles <<- subset(carMiles, HHoldGOR_B02ID == input$inRegionSelected)
        
      # "CO2"
        
      } else if (input$conditionedPanels == 7){
        
        sessionData$Regionco2data <<- subset(co2data, HHoldGOR_B02ID == input$inRegionSelected)
        
      }
    }
  })
  

  
  output$inBaselineCycling <- renderUI({

    input$inRegions
    input$conditionedPanels
    
    if (input$conditionedPanels < 8)
      HTML("Baseline Cycling, Total Population (%): ", sessionData$baselineSummary[["% Cyclists in the Total Population"]], "\n")
    else
      HTML("")
  })

  plotTables <- reactive({
    (input$scenario != 'none')
  })
  
  plotBLDataTable<- reactive({
    data <- tdata
    if (input$bag != 'All'){
      data <- subset(data, age_group == input$bag)
    }
    if (input$bgender != 3)
      data <- subset(data, Sex_B01ID %in% input$bgender)
    
    if (input$bses != "All"){
      data <- subset(data, NSSec_B03ID %in% input$bses)
    }
    
    if (input$bethnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID %in% input$bethnicity)
    }
    data[is.na(data)] <- 0
    
    bldata <<- data
  })
  
  filterHealthData <- reactive({
    
    input$inRegions
    input$inRegionSelectedHealth
    
    data1 <- NULL
    if (input$inHealthVarSwitch == "Deaths")
      data1 <- sessionData$death
    else if (input$inHealthVarSwitch == "YLL")
      data1 <- sessionData$yll
    
    # Temporarily removing YLL total values
    data1 <- subset(data1, age.band != "All Ages")
    data2 <- sessionData$yllReduction
    
    if (input$inHealthAG != 'All'){
      # Temporarily removing YLL total values
      data1 <- subset(data1, age.band == input$inHealthAG)
      data2 <- subset(data2, age.band == input$inHealthAG | age.band == "All Ages")
    }
    if (input$inHealthG != 3){
      # Temporarily removing YLL total values
      data1 <- subset(data1, gender %in% input$inHealthG)
      data2 <- subset(data2, gender %in% input$inHealthG | gender == "Both Gender")
    }
    
    columnName <- paste(paste("MS", input$inHealthMS,sep = ""),  paste("ebik", input$inHealthEB,sep = ""), 
                        paste("eq", input$inHealthEQ,sep = ""), sep="_")
    
    columnName1 <- paste(paste("MS", input$inHealthMS1,sep = ""),  paste("ebik", input$inHealthEB1,sep = ""), 
                         paste("eq", input$inHealthEQ1,sep = ""), sep="_")
    
    dat1 <- data1[,c("age.band", "gender",columnName)]
    colnames(dat1) <- c("age.band", "gender", "scenario")
    
    dat2 <- data2[,c("age.band", "gender",columnName)]
    colnames(dat2) <- c("age.band", "gender", "scenario")
    
    dat3 <- data1[,c("age.band", "gender",columnName1)]
    colnames(dat3) <- c("age.band", "gender", "scenario")
    
    dat4 <- data2[,c("age.band", "gender",columnName1)]
    colnames(dat4) <- c("age.band", "gender", "scenario")
    
    # if alternative region is different
    
    data1AltRegion <- NULL
    data2AltRegion <- NULL
    
    if (input$inRegions != input$inRegionSelectedHealth){
      
      if (input$inHealthVarSwitch == "Deaths")
        data1AltRegion <- subset(death, regions == input$inRegionSelectedHealth)
      else if (input$inHealthVarSwitch == "YLL")
        data1AltRegion <- subset(yll, regions == input$inRegionSelectedHealth)
      
      # Temporarily removing YLL total values
      data1 <- subset(data1AltRegion, age.band != "All Ages")
      data2 <- subset(yllReduction, regions == input$inRegionSelectedHealth)
      
      if (input$inHealthAG != 'All'){
        # Temporarily removing YLL total values
        data1 <- subset(data1, age.band == input$inHealthAG)
        data2 <- subset(data2, age.band == input$inHealthAG | age.band == "All Ages")
      }
      if (input$inHealthG != 3){
        # Temporarily removing YLL total values
        data1 <- subset(data1, gender %in% input$inHealthG)
        data2 <- subset(data2, gender %in% input$inHealthG | gender == "Both Gender")
      }
      
      dat3 <- data1[,c("age.band", "gender",columnName1)]
      colnames(dat3) <- c("age.band", "gender", "scenario")
      
      dat4 <- data2[,c("age.band", "gender",columnName1)]
      colnames(dat4) <- c("age.band", "gender", "scenario")
      
    }
    
    scYllData <<- dat1
    scYllReductionData <<- dat2
    
    scYllData1 <<- dat3
    scYllReductionData1 <<- dat4
  })
  
  #   filterCarMilesData <- reactive ({
  #     # ID  age	Sex_B01ID	NSSec_B03ID	EthGroupTS_B02ID
  #     data <- carMiles
  #     
  #     if (input$inCMAG != 'All'){
  #       data <- subset(data, age == input$inCMAG)
  #     }
  #     if (input$inCMGender != 3)
  #       data <- subset(data, Sex_B01ID %in% input$inCMGender)
  #     
  #     if (input$inCMSES != "All"){
  #       data <- subset(data, NSSec_B03ID %in% input$inCMSES)
  #     }
  #     
  #     if (input$inCMEthnicity != "All"){
  #       data <- subset(data, EthGroupTS_B02ID %in% input$inCMEthnicity)
  #     }
  #     data[is.na(data)] <- 0
  #     #     cat(input$inCMAG, "\n")
  #     #     cat(nrow(data), ":", nrow(carMiles), "\n")
  #     
  #     columnName <- paste(paste("MS", input$inTTMS,sep = ""),  paste("ebik", input$inTTEB,sep = ""), 
  #                         paste("eq", input$inTTEQ,sep = ""), sep="_")
  #     
  #     
  #     data1 <- carMiles[,c("ID", "age","Sex_B01ID","NSSec_B03ID","EthGroupTS_B02ID", "baseline", columnName)]
  #     
  #     names(data1)[names(data1) == columnName] <- 'scenario'
  #     
  #     data1 <- arrange(data1, scenario)
  #     
  #     data2 <- data[,c("ID", "age","Sex_B01ID","NSSec_B03ID","EthGroupTS_B02ID", "baseline", columnName)]
  #     
  #     names(data2)[names(data2) == columnName] <- 'scenario'
  #     
  #     data2 <- arrange(data2, scenario)
  #     
  #     scCarMilesData <<- data1
  #     scCarMilesFilteredData <<- data2
  #     
  #   })
  
  plotMETDataTable<- reactive({
    input$inRegions
    input$inRegionSelected
    
    if (is.null(sessionData$idata) || is.na(sessionData$idata))
      return()
    
    # cat(" In Met table: ", nrow(sessionData$idata), "\n")
    
    #print("col names : ", names(sessionData$idata))
    
    data <- subset(sessionData$idata, select = c(ID,age_group,Sex_B01ID,EthGroupTS_B02ID,NSSec_B03ID,TripPurpose_B04ID, baseline))
    
    
    data["total_mmet"] <- data$baseline
    
    bMETdata <<- data
    
    
    if (input$inGAG != 'All'){
      data <- subset(data, age_group == input$inGAG)
    }
    if (input$inGGender != 3)
      data <- subset(data, Sex_B01ID %in% input$inGGender)
    
    if (input$inGSES != "All"){
      data <- subset(data, NSSec_B03ID %in% input$inGSES)
    }
    
    # if (input$inGTP != "All"){
    #   data <- subset(data, TripPurpose_B04ID == input$inGTP)
    # }
    
    if (input$inGEthnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID %in% input$inGEthnicity)
    }
    pd <<- data
    
    columnName <- paste(paste("MS", input$inGMS,sep = ""),  paste("ebik", input$inGEB,sep = ""), 
                        paste("eq", input$inGEQ,sep = ""), sep="_")
    
    data <- subset(sessionData$idata, select = c("ID","age_group","Sex_B01ID","EthGroupTS_B02ID","NSSec_B03ID",
                                                 "TripPurpose_B04ID",columnName))
    
    data["total_mmet"] <- data[columnName]
    
    scMETdata <<- data
    
    
    #scMETdata
    #scFilteredMETdata
    
    if (input$inGAG != 'All'){
      data <- subset(data, age_group == input$inGAG)
    }
    if (input$inGGender != 3)
      data <- subset(data, Sex_B01ID %in% input$inGGender)
    
    if (input$inGSES != "All"){
      data <- subset(data, NSSec_B03ID %in% input$inGSES)
    }
    
    # if (input$inGTP != "All"){
    #   data <- subset(data, TripPurpose_B04ID == input$inGTP)
    # }
    
    if (input$inGEthnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID %in% input$inGEthnicity)
    }
    
    #data[is.na(data)] <- 0
    
    scFilteredMETdata <<- data
    
    # to save time only if comparision with alternative region is selected
    
    if(input$inRegionSwitch == 'Region'){
      
      # full data
      
      data <- subset(sessionData$Regionidata, select = c("ID","age_group","Sex_B01ID","EthGroupTS_B02ID","NSSec_B03ID","HHoldGOR_B02ID", 
                                                         "TripPurpose_B04ID", columnName))
      
      data["total_mmet"] <- data[columnName]
      
      scMETdataAltRegFull <<- data
      
      # filtered data
      
      if (input$inGAG != 'All'){
        data <- subset(data, age_group == input$inGAG)
      }
      if (input$inGGender != 3)
        data <- subset(data, Sex_B01ID %in% input$inGGender)
      
      if (input$inGSES != "All"){
        data <- subset(data, NSSec_B03ID %in% input$inGSES)
      }
      
      # if (input$inGTP != "All"){
      #   data <- subset(data, TripPurpose_B04ID == input$inGTP)
      # }
      
      if (input$inGEthnicity != "All"){
        data <- subset(data, EthGroupTS_B02ID %in% input$inGEthnicity)
      }
      
      scMETdataAltRegFiltered <<- data
      
    }
    
  })
  
  output$plotMET <- renderChart({
    input$inRegions
    input$flipG
    input$phyGuideline
    plotMETDataTable()
    
    firstColData <- NULL
    secondColData <- NULL
    
    extended_title <- ""
    filtered_title <- ""
    
    if (!is.null(idata) & !is.null(scMETdata)){
      if (input$flipG == 'sep'){
        
        # Keep the data separated
        # scMETdata and scFilteredMETdata
        firstColData = scMETdata
        secondColData = scFilteredMETdata
        
        firstColName <- "Scenario (Total Population)"
        secondColName <- "Scenario (Sub-population)"
        
        extended_title <- paste0("Scenario [", nameOfTheSelectedRegion, "] - ")
        filtered_title <- getMETFilteredTitle("", secondColData)
        
      }else{
        
        # if comparision with alternative region is selected
        
        if(input$inRegionSwitch == 'Region'){ 
          
          firstColData = scFilteredMETdata
          secondColData = scMETdataAltRegFiltered
          extended_title <- paste0("Sub-population [", nameOfTheSelectedRegion, "] - ")
          
          firstColName <- paste0("Scenario [", nameOfTheSelectedRegion, "] (Sub-population)")
          secondColName <- paste0("Scenario - alternative Region [", scenarioAltRegion, "] (Sub-population)")
          
          filtered_title <- getMETFilteredTitle(paste0("Scenario [", nameOfTheSelectedRegion, "]:"), firstColData, paste0(", Scenario - alternative Region [", scenarioAltRegion, "]: "), secondColData)
          
        } else {
          
          # Keep the data mixed
          firstColData = pd
          secondColData = scFilteredMETdata
          extended_title <- paste0("Sub-population [", nameOfTheSelectedRegion, "] - ")
          
          firstColName <- "Baseline (Sub-population)"
          secondColName <- "Scenario (Sub-population)"
          
          filtered_title <- getMETFilteredTitle("", secondColData)
          
        }
        
        # if no filter is selected
        
        if (input$inGGender == 3 && input$inGEthnicity == "All" && input$inGSES == "All" && input$inGAG == "All"){# && input$inGTP == "All"){
          
          extended_title <- paste0("Total Population [", nameOfTheSelectedRegion, "] - ")
          # Replace sub-population with total population for both first and second column names
          firstColName <- gsub("Sub-population", "Total Population", firstColName)
          secondColName <- gsub("Sub-population", "Total Population", secondColName)
          
        
        }
      }
      
      h1 <- Highcharts$new()
      h1$chart(type = "column", style = list(fontFamily = 'Arial, sans-serif',
                                             fontSize = '12px'))
      
      if (input$phyGuideline == 'on'){
        
        # change the title reflecting which outcome measure is used
        
        extended_title <- paste0(extended_title, '% meeting WHO physical activity guidelines')
        
        bc <- createPhyActTable(firstColData)
        bc$Freq <- round(bc$Freq  / nrow(firstColData) * 100, digits = 1)
        
        h1$xAxis(categories = c("Not meeting guidelines (MMETh < 8.75)", "Meeting the guidelines (MMETh >= 8.75)", 
                                "Meeting the higher guidelines (MMETh >= 17.5)"), 
                 title = list(text = '% meeting WHO physical activity guidelines'))
        h1$series(data = bc$Freq, name = firstColName)
        
        bc <- createPhyActTable(secondColData)
        bc$Freq <- round(bc$Freq  / nrow(secondColData) * 100, digits = 1)
        
        h1$series(data = bc$Freq, name = secondColName)
        
        if (nrow(firstColData) < 10){
          h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"),
                      style = list(fontFamily = 'Arial, sans-serif',
                                   fontSize = '14px',
                                   fontWeight = 'bold',
                                   color = "#f00"))
        }else{
          h1$subtitle(text= filtered_title)
        }
        
        h1$yAxis(tickInterval = 20, title = list(text = 'Percentage of the total population'))
        
        h1$plotOptions(column=list(animation=FALSE, 
                                   dataLabels = list(enabled = T, 
                                                     crop = FALSE, 
                                                     rotation = -90, 
                                                     align = 'right', 
                                                     color = '#FFFFFF', 
                                                     width = 100,
                                                     x = 4, 
                                                     y = 10, 
                                                     style = list(fontSize = '10px', fontFamily = 'Arial, sans-serif'))))
        
        # Set exporting style
        
        h1$exporting(enabled = T,
                     chartOptions = list(
                       legend = list(
                         x = -30,
                         itemDistance = 80,
                         itemMarginBottom = 5
                       ),
                       xAxis = list(
                         title = list(
                           y = 10
                         )
                       )
                     ))
        
        
      }else{
        
        # change the title reflecting which outcome measure is used
        
        extended_title <- paste0(extended_title, 'Marginal MET hours (walking, cycling, sports and recreation activity)')
        
        bc <- as.data.frame(table (cut (firstColData$total_mmet, breaks = c(seq(-4.4,52.8, 4.4), max(firstColData$total_mmet)))))
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        bc1max <- max(bc$Freq, na.rm = T)
        
        h1$xAxis(categories = c("0",">0 & <=4.4",">4.4 & <=8.8",">8.8 & <=13.2",">13.2 & <=17.6",
                                ">17.6 & <=22",">22 & <=26.4",">26.4 & <=30.8",">30.8 & <=35.2",
                                ">35.2 & <=39.6",">39.6 & <=44",">44 & <=48.4",">48.4 & <52.8",">52.8"), title = list(text = 'Marginal MET Hours'))
        h1$series(data = bc$Freq, name = firstColName)
        max_val <- 0
        if (nrow(secondColData) > 1)
          max_val <- max(secondColData$total_mmet, na.rm = T)
        h <- NULL
        if (max_val <= 52.8){
          if (max_val >= 4.4){
            bc <- table (cut (secondColData$total_mmet, breaks = c(seq(min(secondColData$total_mmet) - 4.4, ceiling(max(secondColData$total_mmet) - 4.4), by = 4.4), max(secondColData$total_mmet))))
          }else{
            bc <- table (cut (secondColData$total_mmet, breaks = c(-4.4, max(secondColData$total_mmet))))
          }
        }
        else{
          bc <- table (cut (secondColData$total_mmet, breaks = c(seq(-4.4,52.8, 4.4), max(secondColData$total_mmet))))
        }
        
        bc <- as.data.frame(bc)
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        filter <- FALSE
        if (sum(bc$Freq, na.rm = T) > 10)
          filter <- TRUE
        bc2max <- 0
        if (nrow(as.data.frame(bc)) > 0)
          bc2max <- max(bc$Freq, na.rm = T)
        max_y <- max(bc1max, bc2max)
        h1$yAxis(min = 0, max = max(30, max_y), tickInterval = 10, title = list(text = 'Percentage of the total population'))
        
        if(filter){
          h1$series(data = bc$Freq, name = secondColName)
          h1$subtitle(text= filtered_title)
          
        }else{
          h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"),
                      style = list(fontFamily = 'Arial, sans-serif',
                                   fontSize = '14px',
                                   fontWeight = 'bold',
                                   color = "#f00"))
        }
        
        h1$plotOptions(column=list(animation=FALSE))
        
        # Set exporting style
        # Reduce font size of the x-axis
        h1$exporting(enabled = T,
                     chartOptions = list(
                       legend = list(
                         x = -30,
                         itemDistance = 80,
                         itemMarginBottom = 5
                       ),
                       xAxis = list(
                         title = list(
                           y = 10
                         ),
                         labels = list (
                           style = list (
                             fontSize = "8px"  
                           )
                         )
                       )
                     ))
      }
      
    }
    
    h1$title(text = extended_title)
    h1$tooltip(formatter = "#! function() {  return this.series.name +'<br/>' + 'Value: <b>' + this.y + '%'; } !#")
    
    h1$set(dom = "plotMET")
    
    return(h1)
  })
  
  output$plotScenarioMET <- renderChart({
    input$inRegions
    input$flipG
    input$phyGuideline
    
    plotMETDataTable()
    
    firstColData <- NULL
    secondColData <- NULL
    
    extended_title <- ""
    filtered_title <- ""
    
    if (!is.null(scMETdata)){
      
      if (input$flipG == 'sep'){
        
        # if comparision with alternative region is selected
        
        if(input$inRegionSwitch == 'Region'){
          
          # Keep the data separated
          firstColData = scMETdataAltRegFull
          secondColData = scMETdataAltRegFiltered
          
          extended_title <- paste0("Scenario - alternative Region [", scenarioAltRegion, "] - ")
          
          firstColName <- "Scenario - alternative Region (Total Population)"
          secondColName <- "Scenario - alternative Region (Sub-population)"
          
          filtered_title <- getMETFilteredTitle("", secondColData)
          
        } else {
          
          # Keep the data separated
          # scMETdata and scFilteredMETdata
          firstColData = bMETdata
          secondColData = pd
          
          extended_title <- paste0("Baseline [", nameOfTheSelectedRegion, "] - ")
          
          firstColName <- "Baseline (Total Population)"
          secondColName <- "Baseline (Sub-population)"
          
          filtered_title <- getMETFilteredTitle("", secondColData)
          
        }
        
      }else{
        
        
        # if comparision with alternative region is selected
        
        if(input$inRegionSwitch == 'Region'){
          
          # Keep the data mixed
          firstColData = scMETdata
          secondColData = scMETdataAltRegFull
          
          extended_title <- paste0("Total Population [", nameOfTheSelectedRegion, "] - ")
          
          # set columns names
          
          firstColName <- paste0("Scenario [", nameOfTheSelectedRegion, "] (Total Population)")
          secondColName <- paste0("Scenario - alternative Region [", scenarioAltRegion, "] (Total Population)")
          
          # TODO: below 'if' is not applicable in alternative region comparision?
          # secondColName <- "Scenario (Sub-population)"
          # if (nrow(sessionData$idata) == nrow(scMETdata))
          #   secondColName <- "Scenario (Total Population)"
          
          filtered_title <- getMETFilteredTitle(paste0("Scenario [", nameOfTheSelectedRegion, "]:"), firstColData, paste0(", Scenario - alternative Region [", scenarioAltRegion, "]:"), secondColData, titlePrefix = "Total Size: ", showingTotal = TRUE)
          
        } else {
          
          # Keep the data mixed
          firstColData = bMETdata
          secondColData = scMETdata
          
          extended_title <- paste0("Total Population [", nameOfTheSelectedRegion, "] - ")
          
          firstColName <- "Baseline (Total Population)"
          secondColName <- "Scenario (Sub-population)"
          if (nrow(sessionData$idata) == nrow(scMETdata))
            secondColName <- "Scenario (Total Population)"
          
          filtered_title <- getMETFilteredTitle("", secondColData, titlePrefix = "Total Size: ", showingTotal = TRUE)
        }
      }
      
      h1 <- Highcharts$new()
      h1$chart(type = "column", style = list(fontFamily = 'Arial, sans-serif',
                                             fontSize = '12px'))
      
      
      if (input$phyGuideline == 'on'){
        
        # change the title reflecting which outcome measure is used
        
        extended_title <- paste0(extended_title, '% meeting WHO physical activity guidelines')
        
        bc <- createPhyActTable(firstColData)
        bc$Freq <- round(bc$Freq  / nrow(firstColData) * 100, digits = 1)
        
        h1$xAxis(categories = c("Not meeting guidelines (MMETh < 8.75)", "Meeting the guidelines (MMETh >= 8.75)", 
                                "Meeting the higher guidelines (MMETh >= 17.5)"), 
                 title = list(text = '% meeting WHO physical activity guidelines'))
        h1$series(data = bc$Freq, name = firstColName)
        
        bc <- createPhyActTable(secondColData)#$total_mmet)
        bc$Freq <- round(bc$Freq  / nrow(secondColData) * 100, digits = 1)
        
        h1$series(data = bc$Freq, name = secondColName)
        
        if (nrow(firstColData) < 10){
          h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"),
                      style = list(fontFamily = 'Arial, sans-serif',
                                   fontSize = '14px',
                                   fontWeight = 'bold',
                                   color = "#f00"))
        }else{
          h1$subtitle(text= filtered_title)
        }
        
        h1$yAxis(tickInterval = 20, title = list(text = 'Percentage of the total population'))
        
        h1$plotOptions(column=list(animation=FALSE, 
                                   dataLabels = list(enabled = T, 
                                                     crop = FALSE, 
                                                     rotation = -90, 
                                                     align = 'right', 
                                                     color = '#FFFFFF', 
                                                     width = 100,
                                                     x = 4, 
                                                     y = 10, 
                                                     style = list(fotSize = '10px', fontFamily = 'Arial, sans-serif'))))
        
        h1$exporting(enabled = T,
                     chartOptions = list(
                       legend = list(
                         x = -30,
                         itemDistance = 80,
                         itemMarginBottom = 5
                       ),
                       xAxis = list(
                         title = list(
                           y = 10
                         )
                       )
                     ))
        
        
      }else{
        
        # change the title reflecting which outcome measure is used
        
        extended_title <- paste0(extended_title, 'Marginal MET hours (walking, cycling, sports and recreation activity)')
        
        bc <- as.data.frame(table (cut (firstColData$total_mmet, breaks = c(seq(-4.4,52.8, 4.4), max(firstColData$total_mmet)))))
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        bc1max <- max(bc$Freq, na.rm = T)
        
        h1$xAxis(categories = c("0",">0 & <=4.4",">4.4 & <=8.8",">8.8 & <=13.2",">13.2 & <=17.6",
                                ">17.6 & <=22",">22 & <=26.4",">26.4 & <=30.8",">30.8 & <=35.2",
                                ">35.2 & <=39.6",">39.6 & <=44",">44 & <=48.4",">48.4 & <52.8",">52.8"),
                                title = list(text = 'Marginal MET Hours'))
        h1$series(data = bc$Freq, name = firstColName)
        max_val <- 0
        if (nrow(secondColData) > 1)
          max_val <- max(secondColData$total_mmet, na.rm = T)
        h <- NULL
        if (max_val <= 52.8){
          if (max_val >= 4.4){
            bc <- table (cut (secondColData$total_mmet, breaks = c(seq(min(secondColData$total_mmet) - 4.4, ceiling(max(secondColData$total_mmet) - 4.4), by = 4.4), max(secondColData$total_mmet))))
          }else{
            bc <- table (cut (secondColData$total_mmet, breaks = c(-4.4, max(secondColData$total_mmet))))
          }
        }
        else{
          bc <- table (cut (secondColData$total_mmet, breaks = c(seq(-4.4,52.8, 4.4), max(secondColData$total_mmet))))
        }
        
        bc <- as.data.frame(bc)
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        filter <- FALSE
        if (sum(bc$Freq, na.rm = T) > 10)
          filter <- TRUE
        bc2max <- 0
        if (nrow(as.data.frame(bc)) > 0)
          bc2max <- max(bc$Freq, na.rm = T)
        max_y <- max(bc1max, bc2max)
        h1$yAxis(min = 0, max = max(30, max_y), tickInterval = 10, title = list(text = 'Percentage of the total population'))
        
        
        if(filter){
          h1$series(data = bc$Freq, name = secondColName)
          h1$subtitle(text= filtered_title)
          
        }else{
          h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"),
                      style = list(fontFamily = 'Arial, sans-serif',
                                   fontSize = '14px',
                                   fontWeight = 'bold',
                                   color = "#f00"))
        }
        
        h1$plotOptions(column=list(animation=FALSE))
        # Set exporting style
        # Reduce font size of the x-axis
        h1$exporting(enabled = T,
                     chartOptions = list(
                       legend = list(
                         x = -30,
                         itemDistance = 80,
                         itemMarginBottom = 5
                       ),
                       xAxis = list(
                         title = list(
                           y = 10
                         ),
                         labels = list (
                           style = list (
                             fontSize = "8px"  
                           )
                         )
                       )
                     ))
        
        
        
      }
      
    }
    
    h1$title(text = extended_title)
    h1$tooltip(formatter = "#! function() {  return this.series.name +'<br/>' + 'Value: <b>' + this.y + '%'; } !#")
    h1$set(dom = "plotScenarioMET")
    return(h1)
    
  })
  output$plotHealth <- renderChart({
    
    input$inHealthVarSwitch
    input$inRegionSelectedHealth
    filterHealthData()
    
    h1 <- Highcharts$new()
    h1$chart(type = "column", style = list(fontFamily = 'Arial, sans-serif',
                                           fontSize = '12px'))
    
    
    if (nrow(scYllReductionData) > 0){
      
      if (input$inHealthSwitch == "Scenario"){

        eq <- "Off"
        if (input$inHealthEQ == 1)
          eq <- "On"
        
        eb <- "Off"
        if (input$inHealthEB == 1)
          eb <- "On"
        
        h1$series(data = scYllData$scenario, name = paste(paste(paste("% Regular Cyclists",(as.numeric(input$inHealthMS) * 100) ), paste("Equity", eq), paste("Ebike", eb), sep=", "), paste0("[", nameOfTheSelectedRegion ,"]")) )
        
        eq <- "Off"
        if (input$inHealthEQ1 == 1)
          eq <- "On"
        
        eb <- "Off"
        if (input$inHealthEB1 == 1)
          eb <- "On"
        
        h1$series(data = scYllData1$scenario, name = paste(paste(paste("% Regular Cyclists",(as.numeric(input$inHealthMS) * 100) ), paste("Equity", eq), paste("Ebike", eb), sep=", "), paste0("[", scenarioAltRegionHealth, "]")) )
        
        # recode gender code to text values
        
        scYllDataRecoded <- dplyr::inner_join(scYllData, data.frame(gender=c(1,2), genderTextValue=c("Male","Female")), by="gender")
        
        scYllDataRecoded <- dplyr::arrange(scYllDataRecoded, gender, age.band)
        
        h1$xAxis(categories = paste(scYllDataRecoded$genderTextValue, scYllDataRecoded$age.band))
        
      }else{
        # For both gender, create new series
        ugender <- sort(unique(scYllData$gender))
        
        for (i in 1:length(ugender)){
          data <- subset(scYllData, gender == ugender[i])
          seriesName <- "Male"
          if (ugender[i] == 2)
            seriesName <- "Female"
          h1$series(data = data$scenario, name = seriesName)
        }
        
        h1$xAxis(categories = append(input$inHealthAG, " "), title = list(text = 'Age and Gender Group'))
        if (length(unique(scYllData$age.band)) > 1)
          h1$xAxis(categories = unique(scYllData$age.band), title = list(text = 'Age and Gender Groups'))
      }
      if (input$inHealthVarSwitch == "YLL")
        h1$yAxis(title = list(text = 'YLL (Absolute Numbers)'))
      else if (input$inHealthVarSwitch == "Deaths")
        h1$yAxis(title = list(text = 'Averted number of Deaths'))
      
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size = 0)"), 
                  style = list(fontFamily = 'Arial, sans-serif',
                               fontSize = '14px',
                               fontWeight = 'bold',
                               color = "#f00"))
    }
    if (input$inHealthVarSwitch == "YLL")
      h1$title(text = "Reduction in Years of Life Lost (YLL) ")
    else
      h1$title(text = "Averted number of Deaths ")
    h1$set(dom = "plotHealth")
    h1$exporting(enabled = T)
    return(h1)
  })
  
  output$plotHealthReduction <- renderChart({
    input$inHealthVarSwitch
    filterHealthData()
    
    h1 <- Highcharts$new()
    h1$chart(type = "column", style = list(fontFamily = 'Arial, sans-serif',
                                           fontSize = '12px'))
    
    if (nrow(scYllReductionData) > 0){
      if (input$inHealthSwitch == "Scenario"){
        
        eq <- "Off"
        if (input$inHealthEQ == 1)
          eq <- "On"
        
        eb <- "Off"
        if (input$inHealthEB == 1)
          eb <- "On"
        
        h1$series(data = scYllReductionData$scenario, name = paste(paste(paste("% Regular Cyclists",(as.numeric(input$inHealthMS1) * 100) ), paste("Equity", eq), paste("Ebike", eb), sep=", "), paste0("[", nameOfTheSelectedRegion ,"]")) )
        
        eq <- "Off"
        if (input$inHealthEQ1 == 1)
          eq <- "On"
        
        eb <- "Off"
        if (input$inHealthEB1 == 1)
          eb <- "On"
        
        
        h1$series(data = scYllReductionData1$scenario, name = paste(paste(paste("% Regular Cyclists", (as.numeric(input$inHealthMS1) * 100) ), paste("Equity", eq), paste("Ebike", eb), sep=", "), paste0("[", scenarioAltRegionHealth ,"]")) )
        
        # recode gender code to text values
        
        scYllDataRecoded <- dplyr::inner_join(scYllReductionData, data.frame(gender=c(1,2), genderTextValue=c("Male","Female")), by="gender")
        
        scYllDataRecoded <- dplyr::arrange(scYllDataRecoded, gender, age.band)
        
        h1$xAxis(categories = paste(scYllDataRecoded$genderTextValue, scYllDataRecoded$age.band))
        
      }else{
        
        # For both gender, create new series
        ugender <- sort(unique(scYllReductionData$gender))
        
        for (i in 1:length(ugender)){
          data <- subset(scYllReductionData, gender == ugender[i])
          seriesName <- "Male"
          if (ugender[i] == 2)
            seriesName <- "Female"
          h1$series(data = data$scenario, name = seriesName)
        }
        
        h1$xAxis(categories = append(input$inHealthAG, " "), title = list(text = 'Age and Gender Groups'))
        if (length(unique(scYllReductionData$age.band)) > 2)
          h1$xAxis(categories = unique(scYllReductionData$age.band), title = list(text = 'Age and Gender Groups'))
      }
      
      h1$yAxis(title = list(text = 'Percentage reduction in disease burden'))
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size = 0)"),
                  style = list(fontFamily = 'Arial, sans-serif',
                               fontSize = '14px',
                               fontWeight = 'bold',
                               color = "#f00"))
    }
    
    if (input$inHealthVarSwitch == "YLL")
      h1$title(text = "Reduction in percentage of Years of Life Lost (YLL) ")
    else
      h1$title(text = "Reduction in percentage of Deaths ")
    h1$tooltip(valueSuffix= '%')
    h1$set(dom = "plotHealthReduction")
    h1$exporting(enabled = T)
    return(h1)
  })
  
  
  output$plotBaseline <- renderChart({
    if (!is.null(tdata)){
      if (input$scenario == 'i'){
        
        filtered_title <- getFilteredTitle(sessionData$idata)
        max_val <- max(sessionData$idata$total_mmet)
        h <- NULL
        bc <- table (cut (sessionData$idata$total_mmet, breaks = c(seq(min(sessionData$idata$total_mmet), 60, by = 5),max(sessionData$idata$total_mmet)), xlim = c(min(sessionData$idata$total_mmet), 60)))
        extended_title <- paste("Marginal MET hours of total population")
        bc <- as.data.frame(bc)
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        
        h1 <- Highcharts$new()
        h1$title(text = extended_title)
        h1$tooltip(valueSuffix= '%')
        h1$xAxis(categories = bc$Var1, title = list(text = 'Marginal MET Hours'))
        
        h1$chart(type = "column")
        h1$plotOptions(column=list(animation=FALSE))
        h1$series(data = bc$Freq, name = "Total Population")
        h1$set(dom = "plotBaseline")
        h1$exporting(enabled = T)
        return(h1)
      }else{
        h1 <- Highcharts$new()
        h1$set(dom = "plotBaseline")
        h1$exporting(enabled = T)
        return(h1)
      }
    }
  })
  
  getFilteredBDTitle <- function (src){
    filtered_title <- ""
    if (src == "BD")
      filtered_title <- paste("Cycling Multiplier: ", input$inGMS, ", Equity: ", input$inGEQ, " and Ebike: ", input$inGEB , sep = "" )
    else if (src == "FT")
      filtered_title <- paste("Cycling Multiplier: ", input$inFTMS, ", Equity: ", input$inFTEQ, " and Ebike: ", input$inFTEB, sep = "" )
    
    filtered_title
  }
  
  getBaselineFilteredTitle <- function(data){
    filtered_title <- ""
    if (nrow(data) != nrow (bldata)){
      
      displayGender <- "All"
      if (input$bgender == 1){
        displayGender <- "Male"
      }else if (input$bgender == 2){
        displayGender <- "Female"
      }
      
      displayEthnicity <- "All"
      if (input$bethnicity == 1){
        displayEthnicity <- "White"
      }else if (input$bethnicity == 2){
        displayEthnicity <- "Non-White"
      }
      
      displaySES <- "All"
      if (input$bses == 1){
        displaySES <- "Managerial and professional occupations"
      }else if (input$bses == 2){
        displaySES <- "Intermediate occupations and small employers"
      }else if (input$bses == 3){
        displaySES <- "Routine and manual occupations"
      }else if (input$bses == 4){
        displaySES <- "Never worked and long-term unemployed"
      }else if (input$bses == 5){
        displaySES <- "Not classified (including students)"
      }
      
      filtered_title <- paste("Age Group: ", str_trim(input$bag), ", Gender: ", displayGender, ", Socio Economic Classification: ", displaySES, " and Ethnicity: ", displayEthnicity, sep = "" )
      filtered_title
    }else
      filtered_title
  }
  
  getMETFilteredTitle <- function(firstDataTitle, firstData, secondDataTitle = NULL, secondData = NULL, titlePrefix = NULL, showingTotal = FALSE){
    
    npeople <- length(unique(firstData$ID))
    
    filtered_title <- ""
    
    secondDataNPeople <- ""
    secondDataTitleOutput <- ""
    secondExtraSep <- ""
    
    if(!is.null(secondDataTitle)){
      
      secondDataTitleOutput <- secondDataTitle
      secondExtraSep <- " "
      
    }
    
    if(!is.null(secondData)){
      
      secondDataNPeople <- length(unique(secondData$ID))
      secondExtraSep <- " "
      
    }
    
    if(!is.null(titlePrefix)){
      titlePrefixOutput <- titlePrefix
    } else {
      titlePrefixOutput <- "Sample Size: "
    }
    
    # TODO: check if checking activation of any filters is correct replacement for below
    # if (src == "baseline")
    #   dataSource = sessionData$idata
    # else
    #   dataSource = scMETdata
    # 
    # npeople <- length(unique(data$ID))
    # # cat(npeople, "\n")
    # 
    # if (nrow(data) != nrow (dataSource)){
    if (input$inGGender != 3 || input$inGEthnicity != "All" || input$inGSES != "All" || input$inGAG != "All"){# || input$inGTP != "All"){
      
      displayGender <- "All"
      if (input$inGGender == 1){
        displayGender <- "Male"
      }else if (input$inGGender == 2){
        displayGender <- "Female"
      }
      
      displayEthnicity <- "All"
      if (input$inGEthnicity == 1){
        displayEthnicity <- "White"
      }else if (input$inGEthnicity == 2){
        displayEthnicity <- "Non-White"
      }
      
      displaySES <- "All"
      if (input$inGSES == 1){
        displaySES <- "Managerial and professional occupations"
      }else if (input$inGSES == 2){
        displaySES <- "Intermediate occupations and small employers"
      }else if (input$inGSES == 3){
        displaySES <- "Routine and manual occupations"
      }else if (input$inGSES == 4){
        displaySES <- "Never worked and long-term unemployed"
      }else if (input$inGSES == 5){
        displaySES <- "Not classified (including students)"
      }
      
      if(showingTotal){
        
        filters_info <- ""
        
      } else {
        
        filters_info <- paste0( ", Age Group: ", str_trim(input$inGAG), ", Gender: ", displayGender, ", Socio Economic Classification: ", displaySES, " and Ethnicity: ", displayEthnicity)
        
      }
      
      filtered_title <- paste0(titlePrefixOutput,
                              firstDataTitle,
                              ' ',
                              npeople,
                              secondDataTitleOutput,
                              secondExtraSep,
                              secondDataNPeople,
                              filters_info)
      filtered_title
    }else
      filtered_title
  }
  
  getTripsFilteredTitle <- function(firstDataTitle, firstData, secondDataTitle = NULL, secondData = NULL, titlePrefix = NULL, showingTotal = FALSE){
    
    npeople <- unique(firstData$total_population)
    
    filtered_title <- ""
    
    secondDataNPeople <- ""
    secondDataTitleOutput <- ""
    secondExtraSep <- ""
    
    if(!is.null(secondDataTitle)){
      
      secondDataTitleOutput <- secondDataTitle
      secondExtraSep <- " "
      
    }
    
    if(!is.null(secondData)){
      
      secondDataNPeople <- unique(secondData$total_population)
      secondExtraSep <- " "
      
    }
    
    if(!is.null(titlePrefix)){
      titlePrefixOutput <- titlePrefix
    } else {
      titlePrefixOutput <- "Sample Size (trips): "
    }
    
    if (input$inGAG != "All" || input$inGGender != 3 || input$inGEthnicity != "All" || input$inGSES != "All" || input$inGTP != "All" ){
      
      displayGender <- "All"
      if (input$inGGender == 1){
        displayGender <- "Male"
      }else if (input$inGGender == 2){
        displayGender <- "Female"
      }
      
      displayEthnicity <- "All"
      if (input$inGEthnicity == 1){
        displayEthnicity <- "White"
      }else if (input$inGEthnicity == 2){
        displayEthnicity <- "Non-White"
      }
      
      displaySES <- "All"
      if (input$inGSES == 1){
        displaySES <- "Managerial and professional occupations"
      }else if (input$inGSES == 2){
        displaySES <- "Intermediate occupations and small employers"
      }else if (input$inGSES == 3){
        displaySES <- "Routine and manual occupations"
      }else if (input$inGSES == 4){
        displaySES <- "Never worked and long-term unemployed"
      }else if (input$inGSES == 5){
        displaySES <- "Not classified (including students)"
      }
      
      
      # purpose <- c("All" = "All",
      #              "Commuting" = 1,
      #              "Business" = 2, 	
      #              "Education / escort education" = 3, 
      #              "Shopping" = 4,
      #              "Other escort" = 5, 
      #              "Personal business" = 6, 
      #              "Leisure" = 7)
      
      displayPurpose <- 'All'
      if (input$inGTP == 1){
        displayPurpose <- "Commuting"
      }else if (input$inGTP == 2){
        displayPurpose <- "Business"
      }else if (input$inGTP == 3){
        displayPurpose <- "Education / escort education"
      }else if (input$inGTP == 4){
        displayPurpose <- "Shopping"
      }else if (input$inGTP == 5){
        displayPurpose <- "Other escort"
      }else if (input$inGTP == 6){
        displayPurpose <- "Personal business"
      }else if (input$inGTP == 7){
        displayPurpose <- "Leisure"
      }
      
      if(showingTotal){
        
        filters_info <- ""
        
      } else {
        
        filters_info <- paste0(", Purpose: ", displayPurpose, ", Age Group: ", str_trim(input$inGAG), ", Gender: ", displayGender, ", Socio Economic Classification: ", displaySES, " and Ethnicity: ", displayEthnicity)
        
      }
      
      filtered_title <- paste0(titlePrefixOutput,
                              firstDataTitle,
                              ' ',
                              npeople,
                              secondDataTitleOutput,
                              secondExtraSep,
                              secondDataNPeople,
                              filters_info)
      filtered_title
    }else
      filtered_title
  }
  
  generateBDScenarioTable <- reactive({
    
    # Build a reactive relation with regions dropdown menu
    
    input$inRegions
    
    # Filter data of trips
    
    columnName <- paste(paste("MS", input$inGMS,sep = ""),  paste("ebik", input$inGEB,sep = ""), 
                        paste("eq", input$inGEQ,sep = ""), sep="_")
    
    # full baseline
    
    msBaseline <<- readRDS(paste0(tripsdfRegionalInputData, 'full/', input$inRegions, '/baseline.rds'))
    
    # full scenario
    
    msScenario <<- readRDS(paste0(tripsdfRegionalInputData, 'full/', input$inRegions, '/', columnName, '.rds'))
    
    # full alternative region scenario
    
    if (input$inRegionSwitch == "Region"){
      
      msAltRegionScenario <<- readRDS(paste0(tripsdfRegionalInputData, 'full/', input$inRegionSelected, '/', columnName, '.rds'))
    
    }
    
    #tdBaseline
    
    tdScenarioTemp <- readRDS(paste0(tripsdfRegionalInputData, 'filtered/', input$inRegions, '/', columnName, '.rds'))
    
    # filtered scenario
    
    tdScenario <<- subset(tdScenarioTemp, agegroup == input$inGAG & gender == input$inGGender & ses == input$inGSES & ethnicity == input$inGEthnicity
                          & purpose == input$inGTP)
    
    # filtered baseline
    
    tdBaselineTemp <- readRDS(paste0(tripsdfRegionalInputData, 'filtered/', input$inRegions, '/baseline.rds'))
    
    tdBaseline <<- subset(tdBaselineTemp, agegroup == input$inGAG & gender == input$inGGender & ses == input$inGSES & ethnicity == input$inGEthnicity
                          & purpose == input$inGTP)
    
    # bd <<- data2
    
    # filtered alternative region scenario
    
    if (input$inRegionSwitch == "Region"){
      
      tdAltRegionScenarioTemp <- readRDS(paste0(tripsdfRegionalInputData, 'filtered/', input$inRegionSelected, '/', columnName, '.rds'))
      
      tdAltRegionScenario <<- subset(tdAltRegionScenarioTemp, agegroup == input$inGAG & gender == input$inGGender & ses == input$inGSES & ethnicity == input$inGEthnicity
                                     & purpose == input$inGTP)
      
    }
      
  })

  generateFasterTripsTable <- reactive({
    
    lMS <- input$inFTMS
    lEB <- input$inFTEB
    lEQ <- input$inFTEQ
    
    data <- fasterTripData
    data <- subset(data, MS == lMS & equity == lEQ & ebike == lEB)
    
    data[is.na(data)] <- 0
    data <- arrange(data, MS)
    ftdata <<- data
    
  })
  
  generateSlowerTripsTable <- reactive({
    
    lMS <- input$inFTMS
    lEB <- input$inFTEB
    lEQ <- input$inFTEQ
    
    data <- slowerTripData
    data <- subset(data, MS == lMS & equity == lEQ & ebike == lEB)
    
    data[is.na(data)] <- 0
    data <- arrange(data, MS)
    swdata <<- data
    
  })
  
  
  generateScenarioTable<- reactive({
    input$inRegions
    
    lEB <- input$inEB
    lEQ <- input$inEQ
    
    data <- sessionData$sdata
    
    # Update MS
    data <- subset(data, MS %in% generateUniqueMS(input$inRegions))
    
    if (lEQ != "All")
      data <- subset(data, equity == lEQ)
    if (lEB != "All")
      data <- subset(data, ebike == lEB)
    
    data[is.na(data)] <- 0
    data <- arrange(data, MS)
    
    scdata <<- data
  })
  
  reactiveFunction <- reactive({
    
    lEB <- input$inEB
    lEQ <- input$inEQ
    
  })
  
  genericPlot <- function(var){
    
    h1 <- Highcharts$new()
    h1$chart(type = "spline")
    # types of charts: http://api.highcharts.com/highcharts#plotOptions
    h1$yAxis(title = list(text = var))
    
    # Read updated MS according to the selected region
    h1$xAxis(categories = append("Baseline", paste0((sort(unique(generateUniqueMS(input$inRegions)), decreasing = F) * 100), "%")), title = list(text = '% of Regular Cyclists'))
    
    
    if (input$inEB != "All" & input$inEQ != "All"){
      sub1 <- subset(scdata, ebike == as.numeric(input$inEB) & equity == as.numeric(input$inEQ))
      h1$series(data = append(sessionData$baselineSummary[[var]], sub1[[var]]), name = getSeriesName(input$inEQ, input$inEB ))
      
    }
    
    if (input$inEB == "All" & input$inEQ != "All"){
      sub1 <- subset(scdata, ebike == 0 & equity == as.numeric(input$inEQ))
      h1$series(data = append(sessionData$baselineSummary[[var]], sub1[[var]]), name = getSeriesName(input$inEQ, 0 ))
      sub1 <- subset(scdata, ebike == 1 & equity == as.numeric(input$inEQ))
      h1$series(data = append(sessionData$baselineSummary[[var]], sub1[[var]]), name = getSeriesName(input$inEQ, 1 ))
      
    }
    
    
    if (input$inEB != "All" & input$inEQ == "All"){
      sub1 <- subset(scdata, equity == 0 & ebike == as.numeric(input$inEB))
      h1$series(data = append(sessionData$baselineSummary[[var]], sub1[[var]]), name = getSeriesName(0,  input$inEB ))
      sub1 <- subset(scdata, equity == 1 & ebike == as.numeric(input$inEB))
      h1$series(data = append(sessionData$baselineSummary[[var]], sub1[[var]]), name = getSeriesName(1, input$inEB ))
      
    }
    
    if (input$inEB == "All" & input$inEQ == "All"){
      sub1 <- subset(scdata, ebike == 0 & equity == 0)
      h1$series(data = append(sessionData$baselineSummary[[var]], sub1[[var]]), name = getSeriesName(0, 0 ))
      sub1 <- subset(scdata, ebike == 0 & equity == 1)
      h1$series(data = append(sessionData$baselineSummary[[var]], sub1[[var]]), name = getSeriesName(1, 0 ))
      sub1 <- subset(scdata, ebike == 1 & equity == 0)
      h1$series(data = append(sessionData$baselineSummary[[var]], sub1[[var]]), name = getSeriesName(0, 1 ))
      sub1 <- subset(scdata, ebike == 1 & equity == 1)
      h1$series(data = append(sessionData$baselineSummary[[var]], sub1[[var]]), name = getSeriesName(1, 1 ))
      
    }
    
    h1$exporting(enabled = T, 
                 sourceWidth = 1920,
                 sourceWidth = 1080,
                 scale = 16)
    return(h1)
  }
  
  output$plotCycPercent <- renderChart({
    generateScenarioTable()
    h <- genericPlot("% Cyclists in the Total Population")
    h$title(text = "% Cyclists in the Total Population")
    h$set(dom = "plotCycPercent")
    return (h)
  })
  
  output$plotGenericVariable <- renderChart({
    generateScenarioTable()
    #retrieveVariableName()
    if (input$varname == "Car Miles Per person (per week)"){
      h <- genericPlot(input$CMVarName)
      h$title(text = input$CMVarName)
      
    } else if (input$varname == "Years of Life Lost (YLL)"){
      h <- genericPlot(input$HVarName)
      h$title(text = input$HVarName)
      
    } else if (input$varname == "Travel Marginal METs Per Person (per week)"){
      h <- genericPlot("Marginal METs Per Person (per week)")
      h$title(text = input$varname)
      
    } else {
      h <- genericPlot(input$varname)
      h$title(text = input$varname)
      
    }
    h$set(dom = "plotGenericVariable")
    return (h)
  })
  
  
  output$plotBDMode <- renderChart({
    generateBDScenarioTable()
    extended_title <- ""
    firstColData = NULL
    secondColData = NULL
    
    # Check in which form chart should be presented
    # "Scenario versus Baseline" = "comp",
    # "Sub-population versus total population" = "sep"
    
    if (input$flipG == 'sep'){
      
      # Keep the data separated
      # scMETdata and scFilteredMETdata
      
      firstColData = msScenario
      secondColData = tdScenario
      
      extended_title <- paste0("Scenario [", nameOfTheSelectedRegion, "] - Mode Share")
      
      firstColName <- "Scenario (Total Population)" # "Scenario (Total Population)"
      secondColName <- "Scenario (Sub-population)"
      
      # set proper subtitle
      
      filtered_title <- getTripsFilteredTitle("", secondColData)
      
    }else{
      
      # check if comparision with alternative region is selected
      
      if (input$inRegionSwitch == "Region"){
        
        firstColData = tdScenario
        secondColData = tdAltRegionScenario
        
        extended_title <- paste0("Sub-population [", nameOfTheSelectedRegion, "] - Mode Share")
        
        firstColName <- paste0("Scenario [", nameOfTheSelectedRegion, "] (Sub-population)")
        secondColName <- paste0("Scenario - alternative Region [", scenarioAltRegion, "] (Sub-population)") # "Scenario (Total Population)"
        
        # set proper subtitle
        
        filtered_title <- getTripsFilteredTitle(paste0("Scenario [", nameOfTheSelectedRegion, "]:"), firstColData, paste0(", Scenario - alternative Region [", scenarioAltRegion, "]: "), secondColData)
        
      } else {
        
        # Keep the data mixed
        firstColData = tdBaseline
        secondColData = tdScenario
        
        extended_title <- paste0("Sub-population [", nameOfTheSelectedRegion, "] - Mode Share")
        
        firstColName <- "Baseline (Sub-population)"
        secondColName <- "Scenario (Sub-population)"
        
        # set proper subtitle
        
        filtered_title <- getTripsFilteredTitle("", firstColData)
        
      }
      
      # if no filter is selected -> Total-Pop
      
      if (input$inGAG == "All" && input$inGGender == 3 && input$inGEthnicity == "All" && input$inGSES == "All" && input$inGTP == "All"){
        extended_title <- paste0("Total Population [", nameOfTheSelectedRegion, "] - Mode Share")
        # Replace sub-population with total population for both first and second column names
        firstColName <- gsub("Sub-population", "Total Population", firstColName)
        secondColName <- gsub("Sub-population", "Total Population", secondColName)
        
      }
    }
    
    
    h1 <- Highcharts$new()
    h1$chart(type = "column", style = list(fontFamily = 'Arial, sans-serif',
                                           fontSize = '12px'))
    h1$plotOptions(column=list(animation=FALSE))
    
    h1$title(text = extended_title)

    h1$series(data = firstColData$freq, name = firstColName)
    h1$series(data = secondColData$freq, name = secondColName)
    
    h1$xAxis(categories = tp_mode$mode)
    h1$yAxis(title = list(text = 'Percentage of Trips'))
    
    if (sum(firstColData$freq, na.rm = T) <= 10){
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"),
                  style = list(fontFamily = 'Arial, sans-serif',
                               fontSize = '14px',
                               fontWeight = 'bold',
                               color = "#f00"))
    }else{
      h1$subtitle(text= filtered_title)
    }
    
    h1$tooltip(valueSuffix= '%')
    h1$legend(useHTML=TRUE)
    
    h1$set(dom = "plotBDMode")
    h1$exporting(enabled = T,
                 chartOptions = list(
                    legend = list(
                      x = -30,
                      itemDistance = 80,
                      itemMarginBottom = 5
                    )
                  ))
    return (h1)
  })
  
  
  output$plotBDSCMode <- renderChart({
    generateBDScenarioTable()
    if (input$flipG == 'sep'){
      
      # check if comparision with alternative region is selected
      
      if (input$inRegionSwitch == "Region"){
        
        firstColData = msAltRegionScenario
        secondColData = tdAltRegionScenario
        
        extended_title <- paste0("Scenario - alternative Region [", scenarioAltRegion, "] - Mode Share")
        
        firstColName <- "Scenario - alternative Region (Total Population)" 
        secondColName <- "Scenario - alternative Region (Sub-population)"
        
        # set proper subtitle
        
        filtered_title <- getTripsFilteredTitle("", secondColData)
        
      } else {
        
        # Keep the data separated
        # scMETdata and scFilteredMETdata
        firstColData = msBaseline # msScenario
        secondColData = tdBaseline
        
        extended_title <- paste0("Baseline [", nameOfTheSelectedRegion, "] - Mode Share")
        
        firstColName <- "Baseline (Total Population)" # "Scenario (Total Population)"
        secondColName <- "Baseline (Sub-population)"
        
        # set proper subtitle
        
        filtered_title <- getTripsFilteredTitle("", secondColData)
        
      }
      
    }else{
      
      # check if comparision with alternative region is selected
      
      if (input$inRegionSwitch == "Region"){
        
        firstColData = msScenario
        secondColData = msAltRegionScenario
        
        extended_title <- paste0("Total Population [", nameOfTheSelectedRegion, "] - Mode Share")
        
        firstColName <- paste0("Scenario [", nameOfTheSelectedRegion, "] (Total Population)")
        secondColName <- paste0("Scenario - alternative Region [", scenarioAltRegion, "] (Total Population)") 
        
        filtered_title <- getTripsFilteredTitle(paste0("Scenario [", nameOfTheSelectedRegion, "]:"), firstColData, paste0(", Scenario - alternative Region [", scenarioAltRegion, "]:"), secondColData, titlePrefix = "Total Size (trips): ", showingTotal = TRUE)
        
      } else {
        
        firstColData = msBaseline
        secondColData = msScenario
        
        extended_title <- paste0("Total Population [", nameOfTheSelectedRegion, "] - Mode Share")
        
        firstColName <- "Baseline (Total Population)"
        secondColName <- "Scenario (Total Population)"
        
        filtered_title <- getTripsFilteredTitle("", secondColData, titlePrefix = "Total Size (trips): ", showingTotal = TRUE)
        
      }
    }
    
    h1 <- Highcharts$new()
    h1$chart(type = "column", style = list(fontFamily = 'Arial, sans-serif',
                                           fontSize = '12px'))
    h1$plotOptions(column=list(animation=FALSE))
    
    h1$title(text = extended_title)

    h1$series(data = firstColData$freq, name = firstColName)
    h1$series(data = secondColData$freq, name = secondColName)
    
    h1$xAxis(categories = tp_mode$mode)
    h1$yAxis(title = list(text = 'Percentage of Trips'))
    
    if (sum(firstColData$freq, na.rm = T) <= 10){
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"),
                  style = list(fontFamily = 'Arial, sans-serif',
                               fontSize = '14px',
                               fontWeight = 'bold',
                               color = "#f00"))
    }else{
      h1$subtitle(text= filtered_title)
    }
    
    h1$tooltip(valueSuffix= '%')
    
    h1$set(dom = "plotBDSCMode")
    h1$exporting(enabled = T,
                 chartOptions = list(
                    legend = list(
                      x = -30,
                      itemDistance = 80,
                      itemMarginBottom = 5
                    )
                  ))
    return (h1)
    
  })
  
  getModeTimeFilteredTitle <- function(firstDataTitle, firstData, secondDataTitle = NULL, secondData = NULL, titlePrefix = NULL, showingTotal = FALSE){
    
    npeople <- unique(firstData$total_population)
    
    filtered_title <- ""
    
    secondDataNPeople <- ""
    secondDataTitleOutput <- ""
    secondExtraSep <- ""
    
    if(!is.null(secondDataTitle)){
      
      secondDataTitleOutput <- secondDataTitle
      secondExtraSep <- " "
      
    }
    
    if(!is.null(secondData)){
      
      secondDataNPeople <- unique(secondData$total_population)
      secondExtraSep <- " "
      
    }
    
    if(!is.null(titlePrefix)){
      titlePrefixOutput <- titlePrefix
    } else {
      titlePrefixOutput <- "Sample Size: "
    }
    
    if (input$inGAG != "All" || input$inGGender != 3 || input$inGEthnicity != "All" || input$inGSES != "All" || input$inGTP != "All" ){
      displayGender <- "All"
      if (input$inGGender == 1){
        displayGender <- "Male"
      }else if (input$inGGender == 2){
        displayGender <- "Female"
      }
      
      displayEthnicity <- "All"
      if (input$inGEthnicity == 1){
        displayEthnicity <- "White"
      }else if (input$inGEthnicity == 2){
        displayEthnicity <- "Non-White"
      }
      
      displaySES <- "All"
      if (input$inGSES == 1){
        displaySES <- "Managerial and professional occupations"
      }else if (input$inGSES == 2){
        displaySES <- "Intermediate occupations and small employers"
      }else if (input$inGSES == 3){
        displaySES <- "Routine and manual occupations"
      }else if (input$inGSES == 4){
        displaySES <- "Never worked and long-term unemployed"
      }else if (input$inGSES == 5){
        displaySES <- "Not classified (including students)"
      }
      
      
      displayPurpose <- 'All'
      if (input$inGTP == 1){
        displayPurpose <- "Commuting"
      }else if (input$inGTP == 2){
        displayPurpose <- "Business"
      }else if (input$inGTP == 3){
        displayPurpose <- "Education / escort education"
      }else if (input$inGTP == 4){
        displayPurpose <- "Shopping"
      }else if (input$inGTP == 5){
        displayPurpose <- "Other escort"
      }else if (input$inGTP == 6){
        displayPurpose <- "Personal business"
      }else if (input$inGTP == 7){
        displayPurpose <- "Leisure"
      }
      
      if(showingTotal){
        
        filters_info <- ""
        
      } else {
        
        filters_info <- paste0(", Purpose: ", displayPurpose, ", Age Group: ", str_trim(input$inGAG), ", Gender: ", displayGender, ", Socio Economic Classification: ", displaySES, " and Ethnicity: ", displayEthnicity)
        
      }
      
      filtered_title <- paste0(titlePrefixOutput,
                              firstDataTitle,
                              ' ',
                              npeople,
                              secondDataTitleOutput,
                              secondExtraSep,
                              secondDataNPeople,
                              filters_info)
      filtered_title
    }else
      filtered_title
  }
  
  output$plotTTFilteredMode <- renderChart({
    input$inRegions
    
    h1 <- Highcharts$new()
    h1$chart(type = "column",
             style = list(fontFamily = 'Arial, sans-serif', 
                          fontSize = '12px'))
    h1$yAxis(title = list(text = 'Percentage %'))
    
    h1$plotOptions(column=list(animation=FALSE, 
                               dataLabels = list(enabled = T, 
                                                 crop = FALSE, 
                                                 rotation = -90, 
                                                 align = 'center', 
                                                 color = '#FFFFFF', 
                                                 width = 100,
                                                 x = 0, 
                                                 y = 10, 
                                                 style = list(fontSize = '9px'),
                                                 formatter = format_function)))
    
    # construct scenario name + filename
    
    scenarioName <- paste(paste0("MS", input$inGMS), paste0("ebik", input$inGEB), paste0("eq", input$inGEQ), sep="_")
    
    scenarioFilename <- paste0(scenarioName, '.rds')
    
    mname <- c("Walk", "Car", "Public Transport")
    
    if (input$flipTTPlot == "histogram"){
      
      # read histogram data for selected scenario
      
      chartData <- readRDS(paste0(tripTotalTimePrecalculated, "filtered/", input$inRegions, "/histogram/", scenarioFilename))
      
      # filter data
      
      chartData <- subset(chartData, agegroup == input$inGAG & gender == input$inGGender & ses == input$inGSES & ethnicity == input$inGEthnicity
                          & purpose == input$inGTP)
      
      # TODO: na check?
      
      if (!is.null(chartData) && nrow(chartData) > 0){
        
        h1$title(text = paste0("Sub-population [", nameOfTheSelectedRegion, "] - Histogram of Relative Changes in Trip Durations"))
        
        if (input$inGAG == 'All' & input$inGGender == 3 & input$inGSES == "All" & input$inGEthnicity == "All" & input$inGTP == "All"){
           
          h1$title(text = paste0("Total population [", nameOfTheSelectedRegion, "] - Histogram of Relative Changes in Trip Durations"))
        }
        
        availableUmodes <- unique(chartData[["umode"]])
        
        # iterate over umodes
        
        for (i in 1:length(availableUmodes)){
          
          # add serie of umode data
          
          h1$series(data = as.numeric(unlist(chartData[chartData$umode == availableUmodes[i], c(scenarioName)])), name = mname[i]) #"Time Difference from Baseline (%)
          
        }
        
        # TODO: check below condition - previously scTripTimeTraveldata
        
        if (length(availableUmodes) == 1){
          
          h1$xAxis(categories = c("%") , labels = list(padding = 10,
                                                       style = list(fontSize = '10px',
                                                                    whiteSpace = 'nowrap'))
          )
          
        }else{
          
          h1$xAxis(categories = c(">=50% faster", ">=20 and <50% faster",">=0 and <20% faster",">0 and <=20% slower",">20 and <=50% slower",">50 and <=100% slower",">100% slower" ),
                   plotBands = list(
                     list(
                       from = -0.5,
                       to = 2.5,
                       color = 'rgba(21, 155, 12, .1)',
                       label = list(
                         text = "Faster"
                       )
                     ),
                     list(
                       from = 2.5,
                       to = 6.5,
                       color = 'rgba(135, 13, 13, .1)',
                       label = list(
                         text = "Slower"
                       )
                     )
                   ))
        }
        
        h1$tooltip(valueSuffix= '%')
        st <- getModeTimeFilteredTitle("", chartData)
        
        h1$subtitle(text = st)
        
      }else{
        h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size = 0)"),
                    style = list(fontFamily = 'Arial, sans-serif',
                                 fontSize = '14px',
                                 fontWeight = 'bold',
                                 color = "#f00"))
      }
      
    }else{
      
      # read other data for selected scenario
      
      chartData <- readRDS(paste0(tripTotalTimePrecalculated, "filtered/", input$inRegions, "/other/", scenarioFilename))
      
      # filter data
      
      chartData <- subset(chartData, agegroup == input$inGAG & gender == input$inGGender & ses == input$inGSES & ethnicity == input$inGEthnicity
                          & purpose == input$inGTP)
      
      # TODO: na check?
      
      if (!is.null(chartData) && nrow(chartData) > 0){
        
        h1$title(text = paste0("Sub-population [", nameOfTheSelectedRegion, "] - Proportion of Faster/Slower Trips"))
        
        if (input$inGAG == 'All' & input$inGGender == 3 & input$inGSES == "All" & input$inGEthnicity == "All" & input$inGTP == "All"){
          
          h1$title(text = paste0("Total population [", nameOfTheSelectedRegion, "] - Proportion of Faster/Slower Trips"))
        }
        
        availableUmodes <- unique(chartData[["umode"]])
        
        # iterate over umodes
        
        for (i in 1:length(availableUmodes)){
          
          # add serie of umode data
          
          h1$series(data = as.numeric(unlist(chartData[chartData$umode == availableUmodes[i], c(scenarioName)])), name = mname[i]) #"Time Difference from Baseline (%)
          
        }
        
        # TODO: check below condition - previously scTripTimeTraveldata
        
        if (length(availableUmodes) == 1){
          
          h1$xAxis(categories = c("%") , labels = list(padding = 40,
                                                       style = list(fontSize = '10px',
                                                                    whiteSpace = 'nowrap'))
          )
          
        }else{
          
          h1$xAxis(categories = c("Faster", "Slower"))
        }
        
        h1$tooltip(valueSuffix= '%')
        st <- getModeTimeFilteredTitle("", chartData)
        h1$subtitle(text = st)
        
      }else{
        h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size = 0)"),
                    style = list(fontFamily = 'Arial, sans-serif',
                                 fontSize = '14px',
                                 fontWeight = 'bold',
                                 color = "#f00"))
      }
      
    }
    
    h1$set(dom = 'plotTTFilteredMode')
    h1$exporting(enabled = T,
                 chartOptions = list(chart = list(style = list(fontFamily = 'Arial, sans-serif')),
                                     xAxis = list(labels = list(style = list(fontSize = '8px')))))
    return(h1)
  })
  
  output$plotTTTotalMode <- renderChart({
    input$inRegions
    
    h1 <- Highcharts$new()
    h1$chart(type = "column", style = list(fontFamily = 'Arial, sans-serif',
                                           fontSize = '12px'))
    h1$yAxis(title = list(text = 'Percentage %'))
    
    h1$plotOptions(column=list(animation=FALSE, 
                               dataLabels = list(enabled = T, 
                                                 crop = FALSE, 
                                                 rotation = -90, 
                                                 align = 'center', 
                                                 color = '#FFFFFF', 
                                                 width = 100,
                                                 x = 0, 
                                                 y = 10, 
                                                 style = list(fontSize = '9px'),
                                                 formatter = format_function)))
    
    # construct scenario name + filename
    
    scenarioName <- paste(paste0("MS", input$inGMS), paste0("ebik", input$inGEB), paste0("eq", input$inGEQ), sep="_")
    
    scenarioFilename <- paste0(scenarioName, '.rds')
    
    mname <- c("Walk", "Car", "Public Transport")
    
    if (input$flipTTPlot == "histogram"){
      
      # read histogram data for selected scenario
      
      chartData <- readRDS(paste0(tripTotalTimePrecalculated, "full/", input$inRegions, "/histogram/", scenarioFilename))
      
      # TODO: na check?
      
      if (!is.null(chartData) && nrow(chartData) > 0){
        
        h1$title(text = paste0("Total population [", nameOfTheSelectedRegion, "] - Histogram of Relative Changes in Trip Durations"))
        
        availableUmodes <- unique(chartData[["umode"]])
        
        # iterate over umodes
        
        for (i in 1:length(availableUmodes)){
          
          # add serie of umode data
          
          h1$series(data = as.numeric(unlist(chartData[chartData$umode == availableUmodes[i], c(scenarioName)])), name = mname[i]) #"Time Difference from Baseline (%)
          
        }
        
        # TODO: check below condition - previously scTripTimeTraveldata
        
        if (length(availableUmodes) == 1){
 
          h1$xAxis(categories = c("%") , labels = list(padding = 10,
                                                       style = list(fontSize = '10px',
                                                                    whiteSpace = 'nowrap'))
          )
          
        }else{
          
          h1$xAxis(categories = c(">=50% faster", ">=20 and <50% faster",">=0 and <20% faster",">0 and <=20% slower",">20 and <=50% slower",">50 and <=100% slower",">100% slower" ),
                   plotBands = list(
                     list(
                       from = -0.5,
                       to = 2.5,
                       color = 'rgba(21, 155, 12, .1)',
                       label = list(
                         text = "Faster"
                       )
                     ),
                     list(
                       from = 2.5,
                       to = 6.5,
                       color = 'rgba(135, 13, 13, .1)',
                       label = list(
                         text = "Slower"
                       )
                     )
                   ))
        }
        
        h1$tooltip(valueSuffix= '%')
        st <- getModeTimeFilteredTitle("", chartData, titlePrefix="Total Size: ", showingTotal = TRUE)
        
        h1$subtitle(text = st)
        
      }else{
        h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size = 0)"),
                    style = list(fontFamily = 'Arial, sans-serif',
                                 fontSize = '14px',
                                 fontWeight = 'bold',
                                 color = "#f00"))
      }
      
    }else{
      
      # read other data for selected scenario
      
      chartData <- readRDS(paste0(tripTotalTimePrecalculated, "full/", input$inRegions, "/other/", scenarioFilename))
      
      # TODO: na check?
      
      if (!is.null(chartData) && nrow(chartData) > 0){
        
        h1$title(text = paste0("Total population [", nameOfTheSelectedRegion, "] - Proportion of Faster/Slower Trips"))
        
        availableUmodes <- unique(chartData[["umode"]])
        
        # iterate over umodes
        
        for (i in 1:length(availableUmodes)){
          
          # add serie of umode data
          
          h1$series(data = as.numeric(unlist(chartData[chartData$umode == availableUmodes[i], c(scenarioName)])), name = mname[i]) #"Time Difference from Baseline (%)
          
        }
        
        # TODO: check below condition - previously scTripTimeTraveldata
        
        if (length(availableUmodes) == 1){
          
          h1$xAxis(categories = c("%") , labels = list(padding = 40,
                                                       style = list(fontSize = '10px',
                                                                    whiteSpace = 'nowrap'))
          )
          
        }else{
          
          h1$xAxis(categories = c("Faster", "Slower"))
        }
        
        h1$tooltip(valueSuffix= '%')
        st <- getModeTimeFilteredTitle("", chartData, titlePrefix="Total Size: ", showingTotal = TRUE)
        
        h1$subtitle(text = st)
        
      }else{
        h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size = 0)"),
                    style = list(fontFamily = 'Arial, sans-serif',
                                 fontSize = '14px',
                                 fontWeight = 'bold',
                                 color = "#f00"))
      }
      
    }
    

    h1$set(dom = 'plotTTTotalMode')
    h1$exporting(enabled = T,
                 chartOptions = list(chart = list(style = list(fontFamily = 'Arial, sans-serif')),
                                     xAxis = list(labels = list(style = list(fontSize = '8px')))))
    return(h1)
  })
  
  output$plotBDFasterTrips <- renderChart({
    generateFasterTripsTable()
    if (!is.null(ftdata)){
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      h1$plotOptions(column = list(animation=FALSE))
      
      filtered_title <- getFilteredBDTitle("FT")
      extended_title <- paste("Faster Trips: Modal Split of Trips Switched to Cycling in the Selected Scenario")
      h1$title(text = extended_title)
      h1$series(data = ftdata$case, name = "Scenario")
      #       cat("ftdata$case : ", ftdata$modefinal, "\n")
      h1$subtitle(text = paste("Scenario: ", filtered_title), style = list(font = 'bold 12px "Trebuchet MS", Verdana, sans-serif'))
      
      lab_xAxis <- tp_mode$mode[match(ftdata$modefinal, tp_mode$code)]
      
      h1$xAxis(categories = lab_xAxis)#c("Walk", "Car Passenger", "Bus", "Train", "Other", "Bicycle"))
      h1$yAxis(title = list(text = 'Percentage'))
      
      h1$tooltip(valueSuffix= '%')
      
      h1$set(dom = "plotBDFasterTrips")
      h1$exporting(enabled = T)
      return (h1)
    }
  })
  
  output$plotBDSlowerTrips <- renderChart({
    generateSlowerTripsTable()
    if (!is.null(swdata)){
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      h1$plotOptions(column=list(animation=FALSE))
      
      filtered_title <- getFilteredBDTitle("FT")
      extended_title <- paste("Slower Trips:  Modal Split of Trips Switched to Cycling in the Selected Scenario")
      h1$title(text = extended_title)
      h1$series(data = swdata$case, name = "Scenario")
      h1$subtitle(text = paste("Scenario: ", filtered_title), style = list(font = 'bold 12px "Trebuchet MS", Verdana, sans-serif'))
      
      lab_xAxis <- tp_mode$mode[match(swdata$modefinal, tp_mode$code)]
      
      h1$xAxis(categories = lab_xAxis)
      h1$yAxis(title = list(text = 'Percentage'))
      
      h1$tooltip(valueSuffix= '%')
      
      h1$set(dom = "plotBDSlowerTrips")
      h1$exporting(enabled = T)
      return (h1)
    }
  })
  
  #   output$plotCarTripsCycled <- renderChart ({
  #     filterCarMilesData()
  #     h1 <- Highcharts$new()
  #     h1$chart(type = "column")
  #     
  #     if (!is.null(scCarMilesData) && !is.null(scCarMilesFilteredData)){
  #       h1$plotOptions(column=list(animation=FALSE))
  #       
  #       h1$title(text = "Car Miles:  Histogram of Car Miles in the Selected Scenario")
  #       
  #       #       data.decile <- cut2(scCarMilesFilteredData$scenario, g = 10)
  #       #       h1$series(data = as.data.frame(table(data.decile))$Freq, name = "Car Trips (Miles)")
  #       #       h1$xAxis(categories = c(1:10), title = "Decile")
  #       #       h1$yAxis(title = list(text = 'Miles'))
  #       
  #       #       dhit <- hist(scCarMilesFilteredData$scenario)
  #       #       
  #       #       data <- data.frame(breaks = dhit$breaks[-1], counts = dhit$counts, 0)
  #       #       data$freq <- round(data$counts / sum(data$counts) * 100, digits = 1)
  #       #       #       data <- subset(data, freq >= 0.1)
  #       #       data <- subset(data, breaks != 0)
  #       #       
  #       #       h1$series(data =  data$freq, name = "Car Miles")
  #       #       
  #       #       h1$xAxis(categories = data$breaks)
  #       #       
  #       #       h1$yAxis(title = list(text = 'Percentage %'))
  #       #       h1$tooltip(valueSuffix= '%')
  #       
  #       bc <- as.data.frame(table (cut (scCarMilesFilteredData$scenario, breaks = c(seq(0,300, 50), max(scCarMilesFilteredData$scenario)))))
  #       bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
  #       bc1max <- max(bc$Freq, na.rm = T)
  #       
  #       h1$xAxis(categories = as.list(append(c(seq(0,300, 50))[-1], "300+")), title = list(text = 'Car Miles'))
  #       h1$yAxis(title = list(text = 'Percentage %'))
  #       
  #       h1$series(data = bc$Freq, name = "Car Miles")
  #       h1$tooltip(valueSuffix= '%')
  #       
  #       
  #     }
  #     h1$set(dom = "plotCarTripsCycled")
  #     h1$exporting(enabled = T)
  #     return (h1)
  #     
  #   })
  
  
  output$plotFilteredMilesCycled <- renderChart ({
    filterMilesCycledData()
    
    firstColData <- NULL
    secondColData <- NULL
    
    subtitle <- ""
    extended_title <- ""
    
    if (input$flipG == 'sep'){
      
      # check if comparision with alternative region is selected
      
      if (input$inRegionSwitch == "Region"){
        
        # Keep the data separated
        firstColData = scMilesCycledDataAltRegFull
        secondColData = scMilesCycledDataAltRegFiltered
        
        firstColName <- "Scenario - alternative Region (Total Population)"
        secondColName <- "Scenario - alternative Region (Sub-population)"
        
        subtitle <- getMilesCycledFilteredTitle("", secondColData)
        extended_title <- paste0("Scenario - alternative Region [", scenarioAltRegion, "] - Total Miles Cycled per Cyclist per week")
        
      } else {
        
        # Keep the data separated
        firstColData = blMilesCycledData
        secondColData = blMilesCycledFilteredData
        
        firstColName <- "Baseline (Total Population)"
        secondColName <- "Baseline (Sub-population)"
        
        subtitle <- getMilesCycledFilteredTitle("", secondColData)
        extended_title <- paste0("Baseline [", nameOfTheSelectedRegion, "] - Total Miles Cycled per Cyclist per week")
        
      }
      
    }else{
      
      # check if comparision with alternative region is selected
      
      if (input$inRegionSwitch == "Region"){
        
        # Keep the data mixed
        firstColData = scMilesCycledData
        secondColData = scMilesCycledDataAltRegFull
        
        firstColName <- paste0("Scenario [", nameOfTheSelectedRegion, "] (Total Population)")
        secondColName <- paste0("Scenario - alternative Region [", scenarioAltRegion, "] (Total Population)")
        
        extended_title <- paste0("Total Population [", nameOfTheSelectedRegion, "] - Total Miles Cycled per Cyclist per week")
        subtitle <- getMilesCycledFilteredTitle(paste0("Scenario [", nameOfTheSelectedRegion, "]:"), firstColData, paste0(", Scenario - alternative Region [", scenarioAltRegion, "]:"), secondColData, titlePrefix = "Total Size: ", showingTotal = TRUE)
        
      } else {
        
        # Keep the data mixed
        firstColData = blMilesCycledData
        secondColData = scMilesCycledData
        
        firstColName <- "Baseline (Total Population)"
        secondColName <- "Scenario (Total Population)"
        
        subtitle <- getMilesCycledFilteredTitle("", secondColData, titlePrefix = "Total Size: ", showingTotal = TRUE)
        extended_title <- paste0("Total Population [", nameOfTheSelectedRegion, "] - Total Miles Cycled per Cyclist per week")
        
      }
      
    }
    
    h1 <- Highcharts$new()
    h1$chart(type = "column", style = list(fontFamily = 'Arial, sans-serif',
                                           fontSize = '12px'))
    if (max(firstColData$data) > 0 && max(secondColData$data) > 0){
      bc <- as.data.frame(table (cut (firstColData$data, breaks = c(-1, 0, 2, 5, 10, 20, 40, 60, Inf))))
      
      if (input$inGTotOrCyc == 'cyc'){
        bc$Freq <- round(bc$Freq  / sum(bc$Freq[-1]) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage of Cyclists'))
      }
      else{
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage of the total population'))
      }
      
      #h1$xAxis(categories = bc$Var1[-1])#c(2, 5, 10, 20, 40, 60, " > 60"))
      
      h1$series(data = bc$Freq[-1], name = firstColName)
      bc <- NULL
      bc <- as.data.frame(table (cut (secondColData$data, breaks = c(-1, 0, 2, 5, 10, 20, 40, 60, Inf))))
      if (input$inGTotOrCyc == 'cyc'){
        bc$Freq <- round(bc$Freq  / sum(bc$Freq[-1]) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage of Cyclists'))
      }
      else{
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage of the total population'))
      }
      h1$series(data = bc$Freq[-1], name = secondColName)
      
      h1$xAxis(categories = c("> 0 and <= 2", "> 2 and <= 5", "> 5 and <= 10", "> 10 and <= 20","> 20 and <= 40", "> 40 and <= 60", "> 60"))
      
      # add extra subtitle if denominator is "Total Population"
      
      if (input$inGTotOrCyc == 'pop'){
        
        extraSubtitle <- "Proportions calculated using the total population as a denominator, but the bar for those doing zero cycling is not shown."
        
        subtitle <- ifelse(nchar(subtitle) == 0, extraSubtitle, paste0(subtitle, "<br>", extraSubtitle))
        
      }
      
      h1$subtitle(text = subtitle)
      h1$tooltip(formatter = "#! function() {  return this.series.name +'<br/>' + 'Value: <b>' + this.y + '%'; } !#")
      
    }else{
      
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), 
                  style = list(fontFamily = 'Arial, sans-serif',
                               fontSize = '14px',
                               fontWeight = 'bold',
                               color = "#f00"))
      
    }
    
    h1$title(text = extended_title)
    
    h1$set(dom = "plotFilteredMilesCycled")
    h1$exporting(enabled = T,
                 chartOptions = list(
                   title = list(useHTML = FALSE),
                   legend = list(
                     itemDistance = 80,
                     itemMarginBottom = 5
                   ),
                   xAxis = list(
                     title = list(
                       y = 10
                     ),
                     labels = list(
                       style = list(
                         fontSize = '10px'
                       )
                     )
                   )
                 ))
    return (h1)
  })
  
  
  output$plotMilesCycled <- renderChart ({
    filterMilesCycledData()
    
    firstColData <- NULL
    secondColData <- NULL
    
    subtitle <- ""
    extended_title <- ""
    
    # Check in which form chart should be presented
    # "Scenario versus Baseline" = "comp",
    # "Sub-population versus total population" = "sep"
    
    if (input$flipG == 'sep'){
      # Keep the data separated
      firstColData = scMilesCycledData
      secondColData = scMilesCycledFilteredData
      
      firstColName <- "Scenario (Total Population)"
      secondColName <- "Scenario (Sub-population)"
      
      extended_title <- paste0("Scenario [", nameOfTheSelectedRegion, "] - Total Miles Cycled per Cyclist per week")
      subtitle <- getMilesCycledFilteredTitle("", secondColData)
      
    }else{
      
      # check if comparision with alternative region is selected
      
      if (input$inRegionSwitch == "Region"){
        
        # Keep the data mixed
        firstColData = scMilesCycledFilteredData
        secondColData = scMilesCycledDataAltRegFiltered
        
        firstColName <- paste0("Scenario [", nameOfTheSelectedRegion, "] (Sub-population)")
        secondColName <- paste0("Scenario - alternative Region [", scenarioAltRegion, "] (Sub-population)")
        
        extended_title <- paste0("Sub-population [", nameOfTheSelectedRegion, "] - Total Miles Cycled per Cyclist per week")
        subtitle <- getMilesCycledFilteredTitle(paste0("Scenario [", nameOfTheSelectedRegion, "]:"), firstColData, paste0(", Scenario - alternative Region [", scenarioAltRegion, "]: "), secondColData)
        
      } else {
      
        # Keep the data mixed
        firstColData = blMilesCycledFilteredData
        secondColData = scMilesCycledFilteredData
        
        firstColName <- "Baseline (Sub-population)"
        secondColName <- "Scenario (Sub-population)"
        
        extended_title <- paste0("Sub-population [", nameOfTheSelectedRegion, "] - Total Miles Cycled per Cyclist per week")
        subtitle <- getMilesCycledFilteredTitle("", secondColData)
      
      }
      
      # if no filter is selected
      
      if (input$inGAG == "All" && input$inGGender == 3 && input$inGEthnicity == "All" && input$inGSES == "All"){
          # && input$inGTP == "All"){
      
        extended_title <- paste0("Total Population [", nameOfTheSelectedRegion, "] - Total Miles Cycled per Cyclist per week")
        # Replace sub-population with total population for both first and second column names
        firstColName <- gsub("Sub-population", "Total Population", firstColName)
        secondColName <- gsub("Sub-population", "Total Population", secondColName)
        
      
      }
    }
    
    h1 <- Highcharts$new()
    h1$chart(type = "column", style = list(fontFamily = 'Arial, sans-serif',
                                           fontSize = '12px'))
    
    bc <- NULL
    if (max(firstColData$data) > 0 && max(secondColData$data) > 0){
      
      bc <- as.data.frame(table (cut (firstColData$data, breaks = c(-1, 0, 2, 5, 10, 20, 40, 60, Inf))))
      if (input$inGTotOrCyc == 'cyc'){
        bc$Freq <- round(bc$Freq  / sum(bc$Freq[-1]) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage of Cyclists'))
      }
      else{
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage of the total population'))
      }
      
      h1$series(data = bc$Freq[-1], name = firstColName)
      bc <- as.data.frame(table (cut (secondColData$data, breaks = c(-1, 0, 2, 5, 10, 20, 40, 60, Inf))))
      if (input$inGTotOrCyc == 'cyc'){
        bc$Freq <- round(bc$Freq  / sum(bc$Freq[-1]) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage of Cyclists'))
      }
      else{
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage of the total population'))
      }
      
      h1$series(data = bc$Freq[-1], name = secondColName)
      
      # add extra subtitle if denominator is "Total Population"
      
      if (input$inGTotOrCyc == 'pop'){
        
        extraSubtitle <- "Proportions calculated using the total population as a denominator, but the bar for those doing zero cycling is not shown."
      
        subtitle <- ifelse(nchar(subtitle) == 0, extraSubtitle, paste0(subtitle, "<br>", extraSubtitle))
        
      }
      
      h1$subtitle(text = subtitle)
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), 
                  style = list(fontFamily = 'Arial, sans-serif',
                               fontSize = '14px',
                               fontWeight = 'bold',
                               color = "#f00"))
    }
    
    h1$title(text = extended_title)
    
    h1$xAxis(categories = c("> 0 and <= 2", "> 2 and <= 5", "> 5 and <= 10", "> 10 and <= 20","> 20 and <= 40", "> 40 and <= 60", "> 60"))
    h1$set(dom = "plotMilesCycled")
    # h1$yAxis(title = list(text = 'Percentage %'))
    h1$tooltip(formatter = "#! function() {  return this.series.name +'<br/>' + 'Value: <b>' + this.y + '%'; } !#")
    
    h1$exporting(enabled = T,
                 chartOptions = list(
                   title = list(useHTML = FALSE),
                   legend = list(
                     itemDistance = 80,
                     itemMarginBottom = 5
                   ),
                   xAxis = list(
                     title = list(
                       y = 10
                     ),
                     labels = list(
                       style = list(
                         fontSize = '10px'
                       )
                     )
                   )
                 ))
    return (h1)
  })
  
  filterMilesCycledData <- reactive ({
    input$inRegions
    input$inRegionSelected
    
    data <- sessionData$milesCycled
    
    if (input$inGAG != 'All'){
      data <- subset(data, age_group == input$inGAG)
    }
    if (input$inGGender != 3)
      data <- subset(data, Sex_B01ID %in% input$inGGender)
    
    if (input$inGSES != "All"){
      data <- subset(data, NSSec_B03ID %in% input$inGSES)
    }
    
    if (input$inGEthnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID %in% input$inGEthnicity)
    }
    
    # if (input$inGTP != "All"){
    #   data <- subset(data, TripPurpose_B04ID  == input$inGTP)
    # }
    #data[is.na(data)] <- 0
    
    
    columnName <- paste(paste("MS", input$inGMS,sep = ""),  paste("ebik", input$inGEB,sep = ""), 
                        paste("eq", input$inGEQ,sep = ""), sep="_")
    
    #data1 <- milesCycled[,c("ID", "age_group","Sex_B01ID","NSSec_B03ID","EthGroupTS_B02ID", "baseline_milesCycled", columnName)]
    
    data1 <- data.frame(sessionData$milesCycled[,columnName])
    
    data1$data <- data1[,1]
    data1[,1] <- NULL
    
    data2 <- data.frame(data[,columnName])
    
    data2$data <- data2[,1]
    data2[,1] <- NULL
    
    scMilesCycledData <<- data1
    scMilesCycledFilteredData <<- data2
    
    blMilesCycledData <<- data.frame(data = sessionData$milesCycled[,"baseline"])
    blMilesCycledFilteredData <<- data.frame(data = data[,"baseline"])
    
    # to save time only if comparision with alternative region is selected
    
    if(input$inRegionSwitch == 'Region'){
      
      # full data
      
      dataAltRegFull <- data.frame(data = sessionData$RegionmilesCycled[[columnName]])
      
      scMilesCycledDataAltRegFull <<- dataAltRegFull
      
      # filtered data
      
      dataAltRegFiltered <- sessionData$RegionmilesCycled
      
      if (input$inGAG != 'All'){
        dataAltRegFiltered <- subset(dataAltRegFiltered, age_group == input$inGAG)
      }
      if (input$inGGender != 3)
        dataAltRegFiltered <- subset(dataAltRegFiltered, Sex_B01ID %in% input$inGGender)
      
      if (input$inGSES != "All"){
        dataAltRegFiltered <- subset(dataAltRegFiltered, NSSec_B03ID %in% input$inGSES)
      }
      
      if (input$inGEthnicity != "All"){
        dataAltRegFiltered <- subset(dataAltRegFiltered, EthGroupTS_B02ID %in% input$inGEthnicity)
      }
      
      # if (input$inGTP != "All"){
      #   dataAltRegFiltered <- subset(dataAltRegFiltered, TripPurpose_B04ID == input$inGTP)
      # }
      
      
      
      dataAltRegFiltered <- data.frame(data = dataAltRegFiltered[[columnName]])
      
      scMilesCycledDataAltRegFiltered <<- dataAltRegFiltered
      
    }
  })
  
  
  
  output$plotFilteredCarMiles <- renderChart ({
    filterCarMilesData()
    
    firstColData <- NULL
    secondColData <- NULL
    
    subtitle <- ""
    extended_title <- ""
    
    # Check in which form chart should be presented
    # "Scenario versus Baseline" = "comp",
    # "Sub-population versus total population" = "sep"
    
    if (input$flipG == 'sep'){
      
      # Keep the data separated
      firstColData = scCarMilesData
      secondColData = scCarMilesFilteredData
      
      firstColName <- "Scenario (Total Population)"
      secondColName <- "Scenario (Sub-population)"
      
      extended_title <- paste0("Scenario [", nameOfTheSelectedRegion, "] - Car Miles per person per week")
      
      subtitle <- getCarMilesFilteredTitle("", secondColData)
      
    }else{
      
      # check if comparision with alternative region is selected
      
      if (input$inRegionSwitch == "Region"){
        
        # Keep the data mixed
        firstColData = scCarMilesFilteredData
        secondColData = scCarMilesDataAltRegFiltered
        
        firstColName <- paste0("Scenario [", nameOfTheSelectedRegion, "] (Sub-population)")
        secondColName <- paste0("Scenario - alternative Region [", scenarioAltRegion, "] (Sub-population)") # "Scenario (Total Population)"
        
        extended_title <- paste0("Sub-population [", nameOfTheSelectedRegion, "] - Car Miles per person per week")
        
        subtitle <- getCarMilesFilteredTitle(paste0("Scenario [", nameOfTheSelectedRegion, "]:"), firstColData, paste0(", Scenario - alternative Region [", scenarioAltRegion, "]: "), secondColData)
        
      } else {
        
        # Keep the data mixed
        firstColData = blCarMilesFilteredData
        secondColData = scCarMilesFilteredData
        
        firstColName <- "Baseline (Sub-population)"
        secondColName <- "Scenario (Sub-population)"
        
        extended_title <- paste0("Sub-population [", nameOfTheSelectedRegion, "] - Car Miles per person per week")
        
        subtitle <- getCarMilesFilteredTitle("", secondColData)
        
      }
      
      # if no filter is selected
      
      if (input$inGAG == "All" && input$inGGender == 3 && input$inGEthnicity == "All" && input$inGSES == "All"){#  && input$inGTP == "All" ){
      
        extended_title <- paste0("Total Population [", nameOfTheSelectedRegion, "] - Car Miles per person per week")
        # Replace sub-population with total population for both first and second column names
        firstColName <- gsub("Sub-population", "Total Population", firstColName)
        secondColName <- gsub("Sub-population", "Total Population", secondColName)
        
        
      }
      
    }
    
    h1 <- Highcharts$new()
    h1$chart(type = "column", style = list(fontFamily = 'Arial, sans-serif',
                                           fontSize = '12px'))
    
    h1$title(text = extended_title)
    
    if (max(firstColData$data) > 0 && max(secondColData$data) > 0){
      bc <- as.data.frame(table (cut (firstColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(firstColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      
      h1$series(data = bc$Freq, name = firstColName)
      bc <- NULL
      bc <- as.data.frame(table (cut (secondColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(secondColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      h1$series(data = bc$Freq, name = secondColName)
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"),
                  style = list(fontFamily = 'Arial, sans-serif',
                               fontSize = '14px',
                               fontWeight = 'bold',
                               color = "#f00"))
    }
    h1$xAxis(categories = c("0", "> 0 and <= 10", "> 10 and <= 20", "> 20 and <= 50", "> 50 and <= 100","> 100 and <= 200", "> 200"))
    h1$subtitle(text = subtitle)
    
    h1$set(dom = "plotFilteredCarMiles")
    h1$yAxis(title = list(text = 'Percentage of the total population'))
    h1$tooltip(formatter = "#! function() {  return this.series.name +'<br/>' + 'Value: <b>' + this.y + '%'; } !#")
    h1$exporting(enabled = T,
                 chartOptions = list(
                   legend = list(
                     itemDistance = 80,
                     itemMarginBottom = 5
                   )
                 ))
    return (h1)
  })
  
  
  output$plotCarMiles <- renderChart ({
    filterCarMilesData()
    
    firstColData <- NULL
    secondColData <- NULL
    
    extended_title <- ""
    subtitle <- ""
    
    if (input$flipG == 'sep'){
      
      # check if comparision with alternative region is selected
      
      if (input$inRegionSwitch == "Region"){
        
        # Keep the data separated
        firstColData = scCarMilesDataAltRegFull
        secondColData = scCarMilesDataAltRegFiltered
        
        firstColName <- "Scenario - alternative Region (Total Population)" 
        secondColName <- "Scenario - alternative Region (Sub-population)"
        
        extended_title <- paste0("Scenario - alternative Region [", scenarioAltRegion, "] - Car Miles per person per week")
        
        subtitle <- getCarMilesFilteredTitle("", secondColData)
        
      } else {
        
        # Keep the data separated
        firstColData = blCarMilesData
        secondColData = blCarMilesFilteredData
        
        firstColName <- "Baseline (Total Population)"
        secondColName <- "Baseline (Sub-population)"
        
        extended_title <- paste0("Baseline [", nameOfTheSelectedRegion, "] - Car Miles per person per week")
        
        subtitle <- getCarMilesFilteredTitle("", secondColData)
        
      }
      
    }else{
      
      # check if comparision with alternative region is selected
      
      if (input$inRegionSwitch == "Region"){
        
        # Keep the data mixed
        firstColData = scCarMilesData
        secondColData = scCarMilesDataAltRegFull
        
        firstColName <- paste0("Scenario [", nameOfTheSelectedRegion, "] (Total Population)")
        secondColName <- paste0("Scenario - alternative Region [", scenarioAltRegion, "] (Total Population)") 
        
        extended_title <- paste0("Total Population [", nameOfTheSelectedRegion, "] - Car Miles per person per week")
        
        subtitle <- getCarMilesFilteredTitle(paste0("Scenario [", nameOfTheSelectedRegion, "]:"), firstColData, paste0(", Scenario - alternative Region [", scenarioAltRegion, "]:"), secondColData, titlePrefix = "Total Size (trips): ", showingTotal = TRUE)
        
      } else {
        
        # Keep the data mixed
        firstColData = blCarMilesData
        secondColData = scCarMilesData
        
        firstColName <- "Baseline (Total Population)"
        secondColName <- "Scenario (Total Population)"
        
        extended_title <- paste0("Total Population [", nameOfTheSelectedRegion, "] - Car Miles per person per week")
        
        subtitle <- getCarMilesFilteredTitle("", secondColData, titlePrefix = "Total Size (trips): ", showingTotal = TRUE)
        
      }
    }
    
    
    h1 <- Highcharts$new()
    h1$chart(type = "column", style = list(fontFamily = 'Arial, sans-serif',
                                             fontSize = '12px'))
    h1$title(text = extended_title)
    
    bc <- NULL
    if (max(firstColData$data) > 0 && max(secondColData$data) > 0){
      bc <- as.data.frame(table (cut (firstColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(firstColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      
      h1$series(data = bc$Freq, name = firstColName)
      bc <- as.data.frame(table (cut (secondColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(secondColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      h1$series(data = bc$Freq, name = secondColName)
      h1$subtitle(text = subtitle)
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"),
                  style = list(fontFamily = 'Arial, sans-serif',
                               fontSize = '14px',
                               fontWeight = 'bold',
                               color = "#f00"))
    }
    
    h1$xAxis(categories = c("0","> 0 and <= 10", "> 10 and <= 20", "> 20 and <= 50", "> 50 and <= 100","> 100 and <= 200", "> 200"))
    h1$set(dom = "plotCarMiles")
    h1$yAxis(title = list(text = 'Percentage of the total population'))
    h1$tooltip(formatter = "#! function() {  return this.series.name +'<br/>' + 'Value: <b>' + this.y + '%'; } !#")
    
    h1$exporting(enabled = T,
                 chartOptions = list(
                   legend = list(
                     itemDistance = 80,
                     itemMarginBottom = 5
                   )
                 ))
    return (h1)
  })
  
  
  filterCarMilesData <- reactive ({
    input$inRegions
    input$inRegionSelected
    data <- sessionData$carMiles
    
    if (input$inGAG != 'All'){
      data <- subset(data, age_group == input$inGAG)
    }
    if (input$inGGender != 3)
      data <- subset(data, Sex_B01ID %in% input$inGGender)
    
    if (input$inGSES != "All"){
      data <- subset(data, NSSec_B03ID %in% input$inGSES)
    }
    
    if (input$inGEthnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID %in% input$inGEthnicity)
    }
    
    # if (input$inGTP != "All"){
    #   data <- subset(data, TripPurpose_B04ID == input$inGTP)
    # }
    #data[is.na(data)] <- 0
    
    
    columnName <- paste(paste("MS", input$inGMS,sep = ""),  paste("ebik", input$inGEB,sep = ""), 
                        paste("eq", input$inGEQ,sep = ""), sep="_")
    
    #data1 <- milesCycled[,c("ID", "age_group","Sex_B01ID","NSSec_B03ID","EthGroupTS_B02ID", "baseline_milesCycled", columnName)]
    
    data1 <- data.frame(sessionData$carMiles[,columnName])
    
    data1$data <- data1[,1]
    data1[,1] <- NULL
    
    data2 <- data.frame(data[,columnName])
    
    data2$data <- data2[,1]
    data2[,1] <- NULL
    
    scCarMilesData <<- data1
    scCarMilesFilteredData <<- data2
    
    blCarMilesData <<- data.frame(data = sessionData$carMiles[,"baseline"])
    blCarMilesFilteredData <<- data.frame(data = data[,"baseline"])
    
    # to save time only if comparision with alternative region is selected
    
    if(input$inRegionSwitch == 'Region'){
      
      # full data
      
      dataAltRegFull <- data.frame(data = sessionData$RegioncarMiles[[columnName]])
      
      scCarMilesDataAltRegFull <<- dataAltRegFull
      
      # filtered data
      
      dataAltRegFiltered <- sessionData$RegioncarMiles
      
      if (input$inGAG != 'All'){
        dataAltRegFiltered <- subset(dataAltRegFiltered, age_group == input$inGAG)
      }
      
      if (input$inGGender != 3){
        dataAltRegFiltered <- subset(dataAltRegFiltered, Sex_B01ID %in% input$inGGender)
      }
      
      if (input$inGSES != "All"){
        dataAltRegFiltered <- subset(dataAltRegFiltered, NSSec_B03ID %in% input$inGSES)
      }
      
      if (input$inGEthnicity != "All"){
        dataAltRegFiltered <- subset(dataAltRegFiltered, EthGroupTS_B02ID %in% input$inGEthnicity)
      }
      
      # if (input$inGTP != "All"){
      #   dataAltRegFiltered <- subset(dataAltRegFiltered, TripPurpose_B04ID == input$inGTP)
      # }
      
      dataAltRegFiltered <- data.frame(data = dataAltRegFiltered[[columnName]])
      
      scCarMilesDataAltRegFiltered <<- dataAltRegFiltered
    }
    
  })
  
  
  output$plotFilteredCO2 <- renderChart ({
    filterCO2Data()
    
    firstColData <- NULL
    secondColData <- NULL
    
    subtitle <- ""
    extended_title <- ""
    
    # Check in which form chart should be presented
    # "Scenario versus Baseline" = "comp",
    # "Sub-population versus total population" = "sep"
    
    if (input$flipG == 'sep'){
      
      # Keep the data separated
      firstColData = scCO2Data
      secondColData = scCO2FilteredData
      
      firstColName <- "Scenario (Total Population)"
      secondColName <- "Scenario (Sub-population)"
      
      extended_title <- HTML(paste0("Scenario [", nameOfTheSelectedRegion, "] - CO<sub>2</sub> (kg) from car travel per person per week"))
      subtitle <- getCO2FilteredTitle("", secondColData)
      
      
    } else {
      
      # check if comparision with alternative region is selected
      
      if (input$inRegionSwitch == "Region"){
        
        # Keep the data mixed
        firstColData = scCO2FilteredData
        secondColData = scCO2DataAltRegionFiltered
        
        firstColName <- paste0("Scenario [", nameOfTheSelectedRegion, "] (Sub-population)")
        secondColName <- paste0("Scenario - alternative Region [", scenarioAltRegion, "] (Sub-population)")
        
        extended_title <- HTML(paste0("Sub-population [", nameOfTheSelectedRegion, "] - CO<sub>2</sub> (kg) from car travel per person per week"))
        subtitle <- getCO2FilteredTitle(paste0("Scenario [", nameOfTheSelectedRegion, "]:"), firstColData, paste0(", Scenario - alternative Region [", scenarioAltRegion, "]: "), secondColData)
        
      } else {
        
        # Keep the data mixed
        firstColData = blCO2FilteredData
        secondColData = scCO2FilteredData
        
        firstColName <- "Baseline (Sub-population)"
        secondColName <- "Scenario (Sub-population)"
        
        extended_title <- HTML(paste0("Sub-population [", nameOfTheSelectedRegion, "] - CO<sub>2</sub> (kg) from car travel per person per week"))
        subtitle <- getCO2FilteredTitle("", secondColData)
        
      }
      
      # if no filter selected
      
      if (input$inGAG == "All" && input$inGGender == 3 && input$inGEthnicity == "All" && input$inGSES == "All"){# && input$inGTP == "All" ){
        
        extended_title <- HTML(paste0("Total Population [", nameOfTheSelectedRegion, "] - CO<sub>2</sub> (kg) from car travel per person per week"))
      
      }
      
    }
    
    h1 <- Highcharts$new()
    h1$chart(type = "column", style = list(fontFamily = 'Arial, sans-serif',
                                           fontSize = '12px'))
    
    h1$title(text = extended_title,
             useHTML = TRUE)
    
    # cat(" first : ", max(firstColData$data), "\n")
    if (max(firstColData$data) > 0 && max(secondColData$data) > 0){
      bc <- as.data.frame(table (cut (firstColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(firstColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      
      #h1$xAxis(categories = bc$Var1[-1])#c(2, 5, 10, 20, 40, 60, " > 60"))
      
      h1$series(data = bc$Freq, name = firstColName)
      bc <- NULL
      bc <- as.data.frame(table (cut (secondColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(secondColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      h1$series(data = bc$Freq, name = secondColName)
      h1$subtitle(text = subtitle)
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"),
                  style = list(fontFamily = 'Arial, sans-serif',
                               fontSize = '14px',
                               fontWeight = 'bold',
                               color = "#f00"))
    }
    h1$xAxis(categories = c("0", "> 0 and <= 10", "> 10 and <= 20", "> 20 and <= 50", "> 50 and <= 100","> 100 and <= 200", "> 200"))
    
    h1$set(dom = "plotFilteredCO2")
    h1$yAxis(title = list(text = 'Percentage of the total population'))
    h1$tooltip(formatter = "#! function() {  return this.series.name +'<br/>' + 'Value: <b>' + this.y + '%'; } !#")
    # h1$tooltip(valueSuffix= '%')
    h1$exporting(enabled = T,
                 chartOptions = list(
                   title = list(useHTML = FALSE),
                   legend = list(
                     itemDistance = 80,
                     itemMarginBottom = 5
                   ),
                   xAxis = list(
                     title = list(
                       y = 10
                     )
                   )
                 ))
    return (h1)
  })
  
  
  output$plotCO2 <- renderChart ({
    filterCO2Data()
    
    firstColData <- NULL
    secondColData <- NULL
    
    extended_title <- ""
    subtitle <- ""
    
    if (input$flipG == 'sep'){
      
      # check if comparision with alternative region is selected
      
      if (input$inRegionSwitch == "Region"){
        
        # Keep the data separated
        firstColData = scCO2DataAltRegionFull
        secondColData = scCO2DataAltRegionFiltered
        
        firstColName <- "Scenario - alternative Region (Total Population)"
        secondColName <- "Scenario - alternative Region (Sub-population)"
        
        subtitle <- getCO2FilteredTitle("", secondColData)
        extended_title <- HTML(paste0("Scenario - alternative Region [", scenarioAltRegion, "] - CO<sub>2</sub> (kg) from car travel per person per week"))
        
      } else {
        
        # Keep the data separated
        firstColData = blCO2Data
        secondColData = blCO2FilteredData
        
        firstColName <- "Baseline (Total Population)"
        secondColName <- "Baseline (Sub-population)"
        
        extended_title <- HTML(paste0("Baseline [", nameOfTheSelectedRegion, "] - CO<sub>2</sub> (kg) from car travel per person per week"))
        subtitle <- getCO2FilteredTitle("", secondColData)
        
      }
      
    }else{
      
      # check if comparision with alternative region is selected
      
      if (input$inRegionSwitch == "Region"){
        
        # Keep the data mixed
        firstColData = scCO2Data
        secondColData = scCO2DataAltRegionFull
        
        firstColName <- paste0("Scenario [", nameOfTheSelectedRegion, "] (Total Population)")
        secondColName <- paste0("Scenario - alternative Region [", scenarioAltRegion, "] (Total Population)")
        
        extended_title <- HTML(paste0("Total Population [", nameOfTheSelectedRegion, "] - CO<sub>2</sub> (kg) from car travel per person per week"))
        subtitle <- getCO2FilteredTitle(paste0("Scenario [", nameOfTheSelectedRegion, "]:"), firstColData, paste0(", Scenario - alternative Region [", scenarioAltRegion, "]:"), secondColData, titlePrefix = "Total Size: ", showingTotal = TRUE)
        
      } else {
        
        # Keep the data mixed
        firstColData = blCO2Data
        secondColData = scCO2Data
        
        firstColName <- "Baseline (Total Population)"
        secondColName <- "Scenario (Total Population)"
        
        extended_title <- HTML(paste0("Total Population [", nameOfTheSelectedRegion, "] - CO<sub>2</sub> (kg) from car travel per person per week"))
        subtitle <- getCO2FilteredTitle("", secondColData, titlePrefix = "Total Size: ", showingTotal = TRUE)
        
      }
    }
    
    h1 <- Highcharts$new()
    h1$chart(type = "column", style = list(fontFamily = 'Arial, sans-serif',
                                           fontSize = '12px'))
    
    h1$title(text = extended_title,
             useHTML = TRUE)
    bc <- NULL
    if (max(firstColData$data) > 0 && max(secondColData$data) > 0){
      bc <- as.data.frame(table (cut (firstColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(firstColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      
      h1$series(data = bc$Freq, name = firstColName)
      bc <- as.data.frame(table (cut (secondColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(secondColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      h1$series(data = bc$Freq, name = secondColName)
      h1$subtitle(text = subtitle)
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"),
                  style = list(fontFamily = 'Arial, sans-serif',
                               fontSize = '14px',
                               fontWeight = 'bold',
                               color = "#f00"))
    }
    
    h1$xAxis(categories = c("0", "> 0 and <= 10", "> 10 and <= 20", "> 20 and <= 50", "> 50 and <= 100","> 100 and <= 200", "> 200"))
    h1$set(dom = "plotCO2")
    h1$yAxis(title = list(text = 'Percentage of the total population'))
    h1$tooltip(formatter = "#! function() {  return this.series.name +'<br/>' + 'Value: <b>' + this.y + '%'; } !#")
    h1$exporting(enabled = T,
                 chartOptions = list(
                   title = list(useHTML = FALSE),
                   legend = list(
                     itemDistance = 80,
                     itemMarginBottom = 5
                   ),
                   xAxis = list(
                     title = list(
                       y = 10
                     )
                   )
                 ))
    return (h1)
  })
  
  
  filterCO2Data <- reactive ({
    input$inRegions
    input$inRegionSelected
    
    data <- sessionData$co2data
    
    if (input$inGAG != 'All'){
      data <- subset(data, age_group == input$inGAG)
    }
    if (input$inGGender != 3)
      data <- subset(data, Sex_B01ID %in% input$inGGender)
    
    if (input$inGSES != "All"){
      data <- subset(data, NSSec_B03ID == input$inGSES)
    }
    
    if (input$inGEthnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID == input$inGEthnicity)
    }
    
    # if (input$inGTP != "All"){
    #   data <- subset(data, TripPurpose_B04ID == input$inGTP)
    # }
    
    # Remove all NA rows from the dataset
    data[is.na(data)] <- 0
    
    columnName <- paste(paste("MS", input$inGMS,sep = ""),  paste("ebik", input$inGEB,sep = ""), 
                        paste("eq", input$inGEQ,sep = ""), sep="_")
    
    # cat(dim(data), " : ", columnName, "\n")
    
    #data1 <- milesCycled[,c("ID", "age_group","Sex_B01ID","NSSec_B03ID","EthGroupTS_B02ID", "baseline_milesCycled", columnName)]
    
    data1 <- data.frame(sessionData$co2data[,columnName])
    
    data1$data <- data1[,1]
    data1[,1] <- NULL
    
    data2 <- data.frame(data[,columnName])
    
    data2$data <- data2[,1]
    data2[,1] <- NULL
    
    scCO2Data <<- data1
    scCO2FilteredData <<- data2
    
    blCO2Data <<- data.frame(data = sessionData$co2data[,"baseline"])
    blCO2FilteredData <<- data.frame(data = data[,"baseline"])
    
    # to save time only if comparision with alternative region is selected
    
    if(input$inRegionSwitch == 'Region'){
      
      # full data
      
      dataAltRegFull <- data.frame(data = sessionData$Regionco2data[[columnName]])
      
      scCO2DataAltRegionFull <<- dataAltRegFull
      
      # filtered data
      
      dataAltRegFiltered <- sessionData$Regionco2data
      
      if (input$inGAG != 'All'){
        dataAltRegFiltered <- subset(dataAltRegFiltered, age_group == input$inGAG)
      }
      
      if (input$inGGender != 3){
        dataAltRegFiltered <- subset(dataAltRegFiltered, Sex_B01ID %in% input$inGGender)
      }
      
      if (input$inGSES != "All"){
        dataAltRegFiltered <- subset(dataAltRegFiltered, NSSec_B03ID == input$inGSES)
      }
      
      if (input$inGEthnicity != "All"){
        dataAltRegFiltered <- subset(dataAltRegFiltered, EthGroupTS_B02ID == input$inGEthnicity)
      }
      
      # Remove all NA rows from the dataset just like it is for selected (not alternative) region
      
      dataAltRegFiltered[is.na(dataAltRegFiltered)] <- 0
      
      dataAltRegFiltered <- data.frame(data = dataAltRegFiltered[[columnName]])
      
      scCO2DataAltRegionFiltered <<- dataAltRegFiltered
      
    }
    
  })
  
  
  getCO2FilteredTitle <- function(firstDataTitle, firstData, secondDataTitle = NULL, secondData = NULL, titlePrefix = NULL, showingTotal = FALSE){
    
    npeople <- nrow(firstData)
    
    filtered_title <- ""
    
    secondDataNPeople <- ""
    secondDataTitleOutput <- ""
    secondExtraSep <- ""
    
    if(!is.null(secondDataTitle)){
      
      secondDataTitleOutput <- secondDataTitle
      secondExtraSep <- " "
      
    }
    
    if(!is.null(secondData)){
      
      secondDataNPeople <- nrow(secondData)
      secondExtraSep <- " "
      
    }
    
    if(!is.null(titlePrefix)){
      titlePrefixOutput <- titlePrefix
    } else {
      titlePrefixOutput <- "Sample Size: "
    }
    
    if (input$inGAG != "All" || input$inGGender != 3 || input$inGEthnicity != "All" || input$inGSES != "All" ){
      displayGender <- "All"
      if (input$inGGender == 1){
        displayGender <- "Male"
      }else if (input$inGGender == 2){
        displayGender <- "Female"
      }
      
      displayEthnicity <- "All"
      if (input$inGEthnicity == 1){
        displayEthnicity <- "White"
      }else if (input$inGEthnicity == 2){
        displayEthnicity <- "Non-White"
      }
      
      displaySES <- "All"
      if (input$inGSES == 1){
        displaySES <- "Managerial and professional occupations"
      }else if (input$inGSES == 2){
        displaySES <- "Intermediate occupations and small employers"
      }else if (input$inGSES == 3){
        displaySES <- "Routine and manual occupations"
      }else if (input$inGSES == 4){
        displaySES <- "Never worked and long-term unemployed"
      }else if (input$inGSES == 5){
        displaySES <- "Not classified (including students)"
      }
      
      if(showingTotal){
        
        filters_info <- ""
        
      } else {
        
        filters_info <- paste0(", Age Group: ", str_trim(input$inGAG), ", Gender: ", displayGender, ", Socio Economic Classification: ", displaySES, " and Ethnicity: ", displayEthnicity)
        
      }
      
      filtered_title <- paste0(titlePrefixOutput,
                              firstDataTitle,
                              ' ',
                              npeople,
                              secondDataTitleOutput,
                              secondExtraSep,
                              secondDataNPeople,
                              filters_info)
      filtered_title
    }else
      filtered_title
  }
  
  
  
  getMilesCycledFilteredTitle <- function(firstDataTitle, firstData, secondDataTitle = NULL, secondData = NULL, titlePrefix = NULL, showingTotal = FALSE){
    
    ntrips <- nrow(firstData)
    
    filtered_title <- ""
    
    secondDataNPeople <- ""
    secondDataTitleOutput <- ""
    secondExtraSep <- ""
    
    if(!is.null(secondDataTitle)){
      
      secondDataTitleOutput <- secondDataTitle
      secondExtraSep <- " "
      
    }
    
    if(!is.null(secondData)){
      
      secondDataNPeople <- nrow(secondData)
      secondExtraSep <- " "
      
    }
    
    if(!is.null(titlePrefix)){
      titlePrefixOutput <- titlePrefix
    } else {
      titlePrefixOutput <- "Sample Size: "
    }
    
    if (input$inGAG != "All" || input$inGGender != 3 || input$inGEthnicity != "All" || input$inGSES != "All"){
        # || input$inGTP != "All"){
      displayGender <- "All"
      if (input$inGGender == 1){
        displayGender <- "Male"
      }else if (input$inGGender == 2){
        displayGender <- "Female"
      }
      
      displayEthnicity <- "All"
      if (input$inGEthnicity == 1){
        displayEthnicity <- "White"
      }else if (input$inGEthnicity == 2){
        displayEthnicity <- "Non-White"
      }
      
      displaySES <- "All"
      if (input$inGSES == 1){
        displaySES <- "Managerial and professional occupations"
      }else if (input$inGSES == 2){
        displaySES <- "Intermediate occupations and small employers"
      }else if (input$inGSES == 3){
        displaySES <- "Routine and manual occupations"
      }else if (input$inGSES == 4){
        displaySES <- "Never worked and long-term unemployed"
      }else if (input$inGSES == 5){
        displaySES <- "Not classified (including students)"
      }

      if(showingTotal){
        
        filters_info <- ""
        
      } else {
        
        filters_info <- paste0(", Age Group: ", str_trim(input$inGAG), ", Gender: ", displayGender, ", Socio Economic Classification: ", displaySES, " and Ethnicity: ", displayEthnicity)
        
      }
      
      filtered_title <- paste0(titlePrefixOutput,
                               firstDataTitle,
                               ' ',
                               ntrips,
                               secondDataTitleOutput,
                               secondExtraSep,
                               secondDataNPeople,
                               filters_info)
      filtered_title
    }else
      filtered_title
  }
  
  getCarMilesFilteredTitle <- function(firstDataTitle, firstData, secondDataTitle = NULL, secondData = NULL, titlePrefix = NULL, showingTotal = FALSE){
    
    filtered_title <- ""
    
    npeople <- nrow(firstData)
    
    secondDataNPeople <- ""
    secondDataTitleOutput <- ""
    secondExtraSep <- ""
    
    titlePrefixOutput <- ""
    
    if(!is.null(secondDataTitle)){
      
      secondDataTitleOutput <- secondDataTitle
      secondExtraSep <- " "
      
    }
    
    if(!is.null(secondData)){
      
      secondDataNPeople <- nrow(secondData)
      secondExtraSep <- " "
      
    }
    
    if(!is.null(titlePrefix)){
      titlePrefixOutput <- titlePrefix
    } else {
      titlePrefixOutput <- "Sample Size: "
    }
    
    if (input$inGAG != "All" || input$inGGender != 3 || input$inGEthnicity != "All" || input$inGSES != "All" ){
      displayGender <- "All"
      if (input$inGGender == 1){
        displayGender <- "Male"
      }else if (input$inGGender == 2){
        displayGender <- "Female"
      }
      
      displayEthnicity <- "All"
      if (input$inGEthnicity == 1){
        displayEthnicity <- "White"
      }else if (input$inGEthnicity == 2){
        displayEthnicity <- "Non-White"
      }
      
      displaySES <- "All"
      if (input$inGSES == 1){
        displaySES <- "Managerial and professional occupations"
      }else if (input$inGSES == 2){
        displaySES <- "Intermediate occupations and small employers"
      }else if (input$inGSES == 3){
        displaySES <- "Routine and manual occupations"
      }else if (input$inGSES == 4){
        displaySES <- "Never worked and long-term unemployed"
      }else if (input$inGSES == 5){
        displaySES <- "Not classified (including students)"
      }
      
      if(showingTotal){
        
        filters_info <- ""
        
      } else {
        
        filters_info <- paste0(", Age Group: ", str_trim(input$inGAG), ", Gender: ", displayGender, ", Socio Economic Classification: ", displaySES, " and Ethnicity: ", displayEthnicity)
        
      }
      
      filtered_title <- paste0(titlePrefixOutput,
                              firstDataTitle,
                              ' ',
                              npeople,
                              secondDataTitleOutput,
                              secondExtraSep,
                              secondDataNPeople, 
                              filters_info)
      filtered_title
    }else
      filtered_title
  }
  
  # EQ
  observe({
    input$inBDEQ
    updateTextInput(session, "inTTEQ", NULL, input$inBDEQ)
    updateTextInput(session, "inHealthEQ", NULL, input$inBDEQ)
    updateTextInput(session, "inMETEQ", NULL, input$inBDEQ)
    updateTextInput(session, "inMSEQ", NULL, input$inBDEQ)
    updateTextInput(session, "inCMEQ", NULL, input$inBDEQ)
    updateTextInput(session, "inCO2EQ", NULL, input$inBDEQ)
    
    
  })
  
  observe({
    input$inHealthEQ
    updateTextInput(session, "inTTEQ", NULL, input$inHealthEQ)
    updateTextInput(session, "inBDEQ", NULL, input$inHealthEQ)
    updateTextInput(session, "inMETEQ", NULL, input$inHealthEQ)
    updateTextInput(session, "inMSEQ", NULL, input$inHealthEQ)
    updateTextInput(session, "inCMEQ", NULL, input$inHealthEQ)
    updateTextInput(session, "inCO2EQ", NULL, input$inHealthEQ)
  })
  
  observe({
    input$inMETEQ
    updateTextInput(session, "inTTEQ", NULL, input$inMETEQ)
    updateTextInput(session, "inBDEQ", NULL, input$inMETEQ)
    updateTextInput(session, "inHealthEQ", NULL, input$inMETEQ)
    updateTextInput(session, "inMSEQ", NULL, input$inMETEQ)
    updateTextInput(session, "inCMEQ", NULL, input$inMETEQ)
    updateTextInput(session, "inCO2EQ", NULL, input$inMETEQ)
  })
  
  observe({
    input$inMSEQ
    updateTextInput(session, "inTTEQ", NULL, input$inMSEQ)
    updateTextInput(session, "inBDEQ", NULL, input$inMSEQ)
    updateTextInput(session, "inHealthEQ", NULL, input$inMSEQ)
    updateTextInput(session, "inMETEQ", NULL, input$inMSEQ)
    updateTextInput(session, "inCMEQ", NULL, input$inMSEQ)
    updateTextInput(session, "inCO2EQ", NULL, input$inMSEQ)
  })
  
  observe({
    input$inCMEQ
    updateTextInput(session, "inTTEQ", NULL, input$inCMEQ)
    updateTextInput(session, "inBDEQ", NULL, input$inCMEQ)
    updateTextInput(session, "inHealthEQ", NULL, input$inCMEQ)
    updateTextInput(session, "inMETEQ", NULL, input$inCMEQ)
    updateTextInput(session, "inMSEQ", NULL, input$inCMEQ)
    updateTextInput(session, "inCO2EQ", NULL, input$inCMEQ)
  })
  
  observe({
    input$inCO2EQ
    updateTextInput(session, "inTTEQ", NULL, input$inCO2EQ)
    updateTextInput(session, "inBDEQ", NULL, input$inCO2EQ)
    updateTextInput(session, "inHealthEQ", NULL, input$inCO2EQ)
    updateTextInput(session, "inMETEQ", NULL, input$inCO2EQ)
    updateTextInput(session, "inMSEQ", NULL, input$inCO2EQ)
    updateTextInput(session, "inCMEQ", NULL, input$inCO2EQ)
  })
  
  observe({
    input$inTTEQ
    updateTextInput(session, "inCO2EQ", NULL, input$inTTEQ)
    updateTextInput(session, "inBDEQ", NULL, input$inTTEQ)
    updateTextInput(session, "inHealthEQ", NULL, input$inTTEQ)
    updateTextInput(session, "inMETEQ", NULL, input$inTTEQ)
    updateTextInput(session, "inMSEQ", NULL, input$inTTEQ)
    updateTextInput(session, "inCMEQ", NULL, input$inTTEQ)
  })
  
  # inBDEQ
  # inTTEQ
  # inHealthEQ
  # inMETEQ
  # inMSEQ
  # inCMEQ
  # inCO2EB
  
  # EB
  
  observe({
    input$inBDEB
    updateTextInput(session, "inTTEB", NULL, input$inBDEB)
    updateTextInput(session, "inHealthEB", NULL, input$inBDEB)
    updateTextInput(session, "inMETEB", NULL, input$inBDEB)
    updateTextInput(session, "inMSEB", NULL, input$inBDEB)
    updateTextInput(session, "inCMEB", NULL, input$inBDEB)
    updateTextInput(session, "inCO2EB", NULL, input$inBDEB)
  })
  
  observe({input$inHealthEB
    updateTextInput(session, "inTTEB", NULL, input$inHealthEB)
    updateTextInput(session, "inBDEB", NULL, input$inHealthEB)
    updateTextInput(session, "inMETEB", NULL, input$inHealthEB)
    updateTextInput(session, "inMSEB", NULL, input$inHealthEB)
    updateTextInput(session, "inCMEB", NULL, input$inHealthEB)
    updateTextInput(session, "inCO2EB", NULL, input$inHealthEB)
  })
  
  observe({input$inMETEB
    updateTextInput(session, "inTTEB", NULL, input$inMETEB)
    updateTextInput(session, "inBDEB", NULL, input$inMETEB)
    updateTextInput(session, "inHealthEB", NULL, input$inMETEB)
    updateTextInput(session, "inMSEB", NULL, input$inMETEB)
    updateTextInput(session, "inCMEB", NULL, input$inMETEB)
    updateTextInput(session, "inCO2EB", NULL, input$inMETEB)
  })
  
  observe({input$inMSEB
    updateTextInput(session, "inTTEB", NULL, input$inMSEB)
    updateTextInput(session, "inBDEB", NULL, input$inMSEB)
    updateTextInput(session, "inHealthEB", NULL, input$inMSEB)
    updateTextInput(session, "inMETEB", NULL, input$inMSEB)
    updateTextInput(session, "inCMEB", NULL, input$inMSEB)
    updateTextInput(session, "inCO2EB", NULL, input$inMSEB)
  })
  
  observe({input$inCMEB
    updateTextInput(session, "inTTEB", NULL, input$inCMEB)
    updateTextInput(session, "inBDEB", NULL, input$inCMEB)
    updateTextInput(session, "inHealthEB", NULL, input$inCMEB)
    updateTextInput(session, "inMETEB", NULL, input$inCMEB)
    updateTextInput(session, "inMSEB", NULL, input$inCMEB)
    updateTextInput(session, "inCO2EB", NULL, input$inCMEB)
  })
  
  observe({input$inCO2EB
    updateTextInput(session, "inTTEB", NULL, input$inCO2EB)
    updateTextInput(session, "inBDEB", NULL, input$inCO2EB)
    updateTextInput(session, "inHealthEB", NULL, input$inCO2EB)
    updateTextInput(session, "inMETEB", NULL, input$inCO2EB)
    updateTextInput(session, "inMSEB", NULL, input$inCO2EB)
    updateTextInput(session, "inCMEB", NULL, input$inCO2EB)
  })
  
  
  observe({input$inTTEB
    updateTextInput(session, "inCO2EB", NULL, input$inTTEB)
    updateTextInput(session, "inBDEB", NULL, input$inTTEB)
    updateTextInput(session, "inHealthEB", NULL, input$inTTEB)
    updateTextInput(session, "inMETEB", NULL, input$inTTEB)
    updateTextInput(session, "inMSEB", NULL, input$inTTEB)
    updateTextInput(session, "inCMEB", NULL, input$inTTEB)
  })
  
  # inBDEB
  # inHealthEB
  # inMETEB
  # inMSEB
  # inCMEB
  # inCO2EB
  
  observe({
    input$inBDMS
    updateTextInput(session, "inTTMS", NULL, input$inBDMS)
    updateTextInput(session, "inHealthMS", NULL, input$inBDMS)
    updateTextInput(session, "inMETMS", NULL, input$inBDMS)
    updateTextInput(session, "inMSMS", NULL, input$inBDMS)
    updateTextInput(session, "inCMMS", NULL, input$inBDMS)
    updateTextInput(session, "inCO2MS", NULL, input$inBDMS)
  })
  
  observe({
    input$inHealthMS
    updateTextInput(session, "inTTMS", NULL, input$inHealthMS)
    updateTextInput(session, "inBDMS", NULL, input$inHealthMS)
    updateTextInput(session, "inMETMS", NULL, input$inHealthMS)
    updateTextInput(session, "inMSMS", NULL, input$inHealthMS)
    updateTextInput(session, "inCMMS", NULL, input$inHealthMS)
    updateTextInput(session, "inCO2MS", NULL, input$inHealthMS)
  })
  
  observe({input$inMETMS
    updateTextInput(session, "inTTMS", NULL, input$inMETMS)
    updateTextInput(session, "inBDMS", NULL, input$inMETMS)
    updateTextInput(session, "inHealthMS", NULL, input$inMETMS)
    updateTextInput(session, "inMSMS", NULL, input$inMETMS)
    updateTextInput(session, "inCMMS", NULL, input$inMETMS)
    updateTextInput(session, "inCO2MS", NULL, input$inMETMS)
  })
  
  observe({input$inMSMS
    updateTextInput(session, "inTTMS", NULL, input$inMSMS)
    updateTextInput(session, "inBDMS", NULL, input$inMSMS)
    updateTextInput(session, "inHealthMS", NULL, input$inMSMS)
    updateTextInput(session, "inMETMS", NULL, input$inMSMS)
    updateTextInput(session, "inCMMS", NULL, input$inMSMS)
    updateTextInput(session, "inCO2MS", NULL, input$inMSMS)
  })
  
  observe({input$inCMMS
    updateTextInput(session, "inTTMS", NULL, input$inCMMS)
    updateTextInput(session, "inBDMS", NULL, input$inCMMS)
    updateTextInput(session, "inHealthMS", NULL, input$inCMMS)
    updateTextInput(session, "inMETMS", NULL, input$inCMMS)
    updateTextInput(session, "inMSMS", NULL, input$inCMMS)
    updateTextInput(session, "inCO2MS", NULL, input$inCMMS)
  })
  
  observe({input$inCO2MS
    updateTextInput(session, "inTTMS", NULL, input$inCO2MS)
    updateTextInput(session, "inBDMS", NULL, input$inCO2MS)
    updateTextInput(session, "inHealthMS", NULL, input$inCO2MS)
    updateTextInput(session, "inMETMS", NULL, input$inCO2MS)
    updateTextInput(session, "inMSMS", NULL, input$inCO2MS)
    updateTextInput(session, "inCMMS", NULL, input$inCO2MS)
  })
  
  
  observe({input$inTTMS
    updateTextInput(session, "inCO2MS", NULL, input$inTTMS)
    updateTextInput(session, "inBDMS", NULL, input$inTTMS)
    updateTextInput(session, "inHealthMS", NULL, input$inTTMS)
    updateTextInput(session, "inMETMS", NULL, input$inTTMS)
    updateTextInput(session, "inMSMS", NULL, input$inTTMS)
    updateTextInput(session, "inCMMS", NULL, input$inTTMS)
  })
  
  # age
  
  observe({input$inBDAG
    updateTextInput(session, "inTTAG", NULL, input$inBDAG)
    updateTextInput(session, "mag", NULL, input$inBDAG)
    updateTextInput(session, "inMSAG", NULL, input$inBDAG)
    updateTextInput(session, "inCMAG", NULL, input$inBDAG)
    updateTextInput(session, "inCO2AG", NULL, input$inBDAG)
    
  })
  
  observe({input$mag
    updateTextInput(session, "inTTAG", NULL, input$mag)
    updateTextInput(session, "inBDAG", NULL, input$mag)
    updateTextInput(session, "inMSAG", NULL, input$mag)
    updateTextInput(session, "inCMAG", NULL, input$mag)
    updateTextInput(session, "inCO2AG", NULL, input$mag)
  })
  
  observe({input$inMSAG
    updateTextInput(session, "inTTAG", NULL, input$inMSAG)
    updateTextInput(session, "inBDAG", NULL, input$inMSAG)
    updateTextInput(session, "mag", NULL, input$inMSAG)
    updateTextInput(session, "inCMAG", NULL, input$inMSAG)
    updateTextInput(session, "inCO2AG", NULL, input$inMSAG)
  })
  
  observe({input$inCMAG
    updateTextInput(session, "inTTAG", NULL, input$inCMAG)
    updateTextInput(session, "inBDAG", NULL, input$inCMAG)
    updateTextInput(session, "mag", NULL, input$inCMAG)
    updateTextInput(session, "inMSAG", NULL, input$inCMAG)
    updateTextInput(session, "inCO2AG", NULL, input$inCMAG)
  })
  
  observe({input$inCO2AG
    updateTextInput(session, "inTTAG", NULL, input$inCO2AG)
    updateTextInput(session, "inBDAG", NULL, input$inCO2AG)
    updateTextInput(session, "mag", NULL, input$inCO2AG)
    updateTextInput(session, "inMSAG", NULL, input$inCO2AG)
    updateTextInput(session, "inCMAG", NULL, input$inCO2AG)
  })
  
  observe({input$inTTAG
    updateTextInput(session, "inCO2AG", NULL, input$inTTAG)
    updateTextInput(session, "inBDAG", NULL, input$inTTAG)
    updateTextInput(session, "mag", NULL, input$inTTAG)
    updateTextInput(session, "inMSAG", NULL, input$inTTAG)
    updateTextInput(session, "inCMAG", NULL, input$inTTAG)
  })
  
  #   inBDAG
  #   mag
  #   inMSAG
  #   inCMAG
  #   inCO2AG
  
  
  # gender
  
  observe({input$inBDGender
    updateTextInput(session, "inTTGender", NULL, input$inBDGender)
    updateTextInput(session, "inHealthG", NULL, input$inBDGender)
    updateTextInput(session, "mgender", NULL, input$inBDGender)
    updateTextInput(session, "inMSG", NULL, input$inBDGender)
    updateTextInput(session, "inCMG", NULL, input$inBDGender)
    updateTextInput(session, "inGGender", NULL, input$inBDGender)
  })
  
  observe({input$inHealthG
    updateTextInput(session, "inTTGender", NULL, input$inHealthG)
    updateTextInput(session, "inBDGender", NULL, input$inHealthG)
    updateTextInput(session, "mgender", NULL, input$inHealthG)
    updateTextInput(session, "inMSG", NULL, input$inHealthG)
    updateTextInput(session, "inCMG", NULL, input$inHealthG)
    updateTextInput(session, "inGGender", NULL, input$inHealthG)
  })
  
  observe({input$mgender
    updateTextInput(session, "inTTGender", NULL, input$mgender)
    updateTextInput(session, "inBDGender", NULL, input$mgender)
    updateTextInput(session, "inHealthG", NULL, input$mgender)
    updateTextInput(session, "inMSG", NULL, input$mgender)
    updateTextInput(session, "inCMG", NULL, input$mgender)
    updateTextInput(session, "inGGender", NULL, input$mgender)
    
  })
  
  observe({input$inMSG
    updateTextInput(session, "inTTGender", NULL, input$inMSG)
    updateTextInput(session, "inBDGender", NULL, input$inMSG)
    updateTextInput(session, "inHealthG", NULL, input$inMSG)
    updateTextInput(session, "mgender", NULL, input$inMSG)
    updateTextInput(session, "inCMG", NULL, input$inMSG)
    updateTextInput(session, "inGGender", NULL, input$inMSG)
  })
  
  observe({input$inCMG
    updateTextInput(session, "inTTGender", NULL, input$inCMG)
    updateTextInput(session, "inBDGender", NULL, input$inCMG)
    updateTextInput(session, "inHealthG", NULL, input$inCMG)
    updateTextInput(session, "mgender", NULL, input$inCMG)
    updateTextInput(session, "inMSG", NULL, input$inCMG)
    updateTextInput(session, "inGGender", NULL, input$inCMG)
  })
  
  observe({input$inGGender
    updateTextInput(session, "inTTGender", NULL, input$inGGender)
    updateTextInput(session, "inBDGender", NULL, input$inGGender)
    updateTextInput(session, "inHealthG", NULL, input$inGGender)
    updateTextInput(session, "mgender", NULL, input$inGGender)
    updateTextInput(session, "inMSG", NULL, input$inGGender)
    updateTextInput(session, "inCMG", NULL, input$inGGender)
  })
  
  observe({input$inTTGender
    updateTextInput(session, "inGGender", NULL, input$inTTGender)
    updateTextInput(session, "inBDGender", NULL, input$inTTGender)
    updateTextInput(session, "inHealthG", NULL, input$inTTGender)
    updateTextInput(session, "mgender", NULL, input$inTTGender)
    updateTextInput(session, "inMSG", NULL, input$inTTGender)
    updateTextInput(session, "inCMG", NULL, input$inTTGender)
  })
  
  #   inBDGender
  #   inHealthG
  #   mgender
  #   inMSG
  #   inCMG
  #   inGGender
  
  
  # SES
  
  observe({input$inBDSES
    updateTextInput(session, "inTTSES", NULL, input$inBDSES)
    updateTextInput(session, "mses", NULL, input$inBDSES)
    updateTextInput(session, "inMSSES", NULL, input$inBDSES)
    updateTextInput(session, "inCMSES", NULL, input$inBDSES)
    updateTextInput(session, "inGSES", NULL, input$inBDSES)
  })
  
  
  observe({input$mses
    updateTextInput(session, "inTTSES", NULL, input$mses)
    updateTextInput(session, "inBDSES", NULL, input$mses)
    updateTextInput(session, "inMSSES", NULL, input$mses)
    updateTextInput(session, "inCMSES", NULL, input$mses)
    updateTextInput(session, "inGSES", NULL, input$mses)
  })
  
  observe({input$inMSSES
    updateTextInput(session, "inTTSES", NULL, input$inMSSES)
    updateTextInput(session, "inBDSES", NULL, input$inMSSES)
    updateTextInput(session, "mses", NULL, input$inMSSES)
    updateTextInput(session, "inCMSES", NULL, input$inMSSES)
    updateTextInput(session, "inGSES", NULL, input$inMSSES)
  })
  
  observe({input$inCMSES
    updateTextInput(session, "inTTSES", NULL, input$inCMSES)
    updateTextInput(session, "inBDSES", NULL, input$inCMSES)
    updateTextInput(session, "mses", NULL, input$inCMSES)
    updateTextInput(session, "inMSSES", NULL, input$inCMSES)
    updateTextInput(session, "inGSES", NULL, input$inCMSES)
  })
  
  observe({input$inGSES
    updateTextInput(session, "inTTSES", NULL, input$inGSES)
    updateTextInput(session, "inBDSES", NULL, input$inGSES)
    updateTextInput(session, "mses", NULL, input$inGSES)
    updateTextInput(session, "inMSSES", NULL, input$inGSES)
    updateTextInput(session, "inCMSES", NULL, input$inGSES)
  })
  
  observe({input$inTTSES
    updateTextInput(session, "inGSES", NULL, input$inTTSES)
    updateTextInput(session, "inBDSES", NULL, input$inTTSES)
    updateTextInput(session, "mses", NULL, input$inTTSES)
    updateTextInput(session, "inMSSES", NULL, input$inTTSES)
    updateTextInput(session, "inCMSES", NULL, input$inTTSES)
  })
  
  
  #   inBDSES
  #   mses
  #   inMSSES
  #   inCMSES
  #   inGSES
  
  #Ethnicity
  
  
  observe({input$inBDEthnicity
    updateTextInput(session, "inTTEthnicity", NULL, input$inBDEthnicity)
    updateTextInput(session, "methnicity", NULL, input$inBDEthnicity)
    updateTextInput(session, "inMSEthnicity", NULL, input$inBDEthnicity)
    updateTextInput(session, "inCMEthnicity", NULL, input$inBDEthnicity)
    updateTextInput(session, "inGEthnicity", NULL, input$inBDEthnicity)
  })
  
  observe({input$methnicity
    updateTextInput(session, "inTTEthnicity", NULL, input$methnicity)
    updateTextInput(session, "inBDEthnicity", NULL, input$methnicity)
    updateTextInput(session, "inMSEthnicity", NULL, input$methnicity)
    updateTextInput(session, "inCMEthnicity", NULL, input$methnicity)
    updateTextInput(session, "inGEthnicity", NULL, input$methnicity)
    
  })
  
  observe({input$inMSEthnicity
    updateTextInput(session, "inTTEthnicity", NULL, input$inMSEthnicity)
    updateTextInput(session, "inBDEthnicity", NULL, input$inMSEthnicity)
    updateTextInput(session, "methnicity", NULL, input$inMSEthnicity)
    updateTextInput(session, "inCMEthnicity", NULL, input$inMSEthnicity)
    updateTextInput(session, "inGEthnicity", NULL, input$inMSEthnicity)
  })
  
  observe({input$inCMEthnicity
    updateTextInput(session, "inTTEthnicity", NULL, input$inCMEthnicity)
    updateTextInput(session, "inBDEthnicity", NULL, input$inCMEthnicity)
    updateTextInput(session, "methnicity", NULL, input$inCMEthnicity)
    updateTextInput(session, "inMSEthnicity", NULL, input$inCMEthnicity)
    updateTextInput(session, "inGEthnicity", NULL, input$inCMEthnicity)
  })
  
  observe({input$inGEthnicity
    updateTextInput(session, "inTTEthnicity", NULL, input$inGEthnicity)
    updateTextInput(session, "inBDEthnicity", NULL, input$inGEthnicity)
    updateTextInput(session, "methnicity", NULL, input$inGEthnicity)
    updateTextInput(session, "inGEthnicity", NULL, input$inGEthnicity)
    updateTextInput(session, "inCMEthnicity", NULL, input$inGEthnicity)
  })
  
  observe({input$inTTEthnicity
    updateTextInput(session, "inGEthnicity", NULL, input$inTTEthnicity)
    updateTextInput(session, "inBDEthnicity", NULL, input$inTTEthnicity)
    updateTextInput(session, "methnicity", NULL, input$inTTEthnicity)
    updateTextInput(session, "inGEthnicity", NULL, input$inTTEthnicity)
    updateTextInput(session, "inCMEthnicity", NULL, input$inTTEthnicity)
  })
  
  #   inBDEthnicity
  #   methnicity
  #   inGEthnicity
  #   inCMEthnicity
  #   inGEthnicity
  
  
  observe({input$flipMS
    updateTextInput(session, "flipTT", NULL, input$flipMS)
    updateTextInput(session, "flipMETHG", NULL, input$flipMS)
    updateTextInput(session, "inMSflip", NULL, input$flipMS)
    updateTextInput(session, "flipG", NULL, input$flipMS)
    updateTextInput(session, "flipG", NULL, input$flipMS)
  })
  
  observe({input$flipMETHG
    updateTextInput(session, "flipTT", NULL, input$flipMETHG)
    updateTextInput(session, "flipMS", NULL, input$flipMETHG)
    updateTextInput(session, "inMSflip", NULL, input$flipMETHG)
    updateTextInput(session, "flipG", NULL, input$flipMETHG)
    updateTextInput(session, "flipG", NULL, input$flipMETHG)
    
  })
  
  observe({input$inMSflip
    updateTextInput(session, "flipTT", NULL, input$inMSflip)
    updateTextInput(session, "flipMS", NULL, input$inMSflip)
    updateTextInput(session, "flipMETHG", NULL, input$inMSflip)
    updateTextInput(session, "flipG", NULL, input$inMSflip)
    updateTextInput(session, "flipG", NULL, input$flipMS)
    
  })
  
  observe({input$flipG
    updateTextInput(session, "flipTT", NULL, input$flipG)
    updateTextInput(session, "flipMS", NULL, input$flipG)
    updateTextInput(session, "flipMETHG", NULL, input$flipG)
    updateTextInput(session, "inMSflip", NULL, input$flipG)
    updateTextInput(session, "flipG", NULL, input$flipG)
  })
  
  observe({input$flipG
    updateTextInput(session, "flipTT", NULL, input$flipG)
    updateTextInput(session, "flipMS", NULL, input$flipG)
    updateTextInput(session, "flipMETHG", NULL, input$flipG)
    updateTextInput(session, "inMSflip", NULL, input$flipG)
    updateTextInput(session, "flipG", NULL, input$flipG)
  })
  
  observe({input$flipTT
    updateTextInput(session, "flipG", NULL, input$flipTT)
    updateTextInput(session, "flipMS", NULL, input$flipTT)
    updateTextInput(session, "flipMETHG", NULL, input$flipTT)
    updateTextInput(session, "inMSflip", NULL, input$flipTT)
    updateTextInput(session, "flipG", NULL, input$flipTT)
  })
  
  
  

  
  shinyjs::onclick("MSHelp", shinyjs::toggle(id = "MSHelpText", anim = FALSE))
  shinyjs::onclick("MTHelp", shinyjs::toggle(id = "MTHelpText", anim = FALSE))
  shinyjs::onclick("HealthHelp", shinyjs::toggle(id = "HealthHelpText", anim = FALSE))
  shinyjs::onclick("PAHelp", shinyjs::toggle(id = "PAHelpText", anim = FALSE))
  shinyjs::onclick("MCHelp", shinyjs::toggle(id = "MCHelpText", anim = FALSE))
  shinyjs::onclick("CMHelp", shinyjs::toggle(id = "CMHelpText", anim = FALSE))
  shinyjs::onclick("CO2Help", shinyjs::toggle(id = "CO2HelpText", anim = FALSE))
  shinyjs::onclick("mainIntro", shinyjs::toggle(id = "mainIntroText", anim = FALSE))
  
  
  
})

