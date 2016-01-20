library(shiny)
library(DT)
library(devtools)
if (!require(rCharts)) {
  install_github("rCharts", "ramnathv")
  library(rCharts)
}
library(reshape2)
library(dplyr)
library(plyr)
library(ggplot2)
library(stringr)
library(Hmisc)

pd <- idata$total_mmet
bMETdata <- NULL
scMETdata <- NULL
scFilteredMETdata <- NULL
scTimeTraveldata <- NULL
scFilteredTimeTraveldata <- NULL
scFilteredTripTimeTraveldata <- NULL

ftdata <- NULL
swdata <- NULL
bd <- NULL
pdl <- NULL
bldata <- NULL
scYllData <- NULL
scYllReductionData <- NULL

msBaseline <- NULL
msScenario <- NULL
tdBaseline <- NULL
tdScenario <- NULL

scMilesCycledData <- NULL
scMilesCycledFilteredData <- NULL

blMilesCycledData <- NULL
blMilesCycledFilteredData <- NULL

scCarMilesData <- NULL
scCarMilesFilteredData <- NULL

blCarMilesData <- NULL
blCarMilesFilteredData <- NULL

scCO2Data <- NULL
scCO2FilteredData <- NULL

blCO2Data <- NULL
blCO2FilteredData <- NULL

# Functions
source("functions.R")

shinyServer(function(input, output, session){
  
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
  
  
  filterTimeTravelData <- reactive({
    data <- scenariosTimeTravelIdata
    
    if (input$inTTag != 'All'){
      data <- subset(data, age == input$inTTag)
    }
    if (input$inTTgender != 3)
      data <- subset(data, Sex_B01ID %in% input$inTTgender)
    
    if (input$inTTses != "All"){
      data <- subset(data, NSSec_B03ID %in% input$inTTses)
    }
    
    if (input$inTTethnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID %in% input$inTTethnicity)
    }
    data[is.na(data)] <- 0
    
    columnName <- paste(paste("MS", input$inTTMS,sep = ""),  
                        paste("ebik", input$inTTEB,sep = ""), paste("eq", input$inTTEQ,sep = ""), sep="_")
    
    
    data["timetravel"] <- data[,columnName]
    
    data <- arrange(data, timetravel)
    
    #Convert data in minutes to hours
    data["timetravel"] <- data["timetravel"]/60
    
    #     scTimeTraveldata["timetravel"] <<- scenariosTimeTravelIdata[,columnName]
    scFilteredTimeTraveldata <<- data
    #     scFilteredTripTimeTraveldata <<- tripData
  })
  
  tripTimeData <- reactive({
    #     data <- scenariosTripTimeTravelIdata
    #     
    #     if (input$inTTag != 'All'){
    #       data <- subset(data, age_group == input$inTTag)
    #     }
    #     if (input$inTTgender != 3)
    #       data <- subset(data, Sex_B01ID %in% input$inTTgender)
    #     
    #     if (input$inTTses != "All"){
    #       data <- subset(data, NSSec_B03ID %in% input$inTTses)
    #     }
    #     
    #     if (input$inTTethnicity != "All"){
    #       data <- subset(data, EthGroupTS_B02ID %in% input$inTTethnicity)
    #     }
    #     data[is.na(data)] <- 0
    #     
    #     
    #     columnName <- paste(paste("MS", input$inTTMS,sep = ""),  paste("ebik", input$inTTEB,sep = ""), 
    #                         paste("eq", input$inTTEQ,sep = ""), sep="_")
    #     
    #     #tripData <- scenariosTripTimeTravelIdata[,c("baseline", columnName)]
    #     tripData <- data[,c("MainMode_Reduced", columnName)]
    #     
    #     tripData <- as.data.frame(((tripData[[columnName]] - tripData$MainMode_Reduced) / tripData$MainMode_Reduced ) * 100)
    #     
    #     colnames(tripData) <- c("diff")
    #     tripDataSubset <- subset(tripData, diff <= 200 & diff >= -200 )
    #     tripDataSubset <- subset(tripDataSubset, diff != 0 )
    #     scFilteredTripTimeTraveldata <<- tripDataSubset
  })
  
  filterHealthData <- reactive({
    data1 <- yll
    # Temporarily removing YLL total values
    data1 <- subset(data1, age.band != "All Ages")
    data2 <- yll_red
    
    if (input$inHealthAG != 'All'){
      # Temporarily removing YLL total values
      data1 <- subset(data1, age.band == input$inHealthAG)
      data2 <- subset(data2, age.band == input$inHealthAG | age.band == "All Ages")
    }
    if (input$inHealthG !='All'){
      # Temporarily removing YLL total values
      data1 <- subset(data1, gender %in% input$inHealthG)
      data2 <- subset(data2, gender %in% input$inHealthG | gender == "Both Gender")
    }
    
    columnName <- paste(paste("MS", input$inHealthMS,sep = ""),  paste("ebik", input$inHealthEB,sep = ""), 
                        paste("eq", input$inHealthEQ,sep = ""), sep="_")
    # cat(columnName, "\n")
    data1 <- data1[,c("age.band", "gender",columnName)]
    colnames(data1) <- c("age.band", "gender", "scenario")
    
    data2 <- data2[,c("age.band", "gender",columnName)]
    colnames(data2) <- c("age.band", "gender", "scenario")
    
    scYllData <<- data1
    scYllReductionData <<- data2
    
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
    data <- subset(idata, select = c(ID,age_group,Sex_B01ID,EthGroupTS_B02ID,NSSec_B03ID,baseline_mmet))
    data["total_mmet"] <- data$baseline_mmet
    
    data1 <- subset(data, select = baseline_mmet)
    data1["total_mmet"] <- data1$baseline_mmet
    
    bMETdata <<- data1
    
    
    if (input$mag != 'All'){
      data <- subset(data, age_group == input$mag)
    }
    if (input$mgender != 3)
      data <- subset(data, Sex_B01ID %in% input$mgender)
    
    if (input$mses != "All"){
      data <- subset(data, NSSec_B03ID %in% input$mses)
    }
    
    if (input$methnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID %in% input$methnicity)
    }
    data[is.na(data)] <- 0
    
    pd <<- data
    
    columnName <- paste(paste("MS", input$inMETMS,sep = ""),  paste("ebik", input$inMETEB,sep = ""), 
                        paste("eq", input$inMETEQ,sep = ""), sep="_")
    
    data <- subset(idata, select = c("ID","age_group","Sex_B01ID","EthGroupTS_B02ID","NSSec_B03ID",columnName))
    
    data["total_mmet"] <- data[columnName]
    
    scMETdata <<- data
    
    if (input$mag != 'All'){
      data <- subset(data, age_group == input$mag)
    }
    if (input$mgender != 3)
      data <- subset(data, Sex_B01ID %in% input$mgender)
    
    if (input$mses != "All"){
      data <- subset(data, NSSec_B03ID %in% input$mses)
    }
    
    if (input$methnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID %in% input$methnicity)
    }
    
    data[is.na(data)] <- 0
    
    scFilteredMETdata <<- data
  })
  
  output$plotMET <- renderChart({
    input$flipMETHG
    input$phyGuideline
    
    plotMETDataTable()
    
    extended_title <- ""
    
    if (!is.null(idata) & !is.null(scMETdata)){
      if (input$flipMETHG == 'sep'){
        # Keep the data separated
        # scMETdata and scFilteredMETdata
        firstColData = bMETdata
        secondColData = pd
        
        extended_title <- paste("Baseline - Marginal MET Hours", sep = "")
        
        firstColName <- "Baseline (Total Population)"
        secondColName <- "Baseline (Sub-Population)"
        if (nrow(idata) == nrow(pd))
          secondColName <- "Baseline (Total Population)"
        
        #extended_title <- "Baseline - Marginal MET Hours"
        
      }else{
        # Keep the data mixed
        firstColData = bMETdata
        secondColData = scMETdata
        
        extended_title <- paste("Total Population - Marginal MET Hours", sep = "")
        
        firstColName <- "Baseline (Total Population)"
        secondColName <- "Scenario (Sub-Population)"
        if (nrow(idata) == nrow(scMETdata))
          secondColName <- "Scenario (Total Population)"
      }
      
      filtered_title <- getMETFilteredTitle(secondColData, "baseline")
      
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      
      if (input$phyGuideline == 'on'){
        bc <- createPhyActTable(firstColData)
        bc$Freq <- round(bc$Freq  / nrow(firstColData) * 100, digits = 1)
        
        h1$xAxis(categories = c("Not meeting guidelines (METh < 8.75)", "Meeting the guidelines (METh > 8.75)", 
                                "Meeting the higher guidelines (METh > 17.5)"), 
                 title = list(text = 'Marginal MET Hours'))
        h1$series(data = bc$Freq, name = firstColName)
        
        bc <- createPhyActTable(secondColData)#$total_mmet)
        bc$Freq <- round(bc$Freq  / nrow(secondColData) * 100, digits = 1)
        
        h1$series(data = bc$Freq, name = secondColName)
        
        if (nrow(firstColData) < 10){
          h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
        }else{
          h1$subtitle(text= filtered_title)
        }
        
        h1$yAxis(tickInterval = 20, title = list(text = 'Percentage %'))
        
        h1$plotOptions(column=list(animation=FALSE, 
                                   dataLabels = list(enabled = T, 
                                                     crop = FALSE, 
                                                     rotation = -90, 
                                                     align = 'right', 
                                                     color = '#FFFFFF', 
                                                     width = 100,
                                                     x = 4, 
                                                     y = 10, 
                                                     style = list(fontSize = '10px', fontFamily = 'Verdana, sans-serif'))))
        
        
      }else{
        
        bc <- as.data.frame(table (cut (firstColData$total_mmet, breaks = c(seq(-4.4,52.8, 4.4), max(firstColData$total_mmet)))))
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        bc1max <- max(bc$Freq, na.rm = T)
        
        h1$xAxis(categories = as.list(append(c(seq(-4.4,52.8, 4.4))[-1], "> 52.8")), title = list(text = 'Marginal MET Hours'))
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
        h1$yAxis(min = 0, max = max(30, max_y), tickInterval = 10, title = list(text = 'Percentage %'))
        
        if(filter){
          h1$series(data = bc$Freq, name = secondColName)
          h1$subtitle(text= filtered_title)
          
        }else{
          h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
        }
        
        h1$plotOptions(column=list(animation=FALSE))
      }
      
    }
    
    h1$title(text = extended_title)
    h1$tooltip(valueSuffix= '%')
    h1$set(dom = "plotMET")
    h1$exporting(enabled = T)
    return(h1)
  })
  
  output$plotScenarioMET <- renderChart({
    
    plotMETDataTable()
    if (!is.null(scMETdata)){
      
      if (input$flipMETHG == 'sep'){
        # Keep the data separated
        # scMETdata and scFilteredMETdata
        firstColData = scMETdata
        secondColData = scFilteredMETdata
        extended_title <- paste("Scenario - Marginal MET Hours")
        
        firstColName <- "Scenario (Total Population)"
        secondColName <- "Scenario (Sub-population)"
        
        
        
      }else{
        # Keep the data mixed
        firstColData = pd
        secondColData = scFilteredMETdata
        extended_title <- paste("Sub-Population - Marginal MET Hours")
        
        firstColName <- "Baseline (Sub-population)"
        secondColName <- "Scenario (Sub-population)"
        
      }
      
      filtered_title <- getMETFilteredTitle(secondColData, "scenario")
      
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      
      
      if (input$phyGuideline == 'on'){
        bc <- createPhyActTable(firstColData)
        bc$Freq <- round(bc$Freq  / nrow(firstColData) * 100, digits = 1)
        
        h1$xAxis(categories = c("Not meeting guidelines (METh < 8.75)", "Meeting the guidelines (METh > 8.75)", 
                                "Meeting the higher guidelines (METh > 17.5)"),
                 title = list(text = 'Marginal MET Hours'))
        h1$series(data = bc$Freq, name = firstColName)
        
        bc <- createPhyActTable(secondColData)#$total_mmet)
        bc$Freq <- round(bc$Freq  / nrow(secondColData) * 100, digits = 1)
        
        h1$series(data = bc$Freq, name = secondColName)
        
        if (nrow(firstColData) < 10){
          h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
        }else{
          h1$subtitle(text= filtered_title)
        }
        
        h1$yAxis(tickInterval = 20, title = list(text = 'Percentage %'))
        
        h1$plotOptions(column=list(animation=FALSE, 
                                   dataLabels = list(enabled = T, 
                                                     crop = FALSE, 
                                                     rotation = -90, 
                                                     align = 'right', 
                                                     color = '#FFFFFF', 
                                                     width = 100,
                                                     x = 4, 
                                                     y = 10, 
                                                     style = list(fontSize = '10px', fontFamily = 'Verdana, sans-serif'))))
        
      }else{
        
        bc <- as.data.frame(table (cut (firstColData$total_mmet, breaks = c(seq(-4.4,52.8, 4.4), max(firstColData$total_mmet)))))
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        bc1max <- max(bc$Freq, na.rm = T)
        
        h1$xAxis(categories = as.list(append(c(seq(-4.4,52.8, 4.4))[-1], "> 52.8")), title = list(text = 'Marginal MET Hours'))
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
        h1$yAxis(min = 0, max = max(30, max_y), tickInterval = 10, title = list(text = 'Percentage %'))
        
        
        if(filter){
          h1$series(data = bc$Freq, name = secondColName)
          h1$subtitle(text= filtered_title)
          
        }else{
          h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
        }
        
        h1$plotOptions(column=list(animation=FALSE))
      }
      
    }
    
    h1$title(text = extended_title)
    h1$tooltip(valueSuffix= '%')
    h1$set(dom = "plotScenarioMET")
    h1$exporting(enabled = T)
    return(h1)
    
  })
  output$plotYLL <- renderChart({
    filterHealthData()
    h1 <- Highcharts$new()
    h1$chart(type = "column")
    
    
    if (nrow(scYllReductionData) > 0){
      
      # For both gender, create new series
      ugender <- unique(scYllData$gender)
      for (i in 1:length(ugender)){
        data <- subset(scYllData, gender == ugender[i])
        h1$series(data = data$scenario, name = ugender[i])
      }
      
      h1$xAxis(categories = append(input$inHealthAG, " "), title = list(text = 'Age and Gender Groups'))
      if (length(unique(scYllData$age.band)) > 1)
        h1$xAxis(categories = unique(scYllData$age.band), title = list(text = 'Age and Gender Groups'))
      
      
      h1$yAxis(title = list(text = 'YLL (Absolute Numbers)'))
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size = 0)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
    }
    
    h1$title(text = "Years of Life Lost (YLL) for the English Population")
    h1$set(dom = "plotYLL")
    h1$exporting(enabled = T)
    return(h1)
  })
  
  output$plotYLLReduction <- renderChart({
    
    filterHealthData()
    
    h1 <- Highcharts$new()
    h1$chart(type = "column")
    if (nrow(scYllReductionData) > 0){
      
      # For both gender, create new series
      ugender <- unique(scYllReductionData$gender[-1])
      
      for (i in 1:length(ugender)){
        data <- subset(scYllReductionData, gender == ugender[i])
        h1$series(data = data$scenario, name = ugender[i])
      }
      
      h1$xAxis(categories = append(input$inHealthAG, " "), title = list(text = 'Age and Gender Groups'))
      if (length(unique(scYllReductionData$age.band)) > 2)
        h1$xAxis(categories = unique(scYllReductionData$age.band[-1]), title = list(text = 'Age and Gender Groups'))
      
      h1$yAxis(title = list(text = 'Percentage (%)'))
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size = 0)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
    }
    
    h1$title(text = "Reduction in Years of Life Lost (YLL) for the English Population")
    h1$tooltip(valueSuffix= '%')
    h1$set(dom = "plotYLLReduction")
    h1$exporting(enabled = T)
    return(h1)
  })
  
  
  output$plotBaseline <- renderChart({
    if (!is.null(tdata)){
      if (input$scenario == 'i'){
        
        filtered_title <- getFilteredTitle(idata)
        max_val <- max(idata$total_mmet)
        h <- NULL
        bc <- table (cut (idata$total_mmet, breaks = c(seq(min(idata$total_mmet), 60, by = 5),max(idata$total_mmet)), xlim = c(min(idata$total_mmet), 60)))
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
      filtered_title <- paste("Cycling Multiplier: ", input$inBDMS, ", Equity: ", input$inBDEQ, " and Ebike: ", input$inBDEB , sep = "" )
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
  
  getMETFilteredTitle <- function(data, src){
    filtered_title <- ""
    if (src == "baseline")
      dataSource = idata
    else
      dataSource = scMETdata
    
    if (nrow(data) != nrow (dataSource)){
      
      displayGender <- "All"
      if (input$mgender == 1){
        displayGender <- "Male"
      }else if (input$mgender == 2){
        displayGender <- "Female"
      }
      
      displayEthnicity <- "All"
      if (input$methnicity == 1){
        displayEthnicity <- "White"
      }else if (input$methnicity == 2){
        displayEthnicity <- "Non-White"
      }
      
      displaySES <- "All"
      if (input$mses == 1){
        displaySES <- "Managerial and professional occupations"
      }else if (input$mses == 2){
        displaySES <- "Intermediate occupations and small employers"
      }else if (input$mses == 3){
        displaySES <- "Routine and manual occupations"
      }else if (input$mses == 4){
        displaySES <- "Never worked and long-term unemployed"
      }else if (input$mses == 5){
        displaySES <- "Not classified (including students)"
      }
      
      filtered_title <- paste("Age Group: ", str_trim(input$mag), ", Gender: ", displayGender, ", Socio Economic Classification: ", displaySES, " and Ethnicity: ", displayEthnicity, sep = "" )
      filtered_title
    }else
      filtered_title
  }
  
  getTripsFilteredTitle <- function(){
    filtered_title <- ""
    
    if (input$inBDAG != "All" || input$inBDGender != 3 || input$inBDEthnicity != "All" || input$inBDSES != "All" ){
      
      displayGender <- "All"
      if (input$inBDGender == 1){
        displayGender <- "Male"
      }else if (input$inBDGender == 2){
        displayGender <- "Female"
      }
      
      displayEthnicity <- "All"
      if (input$inBDEthnicity == 1){
        displayEthnicity <- "White"
      }else if (input$inBDEthnicity == 2){
        displayEthnicity <- "Non-White"
      }
      
      displaySES <- "All"
      if (input$inBDSES == 1){
        displaySES <- "Managerial and professional occupations"
      }else if (input$inBDSES == 2){
        displaySES <- "Intermediate occupations and small employers"
      }else if (input$inBDSES == 3){
        displaySES <- "Routine and manual occupations"
      }else if (input$inBDSES == 4){
        displaySES <- "Never worked and long-term unemployed"
      }else if (input$inBDSES == 5){
        displaySES <- "Not classified (including students)"
      }
      
      filtered_title <- paste("Age Group: ", str_trim(input$inBDAG), ", Gender: ", displayGender, ", Socio Economic Classification: ", displaySES, " and Ethnicity: ", displayEthnicity, sep = "" )
      filtered_title
    }else
      filtered_title
  }
  
  generateBDScenarioTable <- reactive({
    
    lMS <- input$inBDMS
    lEB <- input$inBDEB
    lEQ <- input$inBDEQ
    
    #     data1 <- msharedtata
    #     data1 <- subset(data1, MS == (as.numeric(lMS) + 1) & equity == lEQ & ebike == lEB)
    #     
    #     data1[is.na(data1)] <- 0
    #     data1 <- arrange(data1, MS)
    #     bd <<- data1
    
    
    # Filter data of trips
    #     cat(paste(paste("MS", input$inBDMS,sep = ""),  paste("ebik", input$inBDEB,sep = ""), 
    #               paste("eq", input$inBDEQ,sep = ""), sep="_"), "\n")
    columnName <- paste(paste("MS", input$inBDMS,sep = ""),  paste("ebik", input$inBDEB,sep = ""), 
                        paste("eq", input$inBDEQ,sep = ""), sep="_")
    # cat(columnName, "\n")
    colList <- c("ID","age_group", "Sex_B01ID","NSSec_B03ID",  "EthGroupTS_B02ID", "MainMode_Reduced", columnName)
    data <- tripData[,colList]
    
    msbl <- subset(tripData, select = MainMode_Reduced)
    msbl <- count(msbl, "MainMode_Reduced")
    names(msbl)[names(msbl)== "MainMode_Reduced"] <- "baseline"
    
    msbl$freq <- round(msbl$freq / sum(msbl$freq) * 100, digit = 1)
    
    msbl <- appendMissingFrequencies(tp_mode, msbl)
    
    msbl <- arrange(msbl, msbl[,1])
    
    msBaseline <<- msbl
    
    mssc <- subset(tripData, select = columnName)
    mssc <- count(mssc, columnName)
    names(mssc)[names(mssc)== columnName] <- "scenario"
    
    mssc$freq <- round(mssc$freq / sum(mssc$freq) * 100, digit = 1)
    
    mssc <- appendMissingFrequencies(tp_mode, mssc)
    
    mssc <- arrange(mssc, mssc[,1])
    
    msScenario <<- mssc
    #tdBaseline
    if (input$inBDAG != 'All'){
      data <- subset(data, age_group == input$inBDAG)
    }
    if (input$inBDGender != 3)
      data <- subset(data, Sex_B01ID %in% input$inBDGender)
    
    if (input$inBDSES != "All"){
      data <- subset(data, NSSec_B03ID %in% input$inBDSES)
    }
    
    if (input$inBDEthnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID %in% input$inBDEthnicity)
    }
    
    data1 <- count(data, columnName)
    names(data1)[names(data1)== columnName] <- "scenario"
    
    data1$freq <- round(data1$freq / sum(data1$freq) * 100, digit = 1)
    
    data1 <- appendMissingFrequencies(tp_mode, data1)
    
    data1 <- arrange(data1, data1[,1])
    
    tdScenario <<- data1
    
    data2 <- count(data, "MainMode_Reduced")
    names(data2)[names(data2)== columnName] <- "baseline"
    
    data2$freq <- round(data2$freq / sum(data2$freq) * 100, digit = 1)
    
    data2 <- appendMissingFrequencies(tp_mode, data2)
    
    data2 <- arrange(data2, data2[,1])
    
    tdBaseline <<- data2
    
    
    
    bd <<- data2
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
    
    #     lMS <- input$inMS
    lEB <- input$inEB
    lEQ <- input$inEQ
    
    data <- sdata
    if (lEQ != "All")
      data <- subset(data, equity == lEQ)
    if (lEB != "All")
      data <- subset(data, ebike == lEB)
    
    data[is.na(data)] <- 0
    data <- arrange(data, MS)
    # data[order(Age),]
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
    
    h1$xAxis(categories = append("Baseline", sort(unique(sdata$MS), decreasing = F)), title = list(text = 'Cycling Multiplier'))
    
    
    if (input$inEB != "All" & input$inEQ != "All"){
      sub1 <- subset(scdata, ebike == as.numeric(input$inEB) & equity == as.numeric(input$inEQ))
      h1$series(data = append(baselineSummary[[var]], sub1[[var]]), name = getSeriesName(input$inEQ, input$inEB ))
      
    }
    
    if (input$inEB == "All" & input$inEQ != "All"){
      sub1 <- subset(scdata, ebike == 0 & equity == as.numeric(input$inEQ))
      h1$series(data = append(baselineSummary[[var]], sub1[[var]]), name = getSeriesName(input$inEQ, 0 ))
      sub1 <- subset(scdata, ebike == 1 & equity == as.numeric(input$inEQ))
      h1$series(data = append(baselineSummary[[var]], sub1[[var]]), name = getSeriesName(input$inEQ, 1 ))
      
    }
    
    
    if (input$inEB != "All" & input$inEQ == "All"){
      sub1 <- subset(scdata, equity == 0 & ebike == as.numeric(input$inEB))
      h1$series(data = append(baselineSummary[[var]], sub1[[var]]), name = getSeriesName(0,  input$inEB ))
      sub1 <- subset(scdata, equity == 1 & ebike == as.numeric(input$inEB))
      h1$series(data = append(baselineSummary[[var]], sub1[[var]]), name = getSeriesName(1, input$inEB ))
      
    }
    
    if (input$inEB == "All" & input$inEQ == "All"){
      sub1 <- subset(scdata, ebike == 0 & equity == 0)
      h1$series(data = append(baselineSummary[[var]], sub1[[var]]), name = getSeriesName(0, 0 ))
      sub1 <- subset(scdata, ebike == 0 & equity == 1)
      h1$series(data = append(baselineSummary[[var]], sub1[[var]]), name = getSeriesName(1, 0 ))
      sub1 <- subset(scdata, ebike == 1 & equity == 0)
      h1$series(data = append(baselineSummary[[var]], sub1[[var]]), name = getSeriesName(0, 1 ))
      sub1 <- subset(scdata, ebike == 1 & equity == 1)
      h1$series(data = append(baselineSummary[[var]], sub1[[var]]), name = getSeriesName(1, 1 ))
      
    }
    
    h1$exporting(enabled = T)
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
    h <- genericPlot(input$varname)
    h$title(text = input$varname)
    h$set(dom = "plotGenericVariable")
    return (h)
  })
  
  
  output$plotBDMode <- renderChart({
    generateBDScenarioTable()
    extended_title <- ""
    firstColData = NULL
    secondColData = NULL
    filtered_title <- getTripsFilteredTitle()
    if (input$flipMS == 'sep'){
      # Keep the data separated
      # scMETdata and scFilteredMETdata
      firstColData = msBaseline # msScenario
      secondColData = tdBaseline
      
      firstColName <- "Baseline (Total Population)" # "Scenario (Total Population)"
      secondColName <- "Baseline (Sub-Population)"
      
      extended_title <- "Baseline - Mode Share"
      
      #         extended_title <- paste("Baseline - Marginal MET Hours", sep = "")
      #         
      #         firstColName <- "Baseline (Total Population)"
      #         secondColName <- "Baseline (Sub-Population)"
      #         if (nrow(idata) == nrow(pd))
      #           secondColName <- "Baseline (Total Population)"
      #         
    }else{
      
      firstColData = msBaseline
      secondColData = msScenario
      firstColName <- "Baseline (Total Population)"
      secondColName <- "Scenario (Total Population)"
      
      extended_title <- "Total Population - Mode Share"
      filtered_title <- ""
      
      #         extended_title <- paste("Baseline Versus Scenario - Marginal MET Hours", sep = "")
      #         
      #         firstColName <- "Baseline (Total Population)"
      #         secondColName <- "Scenario (Sub-Population)"
      #         if (nrow(idata) == nrow(scMETdata))
      #           secondColName <- "Scenario (Total Population)"
    }
    
    h1 <- Highcharts$new()
    h1$chart(type = "column")
    h1$plotOptions(column=list(animation=FALSE))
    
    
    #extended_title <- paste("Mode Share: Total Population versus Selected Scenario")
    
    h1$title(text = extended_title)
    #baseline <- subset(msharedtata, MS == 1)
    h1$series(data = firstColData$freq, name = firstColName)
    h1$series(data = secondColData$freq, name = secondColName)
    
    h1$xAxis(categories = tp_mode$mode)
    h1$yAxis(title = list(text = 'Percentage % of Trips'))
    
    #h1$subtitle(text = paste("Scenario: ", filtered_title), style = list(font = 'bold 12px "Trebuchet MS", Verdana, sans-serif'))
    
    if (sum(firstColData$freq, na.rm = T) <= 10){
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
    }else{
      h1$subtitle(text= filtered_title)
    }
    
    h1$tooltip(valueSuffix= '%')
    
    h1$set(dom = "plotBDMode")
    h1$exporting(enabled = T)
    return (h1)
  })
  
  
  output$plotBDSCMode <- renderChart({
    generateBDScenarioTable()
    if (input$flipMS == 'sep'){
      # Keep the data separated
      # scMETdata and scFilteredMETdata
      
      firstColData = msScenario
      secondColData = tdScenario
      
      firstColName <- "Scenario (Total Population)" # "Scenario (Total Population)"
      secondColName <- "Scenario (Sub-Population)"
      
      extended_title <- "Scenario - Mode Share"
      
      #         extended_title <- paste("Baseline - Marginal MET Hours", sep = "")
      #         
      
      #         if (nrow(idata) == nrow(pd))
      #           secondColName <- "Baseline (Total Population)"
      #         
    }else{
      # Keep the data mixed
      firstColData = tdBaseline
      secondColData = tdScenario
      
      firstColName <- "Baseline (Sub-Population)" # "Scenario (Total Population)"
      secondColName <- "Scenario (Sub-Population)"
      
      extended_title <- "Sub-population - Mode Share"
      
      #         extended_title <- paste("Baseline Versus Scenario - Marginal MET Hours", sep = "")
      #         
      
      #         if (nrow(idata) == nrow(scMETdata))
      #           secondColName <- "Scenario (Total Population)"
    }
    
    h1 <- Highcharts$new()
    h1$chart(type = "column")
    h1$plotOptions(column=list(animation=FALSE))
    
    #filtered_title <- getFilteredBDTitle("BD")
    #extended_title <- paste("Mode Share: Total Population versus Selected Scenario")
    h1$title(text = extended_title)
    #baseline <- subset(msharedtata, MS == 1)
    h1$series(data = firstColData$freq, name = firstColName)
    h1$series(data = secondColData$freq, name = secondColName)
    
    h1$xAxis(categories = tp_mode$mode)
    h1$yAxis(title = list(text = 'Percentage % of Trips'))
    
    #h1$subtitle(text = paste("Scenario: ", filtered_title), style = list(font = 'bold 12px "Trebuchet MS", Verdana, sans-serif'))
    filtered_title <- getTripsFilteredTitle()
    
    if (sum(firstColData$freq, na.rm = T) <= 10){
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
    }else{
      h1$subtitle(text= filtered_title)
    }
    
    h1$tooltip(valueSuffix= '%')
    
    h1$set(dom = "plotBDSCMode")
    h1$exporting(enabled = T)
    return (h1)
    
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
      h1$yAxis(title = list(text = 'Percentage %'))
      
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
      h1$yAxis(title = list(text = 'Percentage %'))
      
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
    h1 <- Highcharts$new()
    h1$chart(type = "column")
    firstColData <- NULL
    secondColData <- NULL
    subtitle <- getMilesCycledFilteredTitle()
    extended_title <- ""
    if (input$inMSflip == 'sep'){
      # Keep the data separated
      firstColData = blMilesCycledData
      secondColData = blMilesCycledFilteredData
      
      firstColName <- "Baseline (Total Population)"
      secondColName <- "Baseline (Sub-Population)"
      
      extended_title <- "Baseline - Total Miles Cycled per Cyclist per week"
      #extended_title <- "Scenario - Mode Share"
      
    }else{
      # Keep the data mixed
      firstColData = blMilesCycledData
      secondColData = scMilesCycledData
      
      firstColName <- "Baseline (Total Population)"
      secondColName <- "Scenario (Total Population)"
      
      subtitle <- ""
      
      extended_title <- "Total population - Total Miles Cycled per Cyclist per week"
    }
    
    h1$title(text = extended_title)
    
    if (max(firstColData$data) > 0 && max(secondColData$data) > 0){
      bc <- as.data.frame(table (cut (firstColData$data, breaks = c(c(-1, 0, 2, 5, 10, 20, 40, 60), max(firstColData$data)))))
      
      if (input$inMSTotOrCyc == 'cyc'){
        bc$Freq <- round(bc$Freq  / sum(bc$Freq[-1]) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage % of Cyclists'))
      }
      else{
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage % in Total Population'))
      }
      
      #h1$xAxis(categories = bc$Var1[-1])#c(2, 5, 10, 20, 40, 60, " > 60"))
      
      h1$series(data = bc$Freq[-1], name = firstColName)
      bc <- NULL
      bc <- as.data.frame(table (cut (secondColData$data, breaks = c(c(-1, 0, 2, 5, 10, 20, 40, 60), max(secondColData$data)))))
      if (input$inMSTotOrCyc == 'cyc'){
        bc$Freq <- round(bc$Freq  / sum(bc$Freq[-1]) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage % of Cyclists'))
      }
      else{
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage % in Total Population'))
      }
      h1$series(data = bc$Freq[-1], name = secondColName)
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
    }
    h1$xAxis(categories = c("> 0 and <= 2", "> 2 and <= 5", "> 5 and <= 10", "> 10 and <= 20","> 20 and <= 40", "> 40 and <= 60", "> 60"))
    h1$subtitle(text = subtitle, style = list(font = 'bold 12px "Trebuchet MS", Verdana, sans-serif'))
    
    h1$set(dom = "plotFilteredMilesCycled")
    # h1$yAxis(title = list(text = 'Percentage %'))
    h1$tooltip(valueSuffix= '%')
    h1$exporting(enabled = T)
    return (h1)
  })
  
  
  output$plotMilesCycled <- renderChart ({
    filterMilesCycledData()
    h1 <- Highcharts$new()
    h1$chart(type = "column")
    firstColData <- NULL
    secondColData <- NULL
    subtitle <- ""
    extended_title <- ""
    if (input$inMSflip == 'sep'){
      # Keep the data separated
      firstColData = scMilesCycledData
      secondColData = scMilesCycledFilteredData
      
      firstColName <- "Scenario (Total Population)"
      secondColName <- "Scenario (Sub-Population)"
      
      extended_title <- "Scenario - Total Miles Cycled per Cyclist per week"
      
    }else{
      # Keep the data mixed
      firstColData = blMilesCycledFilteredData
      secondColData = scMilesCycledFilteredData
      
      firstColName <- "Baseline (Sub-Population)"
      secondColName <- "Scenario (Sub-Population)"
      
      extended_title <- "Sub-population - Total Miles Cycled per Cyclist per week"
    }
    subtitle <- getMilesCycledFilteredTitle()
    h1$title(text = extended_title)
    
    bc <- NULL
    if (max(firstColData$data) > 0 && max(firstColData$data) > 0){
      bc <- as.data.frame(table (cut (firstColData$data, breaks = c(c(-1, 0, 2, 5, 10, 20, 40, 60), max(firstColData$data)))))
      if (input$inMSTotOrCyc == 'cyc'){
        bc$Freq <- round(bc$Freq  / sum(bc$Freq[-1]) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage % of Cyclists'))
      }
      else{
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage % in Total Population'))
      }
      
      h1$series(data = bc$Freq[-1], name = firstColName)
      bc <- as.data.frame(table (cut (secondColData$data, breaks = c(c(-1, 0, 2, 5, 10, 20, 40, 60), max(secondColData$data)))))
      if (input$inMSTotOrCyc == 'cyc'){
        bc$Freq <- round(bc$Freq  / sum(bc$Freq[-1]) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage % of Cyclists'))
      }
      else{
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        h1$yAxis(title = list(text = 'Percentage % in Total Population'))
      }
      
      h1$series(data = bc$Freq[-1], name = secondColName)
      h1$subtitle(text = subtitle, style = list(font = 'bold 12px "Trebuchet MS", Verdana, sans-serif'))
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
    }
    
    h1$xAxis(categories = c("> 0 and <= 2", "> 2 and <= 5", "> 5 and <= 10", "> 10 and <= 20","> 20 and <= 40", "> 40 and <= 60", "> 60"))
    h1$set(dom = "plotMilesCycled")
    # h1$yAxis(title = list(text = 'Percentage %'))
    h1$tooltip(valueSuffix= '%')
    
    h1$exporting(enabled = T)
    return (h1)
  })
  
  filterMilesCycledData <- reactive ({
    data <- milesCycled
    
    if (input$inMSAG != 'All'){
      data <- subset(data, age_group == input$inMSAG)
    }
    if (input$inMSG != 3)
      data <- subset(data, Sex_B01ID %in% input$inMSG)
    
    if (input$inMSSES != "All"){
      data <- subset(data, NSSec_B03ID %in% input$inMSSES)
    }
    
    if (input$inMSEthnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID %in% input$inMSEthnicity)
    }
    #data[is.na(data)] <- 0
    
    
    columnName <- paste(paste("MS", input$inMSMS,sep = ""),  paste("ebik", input$inMSEB,sep = ""), 
                        paste("eq", input$inMSEQ,sep = ""), sep="_")
    
    #data1 <- milesCycled[,c("ID", "age_group","Sex_B01ID","NSSec_B03ID","EthGroupTS_B02ID", "baseline_milesCycled", columnName)]
    
    data1 <- data.frame(milesCycled[,columnName])
    
    data1$data <- data1[,1]
    data1[,1] <- NULL
    
    data2 <- data.frame(data[,columnName])
    
    data2$data <- data2[,1]
    data2[,1] <- NULL
    
    scMilesCycledData <<- data1
    scMilesCycledFilteredData <<- data2
    
    blMilesCycledData <<- data.frame(data = milesCycled[,"baseline_milesCycled"])
    blMilesCycledFilteredData <<- data.frame(data = data[,"baseline_milesCycled"])
  })
  
  
  
  output$plotFilteredCarMiles <- renderChart ({
    filterCarMilesData()
    h1 <- Highcharts$new()
    h1$chart(type = "column")
    firstColData <- NULL
    secondColData <- NULL
    subtitle <- getCarMilesFilteredTitle()
    extended_title <- ""
    if (input$inCMflip == 'sep'){
      # Keep the data separated
      firstColData = blCarMilesData
      secondColData = blCarMilesFilteredData
      
      firstColName <- "Baseline (Total Population)"
      secondColName <- "Baseline (Sub-Population)"
      
      extended_title <- "Baseline - Car Miles per week"
      #extended_title <- "Scenario - Mode Share"
      
    }else{
      # Keep the data mixed
      firstColData = blCarMilesData
      secondColData = scCarMilesData
      
      firstColName <- "Baseline (Total Population)"
      secondColName <- "Scenario (Total Population)"
      
      subtitle <- ""
      
      extended_title <- "Total Population - Car Miles per week"
    }
    
    h1$title(text = extended_title)
    
    # cat(" first : ", max(firstColData$data), "\n")
    if (max(firstColData$data) > 0 && max(secondColData$data) > 0){
      bc <- as.data.frame(table (cut (firstColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(firstColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      
      #h1$xAxis(categories = bc$Var1[-1])#c(2, 5, 10, 20, 40, 60, " > 60"))
      
      h1$series(data = bc$Freq[-1], name = firstColName)
      bc <- NULL
      bc <- as.data.frame(table (cut (secondColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(secondColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      h1$series(data = bc$Freq[-1], name = secondColName)
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
    }
    h1$xAxis(categories = c("> 0 and <= 10", "> 10 and <= 20", "> 20 and <= 50", "> 50 and <= 100","> 100 and <= 200", "> 200"))
    h1$subtitle(text = subtitle, style = list(font = 'bold 12px "Trebuchet MS", Verdana, sans-serif'))
    
    h1$set(dom = "plotFilteredCarMiles")
    h1$yAxis(title = list(text = 'Percentage %'))
    h1$tooltip(valueSuffix= '%')
    h1$exporting(enabled = T)
    return (h1)
  })
  
  
  output$plotCarMiles <- renderChart ({
    filterCarMilesData()
    h1 <- Highcharts$new()
    h1$chart(type = "column")
    firstColData <- NULL
    secondColData <- NULL
    extended_title <- ""
    subtitle <- ""
    if (input$inMSG == 'sep'){
      # Keep the data separated
      firstColData = scCarMilesData
      secondColData = scCarMilesFilteredData
      
      firstColName <- "Scenario (Total Population)"
      secondColName <- "Scenario (Sub-Population)"
      
      extended_title <- "Scenario - Car Miles per week"
      
    }else{
      # Keep the data mixed
      firstColData = blCarMilesFilteredData
      secondColData = scCarMilesFilteredData
      
      firstColName <- "Baseline (Sub-Population)"
      secondColName <- "Scenario (Sub-Population)"
      
      extended_title <- "Sub-Population - Car Miles per week"
    }
    subtitle <- getCarMilesFilteredTitle()
    h1$title(text = extended_title)
    bc <- NULL
    if (max(firstColData$data) > 0 && max(firstColData$data) > 0){
      bc <- as.data.frame(table (cut (firstColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(firstColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      
      h1$series(data = bc$Freq[-1], name = firstColName)
      bc <- as.data.frame(table (cut (secondColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(secondColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      h1$series(data = bc$Freq[-1], name = secondColName)
      h1$subtitle(text = subtitle, style = list(font = 'bold 12px "Trebuchet MS", Verdana, sans-serif'))
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
    }
    
    h1$xAxis(categories = c("> 0 and <= 10", "> 10 and <= 20", "> 20 and <= 50", "> 50 and <= 100","> 100 and <= 200", "> 200"))
    h1$set(dom = "plotCarMiles")
    h1$yAxis(title = list(text = 'Percentage %'))
    h1$tooltip(valueSuffix= '%')
    
    h1$exporting(enabled = T)
    return (h1)
  })
  
  
  filterCarMilesData <- reactive ({
    data <- carMiles
    
    if (input$inMSAG != 'All'){
      data <- subset(data, age_group == input$inMSAG)
    }
    if (input$inMSG != 3)
      data <- subset(data, Sex_B01ID %in% input$inMSG)
    
    if (input$inMSSES != "All"){
      data <- subset(data, NSSec_B03ID %in% input$inMSSES)
    }
    
    if (input$inMSEthnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID %in% input$inMSEthnicity)
    }
    #data[is.na(data)] <- 0
    
    
    columnName <- paste(paste("MS", input$inMSMS,sep = ""),  paste("ebik", input$inMSEB,sep = ""), 
                        paste("eq", input$inMSEQ,sep = ""), sep="_")
    
    #data1 <- milesCycled[,c("ID", "age_group","Sex_B01ID","NSSec_B03ID","EthGroupTS_B02ID", "baseline_milesCycled", columnName)]
    
    data1 <- data.frame(carMiles[,columnName])
    
    data1$data <- data1[,1]
    data1[,1] <- NULL
    
    data2 <- data.frame(data[,columnName])
    
    data2$data <- data2[,1]
    data2[,1] <- NULL
    
    scCarMilesData <<- data1
    scCarMilesFilteredData <<- data2
    
    blCarMilesData <<- data.frame(data = carMiles[,"baseline_carMiles"])
    blCarMilesFilteredData <<- data.frame(data = data[,"baseline_carMiles"])
    
  })
  
  
  output$plotFilteredCO2 <- renderChart ({
    filterCO2Data()
    h1 <- Highcharts$new()
    h1$chart(type = "column")
    firstColData <- NULL
    secondColData <- NULL
    subtitle <- getCO2FilteredTitle()
    extended_title <- ""
    if (input$inCO2flip == 'sep'){
      # Keep the data separated
      firstColData = blCO2Data
      secondColData = blCO2FilteredData
      
      firstColName <- "Baseline (Total Population)"
      secondColName <- "Baseline (Sub-Population)"
      
      extended_title <- "Baseline - CO2 (kg) per week"
      #extended_title <- "Scenario - Mode Share"
      
    }else{
      # Keep the data mixed
      firstColData = blCO2Data
      secondColData = scCO2Data
      
      firstColName <- "Baseline (Total Population)"
      secondColName <- "Scenario (Total Population)"
      
      subtitle <- ""
      
      extended_title <- "Total Population - CO2 (kg) per week"
    }
    
    h1$title(text = extended_title)
    
    # cat(" first : ", max(firstColData$data), "\n")
    if (max(firstColData$data) > 0 && max(secondColData$data) > 0){
      bc <- as.data.frame(table (cut (firstColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(firstColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      
      #h1$xAxis(categories = bc$Var1[-1])#c(2, 5, 10, 20, 40, 60, " > 60"))
      
      h1$series(data = bc$Freq[-1], name = firstColName)
      bc <- NULL
      bc <- as.data.frame(table (cut (secondColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(secondColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      h1$series(data = bc$Freq[-1], name = secondColName)
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
    }
    h1$xAxis(categories = c("> 0 and <= 10", "> 10 and <= 20", "> 20 and <= 50", "> 50 and <= 100","> 100 and <= 200", "> 200"))
    h1$subtitle(text = subtitle, style = list(font = 'bold 12px "Trebuchet MS", Verdana, sans-serif'))
    
    h1$set(dom = "plotFilteredCO2")
    h1$yAxis(title = list(text = 'Percentage %'))
    h1$tooltip(valueSuffix= '%')
    h1$exporting(enabled = T)
    return (h1)
  })
  
  
  output$plotCO2 <- renderChart ({
    filterCO2Data()
    h1 <- Highcharts$new()
    h1$chart(type = "column")
    firstColData <- NULL
    secondColData <- NULL
    extended_title <- ""
    subtitle <- ""
    if (input$inMSG == 'sep'){
      # Keep the data separated
      firstColData = scCO2Data
      secondColData = scCO2FilteredData
      
      firstColName <- "Scenario (Total Population)"
      secondColName <- "Scenario (Sub-Population)"
      
      extended_title <- "Scenario - CO2 (kg) per week"
      
    }else{
      # Keep the data mixed
      firstColData = blCO2FilteredData
      secondColData = scCO2FilteredData
      
      firstColName <- "Baseline (Sub-Population)"
      secondColName <- "Scenario (Sub-Population)"
      
      extended_title <- "Sub-Population - CO2 (kg) per week"
    }
    subtitle <- getCO2FilteredTitle()
    h1$title(text = extended_title)
    bc <- NULL
    if (max(firstColData$data) > 0 && max(firstColData$data) > 0){
      bc <- as.data.frame(table (cut (firstColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(firstColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      
      h1$series(data = bc$Freq[-1], name = firstColName)
      bc <- as.data.frame(table (cut (secondColData$data, breaks = c(c(-1, 0, 10, 20, 50, 100, 200), max(secondColData$data)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      h1$series(data = bc$Freq[-1], name = secondColName)
      h1$subtitle(text = subtitle, style = list(font = 'bold 12px "Trebuchet MS", Verdana, sans-serif'))
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
    }
    
    h1$xAxis(categories = c("> 0 and <= 10", "> 10 and <= 20", "> 20 and <= 50", "> 50 and <= 100","> 100 and <= 200", "> 200"))
    h1$set(dom = "plotCO2")
    h1$yAxis(title = list(text = 'Percentage %'))
    h1$tooltip(valueSuffix= '%')
    
    h1$exporting(enabled = T)
    return (h1)
  })
  
  
  filterCO2Data <- reactive ({
    data <- co2data
    
    if (input$inCO2AG != 'All'){
      data <- subset(data, age_group == input$inCO2AG)
    }
    if (input$inCO2G != 3)
      data <- subset(data, Sex_B01ID %in% input$inCO2G)
    
    if (input$inCO2SES != "All"){
      data <- subset(data, NSSec_B03ID %in% input$inCO2SES)
    }
    
    if (input$inCO2Ethnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID %in% input$inCO2thnicity)
    }
    #data[is.na(data)] <- 0
    
    
    columnName <- paste(paste("MS", input$inCO2MS,sep = ""),  paste("ebik", input$inCO2EB,sep = ""), 
                        paste("eq", input$inCO2EQ,sep = ""), sep="_")
    
    #data1 <- milesCycled[,c("ID", "age_group","Sex_B01ID","NSSec_B03ID","EthGroupTS_B02ID", "baseline_milesCycled", columnName)]
    
    data1 <- data.frame(co2data[,columnName])
    
    data1$data <- data1[,1]
    data1[,1] <- NULL
    
    data2 <- data.frame(data[,columnName])
    
    data2$data <- data2[,1]
    data2[,1] <- NULL
    
    scCO2Data <<- data1
    scCO2FilteredData <<- data2
    
    blCO2Data <<- data.frame(data = co2data[,"baseline_co2"])
    blCO2FilteredData <<- data.frame(data = data[,"baseline_co2"])
    
    summary(dim(scCO2Data), " : ",  dim(scCO2FilteredData), "\n")
    
  })
  
  
  getCO2FilteredTitle <- function(){
    filtered_title <- ""
    if (input$inCO2AG != "All" || input$inCO2G != 3 || input$inCO2Ethnicity != "All" || input$inCO2SES != "All" ){
      displayGender <- "All"
      if (input$inCO2G == 1){
        displayGender <- "Male"
      }else if (input$inCO2G == 2){
        displayGender <- "Female"
      }
      
      displayEthnicity <- "All"
      if (input$inCO2Ethnicity == 1){
        displayEthnicity <- "White"
      }else if (input$inCO2Ethnicity == 2){
        displayEthnicity <- "Non-White"
      }
      
      displaySES <- "All"
      if (input$inCO2SES == 1){
        displaySES <- "Managerial and professional occupations"
      }else if (input$inCO2SES == 2){
        displaySES <- "Intermediate occupations and small employers"
      }else if (input$inCO2SES == 3){
        displaySES <- "Routine and manual occupations"
      }else if (input$inCO2SES == 4){
        displaySES <- "Never worked and long-term unemployed"
      }else if (input$inCO2SES == 5){
        displaySES <- "Not classified (including students)"
      }
      filtered_title <- paste("Age Group: ", str_trim(input$inCO2AG), ", Gender: ", displayGender, ", Socio Economic Classification: ", displaySES, " and Ethnicity: ", displayEthnicity, sep = "" )
      filtered_title
    }else
      filtered_title
  }
  
  
  
  getMilesCycledFilteredTitle <- function(){
    filtered_title <- ""
    if (input$inMSAG != "All" || input$inMSG != 3 || input$inMSEthnicity != "All" || input$inMSSES != "All" ){
      displayGender <- "All"
      if (input$inMSG == 1){
        displayGender <- "Male"
      }else if (input$inMSG == 2){
        displayGender <- "Female"
      }
      
      displayEthnicity <- "All"
      if (input$inMSEthnicity == 1){
        displayEthnicity <- "White"
      }else if (input$inMSEthnicity == 2){
        displayEthnicity <- "Non-White"
      }
      
      displaySES <- "All"
      if (input$inMSSES == 1){
        displaySES <- "Managerial and professional occupations"
      }else if (input$inMSSES == 2){
        displaySES <- "Intermediate occupations and small employers"
      }else if (input$inMSSES == 3){
        displaySES <- "Routine and manual occupations"
      }else if (input$inMSSES == 4){
        displaySES <- "Never worked and long-term unemployed"
      }else if (input$inMSSES == 5){
        displaySES <- "Not classified (including students)"
      }
      filtered_title <- paste("Age Group: ", str_trim(input$inMSAG), ", Gender: ", displayGender, ", Socio Economic Classification: ", displaySES, " and Ethnicity: ", displayEthnicity, sep = "" )
      filtered_title
    }else
      filtered_title
  }
  
  getCarMilesFilteredTitle <- function(){
    filtered_title <- ""
    if (input$inCMAG != "All" || input$inCMG != 3 || input$inCMEthnicity != "All" || input$inCMSES != "All" ){
      displayGender <- "All"
      if (input$inCMG == 1){
        displayGender <- "Male"
      }else if (input$inCMG == 2){
        displayGender <- "Female"
      }
      
      displayEthnicity <- "All"
      if (input$inCMEthnicity == 1){
        displayEthnicity <- "White"
      }else if (input$inCMEthnicity == 2){
        displayEthnicity <- "Non-White"
      }
      
      displaySES <- "All"
      if (input$inCMSES == 1){
        displaySES <- "Managerial and professional occupations"
      }else if (input$inCMSES == 2){
        displaySES <- "Intermediate occupations and small employers"
      }else if (input$inCMSES == 3){
        displaySES <- "Routine and manual occupations"
      }else if (input$inCMSES == 4){
        displaySES <- "Never worked and long-term unemployed"
      }else if (input$inCMSES == 5){
        displaySES <- "Not classified (including students)"
      }
      filtered_title <- paste("Age Group: ", str_trim(input$inCMAG), ", Gender: ", displayGender, ", Socio Economic Classification: ", displaySES, " and Ethnicity: ", displayEthnicity, sep = "" )
      filtered_title
    }else
      filtered_title
  }
  
#   observe({
#     
#     if (input$inBDEQ != 3){
#       updateTextInput(session, "inHealthEQ", NULL, input$inBDEQ)
#       updateTextInput(session, "inMETEQ", NULL, input$inBDEQ)
#       updateTextInput(session, "inMSEQ", NULL, input$inBDEQ)
#       updateTextInput(session, "inCMEQ", NULL, input$inBDEQ)
#       updateTextInput(session, "inCO2EQ", NULL, input$inBDEQ)
#       
#     }
#     
#     if (input$inHealthEQ != 3){
#       updateTextInput(session, "inBDEQ", NULL, input$inHealthEQ)
#       updateTextInput(session, "inMETEQ", NULL, input$inHealthEQ)
#       updateTextInput(session, "inMSEQ", NULL, input$inHealthEQ)
#       updateTextInput(session, "inCMEQ", NULL, input$inHealthEQ)
#       updateTextInput(session, "inCO2EQ", NULL, input$inHealthEQ)
#     }
#     
#     
#     
#     if (input$inMETEQ != 3){
#       updateTextInput(session, "inBDEQ", NULL, input$inMETEQ)
#       updateTextInput(session, "inHealthEQ", NULL, input$inMETEQ)
#       updateTextInput(session, "inMSEQ", NULL, input$inMETEQ)
#       updateTextInput(session, "inCMEQ", NULL, input$inMETEQ)
#       updateTextInput(session, "inCO2EQ", NULL, input$inMETEQ)
#     }
#     
#     if (input$inMSEQ != 3){
#       updateTextInput(session, "inBDEQ", NULL, input$inMSEQ)
#       updateTextInput(session, "inHealthEQ", NULL, input$inMSEQ)
#       updateTextInput(session, "inMETEQ", NULL, input$inMSEQ)
#       updateTextInput(session, "inCMEQ", NULL, input$inMSEQ)
#       updateTextInput(session, "inCO2EQ", NULL, input$inMSEQ)
#     }
#     
#     if (input$inCMEQ != 3){
#       updateTextInput(session, "inBDEQ", NULL, input$inCMEQ)
#       updateTextInput(session, "inHealthEQ", NULL, input$inCMEQ)
#       updateTextInput(session, "inMETEQ", NULL, input$inCMEQ)
#       updateTextInput(session, "inMSEQ", NULL, input$inCMEQ)
#       updateTextInput(session, "inCO2EQ", NULL, input$inCMEQ)
#     }
#     
#     if (input$inCO2EQ != 3){
#       updateTextInput(session, "inBDEQ", NULL, input$inCO2EQ)
#       updateTextInput(session, "inHealthEQ", NULL, input$inCO2EQ)
#       updateTextInput(session, "inMETEQ", NULL, input$inCO2EQ)
#       updateTextInput(session, "inMSEQ", NULL, input$inCO2EQ)
#       updateTextInput(session, "inCMEQ", NULL, input$inCO2EQ)
#     }
# 
#   })
  
  
  
  
  
  # EQ
  
  observeEvent(input$inBDEQ, function(){
    updateTextInput(session, "inHealthEQ", NULL, input$inBDEQ)
    updateTextInput(session, "inMETEQ", NULL, input$inBDEQ)
    updateTextInput(session, "inMSEQ", NULL, input$inBDEQ)
    updateTextInput(session, "inCMEQ", NULL, input$inBDEQ)
    updateTextInput(session, "inCO2EQ", NULL, input$inBDEQ)
  })
  
  observeEvent(input$inHealthEQ, function(){
    updateTextInput(session, "inBDEQ", NULL, input$inHealthEQ)
    updateTextInput(session, "inMETEQ", NULL, input$inHealthEQ)
    updateTextInput(session, "inMSEQ", NULL, input$inHealthEQ)
    updateTextInput(session, "inCMEQ", NULL, input$inHealthEQ)
    updateTextInput(session, "inCO2EQ", NULL, input$inHealthEQ)
  })
  
  observeEvent(input$inMETEQ, function(){
    updateTextInput(session, "inBDEQ", NULL, input$inMETEQ)
    updateTextInput(session, "inHealthEQ", NULL, input$inMETEQ)
    updateTextInput(session, "inMSEQ", NULL, input$inMETEQ)
    updateTextInput(session, "inCMEQ", NULL, input$inMETEQ)
    updateTextInput(session, "inCO2EQ", NULL, input$inMETEQ)
  })
  
  observeEvent(input$inMSEQ, function(){
    updateTextInput(session, "inBDEQ", NULL, input$inMSEQ)
    updateTextInput(session, "inHealthEQ", NULL, input$inMSEQ)
    updateTextInput(session, "inMETEQ", NULL, input$inMSEQ)
    updateTextInput(session, "inCMEQ", NULL, input$inMSEQ)
    updateTextInput(session, "inCO2EQ", NULL, input$inMSEQ)
  })
  
  observeEvent(input$inCMEQ, function(){
    updateTextInput(session, "inBDEQ", NULL, input$inCMEQ)
    updateTextInput(session, "inHealthEQ", NULL, input$inCMEQ)
    updateTextInput(session, "inMETEQ", NULL, input$inCMEQ)
    updateTextInput(session, "inMSEQ", NULL, input$inCMEQ)
    updateTextInput(session, "inCO2EQ", NULL, input$inCMEQ)
  })
  
  observeEvent(input$inCO2EQ, function(){
    updateTextInput(session, "inBDEQ", NULL, input$inCO2EQ)
    updateTextInput(session, "inHealthEQ", NULL, input$inCO2EQ)
    updateTextInput(session, "inMETEQ", NULL, input$inCO2EQ)
    updateTextInput(session, "inMSEQ", NULL, input$inCO2EQ)
    updateTextInput(session, "inCMEQ", NULL, input$inCO2EQ)
  })
  
  # inBDEQ
  # inHealthEQ
  # inMETEQ
  # inMSEQ
  # inCMEQ
  # inCO2EB
  
  # EB
  
  observeEvent(input$inBDEB, function(){
    updateTextInput(session, "inHealthEB", NULL, input$inBDEB)
    updateTextInput(session, "inMETEB", NULL, input$inBDEB)
    updateTextInput(session, "inMSEB", NULL, input$inBDEB)
    updateTextInput(session, "inCMEB", NULL, input$inBDEB)
    updateTextInput(session, "inCO2EB", NULL, input$inBDEB)
  })
  
  observeEvent(input$inHealthEB, function(){
    updateTextInput(session, "inBDEB", NULL, input$inHealthEB)
    updateTextInput(session, "inMETEB", NULL, input$inHealthEB)
    updateTextInput(session, "inMSEB", NULL, input$inHealthEB)
    updateTextInput(session, "inCMEB", NULL, input$inHealthEB)
    updateTextInput(session, "inCO2EB", NULL, input$inHealthEB)
  })
  
  observeEvent(input$inMETEB, function(){
    updateTextInput(session, "inBDEB", NULL, input$inMETEB)
    updateTextInput(session, "inHealthEB", NULL, input$inMETEB)
    updateTextInput(session, "inMSEB", NULL, input$inMETEB)
    updateTextInput(session, "inCMEB", NULL, input$inMETEB)
    updateTextInput(session, "inCO2EB", NULL, input$inMETEB)
  })
  
  observeEvent(input$inMSEB, function(){
    updateTextInput(session, "inBDEB", NULL, input$inMSEB)
    updateTextInput(session, "inHealthEB", NULL, input$inMSEB)
    updateTextInput(session, "inMETEB", NULL, input$inMSEB)
    updateTextInput(session, "inCMEB", NULL, input$inMSEB)
    updateTextInput(session, "inCO2EB", NULL, input$inMSEB)
  })
  
  observeEvent(input$inCMEB, function(){
    updateTextInput(session, "inBDEB", NULL, input$inCMEB)
    updateTextInput(session, "inHealthEB", NULL, input$inCMEB)
    updateTextInput(session, "inMETEB", NULL, input$inCMEB)
    updateTextInput(session, "inMSEB", NULL, input$inCMEB)
    updateTextInput(session, "inCO2EB", NULL, input$inCMEB)
  })
  
  observeEvent(input$inCO2EB, function(){
    updateTextInput(session, "inBDEB", NULL, input$inCO2EB)
    updateTextInput(session, "inHealthEB", NULL, input$inCO2EB)
    updateTextInput(session, "inMETEB", NULL, input$inCO2EB)
    updateTextInput(session, "inMSEB", NULL, input$inCO2EB)
    updateTextInput(session, "inCMEB", NULL, input$inCO2EB)
  })
  
  # inBDEB
  # inHealthEB
  # inMETEB
  # inMSEB
  # inCMEB
  # inCO2EB
  
  observeEvent(input$inBDMS, function(){
    updateTextInput(session, "inHealthMS", NULL, input$inBDMS)
    updateTextInput(session, "inMETMS", NULL, input$inBDMS)
    updateTextInput(session, "inMSMS", NULL, input$inBDMS)
    updateTextInput(session, "inCMMS", NULL, input$inBDMS)
    updateTextInput(session, "inCO2MS", NULL, input$inBDMS)
  })
  
  observeEvent(input$inHealthMS, function(){
    updateTextInput(session, "inBDMS", NULL, input$inHealthMS)
    updateTextInput(session, "inMETMS", NULL, input$inHealthMS)
    updateTextInput(session, "inMSMS", NULL, input$inHealthMS)
    updateTextInput(session, "inCMMS", NULL, input$inHealthMS)
    updateTextInput(session, "inCO2MS", NULL, input$inHealthMS)
  })
  
  observeEvent(input$inMETMS, function(){
    updateTextInput(session, "inBDMS", NULL, input$inMETMS)
    updateTextInput(session, "inHealthMS", NULL, input$inMETMS)
    updateTextInput(session, "inMSMS", NULL, input$inMETMS)
    updateTextInput(session, "inCMMS", NULL, input$inMETMS)
    updateTextInput(session, "inCO2MS", NULL, input$inMETMS)
  })
  
  observeEvent(input$inMSMS, function(){
    updateTextInput(session, "inBDMS", NULL, input$inMSMS)
    updateTextInput(session, "inHealthMS", NULL, input$inMSMS)
    updateTextInput(session, "inMETMS", NULL, input$inMSMS)
    updateTextInput(session, "inCMMS", NULL, input$inMSMS)
    updateTextInput(session, "inCO2MS", NULL, input$inMSMS)
  })
  
  observeEvent(input$inCMMS, function(){
    updateTextInput(session, "inBDMS", NULL, input$inCMMS)
    updateTextInput(session, "inHealthMS", NULL, input$inCMMS)
    updateTextInput(session, "inMETMS", NULL, input$inCMMS)
    updateTextInput(session, "inMSMS", NULL, input$inCMMS)
    updateTextInput(session, "inCO2MS", NULL, input$inCMMS)
  })
  
  observeEvent(input$inCO2MS, function(){
    updateTextInput(session, "inBDMS", NULL, input$inCO2MS)
    updateTextInput(session, "inHealthMS", NULL, input$inCO2MS)
    updateTextInput(session, "inMETMS", NULL, input$inCO2MS)
    updateTextInput(session, "inMSMS", NULL, input$inCO2MS)
    updateTextInput(session, "inCMMS", NULL, input$inCO2MS)
  })
  
  
  # age
  
  observeEvent(input$inBDAG, function(){
    updateTextInput(session, "mag", NULL, input$inBDAG)
    updateTextInput(session, "inMSAG", NULL, input$inBDAG)
    updateTextInput(session, "inCMAG", NULL, input$inBDAG)
    updateTextInput(session, "inCO2AG", NULL, input$inBDAG)
    
  })
  
  observeEvent(input$mag, function(){
    updateTextInput(session, "inBDAG", NULL, input$mag)
    updateTextInput(session, "inMSAG", NULL, input$mag)
    updateTextInput(session, "inCMAG", NULL, input$mag)
    updateTextInput(session, "inCO2AG", NULL, input$mag)
  })
  
  observeEvent(input$inMSAG, function(){
    updateTextInput(session, "inBDAG", NULL, input$inMSAG)
    updateTextInput(session, "mag", NULL, input$inMSAG)
    updateTextInput(session, "inCMAG", NULL, input$inMSAG)
    updateTextInput(session, "inCO2AG", NULL, input$inMSAG)
  })
  
  observeEvent(input$inCMAG, function(){
    updateTextInput(session, "inBDAG", NULL, input$inCMAG)
    updateTextInput(session, "mag", NULL, input$inCMAG)
    updateTextInput(session, "inMSAG", NULL, input$inCMAG)
    updateTextInput(session, "inCO2AG", NULL, input$inCMAG)
  })
  
  observeEvent(input$inCO2AG, function(){
    updateTextInput(session, "inBDAG", NULL, input$inCO2AG)
    updateTextInput(session, "mag", NULL, input$inCO2AG)
    updateTextInput(session, "inMSAG", NULL, input$inCO2AG)
    updateTextInput(session, "inCMAG", NULL, input$inCO2AG)
  })
  
  #   inBDAG
  #   mag
  #   inMSAG
  #   inCMAG
  #   inCO2AG
  
  
  # gender
  
  observeEvent(input$inBDGender, function(){
    updateTextInput(session, "inHealthG", NULL, input$inBDGender)
    updateTextInput(session, "mgender", NULL, input$inBDGender)
    updateTextInput(session, "inMSG", NULL, input$inBDGender)
    updateTextInput(session, "inCMG", NULL, input$inBDGender)
    updateTextInput(session, "inCO2G", NULL, input$inBDGender)
  })
  
  observeEvent(input$inHealthG, function(){
    updateTextInput(session, "inBDGender", NULL, input$inHealthG)
    updateTextInput(session, "mgender", NULL, input$inHealthG)
    updateTextInput(session, "inMSG", NULL, input$inHealthG)
    updateTextInput(session, "inCMG", NULL, input$inHealthG)
    updateTextInput(session, "inCO2G", NULL, input$inHealthG)
  })
  
  observeEvent(input$mgender, function(){
    updateTextInput(session, "inBDGender", NULL, input$mgender)
    updateTextInput(session, "inHealthG", NULL, input$mgender)
    updateTextInput(session, "inMSG", NULL, input$mgender)
    updateTextInput(session, "inCMG", NULL, input$mgender)
    updateTextInput(session, "inCO2G", NULL, input$mgender)
    
  })
  
  observeEvent(input$inMSG, function(){
    updateTextInput(session, "inBDGender", NULL, input$inMSG)
    updateTextInput(session, "inHealthG", NULL, input$inMSG)
    updateTextInput(session, "mgender", NULL, input$inMSG)
    updateTextInput(session, "inCMG", NULL, input$inMSG)
    updateTextInput(session, "inCO2G", NULL, input$inMSG)
  })
  
  observeEvent(input$inCMG, function(){
    updateTextInput(session, "inBDGender", NULL, input$inCMG)
    updateTextInput(session, "inHealthG", NULL, input$inCMG)
    updateTextInput(session, "mgender", NULL, input$inCMG)
    updateTextInput(session, "inMSG", NULL, input$inCMG)
    updateTextInput(session, "inCO2G", NULL, input$inCMG)
  })
  
  observeEvent(input$inCO2G, function(){
    updateTextInput(session, "inBDGender", NULL, input$inCO2G)
    updateTextInput(session, "inHealthG", NULL, input$inCO2G)
    updateTextInput(session, "mgender", NULL, input$inCO2G)
    updateTextInput(session, "inMSG", NULL, input$inCO2G)
    updateTextInput(session, "inCMG", NULL, input$inCO2G)
  })
  
  #   inBDGender
  #   inHealthG
  #   mgender
  #   inMSG
  #   inCMG
  #   inCO2G
  
  
  # SES
  
  observeEvent(input$inBDSES, function(){
    updateTextInput(session, "mses", NULL, input$inBDSES)
    updateTextInput(session, "inMSSES", NULL, input$inBDSES)
    updateTextInput(session, "inCMSES", NULL, input$inBDSES)
    updateTextInput(session, "inCO2SES", NULL, input$inBDSES)
  })
  
  
  observeEvent(input$mses, function(){
    updateTextInput(session, "inBDSES", NULL, input$mses)
    updateTextInput(session, "inMSSES", NULL, input$mses)
    updateTextInput(session, "inCMSES", NULL, input$mses)
    updateTextInput(session, "inCO2SES", NULL, input$mses)
  })
  
  observeEvent(input$inMSSES, function(){
    updateTextInput(session, "inBDSES", NULL, input$inMSSES)
    updateTextInput(session, "mses", NULL, input$inMSSES)
    updateTextInput(session, "inCMSES", NULL, input$inMSSES)
    updateTextInput(session, "inCO2SES", NULL, input$inMSSES)
  })
  
  observeEvent(input$inCMSES, function(){
    updateTextInput(session, "inBDSES", NULL, input$inCMSES)
    updateTextInput(session, "mses", NULL, input$inCMSES)
    updateTextInput(session, "inMSSES", NULL, input$inCMSES)
    updateTextInput(session, "inCO2SES", NULL, input$inCMSES)
  })
  
  observeEvent(input$inCO2SES, function(){
    updateTextInput(session, "inBDSES", NULL, input$inCO2SES)
    updateTextInput(session, "mses", NULL, input$inCO2SES)
    updateTextInput(session, "inMSSES", NULL, input$inCO2SES)
    updateTextInput(session, "inCMSES", NULL, input$inCO2SES)
  })
  
  
  #   inBDSES
  #   mses
  #   inMSSES
  #   inCMSES
  #   inCO2SES
  
  #Ethnicity
  
  
  observeEvent(input$inBDEthnicity, function(){
    updateTextInput(session, "methnicity", NULL, input$inBDEthnicity)
    updateTextInput(session, "inMSEthnicity", NULL, input$inBDEthnicity)
    updateTextInput(session, "inCMEthnicity", NULL, input$inBDEthnicity)
    updateTextInput(session, "inCO2Ethnicity", NULL, input$inBDEthnicity)
  })
  
  observeEvent(input$methnicity, function(){
    updateTextInput(session, "inBDEthnicity", NULL, input$methnicity)
    updateTextInput(session, "inMSEthnicity", NULL, input$methnicity)
    updateTextInput(session, "inCMEthnicity", NULL, input$methnicity)
    updateTextInput(session, "inCO2Ethnicity", NULL, input$methnicity)
    
  })
  
  observeEvent(input$inMSEthnicity, function(){
    updateTextInput(session, "inBDEthnicity", NULL, input$inMSEthnicity)
    updateTextInput(session, "methnicity", NULL, input$inMSEthnicity)
    updateTextInput(session, "inCMEthnicity", NULL, input$inMSEthnicity)
    updateTextInput(session, "inCO2Ethnicity", NULL, input$inMSEthnicity)
  })
  
  observeEvent(input$inCMEthnicity, function(){
    updateTextInput(session, "inBDEthnicity", NULL, input$inCMEthnicity)
    updateTextInput(session, "methnicity", NULL, input$inCMEthnicity)
    updateTextInput(session, "inMSEthnicity", NULL, input$inCMEthnicity)
    updateTextInput(session, "inCO2Ethnicity", NULL, input$inCMEthnicity)
  })
  
  observeEvent(input$inCO2Ethnicity, function(){
    updateTextInput(session, "inBDEthnicity", NULL, input$inCO2Ethnicity)
    updateTextInput(session, "methnicity", NULL, input$inCO2Ethnicity)
    updateTextInput(session, "inMSEthnicity", NULL, input$inCO2Ethnicity)
    updateTextInput(session, "inCMEthnicity", NULL, input$inCO2Ethnicity)
  })
  
  #   inBDEthnicity
  #   methnicity
  #   inMSEthnicity
  #   inCMEthnicity
  #   inCO2Ethnicity
  
  
  observeEvent(input$flipMS, function(){
    updateTextInput(session, "flipMETHG", NULL, input$flipMS)
    updateTextInput(session, "inMSflip", NULL, input$flipMS)
    updateTextInput(session, "inCMflip", NULL, input$flipMS)
    updateTextInput(session, "inCO2flip", NULL, input$flipMS)
  })
  
  observeEvent(input$flipMETHG, function(){
    updateTextInput(session, "flipMS", NULL, input$flipMETHG)
    updateTextInput(session, "inMSflip", NULL, input$flipMETHG)
    updateTextInput(session, "inCMflip", NULL, input$flipMETHG)
    updateTextInput(session, "inCO2flip", NULL, input$flipMETHG)
    
  })
  
  observeEvent(input$inMSflip, function(){
    updateTextInput(session, "flipMS", NULL, input$inMSflip)
    updateTextInput(session, "flipMETHG", NULL, input$inMSflip)
    updateTextInput(session, "inCMflip", NULL, input$inMSflip)
    updateTextInput(session, "inCO2flip", NULL, input$flipMS)
    
  })
  
  observeEvent(input$inCMflip, function(){
    updateTextInput(session, "flipMS", NULL, input$inCMflip)
    updateTextInput(session, "flipMETHG", NULL, input$inCMflip)
    updateTextInput(session, "inMSflip", NULL, input$inCMflip)
    updateTextInput(session, "inCO2flip", NULL, input$inCMflip)
  })
  
  observeEvent(input$inCO2flip, function(){
    updateTextInput(session, "flipMS", NULL, input$inCO2flip)
    updateTextInput(session, "flipMETHG", NULL, input$inCO2flip)
    updateTextInput(session, "inMSflip", NULL, input$inCO2flip)
    updateTextInput(session, "inCMflip", NULL, input$inCO2flip)
  })
  
  
  #flipMS
  #flipMETHG
  #inMSflip
  #inCMflip
  #inCO2flip
  
})