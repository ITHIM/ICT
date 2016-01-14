library(shiny)
library(DT)
library(devtools)
if (!require(rCharts)) {
  install_github('rCharts', 'ramnathv')
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
scCarMilesData <- NULL
scCarMilesFilteredData <- NULL
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
    if (input$inHealthG != 'All'){
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
  
  filterCarMilesData <- reactive ({
    # ID  age	Sex_B01ID	NSSec_B03ID	EthGroupTS_B02ID
    data <- carMiles
    
    if (input$inCMAG != 'All'){
      data <- subset(data, age == input$inCMAG)
    }
    if (input$inCMGender != 3)
      data <- subset(data, Sex_B01ID %in% input$inCMGender)
    
    if (input$inCMSES != "All"){
      data <- subset(data, NSSec_B03ID %in% input$inCMSES)
    }
    
    if (input$inCMEthnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID %in% input$inCMEthnicity)
    }
    data[is.na(data)] <- 0
    #     cat(input$inCMAG, "\n")
    #     cat(nrow(data), ":", nrow(carMiles), "\n")
    
    columnName <- paste(paste("MS", input$inTTMS,sep = ""),  paste("ebik", input$inTTEB,sep = ""), 
                        paste("eq", input$inTTEQ,sep = ""), sep="_")
    
    
    data1 <- carMiles[,c("ID", "age","Sex_B01ID","NSSec_B03ID","EthGroupTS_B02ID", "baseline", columnName)]
    
    names(data1)[names(data1) == columnName] <- 'scenario'
    
    data1 <- arrange(data1, scenario)
    
    data2 <- data[,c("ID", "age","Sex_B01ID","NSSec_B03ID","EthGroupTS_B02ID", "baseline", columnName)]
    
    names(data2)[names(data2) == columnName] <- 'scenario'
    
    data2 <- arrange(data2, scenario)
    
    scCarMilesData <<- data1
    scCarMilesFilteredData <<- data2
    
  })
  
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
  
  output$plotMode <- renderChart({
    plotBLDataTable()
    if (!is.null(bldata)){
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      h1$plotOptions(column=list(animation=FALSE))
      
      filtered_title <- getBaselineFilteredTitle(tdata)
      #cat(filtered_title, "\n" )
      extended_title <- paste("Main Mode: Total population versus Selected Population")
      #(selected population currently defined as ", filtered_title, ")", sep = "")
      h1$title(text = extended_title)
      bcounts <- count(tdata, "MainMode_reduced_val")
      h1$xAxis(categories = bcounts[["MainMode_reduced_val"]], title = list(text = 'Main Mode'))#, style = list(font = 'bold 14px')))
      
      h1$tooltip(valueSuffix= '%')
      
      bcounts$tp <- bcounts$freq / sum(bcounts$freq) * 100
      bcounts$tp <- round(bcounts$tp, digits = 1)
      bcounts$freq <- NULL
      
      scounts <- count(bldata, "MainMode_reduced_val")
      filter <- FALSE
      if (sum(scounts$freq, na.rm = T) >= 10)
        filter <- TRUE
      
      scounts$freq1 <- scounts$freq / sum(scounts$freq) * 100
      scounts$freq1 <- round(scounts$freq1, digits = 1)
      
      ecounts <- data.frame(v1=bcounts$tp, Filtered_Frequency=scounts[match(bcounts$MainMode_reduced_val, scounts$MainMode_reduced_val), 3])
      
      bcounts[["Total Population"]] <- bcounts$tp
      bcounts$tp <- NULL
      
      bcounts[["Selected Population"]] <- ecounts$Filtered_Frequency
      h1$series(data = bcounts[["Total Population"]], name = "Total Population")
      if(filter){
        h1$series(data = bcounts[["Selected Population"]], name = "Selected Population")
        h1$yAxis(min = 0, max = max(80, max(bcounts[["Total Population"]], na.rm=TRUE), max(bcounts[["Selected Population"]], na.rm=TRUE)), tickInterval = 20, title = list(text = 'Percentage %'))
        h1$subtitle(text = paste("Selected Population: ", filtered_title), style = list(font = 'bold 12px "Trebuchet MS", Verdana, sans-serif'))
        
      }else{
        h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
        h1$yAxis(min = 0, max = max(80, max(bcounts[["Total Population"]], na.rm=TRUE)), tickInterval = 20, title = list(text = 'Percentage %'))
      }
      
      h1$set(dom = 'plotMode')
      h1$exporting(enabled = T)
      return (h1)
    }
  })
  
  
  
  output$plotMET <- renderChart({
    input$flipMETHG
    input$phyGuideline
    
    plotMETDataTable()
    
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
        
      }else{
        # Keep the data mixed
        firstColData = bMETdata
        secondColData = scMETdata
        
        extended_title <- paste("Baseline Versus Scenario - Marginal MET Hours", sep = "")
        
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
    h1$set(dom = 'plotMET')
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
        extended_title <- paste("Baseline versus Scenario - Marginal MET hour of Filtered Population")
        
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
    h1$set(dom = 'plotScenarioMET')
    h1$exporting(enabled = T)
    return(h1)
    
  })
  output$plotYLL <- renderChart({
    filterHealthData()
    h1 <- Highcharts$new()
    h1$chart(type = "column")
    
    
    if (nrow(scYllReductionData) > 0){
      
      #       a <- hPlot(freq ~ Exer, data = plyr::count(MASS::survey, c('Sex', 'Exer')), type = 'bar', group = 'Sex', group.na = 'NA\'s')
      #       a$show('inline', include_assets = TRUE, cdn = TRUE)
      #       
      #       
      #       h1$xAxis(categories = paste(scYllData$gender, scYllData$age.band, sep = " "), 
      #                title = list(text = 'Years of Life Lost (YLL)'))
      #       
      #       h1$xAxis(categories = list(name = c('20 - 29','40 - 49','60+'),
      #                                  categories = c("Male", "Female")),
      #                title = list(text = 'Years of Life Lost (YLL)'))
      #     xAxis: {
      #       categories: [{
      #         name: "Fruit",
      #         categories: ["Apple", "Banana", "Orange"]
      #       }, {
      #         name: "Vegetable",
      #         categories: ["Carrot", "Potato", "Tomato"]
      #       }, {
      #         name: "Fish",
      #         categories: ["Cod", "Salmon", "Tuna"]
      #       }]
      #     }
      
      
      
      # scYllData$case <- c(1:2)
      # For both gender, create new series
      ugender <- unique(scYllData$gender)
      for (i in 1:length(ugender)){
        data <- subset(scYllData, gender == ugender[i])
        h1$series(data = data$scenario, name = ugender[i])
      }
    
      # h1$series(data = scYllData$scenario, name = "YLL")
      #       h1$series(data = list(
      #         list(subset(scYllData, gender %in% "Male")$scenario, color = "lightblue"),
      #         list(subset(scYllData, gender %in% "Female")$scenario, color = "lightgreen")
      #       ))
      # paste(scYllData$gender, scYllData$age.band, sep = " ")
      h1$xAxis(categories = scYllData$age.band, title = list(text = 'Years of Life Lost (YLL)'))
      if (length(unique(scYllData$age.band)) > 1)
        h1$xAxis(categories = unique(scYllData$age.band), title = list(text = 'Years of Life Lost (YLL)'))
      
      
      h1$yAxis(title = list(text = 'YLL (Absolute Numbers)'))
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size = 0)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
    }
    
    
    #     h1 <- rCharts::Highcharts$new()
    #     h1$series(data = list(
    #       list(y = 8, url = "https://github.com/metagraf/rHighcharts", color = "lightblue"),
    #       list(y = 14, url = "https://github.com/metagraf/rVega", color = "lightpink"),
    #       list(y = 71, url = "https://github.com/ramnathv/rCharts", color = "lightgreen")
    #     ), type = "column", name = "Number of Stars")
    #     h1$plotOptions(column = list(cursor = 'pointer', point = list(events = list(click = "#! function() { location.href = this.options.url; } !#"))))
    #     h1$xAxis(categories = c("rHighcharts", "rVega", "rCharts"), title = list(text = ""))
    #     h1$yAxis(title = list(text = ""))
    #     h1$legend(enabled = F)
    #     h1$show('inline', include_assets = TRUE, cdn = TRUE)
    
    h1$title(text = "Years of Life Lost (YLL)")
    h1$set(dom = 'plotYLL')
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
      
      # cat("scYllReductionData$age.band[1] ", scYllReductionData$age.band[2] , "\n")
      h1$xAxis(categories = append(input$inHealthAG, " "), title = list(text = 'Reduction in Years of Years of Life Lost (YLL)'))
      if (length(unique(scYllReductionData$age.band)) > 2)
        h1$xAxis(categories = unique(scYllReductionData$age.band[-1]), title = list(text = 'Reduction in Years of Years of Life Lost (YLL)'))
      
      
      # h1$xAxis(categories = scYllReductionData$age.band, title = list(text = 'Reduction in Years of Life Lost (YLL)'))
      #h1$series(data = scYllReductionData$scenario, name = "Reduction in YLL(%)")
      h1$yAxis(title = list(text = 'Percentage (%)'))
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size = 0)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
    }
    
    h1$title(text = "Reduction in Years of Life Lost (YLL)")
    h1$tooltip(valueSuffix= '%')
    h1$set(dom = 'plotYLLReduction')
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
        h1$set(dom = 'plotBaseline')
        h1$exporting(enabled = T)
        return(h1)
      }else{
        h1 <- Highcharts$new()
        h1$set(dom = 'plotBaseline')
        h1$exporting(enabled = T)
        return(h1)
      }
    }
  })
  
  output$plotTripTime <- renderChart({
    filterTimeTravelData()
    #scenariosTimeTravelIdata
    if (!is.null(scFilteredTimeTraveldata) && length(scFilteredTimeTraveldata) > 0){
      
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      h1$plotOptions(column=list(animation=FALSE))
      
      #       minv <- min(scFilteredTimeTraveldata[["timetravel"]])
      #       maxv <- max(scFilteredTimeTraveldata[["timetravel"]])
      if (length (scFilteredTimeTraveldata$timetravel) > 0){
        #         data.decile <- cut(scFilteredTimeTraveldata$timetravel, breaks = quantile(scFilteredTimeTraveldata$timetravel, probs= seq(0, 1, by= 0.1)), include.lowest=T, labels = c(1:10))
        
        data.decile <- cut2(scFilteredTimeTraveldata$timetravel, g = 10)
        
        #       bcounts <- as.data.frame(table(cut(scFilteredTimeTraveldata$timetravel, breaks=seq(minv,maxv, by=2))))
        
        #       h1$series(data = bcounts$Freq, name = "Time Travel (Hours)")
        h1$series(data = as.data.frame(table(data.decile))$Freq, name = "Time Travel (Hours)")
        h1$xAxis(categories = c(1:10), title = "Decile")
        h1$yAxis(title = list(text = '# of Hours'))
        
      }
      
      h1$set(dom = 'plotTripTime')
      h1$exporting(enabled = T)
      return(h1)
    }
    
  })
  
  
  output$plotTripTimeDifference <- renderChart({
    tripTimeData()
    #scenariosTimeTravelIdata
    h1 <- Highcharts$new()
    h1$chart(type = "column")
    h1$yAxis(title = list(text = 'Percentage %'))
    #              , min = 0, max = 80 )
    
    if (!is.null(scFilteredTripTimeTraveldata) && nrow(scFilteredTripTimeTraveldata) > 0){
      
      h1$plotOptions(column = list(animation=FALSE,
                                   dataLabels = list(enabled = T, 
                                                     formatter = format_function,      
                                                     rotation = -90, 
                                                     align = 'right', 
                                                     color = '#FFFFFF', 
                                                     width = 100,
                                                     x = 4, 
                                                     y = 10, 
                                                     style = list(fontSize = '11px', fontFamily = 'Verdana, sans-serif'))),
                     series = list(dataLabels = list(crop = F))
      )
      
      dhist <- hist(scFilteredTripTimeTraveldata$diff)
      
      data <- data.frame(breaks = dhist$breaks[-1], counts = dhist$counts, 0)
      data$freq <- round(data$counts / sum(data$counts) * 100, digits = 1)
      data <- subset(data, freq >= 0.1)
      data <- subset(data, breaks != 0)
      h1$title(text = "Histogram of Relative Changes in Trip Durations for Trips now Cycled")
      h1$series(data =  data$freq, name = "Time Difference from Baseline (%)")
      
      if (nrow(data) == 1){
        xlabel <- data$breaks
        h1$xAxis(categories = c("%") , labels = list(style = list(fontSize = '10px', 
                                                                  fontFamily = 'Verdana, sans-serif',
                                                                  whiteSpace = 'nowrap',
                                                                  paddingLeft = '10px',
                                                                  paddingRight = '10px',
                                                                  paddingTop = '10px'))
        )
        
      }else{
        
        h1$xAxis(categories = paste(data$breaks, "%"))
      }
      
      h1$tooltip(valueSuffix= '%')
      
    }else{
      h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size = 0)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
    }
    
    h1$set(dom = 'plotTripTimeDifference')
    h1$exporting(enabled = T)
    return(h1)
    
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
    h$set(dom = 'plotCycPercent')
    return (h)
  })
  
  output$plotGenericVariable <- renderChart({
    generateScenarioTable()
    #retrieveVariableName()
    h <- genericPlot(input$varname)
    h$title(text = input$varname)
    h$set(dom = 'plotGenericVariable')
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
    
    h1$set(dom = 'plotBDMode')
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
    
    h1$set(dom = 'plotBDSCMode')
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
      
      h1$set(dom = 'plotBDFasterTrips')
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
      
      h1$set(dom = 'plotBDSlowerTrips')
      h1$exporting(enabled = T)
      return (h1)
    }
  })
  
  output$plotCarTripsCycled <- renderChart ({
    filterCarMilesData()
    h1 <- Highcharts$new()
    h1$chart(type = "column")
    
    if (!is.null(scCarMilesData) && !is.null(scCarMilesFilteredData)){
      h1$plotOptions(column=list(animation=FALSE))
      
      h1$title(text = "Car Miles:  Histogram of Car Miles in the Selected Scenario")
      
      #       data.decile <- cut2(scCarMilesFilteredData$scenario, g = 10)
      #       h1$series(data = as.data.frame(table(data.decile))$Freq, name = "Car Trips (Miles)")
      #       h1$xAxis(categories = c(1:10), title = "Decile")
      #       h1$yAxis(title = list(text = 'Miles'))
      
      #       dhit <- hist(scCarMilesFilteredData$scenario)
      #       
      #       data <- data.frame(breaks = dhit$breaks[-1], counts = dhit$counts, 0)
      #       data$freq <- round(data$counts / sum(data$counts) * 100, digits = 1)
      #       #       data <- subset(data, freq >= 0.1)
      #       data <- subset(data, breaks != 0)
      #       
      #       h1$series(data =  data$freq, name = "Car Miles")
      #       
      #       h1$xAxis(categories = data$breaks)
      #       
      #       h1$yAxis(title = list(text = 'Percentage %'))
      #       h1$tooltip(valueSuffix= '%')
      
      bc <- as.data.frame(table (cut (scCarMilesFilteredData$scenario, breaks = c(seq(0,300, 50), max(scCarMilesFilteredData$scenario)))))
      bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
      bc1max <- max(bc$Freq, na.rm = T)
      
      h1$xAxis(categories = as.list(append(c(seq(0,300, 50))[-1], "300+")), title = list(text = 'Car Miles'))
      h1$yAxis(title = list(text = 'Percentage %'))
      
      h1$series(data = bc$Freq, name = "Car Miles")
      h1$tooltip(valueSuffix= '%')
      
      
    }
    h1$set(dom = 'plotCarTripsCycled')
    h1$exporting(enabled = T)
    return (h1)
    
  })
  
  
  output$plotMilesCycled <- renderChart ({
    filterMilesCycledData()
    h1 <- Highcharts$new()
    h1$chart(type = "column")
    firstColData <- NULL
    secondColData <- NULL
    if (input$inMSflip == 'sep'){
      # Keep the data separated
      firstColData = blMilesCycledData
      secondColData = scMilesCycledData
      
      firstColName <- "Scenario (Total Population)"
      #secondColName <- "Scenario (Sub-Population)"
      
      #extended_title <- "Scenario - Mode Share"
      
    }else{
      # Keep the data mixed
      firstColData = blMilesCycledFilteredData
      secondColData = scMilesCycledFilteredData
      
      firstColName <- "Baseline (Sub-Population)" # "Scenario (Total Population)"
      #secondColName <- "Scenario (Sub-Population)"
      
      #extended_title <- "Sub-population - Mode Share"
    }
    
    #     data <- count(firstColData, "scenario")
    #     data$freq <- data$freq / sum(data$freq) * 100
    #     data$freq <- round(data$freq, digits = 1)
    
    # data <- subset(firstColData, scenario != 0)
    
    # dhist <- hist(subset(firstColData, scenario != 0)$scenario)
    
    #cat("Summary: ", summary(firstColData))
    
    # td <<- firstColData
    
#     dhist <- hist(firstColData$scenario)
#     data <- data.frame(breaks = dhist$breaks[-1], counts = dhist$counts)
#     data$freq <- round(data$counts / sum(data$counts) * 100, digits = 1)
#     data <- subset(data, freq > 0)
#     data <- subset(data, breaks != dhist$breaks[2])
#     #h1$title(text = "Histogram of Relative Changes in Trip Durations for Trips now Cycled")
#     h1$series(data =  data$freq, name = "Scenario")
#     h1$xAxis(categories = data$breaks)
    
    #MS2_ebik0_eq0
    
    h1$title(text = "Total Miles Cycled by Cyclists per week")
    bc <- as.data.frame(table (cut (firstColData$baseline_milesCycled, breaks = c(c(-1, 0, 2, 5, 10, 20, 40, 60), max(firstColData$baseline_milesCycled)))))
    
    bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
    # bc1max <- max(bc$Freq, na.rm = T)
    
    #h1$xAxis(categories = as.list(append(c(seq(seq(-5,60, 5))[-1], "> 60"))), title = list(text = 'Scenario'))
    h1$xAxis(categories = bc$Var1[-1])#c(2, 5, 10, 20, 40, 60, " > 60"))
    h1$series(data = bc$Freq[-1], name = "Baseline")
    
    bc <- as.data.frame(table (cut (secondColData$scenario, breaks = c(c(-1, 0, 2, 5, 10, 20, 40, 60), max(secondColData$scenario)))))
    bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
    h1$series(data = bc$Freq[-1], name = "Scenario")

    h1$set(dom = 'plotMilesCycled')
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
    
    data1$scenario <- data1[,1]
    data1[,1] <- NULL
    
    data2 <- data.frame(data[,columnName])
    
    data2$scenario <- data2[,1]
    data2[,1] <- NULL
    
    scMilesCycledData <<- data1
    scMilesCycledFilteredData <<- data2

    blMilesCycledData <<- data.frame(baseline_milesCycled = milesCycled[,"baseline_milesCycled"])
    blMilesCycledFilteredData <<- data.frame(baseline_milesCycled = data[,"baseline_milesCycled"])
  })
  
  
  observeEvent(input$inMETMS, function(){
    updateTextInput(session, "inMSMS", NULL, input$inMETMS)
  })
  
  observeEvent(input$inMSMS, function(){
    updateTextInput(session, "inMETMS", NULL, input$inMSMS)
    updateTextInput(session, "inBDMS", NULL, input$inBDMS)
    updateTextInput(session, "inCMMS", NULL, input$inCMMS)
  })

  
  
  
})