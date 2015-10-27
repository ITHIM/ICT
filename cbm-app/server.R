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
scMETdata <- NULL
scFilteredMETdata <- NULL
scTimeTraveldata <- NULL
scFilteredTimeTraveldata <- NULL
scFilteredTripTimeTraveldata <- NULL
ftdata <- NULL
bd <- NULL
pdl <- NULL

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
    
    pd <<- data
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
    
    #pd <<- data
    
    #data <- scenariosTimeTravelIdata
    
    columnName <- paste(paste("MS", input$inTTMS,sep = ""),  paste("TDR", input$inTTTDR,sep = ""),
                        paste("ebik", input$inTTEB,sep = ""), paste("eq", input$inTTEQ,sep = ""), sep="_")
    
    #tripData <- scenariosTripTimeTravelIdata[,c("baseline", columnName)]
    
    data["timetravel"] <- data[,columnName]
    
    data <- arrange(data, timetravel)
    
    #Convert data in minutes to hours
    data["timetravel"] <- data["timetravel"]/60
    
    #     scTimeTraveldata["timetravel"] <<- scenariosTimeTravelIdata[,columnName]
    scFilteredTimeTraveldata <<- data
    #     scFilteredTripTimeTraveldata <<- tripData
  })
  
  tripTimeData <- reactive({
    data <- scenariosTripTimeTravelIdata
    
    if (input$inTTag != 'All'){
      data <- subset(data, age_group == input$inTTag)
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
    
    
    columnName <- paste(paste("MS", input$inTTMS,sep = ""),  paste("TDR", input$inTTTDR,sep = ""),
                        paste("ebik", input$inTTEB,sep = ""), paste("eq", input$inTTEQ,sep = ""), sep="_")
    
    #cat(columnName, "\n")
    
    tripData <- scenariosTripTimeTravelIdata[,c("baseline", columnName)]
    tripData <- data[,c("baseline", columnName)]
    
    tripData <- as.data.frame((tripData$baseline - tripData[[columnName]]) / tripData$baseline * 100)
    
    colnames(tripData) <- c("diff")
    tripDataSubset <- subset(tripData, diff <= 200 & diff >= -200 )
    scFilteredTripTimeTraveldata <<- tripDataSubset
  })
  
  
  plotMETDataTable<- reactive({
    data <- idata
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
    
    data <- scenariosIdata
    
    columnName <- paste(paste("MS", input$inMETMS,sep = ""),  paste("TDR", input$inMETTDR,sep = ""),
                        paste("ebik", input$inMETEB,sep = ""), paste("eq", input$inMETEQ,sep = ""), sep="_")
    
    data["total_mmet"] <- scenariosIdata[columnName]
    
    scMETdata <<- data
    
    if (input$mag != 'All'){
      data <- subset(data, age == input$mag)
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
    if (!is.null(pd)){
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      h1$plotOptions(column=list(animation=FALSE))
      
      filtered_title <- getBaselineFilteredTitle(tdata)
      #cat(filtered_title, "\n" )
      extended_title <- paste("Main Mode: Total population versus population selected for scenario")
      #(selected population currently defined as ", filtered_title, ")", sep = "")
      h1$title(text = extended_title)
      bcounts <- count(tdata, "MainMode_reduced_val")
      h1$xAxis(categories = bcounts[["MainMode_reduced_val"]], title = list(text = 'Main Mode'))#, style = list(font = 'bold 14px')))
      
      h1$tooltip(valueSuffix= '%')
      
      bcounts$tp <- bcounts$freq / sum(bcounts$freq) * 100
      bcounts$tp <- round(bcounts$tp, digits = 1)
      bcounts$freq <- NULL
      
      scounts <- count(pd, "MainMode_reduced_val")
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
    plotMETDataTable()
    
    input$flipMETHG
    input$phyGuideline
    
    if (!is.null(idata) & !is.null(scMETdata)){
      if (input$flipMETHG == 'sep'){
        # Keep the data separated
        # scMETdata and scFilteredMETdata
        firstColData = idata
        secondColData = pd
        
        extended_title <- paste("Baseline - Marginal MET Hours", sep = "")
        
        firstColName <- "Baseline"
        secondColName <- "Baseline (Filtered)"
        
      }else{
        # Keep the data mixed
        firstColData = idata
        secondColData = scMETdata
        
        extended_title <- paste("Baseline Versus Scenario - Marginal MET Hours", sep = "")
        
        firstColName <- "Baseline"
        secondColName <- "Scenario"
      }
      
      filtered_title <- getMETFilteredTitle(secondColData, "baseline")
      
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      h1$plotOptions(column=list(animation=FALSE))
      
      
      if (input$phyGuideline == 'on'){
        bc <- createPhyActTable(firstColData)
        bc$Freq <- round(bc$Freq  / nrow(firstColData) * 100, digits = 1)
        
        h1$xAxis(categories = c("Lower the guidelines (METh < 8.75)", "Meeting the guidelines (METh > 8.75)", 
                                "Meeting the guidelines (METh > 17.5)"), 
                 title = list(text = 'Marginal MET Hours'))
        h1$series(data = bc$Freq, name = paste(firstColName, "Population", sep = " "))
        
        bc <- createPhyActTable(secondColData)#$total_mmet)
        bc$Freq <- round(bc$Freq  / nrow(secondColData) * 100, digits = 1)
        
        h1$series(data = bc$Freq, name = paste(secondColName, "Population", sep = " "))
        
        if (nrow(firstColData) < 10){
          h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
        }else{
          h1$subtitle(text= filtered_title)
        }
        
        h1$yAxis(tickInterval = 20, title = list(text = 'Percentage %'))
        
      }else{
        
        bc <- as.data.frame(table (cut (firstColData$total_mmet, breaks = c(seq(-4.4,52.8, 4.4), max(firstColData$total_mmet)))))
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        bc1max <- max(bc$Freq, na.rm = T)
        
        h1$xAxis(categories = as.list(append(c(seq(-4.4,52.8, 4.4))[-1], "52.7+")), title = list(text = 'Marginal MET Hours'))
        h1$series(data = bc$Freq, name = paste(firstColName, "Population", sep = " "))
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
          h1$series(data = bc$Freq, name = paste(secondColName, "Population", sep = " "))
          h1$subtitle(text= filtered_title)
          
        }else{
          h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
        }
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
        extended_title <- paste("Marginal MET hours of Scenario")
        
        firstColName <- "Scenario"
        secondColName <- "Scenario (Filtered)"
        
        
        
      }else{
        # Keep the data mixed
        firstColData = pd
        secondColData = scFilteredMETdata
        extended_title <- paste("Baseline versus Scenario - Marginal MET hour of Filtered Population")
        
        firstColName <- "Baseline (Filtered)"
        secondColName <- "Scenario (Filtered)"
        
      }
      
      filtered_title <- getMETFilteredTitle(secondColData, "scenario")
      
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      h1$plotOptions(column=list(animation=FALSE))
      
      if (input$phyGuideline == 'on'){
        bc <- createPhyActTable(firstColData)
        bc$Freq <- round(bc$Freq  / nrow(firstColData) * 100, digits = 1)
        
        h1$xAxis(categories = c("Lower the guidelines (METh < 8.75)", "Meeting the guidelines (METh > 8.75)", 
                                "Meeting the guidelines (METh > 17.5)"), 
                 title = list(text = 'Marginal MET Hours'))
        h1$series(data = bc$Freq, name = paste(firstColName, "Population", sep = " "))
        
        bc <- createPhyActTable(secondColData)#$total_mmet)
        bc$Freq <- round(bc$Freq  / nrow(secondColData) * 100, digits = 1)
        
        h1$series(data = bc$Freq, name = paste(secondColName, "Population", sep = " "))
        
        if (nrow(firstColData) < 10){
          h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
        }else{
          h1$subtitle(text= filtered_title)
        }
        
        h1$yAxis(tickInterval = 20, title = list(text = 'Percentage %'))
        
      }else{
        
        bc <- as.data.frame(table (cut (firstColData$total_mmet, breaks = c(seq(-4.4,52.8, 4.4), max(firstColData$total_mmet)))))
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        bc1max <- max(bc$Freq, na.rm = T)
        
        h1$xAxis(categories = as.list(append(c(seq(-4.4,52.8, 4.4))[-1], "52.7+")), title = list(text = 'Marginal MET Hours'))
        h1$series(data = bc$Freq, name = paste(firstColName, "Population", sep = " "))
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
          h1$series(data = bc$Freq, name = paste(secondColName, "Population", sep = " "))
          h1$subtitle(text= filtered_title)
          
        }else{
          h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
        }
      }
      
    }
    
    h1$title(text = extended_title)
    h1$tooltip(valueSuffix= '%')
    h1$set(dom = 'plotScenarioMET')
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
    if (!is.null(scFilteredTripTimeTraveldata) && length(scFilteredTripTimeTraveldata) > 0){
      
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      
      h1$plotOptions(column = list(dataLabels = list(enabled = T, 
                                                     crop = FALSE, 
                                                     rotation = -90, 
                                                     align = 'right', 
                                                     color = '#FFFFFF', 
                                                     width = 100,
                                                     x = 4, 
                                                     y = 10, 
                                                     style = list(fontSize = '13px', fontFamily = 'Verdana, sans-serif'))),
                     series = list(dataLabels = list(crop = F))
      )
      
      if (length (scFilteredTripTimeTraveldata$diff) > 0){
        
        #         data.decile <- cut2(scFilteredTripTimeTraveldata$diff, g = 10)
        #         
        #         data <- as.data.frame(table(data.decile))
        
        dhit <- hist(scFilteredTripTimeTraveldata$diff)
        #data <- data.frame(breaks = dhit$breaks[-length(dhit$breaks)], counts = dhit$counts)
        data <- data.frame(breaks = dhit$breaks, counts = append(dhit$counts, 0))
        data$freq <- round(data$counts / sum(data$counts) * 100, digits = 1)
        data <- subset(data, freq >= 0.1)
        
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
          #h1$xAxis(categories = "20%")
          cat(" Single label: ", xlabel, "\n")
          
        }else{
          
          h1$xAxis(categories = paste(data$breaks, "%"))
        }
        #         cat("length: ", nrow(data), "Freq: ", data$freq, "\n Breaks: ",  paste(data$breaks, "%"), "\n")
        h1$yAxis(min = 0, max = 100, tickInterval = 20, title = list(text = 'Percentage %'))
        
        h1$tooltip(valueSuffix= '%')
        
      }
      
      h1$set(dom = 'plotTripTimeDifference')
      h1$exporting(enabled = T)
      return(h1)
    }
    
  })
  
  getFilteredBDTitle <- function (src){
    filtered_title <- ""
    if (src == "BD")
      filtered_title <- paste("Cycling Multiplier: ", input$inBDMS, ", TDR: ", input$inBDTDR, ", Ebike: ", input$inBDEB, " and Equity: ", input$inBDEQ, sep = "" )
    else if (src == "FT")
      filtered_title <- paste("Cycling Multiplier: ", input$inFTMS, ", TDR: ", input$inFTTDR, ", Ebike: ", input$inFTEB, " and Equity: ", input$inFTEQ, sep = "" )
    
    filtered_title
  }
  
  getBaselineFilteredTitle <- function(data){
    filtered_title <- "Total Population (Baseline)"
    if (nrow(data) != nrow (pd)){
      
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
    filtered_title <- "Total Population (Baseline)"
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
  
  
  generateBDScenarioTable<- reactive({
    
    lMS <- input$inBDMS
    lTDR <- input$inBDTDR
    lEB <- input$inBDEB
    lEQ <- input$inBDEQ
    
    data <- msharedtata
    data <- subset(data, MS == (as.numeric(lMS) + 1) & TDR == lTDR & equity == lEQ & ebike == lEB)
    
    data[is.na(data)] <- 0
    data <- arrange(data, MS)
    bd <<- data
  })
  
  
  generateFasterTripsTable <- reactive({
    
    lMS <- input$inFTMS
    lTDR <- input$inFTTDR
    lEB <- input$inFTEB
    lEQ <- input$inFTEQ
    
    data <- fasterTripData
    data <- subset(data, MS == lMS & TDR == lTDR & equity == lEQ & ebike == lEB)
    
    data[is.na(data)] <- 0
    data <- arrange(data, MS)
    ftdata <<- data
    
  })
  
  
  generateScenarioTable<- reactive({
    
    #     lMS <- input$inMS
    lTDR <- input$inTDR
    lEB <- input$inEB
    lEQ <- input$inEQ
    
    data <- sdata
    if (lTDR != "All")
      data <- subset(data, TDR == lTDR)
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
    
    lTDR <- input$inTDR
    lEB <- input$inEB
    lEQ <- input$inEQ
    
  })
  
  genericPlot <- function(var){
    
    h1 <- Highcharts$new()
    h1$chart(type = "spline")
    # types of charts: http://api.highcharts.com/highcharts#plotOptions
    h1$yAxis(title = list(text = var))
    h1$xAxis(categories = sort(unique(sdata$MS), decreasing = F), title = list(text = 'Cycling Multiplier'))
    
    h1$series(data = baselineSummary[[var]] , name = "Baseline")
    
    if (input$inTDR == "All"){
      if (input$inEB != "All" & input$inEQ != "All"){
        sub1 <- append(0, subset(scdata, TDR == 0.7 & ebike == as.numeric(input$inEB) & equity == as.numeric(input$inEQ)))
        h1$series(data = sub1[[var]], name = paste("TDR 0.7 (EB ", input$inEB, " and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.8 & ebike == as.numeric(input$inEB) & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.8 (EB ", input$inEB, " and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.9 & ebike == as.numeric(input$inEB) & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.9 (EB ", input$inEB, " and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 1.0 & ebike == as.numeric(input$inEB) & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 1.0 (EB ", input$inEB, " and EQ ", input$inEQ, ")" , sep = ""))
      }
      
      if (input$inEB == "All" & input$inEQ != "All"){
        sub1 <- subset(scdata, TDR == 0.7 & ebike == 0 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.7 (EB 0 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.7 & ebike == 1 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.7 (EB 1 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.8 & ebike == 0 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.8 (EB 0 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.8 & ebike == 1 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.8 (EB 1 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.9 & ebike == 0 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.9 (EB 0 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.9 & ebike == 1 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.9 (EB 1 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 1.0 & ebike == 0 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 1.0 (EB 0 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 1.0 & ebike == 1 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 1.0 (EB 1 and EQ ", input$inEQ, ")" , sep = ""))
      }
      
      
      if (input$inEB != "All" & input$inEQ == "All"){
        sub1 <- subset(scdata, TDR == 0.7 & equity == 0 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 0.7 (EB ", input$inEB, " and EQ 0)" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.7 & equity == 1 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 0.7 (EB ", input$inEB, " and EQ 1)" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.8 & equity == 0 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 0.8 (EB", input$inEB, " and EQ 0)" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.8 & equity == 1 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 0.8 (EB ", input$inEB, " and EQ 1)" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.9 & equity == 0 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 0.9 (EB ", input$inEB, " and EQ 0)" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.9 & equity == 1 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 0.9 (EB ", input$inEB, " and EQ 1)" , sep = ""))
        sub1 <- subset(scdata, TDR == 1.0 & equity == 0 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 1.0 (EB ", input$inEB, "and EQ 0)" , sep = ""))
        sub1 <- subset(scdata, TDR == 1.0 & equity == 1 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 1.0 (EB ", input$inEB, "and EQ 1)" , sep = ""))
      }
      
      if (input$inEB == "All" & input$inEQ == "All"){
        sub1 <- subset(scdata, TDR == 0.7 & ebike == 0 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 0.7 (EB 0 and EQ 0)")
        sub1 <- subset(scdata, TDR == 0.7 & ebike == 0 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 0.7 (EB 0 and EQ 1)")
        sub1 <- subset(scdata, TDR == 0.7 & ebike == 1 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 0.7 (EB 1 and EQ 0)")
        sub1 <- subset(scdata, TDR == 0.7 & ebike == 1 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 0.7 (EB 1 and EQ 1)")
        
        
        sub1 <- subset(scdata, TDR == 0.8 & ebike == 0 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 0.8 (EB 0 and EQ 0)")
        sub1 <- subset(scdata, TDR == 0.8 & ebike == 0 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 0.8 (EB 0 and EQ 1)")
        sub1 <- subset(scdata, TDR == 0.8 & ebike == 1 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 0.8 (EB 1 and EQ 0)")
        sub1 <- subset(scdata, TDR == 0.8 & ebike == 1 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 0.8 (EB 1 and EQ 1)")
        
        sub1 <- subset(scdata, TDR == 0.9 & ebike == 0 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 0.9 (EB 0 and EQ 0)")
        sub1 <- subset(scdata, TDR == 0.9 & ebike == 0 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 0.9 (EB 0 and EQ 1)")
        sub1 <- subset(scdata, TDR == 0.9 & ebike == 1 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 0.9 (EB 1 and EQ 0)")
        sub1 <- subset(scdata, TDR == 0.9 & ebike == 1 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 0.9 (EB 1 and EQ 1)")
        
        sub1 <- subset(scdata, TDR == 1.0 & ebike == 0 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 1.0 (EB 0 and EQ 0)")
        sub1 <- subset(scdata, TDR == 1.0 & ebike == 0 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 1.0 (EB 0 and EQ 1)")
        sub1 <- subset(scdata, TDR == 1.0 & ebike == 1 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 1.0 (EB 1 and EQ 0)")
        sub1 <- subset(scdata, TDR == 1.0 & ebike == 1 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 1.0 (EB 1 and EQ 1)")
      }
      
    }else{
      
      if (input$inEB != "All" & input$inEQ != "All"){
        sub1 <- subset(scdata, TDR == input$inTDR & ebike == as.numeric(input$inEB) & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB ", input$inEB, " and EQ ", input$inEQ, ")" , sep = ""))
        
      }
      
      if (input$inEB == "All" & input$inEQ != "All"){
        sub1 <- subset(scdata, TDR == input$inTDR & ebike == 0 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB 0 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == input$inTDR & ebike == 1 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB 1 and EQ ", input$inEQ, ")" , sep = ""))
        
      }
      
      
      if (input$inEB != "All" & input$inEQ == "All"){
        sub1 <- subset(scdata, TDR == input$inTDR & equity == 0 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB ", input$inEB, " and EQ 0)" , sep = ""))
        sub1 <- subset(scdata, TDR == input$inTDR & equity == 1 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB ", input$inEB, " and EQ 1)" , sep = ""))
        
      }
      
      if (input$inEB == "All" & input$inEQ == "All"){
        sub1 <- subset(scdata, TDR == input$inTDR & ebike == 0 & equity == 0)
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB 0 and EQ 0)"))
        sub1 <- subset(scdata, TDR == input$inTDR & ebike == 0 & equity == 1)
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB 0 and EQ 1)"))
        sub1 <- subset(scdata, TDR == input$inTDR & ebike == 1 & equity == 0)
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB 1 and EQ 0)"))
        sub1 <- subset(scdata, TDR == input$inTDR & ebike == 1 & equity == 1)
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB 1 and EQ 1)"))
        
      }
    }
    h1$exporting(enabled = T)
    return(h1)
  }
  
  output$plotCycPercent <- renderChart({
    generateScenarioTable()
    h <- genericPlot("% Cycl. Total Population")
    h$set(dom = 'plotCycPercent')
    return (h)
  })
  
  output$plotGenericVariable <- renderChart({
    generateScenarioTable()
    #retrieveVariableName()
    h <- genericPlot(input$varname)
    h$set(dom = 'plotGenericVariable')
    return (h)
  })
  
  
  output$plotBDMode <- renderChart({
    generateBDScenarioTable()
    if (!is.null(bd)){
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      h1$plotOptions(column=list(animation=FALSE))
      
      filtered_title <- getFilteredBDTitle("BD")
      extended_title <- paste("Mode Share: Total Population versus Selected Scenario")
      h1$title(text = extended_title)
      baseline <- subset(msharedtata, MS == 1)
      h1$series(data = baseline$case, name = "Baseline")
      h1$series(data = bd$case, name = "Scenario")
      
      h1$xAxis(categories = c("Walk", "Car Driver", "Car Passenger", "Bus", "Train", "Other", "Bicycle", "Ebike"))
      
      h1$subtitle(text = paste("Scenario: ", filtered_title), style = list(font = 'bold 12px "Trebuchet MS", Verdana, sans-serif'))
      
      h1$tooltip(valueSuffix= '%')
      
      h1$set(dom = 'plotBDMode')
      h1$exporting(enabled = T)
      return (h1)
    }
  })
  
  
  output$plotBDFasterTrips <- renderChart({
    generateFasterTripsTable()
    if (!is.null(ftdata)){
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      h1$plotOptions(column=list(animation=FALSE))
      
      filtered_title <- getFilteredBDTitle("FT")
      extended_title <- paste("Faster Trips: Selected Scenario")
      h1$title(text = extended_title)
      h1$series(data = ftdata$case, name = "Scenario")
      h1$subtitle(text = paste("Scenario: ", filtered_title), style = list(font = 'bold 12px "Trebuchet MS", Verdana, sans-serif'))
      
      h1$xAxis(categories = c("Walk", "Car Passenger", "Bus", "Train", "Other", "Bicycle"))
      
      h1$tooltip(valueSuffix= '%')
      
      h1$set(dom = 'plotBDFasterTrips')
      h1$exporting(enabled = T)
      return (h1)
    }
  })
  
})