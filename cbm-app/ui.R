library(DT)
library(devtools)
if (!require(rCharts)) {
  install_github('rCharts', 'ramnathv')
  library(rCharts)
}
source("data-processing.R")
uMS <- append("All", sort(unique(sdata$MS)))
uTDR <- append("All", sort(unique(sdata$TDR), decreasing = F))
uEQ <- append("All",sort(unique(sdata$equity)))
uEB <- append("All", sort(unique(sdata$ebike)))

uBDMS <- (sort(unique(msharedtata$MS)[-1]) - 1)
uBDTDR <- sort(unique(msharedtata$TDR), decreasing = F)
uBDEQ <- sort(unique(msharedtata$equity))
uBDEB <- sort(unique(msharedtata$ebike))

variableList <- t(as.matrix(colnames(sdata)))
variableList <- variableList[,6:length(colnames(sdata))]

scenarios <- c("Trip Mode Share" = "t",
               "Individual METs" =    "i")

METSwitchRButton <- c("Baseline and Scenario" = "sep",
                      "Baseline versus Scenario" =    "comp")

phyGLRButton <- c("On" = "on",
                  "Off" =    "off")


ag <- "All"
ag <- append(ag, sort(unique(as.character(tdata$age_group))))

healthAG <- c("All", "20 - 39", "40 - 59", "60+")

ses <- c("All" = "All",
         "Managerial and professional occupations" = 1,
         "Intermediate occupations and small employers" = 2,
         "Routine and manual occupations" = 3,
         "Never worked and long-term unemployed" = 4,
         "Not classified (including students)" = 5)

ethnicity <- c("All" = "All", "White" = 1, "Non-white" = 2)

gender <- c("All" = 3,
            "Male" = 1,
            "Female" = 2)

genderForHealthCalculations <- c("All", 
                                 "Male",
                                 "Female")

shinyUI(fluidPage(width="100%", height="100%",
                  headerPanel("Co-Benefit Model (Prototype)"),
                  sidebarPanel(
                    conditionalPanel(condition="input.conditionedPanels == 1",
                                     selectInput(inputId = "inEQ", label = h4("Select Equity (EQ):"), choices =  uEQ),
                                     selectInput(inputId = "inTDR", label = h4("Select Travel Distance Reduction (TDR):"), choices =  uTDR, selected = uTDR[length(uTDR)]),
                                     selectInput(inputId = "inEB", label = h4("Select Ebike (EB):"), choices =  uEB),
                                     selectInput('varname', label = h4('Plot Variable:'), variableList)
                    ),
                    
                    conditionalPanel(condition="input.conditionedPanels == 2",
                                     selectInput(inputId = "inBDMS", label = h4("Select Cycling Multiplier:"), choices =  uBDMS),
                                     selectInput(inputId = "inBDEQ", label = h4("Select Equity (EQ):"), choices =  uBDEQ),
                                     selectInput(inputId = "inBDTDR", label = h4("Select Travel Distance Reduction (TDR):"), choices =  uBDTDR, selected = uTDR[length(uTDR)]),
                                     selectInput(inputId = "inBDEB", label = h4("Select Ebike (EB):"), choices =  uBDEB),
                                     
                                     selectizeInput("inBDAG", "Age Group:", ag, selected = ag[1], multiple = F),
                                     radioButtons("inBDGender", "Gender: ", gender, inline = TRUE),
                                     selectizeInput("inBDSES", "Socio Economic Classification :", ses, selected = ses[1], multiple = F),
                                     selectizeInput("inBDEthnicity", "Ethnic Group:", ethnicity, selected = ethnicity[1], multiple = F)
                    ),
                    
                    conditionalPanel(condition="input.conditionedPanels == 3",
                                     #                                      radioButtons("scenario", "Scenario:", scenarios, inline = TRUE),
                                     selectizeInput("bag", "Age Group:", ag, selected = ag[1], multiple = F),
                                     radioButtons("bgender", "Gender: ", gender, inline = TRUE),
                                     selectizeInput("bses", "Socio Economic Classification :", ses, selected = ses[1], multiple = F),
                                     selectizeInput("bethnicity", "Ethnic Group:", ethnicity, selected = ethnicity[1], multiple = F)
                    ),
                    
                    conditionalPanel(condition="input.conditionedPanels == 4",
                                     selectInput(inputId = "inMETMS", label = "Select Cycling Multiplier:", choices =  uBDMS),
                                     selectInput(inputId = "inMETEQ", label = "Select Equity (EQ):", choices =  uBDEQ),
                                     selectInput(inputId = "inMETTDR", label = "Select Travel Distance Reduction (TDR):", choices =  uBDTDR, selected = uTDR[length(uTDR)]),
                                     selectInput(inputId = "inMETEB", label = "Select Ebike (EB):", choices =  uBDEB),
                                     
                                     HTML("<hr>"),
                                     selectizeInput("mag", "Age Group:", ag, selected = ag[1], multiple = F),
                                     radioButtons("mgender", "Gender: ", gender, inline = TRUE),
                                     selectizeInput("mses", "Socio Economic Classification :", ses, selected = ses[1], multiple = F),
                                     selectizeInput("methnicity", "Ethnic Group:", ethnicity, selected = ethnicity[1], multiple = F),
                                     HTML("<hr>"),
                                     radioButtons("flipMETHG", label = "Flip Histogram:", METSwitchRButton, inline = TRUE),
                                     HTML("<hr>"),
                                     radioButtons("phyGuideline", label = "% Meeting WHO Physical Guideline", phyGLRButton, selected = phyGLRButton[2], inline = TRUE)
                    ),
                    
                    conditionalPanel(condition="input.conditionedPanels == 5",
                                     selectInput(inputId = "inHealthMS", label = "Select Cycling Multiplier:", choices =  uBDMS),
                                     selectInput(inputId = "inHealthEQ", label = "Select Equity (EQ):", choices =  uBDEQ),
                                     selectInput(inputId = "inHealthTDR", label = "Select Travel Distance Reduction (TDR):", choices =  uBDTDR, selected = uTDR[length(uTDR)]),
                                     selectInput(inputId = "inHealthEB", label = "Select Ebike (EB):", choices =  uBDEB),
                                     HTML("<hr>"),
                                     selectizeInput("inHealthAG", "Age Group:", healthAG, selected = healthAG[1], multiple = F),
                                     
                                     
                                     radioButtons("inHealthG", "Gender: ", genderForHealthCalculations, inline = TRUE)
                                     
                    ),
                    conditionalPanel(condition="input.conditionedPanels == 6",
                                     selectInput(inputId = "inTTMS", label = "Select Cycling Multiplier:", choices =  uBDMS),
                                     selectInput(inputId = "inTTEQ", label = "Select Equity (EQ):", choices =  uBDEQ),
                                     selectInput(inputId = "inTTTDR", label = "Select Travel Distance Reduction (TDR):", choices =  uTDR[length(uTDR)], selected = uTDR[length(uTDR)]),
                                     selectInput(inputId = "inTTEB", label = "Select Ebike (EB):", choices =  uBDEB),
                                     HTML("<hr>"),
                                     selectizeInput("inTTag", "Age Group:", ag, selected = ag[1], multiple = F),
                                     radioButtons("inTTgender", "Gender: ", gender, inline = TRUE),
                                     selectizeInput("inTTses", "Socio Economic Classification :", ses, selected = ses[1], multiple = F),
                                     selectizeInput("inTTethnicity", "Ethnic Group:", ethnicity, selected = ethnicity[1], multiple = F)
                    ),
                    conditionalPanel(condition="input.conditionedPanels == 7",
                                     selectInput(inputId = "inFTMS", label = "Select Cycling Multiplier:", choices =  uBDMS),
                                     selectInput(inputId = "inFTEQ", label = "Select Equity (EQ):", choices =  uBDEQ),
                                     selectInput(inputId = "inFTTDR", label = "Select Travel Distance Reduction (TDR):", choices =  uBDTDR, selected = uTDR[length(uTDR)]),
                                     selectInput(inputId = "inFTEB", label = "Select Ebike (EB):", choices =  uBDEB)
                    ),
                    conditionalPanel(condition="input.conditionedPanels == 8",
                                     selectInput(inputId = "inCMMS", label = "Select Cycling Multiplier:", choices =  uBDMS),
                                     selectInput(inputId = "inCMEQ", label = "Select Equity (EQ):", choices =  uBDEQ),
                                     selectInput(inputId = "inCMTDR", label = "Select Travel Distance Reduction (TDR):", choices =  uTDR[length(uTDR)], selected = uTDR[length(uTDR)]),
                                     selectInput(inputId = "inCMEB", label = "Select Ebike (EB):", choices =  uBDEB),
                                     HTML("<hr>"),
                                     selectizeInput("inCMAG", "Age Group:", ag, selected = ag[1], multiple = F),
                                     radioButtons("inCMGender", "Gender: ", gender, inline = TRUE),
                                     selectizeInput("inCMSES", "Socio Economic Classification :", ses, selected = ses[1], multiple = F),
                                     selectizeInput("inCMEthnicity", "Ethnic Group:", ethnicity, selected = ethnicity[1], multiple = F)
                    )
                    
                    
                  ),
                  
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Scenarios", value = 1,
                               showOutput('plotCycPercent', "highcharts"),
                               h6("(Click on the legend to enable/disable a line)", align="center"),
                               showOutput('plotGenericVariable', "highcharts"),
                               h6("(Click on the legend to enable/disable a line)", align="center"),
                               HTML('<style>iframe.rChart{ width: 100%; height: 400px;}</style>')
                      ),
                      tabPanel("Scenarios - Mode Share", value = 2,
                               showOutput('plotBDMode', "highcharts"),
                               showOutput('plotBDSCMode', "highcharts")
                      ),
                      
                      tabPanel("Mode of Travel", value = 3,
                               showOutput('plotMode', "highcharts")
                      ), 
                      tabPanel("Physical Activity", value = 4,
                               showOutput('plotMET', "highcharts"),
                               showOutput('plotScenarioMET', "highcharts")
                      ),
                      tabPanel("Health", value = 5,
                               showOutput('plotYLL', "highcharts"),
                               showOutput('plotYLLReduction', "highcharts")
                               
                      ),
                      tabPanel("Trip Duration", value = 6,
                               #showOutput('plotTripTime', "highcharts"),
                               showOutput('plotTripTimeDifference', "highcharts")
                               
                      ),
                      tabPanel("Trip Faster/Slower", value = 7,
                               showOutput('plotBDFasterTrips', "highcharts"),
                               showOutput('plotBDSlowerTrips', "highcharts")
                      ),
                      tabPanel("Car Miles", value = 8,
                               showOutput('plotCarTripsCycled', "highcharts")
                      ),
                      id = "conditionedPanels"
                    )
                  )
))