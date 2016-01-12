library(DT)
library(devtools)
if (!require(rCharts)) {
  install_github('rCharts', 'ramnathv')
  library(rCharts)
}
library(shinyBS)
source("data-processing.R")
uniqueMS <- sort(unique(sdata$MS))
uMS <- append("All", sort(unique(sdata$MS)))
uEQ <- append("All",sort(unique(sdata$equity)))
uEB <- append("All", sort(unique(sdata$ebike)))

uBDMS <- (sort(unique(msharedtata$MS)[-1]) - 1)
uBDEQ <- sort(unique(msharedtata$equity))
uBDEB <- sort(unique(msharedtata$ebike))

variableList <- t(as.matrix(colnames(sdata)))
variableList <- variableList[,6:length(colnames(sdata))]

scenarios <- c("Trip Mode Share" = "t",
               "Individual METs" =    "i")

METSwitchRButton <- c("Baseline and Scenario" = "sep",
                      "Scenario versus Baseline" =    "comp")

switchRButton <- c("Scenario versus Baseline" =    "comp",
                   "Sub-population versus total population" = "sep")

phyGLRButton <- c("On" = "on",
                  "Off" =    "off")

onOffRButton <- c("On" = 1,
                  "Off" = 0)

allOnOffRButton <- c("All" = "All",
                     "On" = 1,
                     "Off" = 0)

# ag <- "All"
# ag <- append(ag, sort(unique(as.character(tdata$age_group))))

ag <- "All"
ag <- append(ag, sort(unique(as.character(tripData$age_group))))

healthAG <- c("All", "18 - 39", "40 - 59", "60 - 84")

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
                  #headerPanel("Integrated Cycling Tool (Prototype)", windowTitle = "Integrated Cycling Tool"),
                  #headerPanel("Integrated Cycling Tool (Prototype)", windowTitle = "Integrated Cycling Tool")
                  bsCollapse(id = "intro", bsCollapsePanel(
                    tags$div(title = "Click here to open an introductory document", 
                             h4("Impacts of Cycling Tool (Prototype) (Click to expand)")), 
                    includeMarkdown("README.md"))),
                  sidebarPanel(
                    conditionalPanel(condition="input.conditionedPanels == 1",
                                     radioButtons(inputId = "inEQ", label = "Select Equity (EQ):", allOnOffRButton, inline = TRUE),
                                     #selectInput(inputId = "inEQ", label = "Select Equity (EQ):", choices =  uEQ),
                                     #selectInput(inputId = "inEB", label = "Select Ebike (EB):", choices =  uEB),
                                     radioButtons(inputId = "inEB", label = "Select Ebike (EB):", allOnOffRButton, inline = TRUE),
                                     selectInput('varname', label = 'Plot Variable:', variableList)
                    ),
                    
                    conditionalPanel(condition="input.conditionedPanels == 2",
                                     selectInput(inputId = "inBDMS", label = "Select Cycling Multiplier:", choices =  uniqueMS),#, selected = uBDMS[2]),
                                     radioButtons(inputId = "inBDEQ", label = "Select Equity (EQ):", onOffRButton, inline = TRUE),
                                     # selectInput(inputId = "inBDEQ", label = h4("Select Equity (EQ):"), choices =  uBDEQ),
                                     radioButtons(inputId = "inBDEB", label = "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE),
                                     # selectInput(inputId = "inBDEB", label = h4("Select Ebike (EB):"), choices =  uBDEB),
                                     HTML("<hr>"),
                                     selectizeInput("inBDAG", "Age Group:", ag, selected = ag[1], multiple = F),
                                     radioButtons("inBDGender", "Gender: ", gender, inline = TRUE),
                                     selectizeInput("inBDSES", "Socio Economic Classification :", ses, selected = ses[1], multiple = F),
                                     #selectizeInput("inBDEthnicity", "Ethnic Group:", ethnicity, selected = ethnicity[1], multiple = F),
                                     radioButtons("inBDEthnicity", label = "Ethnic Group:", ethnicity, inline = TRUE),
                                     HTML("<hr>"),
                                     radioButtons("flipMS", label = "Flip Histogram:", switchRButton, inline = TRUE)
                                     
                    ),
                    conditionalPanel(condition="input.conditionedPanels == 3",
                                     selectInput(inputId = "inHealthMS", label = "Select Cycling Multiplier:", choices =  uniqueMS),#uBDMS, selected = uBDMS[2]),
                                     #selectInput(inputId = "inHealthEQ", label = "Select Equity (EQ):", choices =  uBDEQ),
                                     radioButtons(inputId = "inHealthEQ", label = "Select Equity (EQ):", onOffRButton, inline = TRUE),
                                     #selectInput(inputId = "inHealthEB", label = "Select Ebike (EB):", choices =  uBDEB),
                                     radioButtons(inputId = "inHealthEB", label = "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE),
                                     HTML("<hr>"),
                                     selectizeInput("inHealthAG", "Age Group:", healthAG, selected = healthAG[1], multiple = F),
                                     radioButtons("inHealthG", "Gender: ", genderForHealthCalculations, inline = TRUE)
                                     
                    ),
                    conditionalPanel(condition="input.conditionedPanels == 4",
                                     selectInput(inputId = "inMETMS", label = "Select Cycling Multiplier:", choices =  uniqueMS),#, selected = uBDMS[2]),
                                     #selectInput(inputId = "inMETEQ", label = "Select Equity (EQ):", choices =  uBDEQ),
                                     radioButtons("inMETEQ", "Select Equity (EQ):", onOffRButton, inline = TRUE),
                                     #selectInput(inputId = "inMETEB", label = "Select Ebike (EB):", choices =  uBDEB),
                                     radioButtons("inMETEB", "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE),
                                     
                                     HTML("<hr>"),
                                     selectizeInput("mag", "Age Group:", ag, selected = ag[1], multiple = F),
                                     radioButtons("mgender", "Gender: ", gender, inline = TRUE),
                                     selectizeInput("mses", "Socio Economic Classification :", ses, selected = ses[1], multiple = F),
                                     #selectizeInput("methnicity", "Ethnic Group:", ethnicity, selected = ethnicity[1], multiple = F),
                                     radioButtons("methnicity", label = "Ethnic Group:", ethnicity, inline = TRUE),
                                     HTML("<hr>"),
                                     radioButtons("flipMETHG", label = "Flip Histogram:", METSwitchRButton, inline = TRUE),
                                     HTML("<hr>"),
                                     radioButtons("phyGuideline", label = "% Meeting WHO Physical Guideline", phyGLRButton, selected = phyGLRButton[2], inline = TRUE)
                    )
#                     ,
#                     conditionalPanel(condition="input.conditionedPanels == 5",
#                                      selectInput(inputId = "inMSMS", label = "Select Cycling Multiplier:", choices =  uniqueMS),#uBDMS, selected = uBDMS[2]),
#                                      radioButtons(inputId = "inMSEQ", label = "Select Equity (EQ):", onOffRButton, inline = TRUE),
#                                      radioButtons(inputId = "inMSEB", label = "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE),
#                                      HTML("<hr>"),
#                                      selectizeInput("inMSAG", "Age Group:", ag, multiple = F),
#                                      radioButtons("inMSG", "Gender: ", genderForHealthCalculations, inline = TRUE),
#                                      selectizeInput("inMSSES", "Socio Economic Classification :", ses),
#                                      radioButtons("inMSEthnicity", label = "Ethnic Group:", ethnicity, inline = TRUE),
#                                      HTML("<hr>"),
#                                      radioButtons("inMSflip", label = "Flip Histogram:", switchRButton, inline = TRUE)
#                                      
#                     ),
#                     conditionalPanel(condition="input.conditionedPanels == 6",
#                                      selectInput(inputId = "inCMMS", label = "Select Cycling Multiplier:", choices =  uniqueMS),#uBDMS, selected = uBDMS[2]),
#                                      radioButtons(inputId = "inCMEQ", label = "Select Equity (EQ):", onOffRButton, inline = TRUE),
#                                      radioButtons(inputId = "inCMEB", label = "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE),
#                                      HTML("<hr>"),
#                                      selectizeInput("inCMAG", "Age Group:", ag, multiple = F),
#                                      radioButtons("inCMG", "Gender: ", genderForHealthCalculations, inline = TRUE),
#                                      selectizeInput("inCMSES", "Socio Economic Classification :", ses),
#                                      radioButtons("inCMEthnicity", label = "Ethnic Group:", ethnicity, inline = TRUE)
#                                      
#                     )
                    
                    
                    
                    #,
                    #                     
                    #                     conditionalPanel(condition="input.conditionedPanels == 3",
                    #                                      #                                      radioButtons("scenario", "Scenario:", scenarios, inline = TRUE),
                    #                                      selectizeInput("bag", "Age Group:", ag, selected = ag[1], multiple = F),
                    #                                      radioButtons("bgender", "Gender: ", gender, inline = TRUE),
                    #                                      selectizeInput("bses", "Socio Economic Classification :", ses, selected = ses[1], multiple = F),
                    #                                      selectizeInput("bethnicity", "Ethnic Group:", ethnicity, selected = ethnicity[1], multiple = F)
                    #                     ),
                    #                     
                    #                     conditionalPanel(condition="input.conditionedPanels == 4",
                    #                                      selectInput(inputId = "inMETMS", label = "Select Cycling Multiplier:", choices =  uBDMS),
                    #                                      selectInput(inputId = "inMETEQ", label = "Select Equity (EQ):", choices =  uBDEQ),
                    #                                      selectInput(inputId = "inMETEB", label = "Select Ebike (EB):", choices =  uBDEB),
                    #                                      
                    #                                      HTML("<hr>"),
                    #                                      selectizeInput("mag", "Age Group:", ag, selected = ag[1], multiple = F),
                    #                                      radioButtons("mgender", "Gender: ", gender, inline = TRUE),
                    #                                      selectizeInput("mses", "Socio Economic Classification :", ses, selected = ses[1], multiple = F),
                    #                                      selectizeInput("methnicity", "Ethnic Group:", ethnicity, selected = ethnicity[1], multiple = F),
                    #                                      HTML("<hr>"),
                    #                                      radioButtons("flipMETHG", label = "Flip Histogram:", METSwitchRButton, inline = TRUE),
                    #                                      HTML("<hr>"),
                    #                                      radioButtons("phyGuideline", label = "% Meeting WHO Physical Guideline", phyGLRButton, selected = phyGLRButton[2], inline = TRUE)
                    #                     ),
                    #                     
                    #                     conditionalPanel(condition="input.conditionedPanels == 5",
                    #                                      selectInput(inputId = "inHealthMS", label = "Select Cycling Multiplier:", choices =  uBDMS),
                    #                                      selectInput(inputId = "inHealthEQ", label = "Select Equity (EQ):", choices =  uBDEQ),
                    #                                      selectInput(inputId = "inHealthEB", label = "Select Ebike (EB):", choices =  uBDEB),
                    #                                      HTML("<hr>"),
                    #                                      selectizeInput("inHealthAG", "Age Group:", healthAG, selected = healthAG[1], multiple = F),
                    #                                      
                    #                                      
                    #                                      radioButtons("inHealthG", "Gender: ", genderForHealthCalculations, inline = TRUE)
                    #                                      
                    #                     ),
                    #                     conditionalPanel(condition="input.conditionedPanels == 6",
                    #                                      selectInput(inputId = "inTTMS", label = "Select Cycling Multiplier:", choices =  uBDMS),
                    #                                      selectInput(inputId = "inTTEQ", label = "Select Equity (EQ):", choices =  uBDEQ),
                    #                                      selectInput(inputId = "inTTEB", label = "Select Ebike (EB):", choices =  uBDEB),
                    #                                      HTML("<hr>"),
                    #                                      selectizeInput("inTTag", "Age Group:", ag, selected = ag[1], multiple = F),
                    #                                      radioButtons("inTTgender", "Gender: ", gender, inline = TRUE),
                    #                                      selectizeInput("inTTses", "Socio Economic Classification :", ses, selected = ses[1], multiple = F),
                    #                                      selectizeInput("inTTethnicity", "Ethnic Group:", ethnicity, selected = ethnicity[1], multiple = F)
                    #                     ),
                    #                     conditionalPanel(condition="input.conditionedPanels == 7",
                    #                                      selectInput(inputId = "inFTMS", label = "Select Cycling Multiplier:", choices =  uBDMS),
                    #                                      selectInput(inputId = "inFTEQ", label = "Select Equity (EQ):", choices =  uBDEQ),
                    #                                      selectInput(inputId = "inFTEB", label = "Select Ebike (EB):", choices =  uBDEB)
                    #                     ),
                    #                     conditionalPanel(condition="input.conditionedPanels == 8",
                    #                                      selectInput(inputId = "inCMMS", label = "Select Cycling Multiplier:", choices =  uBDMS),
                    #                                      selectInput(inputId = "inCMEQ", label = "Select Equity (EQ):", choices =  uBDEQ),
                    #                                      selectInput(inputId = "inCMEB", label = "Select Ebike (EB):", choices =  uBDEB),
                    #                                      HTML("<hr>"),
                    #                                      selectizeInput("inCMAG", "Age Group:", ag, selected = ag[1], multiple = F),
                    #                                      radioButtons("inCMGender", "Gender: ", gender, inline = TRUE),
                    #                                      selectizeInput("inCMSES", "Socio Economic Classification :", ses, selected = ses[1], multiple = F),
                    #                                      selectizeInput("inCMEthnicity", "Ethnic Group:", ethnicity, selected = ethnicity[1], multiple = F)
                    #                     )
                    #                     
                    #                     
                    #                   ),
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
                      tabPanel("Mode Share", value = 2,
                               showOutput('plotBDMode', "highcharts"),
                               showOutput('plotBDSCMode', "highcharts")
                      ),
                      tabPanel("Health", value = 3,
                               showOutput('plotYLL', "highcharts"),
                               showOutput('plotYLLReduction', "highcharts")
                               
                      ),
                      tabPanel("Physical Activity", value = 4,
                               showOutput('plotMET', "highcharts"),
                               showOutput('plotScenarioMET', "highcharts")
                      )
#                       ,
#                       tabPanel("Miles Cycled", value = 5,
#                                showOutput('plotMilesCycled', 'highCharts')
#                       ),
#                       tabPanel("Car Miles", value = 6
#                                #showOutput('plotMilesCycled', 'highCharts')
#                       )
                      
                      
                      
                      #,
                      #                       
                      #                       tabPanel("Mode of Travel", value = 3,
                      #                                showOutput('plotMode', "highcharts")
                      #                       ), 
                      #                       tabPanel("Physical Activity", value = 4,
                      #                                showOutput('plotMET', "highcharts"),
                      #                                showOutput('plotScenarioMET', "highcharts")
                      #                       ),
                      
                      #                       tabPanel("Trip Duration", value = 6,
                      #                                #showOutput('plotTripTime', "highcharts"),
                      #                                showOutput('plotTripTimeDifference', "highcharts")
                      #                                
                      #                       ),
                      #                       tabPanel("Trip Faster/Slower", value = 7,
                      #                                showOutput('plotBDFasterTrips', "highcharts"),
                      #                                showOutput('plotBDSlowerTrips', "highcharts")
                      #                       ),
                      #                       tabPanel("Car Miles", value = 8,
                      #                                showOutput('plotCarTripsCycled', "highcharts")
                      #                       )
                      ,
                      id = "conditionedPanels"
                    )
                  )
))