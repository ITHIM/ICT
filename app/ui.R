source("setup.R")
uniqueMS <- sort(unique(sdata$MS))
uMS <- append("All", sort(unique(sdata$MS)))
uEQ <- append("All",sort(unique(sdata$equity)))
uEB <- append("All", sort(unique(sdata$ebike)))

# uBDMS <- (sort(unique(msharedtata$MS)[-1]) - 1)
# uBDEQ <- sort(unique(msharedtata$equity))
# uBDEB <- sort(unique(msharedtata$ebike))

variableList <- t(as.matrix(colnames(sdata)))
variableList <- variableList[,6:length(colnames(sdata))]

scenarios <- c("Trip Mode Share" = "t",
               "Individual METs" =    "i")

METSwitchRButton <- c("Baseline and Scenario" = "sep",
                      "Scenario versus Baseline" =    "comp")

switchRButton <- c("Scenario versus Baseline" =    "comp",
                   "Sub-population versus total population" = "sep")

denominatorRButton <- c("Total Population" = "pop",
                        "Total Cyclists" = "cyc")

phyGLRButton <- c("On" = "on",
                  "Off" =    "off")

onOffRButton <- c("On" = 1,
                  "Off" = 0)

allOnOffRButton <- c("All" = "All",
                     "On" = 1,
                     "Off" = 0)

healthRButton <- c("Years of Life Lost (YLL)" = "YLL", 
                   "Deaths" = "Deaths")

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

shinyUI(fluidPage(useShinyjs(),
                  width="100%", height="100%",
                  a(bsCollapse(id = "intro", bsCollapsePanel(
                    tags$div(title = "Click here to open an introductory document", 
                             h4("Impacts of Cycling Tool"),
                             tags$style(HTML("
                              h4 {
                                font-size: 25px;
                                font-weight: bold;
                                line-height: 1.1;
                                text-decoration:underline;
                              }
                              h4:hover {
                                background-color: #C0C0C0;
                              } 
                              /* selected link
                              a:active {
                                color: yellow;
                              }  */
                        
                            "))
                             
                    ), 
                    includeMarkdown("README.md")))),
                  sidebarPanel(
                    conditionalPanel(condition="input.conditionedPanels == 1",
                                     radioButtons(inputId = "inEQ", label = "Select Equity (EQ):", allOnOffRButton, inline = TRUE),
                                     radioButtons(inputId = "inEB", label = "Select Ebike (EB):", allOnOffRButton, inline = TRUE),
                                     selectInput("varname", label = "Plot Variable:", variableList)
                    ),
                    
                    conditionalPanel(condition="input.conditionedPanels == 2",
                                     selectInput(inputId = "inBDMS", label = "Select Cycling Multiplier:", choices =  uniqueMS),#, selected = uBDMS[2]),
                                     radioButtons(inputId = "inBDEQ", label = "Select Equity (EQ):", onOffRButton, inline = TRUE),
                                     radioButtons(inputId = "inBDEB", label = "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE),
                                     HTML("<hr>"),
                                     selectizeInput("inBDAG", "Age Group:", ag, selected = ag[1], multiple = F),
                                     radioButtons("inBDGender", "Gender: ", gender, inline = TRUE),
                                     selectizeInput("inBDSES", "Socio Economic Classification :", ses, selected = ses[1], multiple = F),
                                     radioButtons("inBDEthnicity", label = "Ethnic Group:", ethnicity, inline = TRUE),
                                     HTML("<hr>"),
                                     radioButtons("flipMS", label = "Flip Histogram:", switchRButton, inline = TRUE)
                                     
                    ),
                    conditionalPanel(condition="input.conditionedPanels == 3",
                                     selectInput(inputId = "inHealthMS", label = "Select Cycling Multiplier:", choices =  uniqueMS),#uBDMS, selected = uBDMS[2]),
                                     radioButtons(inputId = "inHealthEQ", label = "Select Equity (EQ):", onOffRButton, inline = TRUE),
                                     radioButtons(inputId = "inHealthEB", label = "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE),
                                     HTML("<hr>"),
                                     radioButtons("inHealthSwitch", label = "Comparison with:", c("Baseline", "Scenario"), inline = TRUE),
                                     HTML("<hr>"),
                                     conditionalPanel(
                                       condition = "input.inHealthSwitch == 'Scenario'",
                                       selectInput(inputId = "inHealthMS1", label = "Select Cycling Multiplier:", choices =  uniqueMS),
                                       radioButtons(inputId = "inHealthEQ1", label = "Select Equity (EQ):", onOffRButton, inline = TRUE),
                                       radioButtons(inputId = "inHealthEB1", label = "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE)
                                     ),
                                     HTML("<hr>"),
                                     selectizeInput("inHealthAG", "Age Group:", healthAG, selected = healthAG[1], multiple = F),
                                     radioButtons("inHealthG", "Gender: ", genderForHealthCalculations, inline = TRUE),
                                     HTML("<hr>"),
                                     radioButtons("inHealthVarSwitch", label = "Variable:", healthRButton, inline = TRUE)
                                     
                    ),
                    conditionalPanel(condition="input.conditionedPanels == 4",
                                     selectInput(inputId = "inMETMS", label = "Select Cycling Multiplier:", choices =  uniqueMS),#, selected = uBDMS[2]),
                                     radioButtons("inMETEQ", "Select Equity (EQ):", onOffRButton, inline = TRUE),
                                     radioButtons("inMETEB", "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE),
                                     
                                     HTML("<hr>"),
                                     selectizeInput("mag", "Age Group:", ag, selected = ag[1], multiple = F),
                                     radioButtons("mgender", "Gender: ", gender, inline = TRUE),
                                     selectizeInput("mses", "Socio Economic Classification :", ses, selected = ses[1], multiple = F),
                                     radioButtons("methnicity", label = "Ethnic Group:", ethnicity, inline = TRUE),
                                     HTML("<hr>"),
                                     radioButtons("flipMETHG", label = "Flip Histogram:", switchRButton, inline = TRUE),
                                     HTML("<hr>"),
                                     radioButtons("phyGuideline", label = "% Meeting WHO Physical Guideline", phyGLRButton, selected = phyGLRButton[2], inline = TRUE)
                    )
                    ,
                    conditionalPanel(condition="input.conditionedPanels == 5",
                                     selectInput(inputId = "inMSMS", label = "Select Cycling Multiplier:", choices =  uniqueMS),#uBDMS, selected = uBDMS[2]),
                                     radioButtons(inputId = "inMSEQ", label = "Select Equity (EQ):", onOffRButton, inline = TRUE),
                                     radioButtons(inputId = "inMSEB", label = "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE),
                                     HTML("<hr>"),
                                     selectizeInput("inMSAG", "Age Group:", ag, multiple = F),
                                     radioButtons("inMSG", "Gender: ", gender, inline = TRUE),
                                     selectizeInput("inMSSES", "Socio Economic Classification :", ses, multiple = F),
                                     radioButtons("inMSEthnicity", label = "Ethnic Group:", ethnicity, inline = TRUE),
                                     HTML("<hr>"),
                                     radioButtons("inMSflip", label = "Flip Histogram:", switchRButton, inline = TRUE),
                                     HTML("<hr>"),
                                     radioButtons("inMSTotOrCyc", label = "Denominator:", denominatorRButton, inline = TRUE)
                    ),
                    conditionalPanel(condition="input.conditionedPanels == 6",
                                     selectInput(inputId = "inCMMS", label = "Select Cycling Multiplier:", choices =  uniqueMS),#uBDMS, selected = uBDMS[2]),
                                     radioButtons(inputId = "inCMEQ", label = "Select Equity (EQ):", onOffRButton, inline = TRUE),
                                     radioButtons(inputId = "inCMEB", label = "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE),
                                     HTML("<hr>"),
                                     selectizeInput("inCMAG", "Age Group:", ag, multiple = F),
                                     radioButtons("inCMG", "Gender: ", gender, inline = TRUE),
                                     selectizeInput("inCMSES", "Socio Economic Classification :", ses),
                                     radioButtons("inCMEthnicity", label = "Ethnic Group:", ethnicity, inline = TRUE),
                                     HTML("<hr>"),
                                     radioButtons("inCMflip", label = "Flip Histogram:", switchRButton, inline = TRUE)
                                     
                    ),
                    conditionalPanel(condition="input.conditionedPanels == 7",
                                     selectInput(inputId = "inCO2MS", label = "Select Cycling Multiplier:", choices =  uniqueMS),#uBDMS, selected = uBDMS[2]),
                                     radioButtons(inputId = "inCO2EQ", label = "Select Equity (EQ):", onOffRButton, inline = TRUE),
                                     radioButtons(inputId = "inCO2EB", label = "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE),
                                     HTML("<hr>"),
                                     selectizeInput("inCO2AG", "Age Group:", ag, multiple = F),
                                     radioButtons("inCO2G", "Gender: ", gender, inline = TRUE),
                                     selectizeInput("inCO2SES", "Socio Economic Classification :", ses),
                                     radioButtons("inCO2Ethnicity", label = "Ethnic Group:", ethnicity, inline = TRUE),
                                     HTML("<hr>"),
                                     radioButtons("inCO2flip", label = "Flip Histogram:", switchRButton, inline = TRUE)
                    )
                    
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Scenarios", value = 1,
                               showOutput("plotCycPercent", "highcharts"),
                               showOutput("plotGenericVariable", "highcharts")
#                                h6("(Click on the legend to enable/disable a line)", align="center"),
#                                HTML("<style>iframe.rChart{ width: 100%; height: 400px;}</style>")
                      ),
                      tabPanel("Mode Share", value = 2,
                               a(id = "MSHelp", "Help", href = "#"),
                               # showOutput("myChart","Nvd3")
#                                div(class='wrapper',
#                                    tags$style(".Nvd3{ height: 400px;}"),
#                                    showOutput("myChart","Nvd3")
#                                )
                               
                               hidden (div(id = "MSHelpText",
                                           helpText("Displays plots for mode share of trips based on main mode only.  
                                                    A scenario is selected by a combination of three inputs: Cycling Multiplier, Equity and Ebike. 
                                                    Users can investigate the mode share for the whole pouplation and population subgroups either at baseline or under one of the scenarios.")
                               )),
                               showOutput("plotBDMode", "highcharts"),
                               showOutput("plotBDSCMode", "highcharts")
                      ),
                      tabPanel("Health", value = 3,
                               a(id = "HealthHelp", "Help", href = "#"),
                               hidden (div(id = "HealthHelpText",
                                           helpText("Displays two plots for health gains in terms of Years of Life Lost (YLL) and Reduction in YLLs. YLL is an estimate of the average years a person would have lived if he or she had not died prematurely. 
                                            A scenario is selected by a combination of three inputs: Cycling Multiplier, Equity and Ebike.
                                            Sub-population filter is only applied for age and gender groups.")
                               )),
                               showOutput("plotHealth", "highcharts"),
                               showOutput("plotHealthReduction", "highcharts")
                      ),
                      tabPanel("Physical Activity", value = 4,
                               a(id = "PAHelp", "Help", href = "#"),
                               hidden (div(id = "PAHelpText",
                                           helpText(p("Displays histogram of total physical activity and also the fraction of the population meeting the WHO guidelines. 
                                                    For comparison with WHO guidelines, we have assumed that 150 minutes of walking per week (the mininum PA to meet the guidelines), roughly translates into 100 minutes of cycling per week. 
                                                    In terms of Marginal MET (MMET) per week, it translates into 8.5 MMET. Higher PA guideline assumes 300 minutes of walking per week, or 200 minutes of cycling per week, which translates to 17.5 MMET per week. 
                                                    Allows comparison of total and sub-population comparison of selected scenario and baseline.
                                                    "))
                               )),
                               showOutput("plotMET", "highcharts"),
                               showOutput("plotScenarioMET", "highcharts")
                      ),
                      tabPanel("Miles Cycled", value = 5,
                               a(id = "MCHelp", "Help", href = "#"),
                               hidden (div(id = "MCHelpText",
                                           helpText("Displays two plots for total miles cycled per cyclist per week, where a selected scenario is compared with the baseline. Users can switch from total population to total cyclist by the Denomintor switch.")
                               )),
                               showOutput("plotMilesCycled", "highcharts"),
                               showOutput("plotFilteredMilesCycled", "highcharts")
                      ),
                      tabPanel("Car Miles", value = 6,
                               a(id = "CMHelp", "Help", href = "#"),
                               hidden (div(id = "CMHelpText",
                                           helpText("Displays two plots for the total Car Miles (Car driver, Car passenger, Motorcycle and Taxi) per week for the whole populatioin in the selected scenario and baseline. Users can compare total and sub-population of scenario and baseline with each other")
                               )),
                               showOutput("plotFilteredCarMiles", "highcharts"),
                               showOutput("plotCarMiles", "highcharts")
                      ),
                      tabPanel("CO2", value = 7,
                               a(id = "CO2Help", "Help", href = "#"),
                               hidden (div(id = "CO2HelpText",
                                           helpText("Displays two plots for CO2 produced by only car related trips (both driver and passenger). Users can compare a selected scenario against the baseline")
                               )),
                               showOutput("plotFilteredCO2", "highcharts"),
                               showOutput("plotCO2", "highcharts")
                      ),
                      id = "conditionedPanels"
                    )
                  )
))