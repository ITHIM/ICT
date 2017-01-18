source("setup.R")

regions <- generateRegionsList()

# Remove MS 0, as it represents baseline values
uMS <- append("All", sort(unique(sdata$MS))[-1])
uEQ <- append("All",sort(unique(sdata$equity)))
uEB <- append("All", sort(unique(sdata$ebike)))

variableList <- t(as.matrix(colnames(sdata)))
variableList <- variableList[,7:length(colnames(sdata))]

# Mode Share
# Miles Cycled
# Physical Activity
# Health
# Car Miles
# CO2

variableRButton <- c("Number of Cyclists" =    "% Cyclists in the Total Population",
                     "Miles Cycled" = "Total Car Miles Cycled (per week)",
                     "Physical Activity" = "Marginal METs Per Person (per week)",
                     "Health" = "Years of Life Lost (YLL)",
                     "Car Miles" = "Car Miles Per person (per week)",
                     "CO2" = "Transport CO2 Per Person (per week)"
)
carMilesRButton <- c("Car Miles" = "Car Miles Per person (per week)",
                     "Car Miles Reduced" = "Car Miles Reduced Per person (per week)")


# % cyclists in the total population	Miles cycled per person per week	Car miles cycled  per week	Marginal METs per person per week	Car miles per person per week	Car miles reduced per person per week	CO2 from car travel per person per week


scenarios <- c("Trip Mode Share" = "t",
               "Individual METs" =    "i")

METSwitchRButton <- c("Baseline and Scenario" = "sep",
                      "Scenario versus Baseline" =    "comp")

switchRButton <- c("Scenario versus Baseline/alternative Scenario" =    "comp",
                   "Sub-population versus total population" = "sep")

denominatorRButton <- c("Total Population" = "pop",
                       "Total Cyclists" = "cyc")

# denominatorRButton <- c("Total Cyclists" = "cyc")


phyGLRButton <- c("Marginal MET hours" =    "off",
                  "% meeting WHO physical activity guidelines" = "on")

TTRButton <- c("Histogram" =    "histogram",
               "Slower/Faster trips" = "trip speed")

onOffRButton <- c("On" = 1,
                  "Off" = 0)

allOnOffRButton <- c("All" = "All",
                     "On" = 1,
                     "Off" = 0)

healthRButton <- c("Years of Life Lost (YLL)" = "YLL", 
                   "Deaths" = "Deaths")

ag <- c("All", "18 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79")

healthAG <- c("All", "18 - 39", "40 - 59", "60 - 79")

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

# default MS/DP values are: 0.05 0.10 0.15 0.25 0.50 0.75 1.00
# for init use the first (default) region from the list
uniqueMS <- generateUniqueMS(region=unname(regions[1]))

# for init use the first (default) region from the list
# used in "Mode Share" in alternative region
regionsList <- generateRegionsList(region=unname(regions[1]))

shinyUI(fluidPage(
  list(tags$title(HTML('Impacts of Cycling Tool'))),
  useShinyjs(),
  width="100%", height="100%",
  uiOutput("oSample"),
  tags$head(
    includeScript("www/assets/extra.js"),
    includeCSS("www/assets/extra.css")
  ),
  a(id = "mainIntro", list(h4("Impacts of Cycling Tool"), h6("(show/hide information)")), href = "#", title = "Click here to open an introductory document"),
  hidden (div(id = "mainIntroText",
              includeMarkdown("README.md")
  )),
  selectInput(inputId = "inRegions", label = "Select Region:", choices =  regions),
  # there is an issue with bsCollapse - it doesn't close
  # a(bsCollapse(id = "intro",
  #              bsCollapsePanel(
  #                #                  tags$div(title="Show/Hide zone legend",
  #                #                           a(id = "toggle_panel", style="font-size: 80%", span(class="glyphicon glyphicon-circle-arrow-up", "Hide"))
  #                #                  ),
  #                tags$div(title = "Click here to open an introductory document",
  #                         h4("Impacts of Cycling Tool"),
  #                         h6("(show/hide information)"),
  #                         tags$style(HTML("
  #                             h4 {
  #                               font-size: 25px;
  #                               font-weight: bold;
  #                               line-height: 1.1;
  #                               //text-decoration:underline;
  #                             }
  #                             h4:hover {
  #                               background-color: #C0C0C0;
  #                             }
  #                             h6 {
  #                               color: #6495ED;
  #                               //text-decoration:none;
  #                             }
  #                           "))
  # 
  #                ),
  # 
  #                includeMarkdown("README.md")))
  # 
  # 
  # 
  # )
  # ,
  sidebarPanel(
    
    tags$div(title="Shows % of the total population for the selected region who cycle at least weekly at baseline",
             uiOutput("inBaselineCycling")
    ),
    
    HTML("<hr>"),
    conditionalPanel(condition="input.conditionedPanels == 1",
                     tags$div(title="Select percentage of total regional population who are as likely to cycle based on trip distance as existing cyclists",
                              selectInput(inputId = "inBDMS", label = "Select % of Population who are Potential Cyclists:", choices =  uniqueMS)
                     ),
                     
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
    conditionalPanel(condition="input.conditionedPanels == 2",
                     selectInput(inputId = "inTTMS", label = "Select % of Population who are Potential Cyclists:", choices =  uniqueMS),#, selected = uBDMS[2]),
                     radioButtons(inputId = "inTTEQ", label = "Select Equity (EQ):", onOffRButton, inline = TRUE),
                     radioButtons(inputId = "inTTEB", label = "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE),
                     HTML("<hr>"),
                     selectizeInput("inTTAG", "Age Group:", ag, selected = ag[1], multiple = F),
                     radioButtons("inTTGender", "Gender: ", gender, inline = TRUE),
                     selectizeInput("inTTSES", "Socio Economic Classification :", ses, selected = ses[1], multiple = F),
                     radioButtons("inTTEthnicity", label = "Ethnic Group:", ethnicity, inline = TRUE),
                     #HTML("<hr>"),
                     #radioButtons("flipTT", label = "Flip Histogram:", switchRButton, inline = TRUE),
                     HTML("<hr>"),
                     radioButtons("flipTTPlot", label = "Flip Plot:", TTRButton, inline = TRUE)

    ),
    conditionalPanel(condition="input.conditionedPanels == 3",
                     selectInput(inputId = "inMSMS", label = "Select % of Population who are Potential Cyclists:", choices =  uniqueMS),#uBDMS, selected = uBDMS[2]),
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
    conditionalPanel(condition="input.conditionedPanels == 4",
                     
                     tags$div(title="Select percentage of total regional population who are as likely to cycle based on trip distance as existing cyclists",
                              selectInput(inputId = "inMETMS", label = "Select % of Population who are Potential Cyclists:", choices =  uniqueMS)
                     ),
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
                     radioButtons("phyGuideline", label = "Physical activity outcome measure:", phyGLRButton, inline = TRUE)
    )
    ,
    conditionalPanel(condition="input.conditionedPanels == 5",
                     selectInput(inputId = "inHealthMS", label = "Select % of Population who are Potential Cyclists:", choices =  uniqueMS),#uBDMS, selected = uBDMS[2]),
                     radioButtons(inputId = "inHealthEQ", label = "Select Equity (EQ):", onOffRButton, inline = TRUE),
                     radioButtons(inputId = "inHealthEB", label = "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE),
                     HTML("<hr>"),
                     radioButtons("inHealthSwitch", label = "Comparison with:", c("Baseline" = "Baseline","An alternative scenario"= "Scenario"), inline = TRUE),
                     HTML("<hr>"),
                     conditionalPanel(
                       condition = "input.inHealthSwitch == 'Scenario'",
                       selectInput(inputId = "inHealthMS1", label = "Select % of Population who are Potential Cyclists:", choices =  uniqueMS),
                       radioButtons(inputId = "inHealthEQ1", label = "Select Equity (EQ):", onOffRButton, inline = TRUE),
                       radioButtons(inputId = "inHealthEB1", label = "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE)
                     ),
                     HTML("<hr>"),
                     # Use same age groups for now
                     selectizeInput("inHealthAG", "Age Group:", healthAG, selected = healthAG[1], multiple = F),
                     radioButtons("inHealthG", "Gender: ", gender, inline = TRUE),
                     HTML("<hr>"),
                     radioButtons("inHealthVarSwitch", label = "Variable:", healthRButton, inline = TRUE)
                     
    ),
    conditionalPanel(condition="input.conditionedPanels == 6",
                     selectInput(inputId = "inCMMS", label = "Select % of Population who are Potential Cyclists:", choices =  uniqueMS),#uBDMS, selected = uBDMS[2]),
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
                     tags$div(title="Select percentage of total regional population who are as likely to cycle based on trip distance as existing cyclists",
                              selectInput(inputId = "inCO2MS", label = "Select % of Population who are Potential Cyclists:", choices =  uniqueMS)
                     ),
                     radioButtons(inputId = "inCO2EQ", label = "Select Equity (EQ):", onOffRButton, inline = TRUE),
                     radioButtons(inputId = "inCO2EB", label = "Select Ebike (EB):", onOffRButton, selected = onOffRButton[2], inline = TRUE),
                     HTML("<hr>"),
                     selectizeInput("inCO2AG", "Age Group:", ag, multiple = F),
                     radioButtons("inCO2G", "Gender: ", gender, inline = TRUE),
                     selectizeInput("inCO2SES", "Socio Economic Classification :", ses),
                     radioButtons("inCO2Ethnicity", label = "Ethnic Group:", ethnicity, inline = TRUE),
                     HTML("<hr>"),
                     radioButtons("inCO2flip", label = "Flip Histogram:", switchRButton, inline = TRUE)
    ),
    conditionalPanel(condition="input.conditionedPanels == 8",
                     radioButtons(inputId = "inEQ", label = "Select Equity (EQ):", allOnOffRButton, inline = TRUE),
                     radioButtons(inputId = "inEB", label = "Select Ebike (EB):", allOnOffRButton, inline = TRUE),
                     HTML("<hr>"),
                     
                     radioButtons("varname", label = "Plot Variable:", variableRButton),
                     
                     conditionalPanel(
                       condition = "input.varname == 'Car Miles Per person (per week)'",
                       radioButtons("CMVarName", label = "Car Miles Variable:", carMilesRButton)
                     )
    ),
    # for now only "Mode Share", "Physical Activity", "Car Miles", "CO2"
    conditionalPanel(condition="[1, 4, 6, 7].indexOf(parseInt(input.conditionedPanels)) > -1",
                      HTML("<hr>"),
                      radioButtons("inRegionSwitch", label = "Comparison with:", c("Baseline" = "Baseline", "An alternative Region" = "Region"), inline = TRUE),
                      conditionalPanel(
                        condition = "input.inRegionSwitch == 'Region'",
                        HTML("<hr>"),
                        selectInput(inputId = "inRegionSelected", label = "Select Region:", choices = regionsList),
                        hidden(p(id = "region-switch-warning", class = "region-switch-warnings", ""))
                      )
    )
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Mode Share", value = 1,
               a(id = "MSHelp", "Help?", href = "#"),
               hidden (div(id = "MSHelpText",
                           helpText(HTML("Displays plots for mode share of trips based on main mode only. A scenario is selected by a combination of 
                                         three inputs: % of Population who are Potential Cyclists, Equity and Ebike. Users can choose to compare mode share between selected 
                                         sub-populations and the total population, and/or between selected scenarios and baseline."))
               )),
               showOutput("plotBDMode", "highcharts"),
               showOutput("plotBDSCMode", "highcharts")
      ),
      tabPanel("Journey Time", value = 2,
               a(id = "MTHelp", "Help?", href = "#"),
               hidden (div(id = "MTHelpText",
                           helpText(HTML("Displays plots of the change in journey time for trips that have been switched to cycling in a scenario, stratified by the previous main mode of the trip. A scenario is selected by a combination of
                                         three inputs: % of Population who are Potential Cyclists, Equity and Ebike. Users can choose to compare mode share between selected
                                         sub-populations and the total population, and/or between selected scenarios and baseline."))
               ))
      ,
      showOutput("plotTTFilteredMode", "highcharts"),
      showOutput("plotTTTotalMode", "highcharts")
      ),
      tabPanel("Miles Cycled", value = 3,
               a(id = "MCHelp", "Help?", href = "#"),
               hidden (div(id = "MCHelpText",
                           helpText(HTML("Displays two plots for total miles cycled per cyclist per week, where a selected scenario is compared with
                                         the baseline. Note that the bar charts do not include a bar for people with zero cycling.
                                         In order to see the total number of cyclists in scenarios, please refer to the &#39;Number of Cyclists&#39; in the <i>Summary</i> tab.
                                         Users can use the &#39;Denominator&#39; option to switch between showing percentages relative to a) the
                                         total population or b) all cyclists.  Users can choose to compare miles cycled between selected sub-populations and the
                                         total population, and/or between selected scenarios and baseline."))
               ))
               ,
               showOutput("plotMilesCycled", "highcharts"),
               showOutput("plotFilteredMilesCycled", "highcharts")
      ),
      tabPanel("Physical Activity", value = 4,
               a(id = "PAHelp", "Help?", href = "#"),
               hidden (div(id = "PAHelpText",
                           helpText(
                             p("Displays histogram of total physical activity and also the percentage of the population meeting the physical activity guidelines of the World Health Organization (WHO). "),
                             p(HTML("The <a href='http://www.who.int/dietphysicalactivity/factsheet_recommendations/en/' target='_blank'>WHO guidelines</a> are for 150 minutes of moderate intensity or 75 minutes of vigorous intensity activity, 
                              with additional benefits by achieving 300 minutes of moderate intensity or 150 minutes of vigorous intensity activity. We have translated these guidelines into Marginal Metabolic Equivalent Task (MMET) 
                            hours per week. MMETs represent the body mass adjusted energy expenditure above resting. 
                            To do this we have assumed that moderate intensity activity is 3.5 MMETs, meaning that the lower target is 8.75 MMET hours per week, 
                            and the higher target is 17.5 MMET hours per week. We have assumed the MMET rates are 3.6 for walking, 5.4 for cycling, and 3.5 for ebikes (<a href='http://www.ncbi.nlm.nih.gov/pubmed/26441297' target='_blank'>Costa et al., 2015</a>  and <a href='http://link.springer.com/article/10.1007/s00421-012-2382-0/fulltext.html' target='_blank'>Sperlich et al., 2012</a>). 
                            Thus the lower target could be achieved by 145 minutes per week of walking, 97 minutes of cycling, or 150 minutes of ebiking. ")),
                             p(HTML("Non-travel activity is estimated using self-reported data from probabilistically matched individuals of a similar age, gender, and ethnicity from the <a href = 'https://data.gov.uk/dataset/health_survey_for_england' target='_blank'>Health Survey for England 2012</a>. ")),
                             p("Users can choose to compare physical activity between selected sub-populations and the total population, and/or between selected scenarios and baseline. "))
               )),
               showOutput("plotMET", "highcharts"),
               showOutput("plotScenarioMET", "highcharts")
      ),
      tabPanel("Health", value = 5,
               a(id = "HealthHelp", "Help?", href = "#"),
               hidden (div(id = "HealthHelpText",
                           helpText(HTML("
                                  Displays two plots for health gains measured as Years of Life Lost (YLL) and Premature Deaths Averted. 
                                  YLLs are taken from the <a href='http://www.healthdata.org/gbd' target='_blank'>Global Burden of Disease Study for the UK 2013</a>. 
                                  YLL is an estimate of the age specific life expectancy against an &#39;ideal&#39; reference population. 
                                  A scenario is selected by a combination of three inputs: % of Population who are Potential Cyclists, Equity and Ebike &#45; 
                                  this scenario can then be compared against baseline or against an alternative scenario. Results are presented by 
                                  age and gender, or the display can be restricted to particular age and gender groups using the subpopulation option. "))
               )),
               showOutput("plotHealth", "highcharts"),
               showOutput("plotHealthReduction", "highcharts")
      ),
      tabPanel("Car Miles", value = 6,
               a(id = "CMHelp", "Help?", href = "#"),
               hidden (div(id = "CMHelpText",
                           helpText(HTML("Displays two plots for total Car Miles per week for the whole populatioin in the selected
                                                    scenario and baseline. Car Miles are calculated as the sum of all miles spent travelling as a
                                                    car/van driver, a car/van passenger, by motorcycle or by taxi.  Users can choose to compare car
                                                    miles between selected sub-populations and the total population, and/or between selected scenarios and baseline."))
               ))
               ,
               showOutput("plotFilteredCarMiles", "highcharts"),
               showOutput("plotCarMiles", "highcharts")
      ),
      tabPanel(HTML("CO<sub>2<sub>"), value = 7,
               a(id = "CO2Help", "Help?", href = "#"),
               hidden (div(id = "CO2HelpText",
                           helpText(HTML("Displays two plots for CO2 produced during car travel, defined as travel as a car/van driver or 
                                                          car/van passenger.  Users can choose to compare CO2 emissions from car travel between selected 
                                                          sub-populations and the total population, and/or between selected scenarios and baseline."))
               )),
               showOutput("plotFilteredCO2", "highcharts"),
               showOutput("plotCO2", "highcharts")
      ),
      tabPanel("Summary", value = 8,
               showOutput("plotGenericVariable", "highcharts")
      ),
      tabPanel("About", value = 9,
               includeHTML("about.html")
      ),
      
      id = "conditionedPanels"
    )
  )
))