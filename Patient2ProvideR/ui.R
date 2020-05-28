library(shinythemes)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(readr)
library(janitor)
library(dplyr)
library(reactable)
library(zipcode)
library(geosphere)
library(d3heatmap)
library(stringr)
library(shinycssloaders)
library(kableExtra)
library(tigris)
library(leaflet)
library(mapview)
library(viridis)


# Define UI for application that draws a histogram
shinyUI(fluidPage(#shinythemes::themeSelector(),
                  theme = shinytheme("simplex"),
navbarPage("Patient2ProvideR",
           mainPanel(img(src='img/image.png', align = "center", width="100%"),
                     br(), br(), br(), "Dar'ya Pozhidayeva | Oregon Health & Science University |
                     OCTRI Evaluation Core | 2020"),
           
                             tabPanel("Overview & Instructions",
                             div(
                             h3("Overview"),
                             hr(),
                             "Disparities in spatial access to healthcare are a prevalent and well-documented barrier disproportionately impacting rural communities. Spatial access refers to the access a patient has to a primary healthcare provider in the context of their geographic location. Geographic location is an essential factor underlying these disparities through limiting physical access to healthcare. For this reason, spatial access is often analyzed in terms of travel times and distances between two distinct geographic points--in healthcare these points represent clinics, hospitals or patients.",
                             br(), br(),
                             "Healthcare is a field that regularly uses maps to illustrate indicators such as accessibility, but which often lacks resources and/or the ability to obtain them quickly. This limitation impacts community and researcher access to software such as Geographic Information System (GIS) which, although reliable and widely utilized, is also expensive and complex.",
                             br(), br(),
                             "To address this lack of user-friendly mapping software, we developed this application for geospatial visualization that can be used reliably for a number of location-based healthcare datasets. With the goal of increasing the approachability of geospatial visualization, our team selected three distinct location-based datasets representing available healthcare provider and patient locations across Oregon and used them to develop a mapping and visualization application in R and R shiny.",
                             br(),br(),
                             h3("Instructions & Usage"),
                             hr(),
                             "The general usage of this app involves uploading data in the 'Data Entry' tab navigation bar, which is then processed and visualized in the 'Results and Visualization tab.",
                             "This section discusses the specific requirements for data formating before data is uploaded for analysis.",
                             br(), br(),
                             h4("Data Format Requirements"),
                             "Data to be analysed in this application will likely require cleaning and wrangling before being uploaded. Pre-processing data is", strong("crucial"), "for the analysis to be properly executed.",
                             "Data entered must be in the", strong("CSV"), "file format and", strong("at minimum"), "contain three columns corresponding to: a patient-provider indicator, zipcodes and associated counties.",
                             "Zipcodes and counties entered in the data can pertain to any of the 50 states in the United States.",
                             "The table below shows a general example of the expected format for data entry. The column names in data entered into the app",  strong("must match the example below exactly"), "(with the exception of case).",
                             br(), br(),
                             reactableOutput("example_data"),
                             br(), br(),
                             "The data must be in the long format where patient and provider data is listed in a single indicator column.",
                             "The patient and provider indicator must appear exactly as it does above, however this indicator is generic. Therefore if your data is grouped differently, one group must be assigned the patient designation and the other the provider designation.",
                             "An option to add a", strong("sub indicator"), "column is provided in data entry.",
                             strong("The entry of a sub indicator is not required."),
                             "This sub indicator can be either a numeric or character type variable.",
                             br(), br(),
                             h4("Data Entry"),
                             "The data entry tab in the navigation bar provides an area in where data can be uploaded. Once uploaded, the read in settings can be adjusted until the data appears in the format described above.",
                             strong("It is important in this step to select the correct settings for data to be read in otherwise the analysis will fail or produce inaccurate results."),
                             "When the data looks correct, navigating to the 'Results and Visualization' tab will automatically begin the analysis.",
                             br(), br(),
                             h4("Results"),
                             "Once the data is processed, the results will load automatically on their corresponding pages. Download buttons are available for all visible data tables as well as maps produced.",
                             "It is important to note that turn around time to generate results will vary widely depending on the size of the data entry, therefore once the data is loaded and being analyzed,",
                             strong("the page should not be refreshed. If the page for the application is refreshed during the analysis step, the data entry and analysis will have to be restarted completely."),
                             br(), br(),
                             h4("Direct Example: Oregon Data Set"),
                             "Here is a direct example of a data set representing patients and providers from the state of Oregon. In this data, patients represnt the locations of random households in Oregon and providers represent the locations of any available dental clinics according to the Oregon Health Authority (OHA).",
                             "To create the data set we label all households with the 'patient' indicator and all dental clinics with the 'provider' inidicator.",
                             "Here we include a sub indicator column which represents a likert scale survey response (ranging from 1 to 10).", 
                             br(), br(),
                             reactableOutput("direct_example"),
                             br(),
                             "Once read in, this prepared CSV table will be used by the app to generate a heatmap, summary tables and maps.",
                             br(),br(),br(), style = "font-size:16px")
                             ),
                             tabPanel("Data Entry",
                             sidebarLayout(
                                          sidebarPanel(
                                          fileInput("file", "Choose CSV File",
                                                                  multiple = FALSE,
                                                                  accept = c("text/csv",
                                                                             "text/comma-separated-values,text/plain",
                                                                             ".csv")),
                                          # Input: Checkbox if file has header ----
                                          checkboxInput("header", "Header", TRUE),
                                          
                                          # Input: Select separator ----
                                          radioButtons("sep", "Separator",
                                                       choices = c(Comma = ",",
                                                                   Semicolon = ";",
                                                                   Tab = "\t"),
                                                       selected = ","),
                                          
                                          # Input: Select quotes ----
                                          radioButtons("quote", "Quote",
                                                       choices = c(None = "",
                                                                   "Double Quote" = '"',
                                                                   "Single Quote" = "'"),
                                                       selected = '"'),
                                          uiOutput("state"),
                                          #Input: Checkbox if data contains county level information ---
                                          #Input: Checkbox if data contains county level information ---
                                          radioButtons("additionalinfo","Does the data contain an additional indicator column?",
                                                       choices = c(Yes = "yes",
                                                                   No = "no"))),
                                          
                                          mainPanel(
                                            h4("Once the data is uploaded it will appear below."), 
                                            br(),
                                            br(),
                                          uiOutput('condPanel'),
                                          br(),
                                          withSpinner(reactableOutput("contents"), color ="#E4492A"),
                                          uiOutput('condPanel_note'), br())
                                          )),
                             
                             navbarMenu("Results and Visualization",
                              tabPanel("Summary and Distance Heatmap",
                              h3("Heatmap Output"),
                                                "The heatmap shows the calculated pairwise distances between patient counties and providers provider counties available in the data entered.",
                                                "Distances are shown in Miles. To view specific distance pairs, hover over any cell in the heatmap with your cursor.",
                                                "In this map, red corresponds to shorter distances and blue to longer.",
                                                uiOutput("datawarning"),
                                                br(), br(),
                                                withSpinner(d3heatmapOutput("heatmap", width = "100%", height = "600px"), color ="#E4492A"),
                                                br(),
                                               "You can download the distance matrix used to generate this heatmap here:", br(), br(),
                                                downloadButton('downloadheatmap',"Download Distance Matrix"),
                                                hr(),
                                                h3("Data Summary Table"),
                                                "The summary table below shows counts of unique zipcodes and counties for each of the patient and provider subgroups.",
                                                br(), "The tables that follow the general summary show the range of distances for each subgroup by county.",
                                                reactableOutput("summary"),
                                                hr(),
                                                h3("Patient to Provider Distances by County"),
                                                br(),
                                                reactableOutput("patientdistances"),
                                                downloadButton('downloadpatientdists',"Download Table Above"),
                                                br(), br(), br(),
                                                hr(),
                                                h3("Provider to Patient Distances by County"),
                                                reactableOutput("providerdistances"),
                                                downloadButton('downloadproviderdists',"Download Table Above"),
                                                hr()),
                              
                                      tabPanel("Choropleth Map",
                                               sidebarLayout(
                                                 sidebarPanel(
                                                 "Select how the data should be viewed in the choropleth:", br(), br(),
                                                 radioButtons("data_pov", "Mapping Point of View",
                                                                           choices = c("Patient to Provider" = "patient_view",
                                                                                       "Provider to Patient" = "provider_view"),
                                                                           selected = "patient_view"),
                                                 radioButtons("distancesummary", "Distance Summary Method",
                                                              choices = c("Minimum" = "dists_min",
                                                                          "Mean" = "dists_mean", 
                                                                          "Max" = "dists_max")),
                                                 "Select the variable overlay:",
                                                 checkboxInput("ind_overlay", "Overlay Patient and Provider Locations", FALSE),
                                                 uiOutput("add_ind_show")),
                                                 mainPanel(
                                                 h3("Leaflet Choropleth Map"),
                                                 "The choropleth below is a visualization of the calculated distances by county from the data provided.",
                                                 "The mapped point of view as well as the distance summary method can be changed using the selection pane on the left.",
                                                 uiOutput("datawarning1"),
                                                 uiOutput("ind_overlay_warning"),
                                                 withSpinner(leafletOutput("choropleth", width = "100%", height = "600px"), color ="#E4492A"),
                                                 br(),
                                                 br(),
                                                 downloadButton("savemap", "Download Map Snapshot")
                                               ))),
                              
                                      tabPanel("Explore Additional Indicators",
                                               mainPanel(
                                               h3("Additional Indicators in Data"),
                                               "if you selected 'yes' to additional indicators in your data you can plot additional maps using those indicators here.",
                                               "Additional indicators include subgroups for the patient or provider indicator which can be numeric or character variable types.",
                                               br(),
                                               br(),
                                               uiOutput("datawarning2"),
                                               uiOutput("add_ind_message"),
                                               withSpinner(leafletOutput("additional_map", width = "900px", height = "800px"), color ="#E4492A"),
                                               br(),
                                               br(),
                                               downloadButton("savemap_additional", "Download Map Snapshot"))
                                               )),
                             tabPanel("Methodology",
                                      div(
                                      h3("Methods Used to Create Patient2ProvideR"),
                                      hr(),
                                      h4("Test Data Set Used to Build P2P"),
                                      "The Patient2ProvideR app was built using test data created from provider locations from the Oregon Health Authority (OHA) and Oregon Medical Board (OMB). Patient sample data was 
                                      created using data from the Marketing Systems Group (MSG) through random stratified sampling with oversampling for rural counties. However for user data entry, any U.S. state is recognized.",
                                      h4("App Build"),
                                      "The application was built in R/R Studio using the shiny package. The application geocodes the zipcodes provided in the data file using the zipcode package and calculates the pairwise, Vincenty Ellipsoid straight-line distances in miles between patient and provider locations using the distm() function in the geosphere package.  The distances are then visualized in an interactive heatmap and choropleth, where the choropleth is generated using shapefiles from the tigris package which are visualized using the leaflet package.",
                                      h4("Heatmap"),
                                      "The heatmap is generated is the d3heatmap package. The pairwise distances calculated between patient and provider for zipcodes in the data are shown according to their counties. Clustering is enabled in order to get a quick overview of calculated distances.",
                                      h4("Choropleth"),
                                      "The main choropleth is generated by joining the calculated distances with U.S. shape files from the tigris package, which are then visualized using leaflet. Here, users can select how to summarize the distances calculated for their data as well as the data perspective. This is due to the fact that multiple zipcodes are often associated with the same county. Therefore, the choropleth produced is different for each summary method. Users can also select whether they want to overlay the original patient and provider locations on top of the choropleth as well as any additional indicator provided.",
                                      h4("Additional Indicators Map"),
                                      "In the last tab, users can plot their additional indicators if they’ve opted in to providing them. This will give a chance to show an independent map containing only indicators without the choropleth.",
                                      style = "font-size:16px")),
           
                             navbarMenu("More",
                                        tabPanel("About Our Team",
                                                 img(src='img/ohsu_tram.jpg', align = "center", width="100%"), br(),
                                                 hr(),
                                                 sidebarLayout(
                                                 sidebarPanel(
                                                 h3("Our Team"),
                                                 img(src='img/Adrienne.jpg', width ="140px", height = "150px"),br(),
                                                 strong("Adrienne Zell, Ph.D."), br(),
                                                 "Director, OHSU Evaluation Core",br(),
                                                 "Assistant Director, OCTRI", br(), br(),
                                                 
                                                 img(src='img/Liz.jpg', width ="140px", height = "150px"),br(),
                                                 strong("Elizabeth Wenzel, M.P.H."), br(),
                                                 "Senior Research Assistant", br(),
                                                 "OHSU Evaluation Core", br(), br(),
                                                 
                                                 img(src='img/Amy.jpg', width ="140px", height = "150px"),br(),
                                                 strong("Amy Wilson, M.P.H."), br(),
                                                 "Senior Research Assistant", br(),
                                                 "OHSU Evaluation Core", br(), br(),
                                                 
                                                 img(src='img/Matt.jpg', width ="140px", height = "150px"),br(),
                                                 strong("Matt Honoré, M.P.A."), br(),
                                                 "Senior Research Assistant", br(),
                                                 "BUILD EXITO", br(), br(),
                                                 
                                                 img(src='img/Kristina.jpg', width ="140px", height = "150px"),br(),
                                                 strong("Kristina Nelson,"), br(),
                                                 "Research Assistant II", br(),
                                                 "BUILD EXITO", br(), br(),
                                                 
                                                 img(src='img/Darya1.jpg', width ="84%", height = "75%"),br(),
                                                 strong("Dar'ya Pozhidayeva, B.S."), br(),
                                                 "Research Assistant II", br(),
                                                 "OHSU Evaluation Core", br(), br(), width = 2),
                                                 
                                                 mainPanel(h1("OHSU Evaluation Core"),
                                                 div(
                                                 "The OHSU Evaluation Core assists OHSU researchers and community organizations with planning and implementing effective program evaluation. We use rigorous and reproducible methods to measure health impacts and improve the wellbeing of patients, families, and communities.",
                                                 br(), br(),
                                                 h3("What do we do?"),
                                                 "- Designing and implementing evaluation plans, logic models, and SMART goals",
                                                 br(),br(),
                                                 "- Selecting and designing measurement instruments, including surveys and focus group/interview protocols",
                                                 br(),br(),
                                                 "- Collecting and analyzing qualitative and quantitative data",
                                                 br(),br(),
                                                 "- Building evaluation capacity through trainings and technical assistance",
                                                 br(),br(),
                                                 "- Developing communication and dissemination strategies, supporting the translation of research into practice",
                                                 br(),br(), 
                                                 "As evaluators, we work in partnership with researchers, organization staff, and intervention participants to measure and communicate program efficacy. We also collaborate with and provide referrals to OHSU programs including the Community Research Hub, the Biostatistics and Design Program, Clinical Research Informatics, the OHSU Library, and BUILD EXITO. ",
                                                 h3("Our Sponsors"),
                                                 "The OHSU Evaluation Core is sponsored by the Oregon Clinical & Translational Research Institute, Knight Cancer Institute, and our clients and partners.",
                                                 br(), h3("Where are we located?"), br(), br(),
                                                 div(img(src='img/SON.jpg'), style="text-align: center;"),
                                                 br(), br(),
                                                 "The Evaluation Core is located on the OHSU campus in the heart of Portland in the School of Nursing building (SON). ",
                                                 "If you'd like to learn more about us, the Oregon Clinical and Translational Research Institute (OCTRI) or OHSU, please visit our website:",
                                                 tags$a(href="https://www.ohsu.edu/octri", "here!"), style = "font-size:16px")
                                                 ))),
                                                 
                                        tabPanel("Provide Feedback",
                                                 h1("Thanks for using our app!"),
                                                 br(),
                                                 div(
                                                   "Although this app is still currently under development, all the code is publicly available on Github!", br(),
                                                   tags$a(href="https://github.com/Pozhidayevad", "Take me to Github"), br(), br(),
                                                   "If you have any questions, feedback, suggestions or just want to share your experience and what you've used the app for--we'd love to hear about it!",
                                                     br(),
                                                                                "Feel free to reach out to us at", strong("evaluation@ohsu.edu"),
                                                                                "or reach out to the creater directly at", strong("pozhiday@ohsu.edu."),
                                                     br(),
                                                                                "We look forward to hearing from you!", style = "font-size:16px"))
                             ))

    
))
