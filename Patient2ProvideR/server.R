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

server <- function(input, output) {
  
#EXAMPLE REACTABLES FOR INSTRUCTIONS IN UI
  output$example_data <- renderReactable({
    example <-
      tibble(
        indicator = c("patient", "patient", "provider", "provider"),
        zipcode = c("00001", "00002", "00003", "00004"),
        county = c("county1", "county2", "county3", "county2"),
        sub_indicator = c("treatment1", "treatment2", "treatment1", "treatment2")
      )
    reactable(example)
  })

#EXAMPLE REACTABLES FOR INSTRUCTIONS IN UI
  output$direct_example <- renderReactable({
    example <-
      tibble(
        indicator = c("patient", "patient", "provider", "provider"),
        zipcode = c("97022", "97406", "97504", "97232"),
        county = c("Clackamas", "Curry", "JACKSON", "MULTNOMAH"),
        sub_indicator = c("2", "4", "7", "1")
      )
    reactable(example)
  })
  
  

#CATCHING AND PROCESSING DATA FROM USER FILE ENTRY
  reactive_table_content <- reactive({
    req(input$file)
    tryCatch({
      user_data <- read.csv(
        input$file$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      ) %>% clean_names()
    },
    error = function(e) {
      stop(safeError(e))
    })
  })

# CONDITIONAL TEXT INDICATING WHETHER A FILE IS SELECTED OR NOT
  output$condPanel <- renderUI({
    if (is.null(input$file))
    {
      strong("No file selected.")
    }
    else
    {
      strong(
        "Please review the data before proceeding. Does everything below look correct? If not, adjust the options on the left until the data is appears correct."
      )
    }
  })

# CONDITIONAL TEXT GENERATED AFTER THE FILE HAS BEEN ENTERED -- PROMPT TO ANALYSIS
  output$condPanel_note <- renderUI({
    if (!is.null(input$file))
      strong(
        "If everything looks correct, proceed to the Results and Visualization tab.",
        br(),
        br(),
        "Note: Analyses run automatically and may take some time depending on your data set, so please be patient and do not refresh the page."
      )
  })

  
#TABLE OUTPUT THAT ALLOWS USER TO CHECK DATA BEFORE PROCEEDING TO ANALYSIS
output$contents <- renderReactable({
reactable(reactive_table_content(), width = "700px", height = "500px")
})

#STATE SELECTION IN DATA ENTRY
output$state <- renderUI({
  selectInput("state", 
              label = "State Representing Data", 
              choices = state.abb)
})

#ADDITIONAL INDICATOR SELECTION
output$add_ind_show <- renderUI({
  if(input$additionalinfo == "yes")
    checkboxInput("add_ind_show", "Show Additional Indicator", FALSE)
  
})


#DATA FILE PROCESSING AND GEOCODING 
total_data <- reactive({
  data("zipcode")
  
  reactive_table_content() %>%
    mutate(indicator = tolower(indicator)) %>%
    mutate(zipcode = clean.zipcodes(zipcode)) %>%
    mutate(county = tolower(county)) %>%
    filter(!is.na(zipcode)) %>%
    filter(!is.na(indicator)) %>%
    filter(!is.na(county)) %>%
    dplyr::rename("zip" = zipcode) %>%
    mutate(zip = as.character(zip)) %>%
    left_join(zipcode, by = "zip") %>%
    filter(!is.na(latitude)) %>%
    filter(!is.na(longitude)) 
  
})

#GENERATING REACTIVE VARIABLE FOR PATIENT DATA
patient_data <- reactive({
  total_data() %>%
        filter(indicator == "patient")
})

#GENERATING REACTIVE VARIABLE FOR PROVIDER DATA
provider_data <- reactive({
  total_data() %>%
        filter(indicator == "provider") 

})

#HEATMAP PAGE WARNING IF DATA IS NULL
output$datawarning <- renderUI({
  if (is.null(input$file)) 
    div(HTML(paste(tags$span(style="color:#E4492A; font-size:23px; font-family:lucida grande", "There is no data currently entered for analysis.")))
    )
})


#GENERATE THE DISTANCE MATRIX AND CALCULATE DISTANCES BASED ON THE DATA
distance_matrix <- reactive({
  patient_dataframe <- patient_data()     
  provider_dataframe <- provider_data()
  
  results <- geosphere::distm(patient_dataframe[c("longitude","latitude")], provider_dataframe[c("longitude","latitude")], fun = distVincentyEllipsoid)*0.0006213712
  
  rownames(results) <- str_to_title(patient_dataframe$county)
  colnames(results) <- str_to_title(provider_dataframe$county)
  
  results
})


#CREATE THE HEATMAP OUTPUT
output$heatmap <- renderD3heatmap({
  
patient_dataframe <- patient_data()     
provider_dataframe <- provider_data()
  
results <- distance_matrix()
rownames(results) <- str_to_title(patient_dataframe$county)
colnames(results) <- str_to_title(provider_dataframe$county)

d3heatmap(results, dendrogram = c("both"), 
          yaxis_font_size = 14, 
          xaxis_font_size =14,
          revC = TRUE)
})

#MATRIX HEATMAP DOWNLOAD BUTTON
output$downloadheatmap <- downloadHandler(
  filename = function(){"Patient2ProvideR Heatmap Distance Matrix.csv"}, 
  content = function(fname){
    write.csv(distance_matrix(), fname)
  }
)


#SUMMARY TABLE OUTPUT VIA REACTABLE
output$summary <- renderReactable({
  patient_data <- patient_data()
  provider_data <- provider_data()
  
summary_table <- tribble(
  ~"Indicator Summary", ~"n",
  "Unique Patient Zipcodes",   c(length(unique(patient_data$zip))),
  "Unique Patient Counties",   c(length(unique(patient_data$county))),
  "Unique Provider Zipcodes",   c(length(unique(provider_data$zip))),
  "Unique Provider Counties",  c(length(unique(provider_data$county))))
  
  reactable(summary_table, width = "400px", height = "250px")
})


#DISTANCE TABLES VIA REACTIVE TABLE-PATIENT TO PROVIDER DISTANCES
patient_distances <- reactive({
  
  patient_dataframe <- patient_data()     
  provider_dataframe <- provider_data()
  results <- distance_matrix()
  rownames(results) <- str_to_title(patient_dataframe$county)
  colnames(results) <- str_to_title(provider_dataframe$county)
  
  results_gathered <- data.frame(provider = rownames(results)[row(results)], patient = colnames(results)[col(results)], dists = c(results))
  
  summarized_distances <- results_gathered %>%
    filter(dists > 0) %>%
    dplyr::group_by(patient) %>%
    dplyr::summarize(dists_min = min(dists),
                     dists_mean = mean(dists),
                     dists_max = max(dists))
})

output$patientdistances <- renderReactable({
distances_dataframe <- patient_distances()
  
  distances_dataframe <- distances_dataframe %>% 
    mutate(dists_min = round(dists_min, 2)) %>%
    mutate(dists_mean = round(dists_mean, 2)) %>%
    mutate(dists_max = round(dists_max, 2))
  
  distances_dataframe <-  rename(distances_dataframe, "Patient County" = patient,
                                 "Minimum Distance (Miles)" = dists_min,
                                 "Mean Distance (Miles)" = dists_mean,
                                 "Max Distance (Miles)" = dists_max)
  reactable(distances_dataframe, width = "100%", height = "400px")
})


#DISTANCE TABLES VIA REACTIVE TABLE-PROVIDER TO PATIENT DISTANCES
provider_distances <- reactive({
  patient_dataframe <- patient_data()     
  provider_dataframe <- provider_data()
  
  results <- distance_matrix()
  rownames(results) <- str_to_title(patient_dataframe$county)
  colnames(results) <- str_to_title(provider_dataframe$county)
  
  results_gathered <- data.frame(provider = rownames(results)[row(results)], patient = colnames(results)[col(results)], dists = c(results))
  
  summarized_minimum_distances <- results_gathered %>%
    filter(dists > 0) %>%
    dplyr::group_by(provider) %>%
    dplyr::summarize(dists_min = min(dists),
                     dists_mean = mean(dists),
                     dists_max = max(dists))
})

output$providerdistances <- renderReactable({
  distances_dataframe <- provider_distances()
  
  distances_dataframe <- distances_dataframe %>% 
    mutate(dists_min = round(dists_min, 2)) %>%
    mutate(dists_mean = round(dists_mean, 2)) %>%
    mutate(dists_max = round(dists_max, 2))
  
  distances_dataframe <-  rename(distances_dataframe, "Provider County" = provider,
                                 "Minimum Distance (Miles)" = dists_min,
                                 "Mean Distance (Miles)" = dists_mean,
                                 "Max Distance (Miles)" = dists_max)
  reactable(distances_dataframe, width = "100%", height = "400px")
})


#DOWNLOAD BUTTON FOR THE PATIENT DISTANCES TABLE
output$downloadpatientdists <- downloadHandler(
  filename = function(){"Patient2ProvideR Patient Distances.csv"}, 
  content = function(fname){
    write.csv(patient_distances(), fname)
  }
)

#DOWNLOAD BUTTON FOR THE PROVIDER DISTANCES TABLE
output$downloadproviderdists <- downloadHandler(
  filename = function(){"Patient2ProvideR Provider Distances.csv"}, 
  content = function(fname){
    write.csv(provider_distances(), fname)
  }
)

#DATA WARNING IF DATA FILE IS MISSING
output$datawarning1 <- renderUI({
  if (is.null(input$file)) 
    div(HTML(paste(tags$span(style="color:#E4492A; font-size:23px; font-family:lucida grande",  "There is no data currently entered for analysis.")))
    )
})

#GENERATE THE CHOROPLETH MAPS DEPENDING ON INPUT
choropleth_map <- reactive({
  
patient_view <- patient_distances() %>%
    dplyr::rename("county" = patient)
  
provider_view <- provider_distances() %>%
    dplyr::rename("county" = provider)

USA_counties <- tigris::counties(state = input$state, cb = TRUE, resolution = "500k", year = NULL)
states_merged <- tigris::geo_join(USA_counties, get(input$data_pov), "NAME", "county")

pal <- colorNumeric("Reds", na.color = "#808080", domain = states_merged[input$distancesummary][[1]])
popup_sb <- paste0("Miles: ", as.character(states_merged[input$distancesummary][[1]]))


#IF NO ADDITIONAL INFORMATION AND NO OTHER OVERLAY--PLAIN CHOROPLETH
if (!is.null(input$file) && input$additionalinfo == "no" && input$ind_overlay == FALSE && input$add_ind_show == FALSE) 
{
leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addPolygons(data = states_merged, 
                  fillColor = ~pal(states_merged[input$distancesummary][[1]]), 
                  fillOpacity = 0.7, 
                  weight = 0.2, 
                  smoothFactor = 0.2, 
                  popup = ~popup_sb) %>%
      addLegend(pal = pal, 
                values = states_merged[input$distancesummary][[1]],
                position = "topleft", 
                title = "Distance (in Miles)",
                labFormat = labelFormat())
}


#IF THERE IS ADDITIONAL INFORMATION BUT NO OTHER OVERLAY IS SELECTED--PLAIN CHOROPLETH
else if (!is.null(input$file) && input$additionalinfo == "yes" && input$ind_overlay == FALSE && input$add_ind_show == FALSE) 
{
  leaflet() %>%
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    addPolygons(data = states_merged, 
                fillColor = ~pal(states_merged[input$distancesummary][[1]]), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values = states_merged[input$distancesummary][[1]],
              position = "topleft", 
              title = "Distance (in Miles)",
              labFormat = labelFormat())
}

#IF THERE IS ADDITIONAL INFORMATION AND OVERLAY IS SELECTED--CHOROPLETH WITH INDICATOR OVERLAY ONLY
else if (!is.null(input$file) && input$additionalinfo == "yes" && input$add_ind_show == FALSE  && input$ind_overlay == TRUE) {
  
  popup <- paste0(as.character(subset(total_data())$indicator))
  factpal <- colorFactor(c("orange", "midnightblue"), 
                         subset(total_data())$indicator)
  
  
  leaflet(total_data()) %>%
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    addPolygons(data = states_merged, 
                fillColor = ~pal(states_merged[input$distancesummary][[1]]), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values = states_merged[input$distancesummary][[1]],
              position = "topleft", 
              title = "Distance (in Miles)",
              labFormat = labelFormat()) %>%
    addCircleMarkers(lat=subset(total_data())$latitude, lng=subset(total_data())$longitude, 
                     radius = 2, 
                     color = ~factpal(indicator),
                     opacity = 0.5,
                     popup = popup)
}
#IF THERE IS ADDITIONAL INFORMATION AND OVERLAY IS SELECTED--CHOROPLETH WITH PATIENT+PROVIDER OVERLAY ONLY
else if (!is.null(input$file) && input$additionalinfo == "yes" && input$add_ind_show == TRUE  && input$ind_overlay == FALSE) {
  
  additional_indicator <- total_data()
  
  leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  addPolygons(data = states_merged, 
              fillColor = ~pal(states_merged[input$distancesummary][[1]]), 
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2, 
              popup = ~popup_sb) %>%
  addLegend(pal = pal, 
            values = states_merged[input$distancesummary][[1]],
            position = "topleft", 
            title = "Distance (in Miles)",
            labFormat = labelFormat())%>%
  addCircleMarkers(lat=subset(additional_indicator)$latitude, lng=subset(additional_indicator)$longitude, 
                   radius = 2, 
                   color = subset(additional_indicator)$sub_indicator, 
                   opacity = 0.5)
}

#IF THERE IS NO ADDITIONAL INFORMATION AND OVERLAY IS SELECTED--CHOROPLETH WITH PATIENT+PROVIDER OVERLAY ONLY
else if (!is.null(input$file) && input$additionalinfo == "no" && input$ind_overlay == TRUE) {
  
  popup <- paste0(as.character(subset(total_data())$indicator))
  factpal <- colorFactor(magma(length(unique(subset(total_data())$indicator))), 
                         subset(total_data())$indicator)
  
  leaflet(total_data()) %>%
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    addPolygons(data = states_merged, 
                fillColor = ~pal(states_merged[input$distancesummary][[1]]), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values = states_merged[input$distancesummary][[1]],
              position = "topleft", 
              title = "Distance (in Miles)",
              labFormat = labelFormat()) %>%
    addCircleMarkers(lat=subset(total_data())$latitude, lng=subset(total_data())$longitude, 
                     radius = 2, 
                     color = ~factpal(indicator),
                     opacity = 0.5)
}


})

# OUTPUT THE CORRECT CHOROPLETH MAP
output$choropleth <- renderLeaflet({
  choropleth_map()
})


#SHOW ERROR MESSAGE IF TWO OVERLAY OPTIONS ARE SELECTED AT THE SAME TIME
output$ind_overlay_warning <- renderUI({
  if (!is.null(input$file) && input$additionalinfo == "yes" && input$ind_overlay == TRUE && input$add_ind_show == TRUE) 
    div(HTML(paste(tags$span(style="color:#E4492A; font-size:23px; font-family:lucida grande",  "Please select one overlay option at a time.")))
    )
})


#DOWNLOAD A SNAPSHOT OF THE MAP
output$savemap <- downloadHandler(
  filename = "Patient2ProvideR_Choropleth_Map_Snapshot.png",
  content = function(file) {
    mapshot(choropleth_map(), file = file, 
            cliprect = "viewport", 
            selfcontained = FALSE)
  })

#GENERATE THE OUTPUT FOR THE ADDITIONAL MAP
add_map <- reactive({
if (!is.null(input$file) && input$additionalinfo == "yes") 
{  
popup <- paste0("Indicator Value: ", as.character(subset(total_data())$sub_indicator))
factpal <- colorFactor(magma(length(unique(subset(total_data())$sub_indicator))), 
                       subset(total_data())$sub_indicator)

  leaflet(total_data()) %>%
    addProviderTiles("OpenStreetMap.Mapnik") %>%
    addCircleMarkers(lat=subset(total_data())$latitude, lng=subset(total_data())$longitude, 
                     radius = 2, 
                     opacity = 0.5,
                     color = ~factpal(sub_indicator),
                     popup = popup) %>%
     addLegend(pal = factpal, 
              values = subset(total_data())$sub_indicator,
              position = "topleft", 
              title = "Indicator Value",
              opacity = 0.8,
              labFormat = labelFormat())
  }
  
  else if (!is.null(input$file) && input$additionalinfo == "no")
  { }
})

#RENDER THE ADDITIONAL MAP
output$additional_map <- renderLeaflet({
  add_map()
})

#DATA WARNING FOR MISSING FILE IN ADDITIONAL INDICATORS
output$datawarning2 <- renderUI({
  if (is.null(input$file)) 
    div(HTML(paste(tags$span(style="color:#E4492A; font-size:23px; font-family:lucida grande",  "There is no data currently entered for analysis.")))
    )
})

#DATA WARNING NO INDICATOR
output$add_ind_message <- renderUI({
  if (!is.null(input$file) && input$additionalinfo == "no") 
    div(HTML(paste(tags$span(style="color:#777777; font-size:23px; font-family:lucida grande",  "No additional indicator in data to plot."))))
})

#DOWNLOAD A SNAPSHOT OF THE MAP
output$savemap_additional <- downloadHandler(
  filename = "Patient2ProvideR_Choropleth_Map_Snapshot_Additional_Indicators.png",
  content = function(file) {
    mapshot(add_map(), file = file, 
            cliprect = "viewport", 
            selfcontained = FALSE)
  })


}
