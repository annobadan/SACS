
###'######################################################################
###'
###' Shiny Web Application
###' 
###' Spatial Distribution of Kindergarten Program Types in California
###' 
###' Academic Years 2015-16, 2016-17, and 2017-18
###' 
###' 
###' 20180913 JoonHo Lee
###' 
###' 

###'######################################################################
###'
###' Basic settings
###'
###'

### Load packages for shiny app development
library(shiny)
library(shinythemes)
library(ggthemes)
library(scales)
library(tidyverse)
library(DT)
library(forcats)


### Load packages for leaflet mapping
library(leaflet)
library(leaflet.extras)
library(htmlwidgets)
library(RColorBrewer)
library(sf)



###'######################################################################
###'
###' Data Preparation
###'
###'

# ### Set temporary working directory for testing
# setwd("~/SACS/shiny_web_apps/Kindergartens-in-California")


### Load the prepared datasets
load(file = "data/df_elem.rda")
load(file = "data/CA_tracts_shp.rda")
load(file = "data/CA_unified_shp.rda")
load(file = "data/CA_elementary_shp.rda")
load(file = "data/CA_secondary_shp.rda")


### Define layer dataframes by year
df_elem_2015 <- df_elem %>%
  filter(AcademicYear == 2015)

df_elem_2016 <- df_elem %>%
  filter(AcademicYear == 2016)

df_elem_2017 <- df_elem %>%
  filter(AcademicYear == 2017)



###'######################################################################
###'
###' Set color palette
###' 
###' 

pal <- colorFactor(palette = c("red", "blue", "green"), 
                   levels = c("Full day", "Part day", "None"), 
                   ordered = FALSE)

inc_pal <- colorQuantile(palette = "YlGn", n = 5,  
                         domain = log(CA_tracts_shp$median_inc_est))



###'######################################################################
###'
###' User interface
###'
###'

ui <- bootstrapPage(
  
  ### Set CSS style
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  
  ###'############
  ###' Output  ###
  ###'############ 
  
  leafletOutput("map", width = "100%", height = "100%") 
  
  # 
  # ###'############
  # ###' Inputs  ###
  # ###'############ 
  # 
  # absolutePanel(top = 10, right = 10,
  #               
  #               radioButtons(inputId = "Kinder_vs_TransK", 
  #                            label = "Kindergarten Type:", 
  #                            choices = c("Kindergarten", 
  #                                        "Transitional Kindergarten"))
  #               
  # )
)



###'######################################################################
###'
###' Server logic
###'
###'

server <- function(input, output, session) {
  
  
  ###'###########################################
  ###'
  ###' (2) Create Output: Leaflet Map
  ###' 
  ###' 
  
  output$map <- renderLeaflet({
    
    df_elem_2017 %>% 
      
      ### (1) Initiate leaflet map and basic settings
      leaflet() %>%
      addSearchOSM() %>%
      # addReverseSearchOSM() %>%
      addResetMapButton() %>%
      
      
      ### (2) Set tiles
      # addTiles() %>%
      # addProviderTiles(provider = "CartoDB") %>%
      
      addMapPane(name = "polygons", zIndex = 410) %>% 
      addMapPane(name = "maplabels", zIndex = 420) %>%  # higher zIndex rendered on top
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels", 
                       options = leafletOptions(pane = "maplabels"),
                       group = "map labels") %>%
      
      
      ### (3) Add Census tract and school district polygons
      addPolygons(data = CA_tracts_shp,
                  weight = 1, 
                  stroke = TRUE,
                  color = "darkgray", 
                  fillColor = ~inc_pal(log(median_inc_est)), 
                  fillOpacity = 0.5) %>%

      addPolygons(data = CA_unified_shp,
                  label = ~NAME,
                  weight = 1,
                  stroke = TRUE,
                  color = "black",
                  fillOpacity = 0) %>%

      addPolygons(data = CA_elementary_shp,
                  label = ~NAME,
                  weight = 1,
                  stroke = TRUE,
                  color = "black",
                  fillOpacity = 0) %>%

      addPolygons(data = CA_secondary_shp,
                  label = ~NAME,
                  weight = 1,
                  stroke = TRUE,
                  color = "black",
                  fillOpacity = 0) %>%


      ### (4-1) Add Circle Markers - Kindergarten
      addCircleMarkers(data = df_elem_2015, 
                       lng = ~Longitude, lat = ~Latitude, 
                       radius = 1, 
                       opacity = 1, 
                       color = ~pal(Kindergarten3), 
                       # label = ~paste0(SchoolName, 
                       #                 " (FRPM = ", round(MN_PCT_FRPM, 2), "%)"), 
                       popup = ~paste0("<b>", SchoolName, "</b>", 
                                       "<br/>", "School Type: ", SOC, 
                                       "<br/>", "Total Enrollment: ", Total_Enroll, 
                                       "<br/>", "Kindergarten Enrollment: ", 
                                       N_KDGN, " (", round(PCT_KDGN, 1), "%", ")",   
                                       # "<br/>", "Kindergarten: ", 
                                       # Kindergarten3, 
                                       # "<br/>", "Transitional Kindergarten: ", 
                                       # Transitional_K3,
                                       # "<br/>", "White: ", 
                                       # round(PCT_White, 1), "%", 
                                       # "<br/>", "Hispanic or Latino: ",   
                                       # round(PCT_Latino, 1), "%",
                                       "<br/>", "FRPM: ", 
                                       round(PCT_FRPM_CALPADS_Undup, 1), "%"),
                       group = "Kindergarten 2015-16") %>%
      
      addCircleMarkers(data = df_elem_2016, 
                       lng = ~Longitude, lat = ~Latitude, 
                       radius = 1, 
                       opacity = 1,
                       color = ~pal(Kindergarten3), 
                       # label = ~paste0(SchoolName, 
                       #                 " (FRPM = ", round(MN_PCT_FRPM, 2), "%)"), 
                       popup = ~paste0("<b>", SchoolName, "</b>", 
                                       "<br/>", "School Type: ", SOC, 
                                       "<br/>", "Total Enrollment: ", Total_Enroll, 
                                       "<br/>", "Kindergarten Enrollment: ", 
                                       N_KDGN, " (", round(PCT_KDGN, 1), "%", ")",   
                                       # "<br/>", "Kindergarten: ", 
                                       # Kindergarten3, 
                                       # "<br/>", "Transitional Kindergarten: ", 
                                       # Transitional_K3,
                                       # "<br/>", "White: ", 
                                       # round(PCT_White, 1), "%", 
                                       # "<br/>", "Hispanic or Latino: ",   
                                       # round(PCT_Latino, 1), "%",
                                       "<br/>", "FRPM: ", 
                                       round(PCT_FRPM_CALPADS_Undup, 1), "%"),
                       group = "Kindergarten 2016-17") %>%
      
      addCircleMarkers(data = df_elem_2017, 
                       lng = ~Longitude, lat = ~Latitude, 
                       radius = 1, 
                       opacity = 1,
                       color = ~pal(Kindergarten3), 
                       # label = ~paste0(SchoolName, 
                       #                 " (FRPM = ", round(MN_PCT_FRPM, 2), "%)"), 
                       popup = ~paste0("<b>", SchoolName, "</b>", 
                                       "<br/>", "School Type: ", SOC, 
                                       "<br/>", "Total Enrollment: ", Total_Enroll, 
                                       "<br/>", "Kindergarten Enrollment: ", 
                                       N_KDGN, " (", round(PCT_KDGN, 1), "%", ")",   
                                       # "<br/>", "Kindergarten: ", 
                                       # Kindergarten3, 
                                       # "<br/>", "Transitional Kindergarten: ", 
                                       # Transitional_K3,
                                       # "<br/>", "White: ", 
                                       # round(PCT_White, 1), "%", 
                                       # "<br/>", "Hispanic or Latino: ",   
                                       # round(PCT_Latino, 1), "%",
                                       "<br/>", "FRPM: ", 
                                       round(PCT_FRPM_CALPADS_Undup, 1), "%"),
                       group = "Kindergarten 2017-18") %>%
      
      
      ### (4-2) Add Circle Markers - Transitional Kindergarten
      addCircleMarkers(data = df_elem_2015, 
                       lng = ~Longitude, lat = ~Latitude, 
                       radius = 1, 
                       opacity = 1, 
                       color = ~pal(Transitional_K3), 
                       # label = ~paste0(SchoolName, 
                       #                 " (FRPM = ", round(MN_PCT_FRPM, 2), "%)"), 
                       popup = ~paste0("<b>", SchoolName, "</b>", 
                                       "<br/>", "School Type: ", SOC, 
                                       "<br/>", "Total Enrollment: ", Total_Enroll, 
                                       "<br/>", "Kindergarten Enrollment: ", 
                                       N_KDGN, " (", round(PCT_KDGN, 1), "%", ")",   
                                       # "<br/>", "Kindergarten: ", 
                                       # Kindergarten3, 
                                       # "<br/>", "Transitional Kindergarten: ", 
                                       # Transitional_K3,
                                       # "<br/>", "White: ", 
                                       # round(PCT_White, 1), "%", 
                                       # "<br/>", "Hispanic or Latino: ",   
                                       # round(PCT_Latino, 1), "%",
                                       "<br/>", "FRPM: ", 
                                       round(PCT_FRPM_CALPADS_Undup, 1), "%"),
                       group = "Transitional Kindergarten 2015-16") %>%
      
      addCircleMarkers(data = df_elem_2016, 
                       lng = ~Longitude, lat = ~Latitude, 
                       radius = 1, 
                       opacity = 1,
                       color = ~pal(Transitional_K3), 
                       # label = ~paste0(SchoolName, 
                       #                 " (FRPM = ", round(MN_PCT_FRPM, 2), "%)"), 
                       popup = ~paste0("<b>", SchoolName, "</b>", 
                                       "<br/>", "School Type: ", SOC, 
                                       "<br/>", "Total Enrollment: ", Total_Enroll, 
                                       "<br/>", "Kindergarten Enrollment: ", 
                                       N_KDGN, " (", round(PCT_KDGN, 1), "%", ")",   
                                       # "<br/>", "Kindergarten: ", 
                                       # Kindergarten3, 
                                       # "<br/>", "Transitional Kindergarten: ", 
                                       # Transitional_K3,
                                       # "<br/>", "White: ", 
                                       # round(PCT_White, 1), "%", 
                                       # "<br/>", "Hispanic or Latino: ",   
                                       # round(PCT_Latino, 1), "%",
                                       "<br/>", "FRPM: ", 
                                       round(PCT_FRPM_CALPADS_Undup, 1), "%"),
                       group = "Transitional Kindergarten 2016-17") %>%
      
      addCircleMarkers(data = df_elem_2017, 
                       lng = ~Longitude, lat = ~Latitude, 
                       radius = 1, 
                       opacity = 1,
                       color = ~pal(Transitional_K3), 
                       # label = ~paste0(SchoolName, 
                       #                 " (FRPM = ", round(MN_PCT_FRPM, 2), "%)"), 
                       popup = ~paste0("<b>", SchoolName, "</b>", 
                                       "<br/>", "School Type: ", SOC, 
                                       "<br/>", "Total Enrollment: ", Total_Enroll, 
                                       "<br/>", "Kindergarten Enrollment: ", 
                                       N_KDGN, " (", round(PCT_KDGN, 1), "%", ")",   
                                       # "<br/>", "Kindergarten: ", 
                                       # Kindergarten3, 
                                       # "<br/>", "Transitional Kindergarten: ", 
                                       # Transitional_K3,
                                       # "<br/>", "White: ", 
                                       # round(PCT_White, 1), "%", 
                                       # "<br/>", "Hispanic or Latino: ",   
                                       # round(PCT_Latino, 1), "%",
                                       "<br/>", "FRPM: ", 
                                       round(PCT_FRPM_CALPADS_Undup, 1), "%"),
                       group = "Transitional Kindergarten 2017-18") %>%
      
      
      ### (5) Add Layer Control
      addLayersControl(baseGroups = c("Kindergarten 2015-16", 
                                      "Kindergarten 2016-17", 
                                      "Kindergarten 2017-18", 
                                      "Transitional Kindergarten 2015-16", 
                                      "Transitional Kindergarten 2016-17", 
                                      "Transitional Kindergarten 2017-18"), 
                       options = layersControlOptions(collapsed = FALSE), 
                       position = "topleft") %>%
      
      
      ### (6) Add legend
      addLegend(title = "Kindergarten Program Type", 
                pal = pal, 
                values = c("Full day", "Part day", "None"), 
                position = "topleft", 
                opacity = 10) %>%
      
      addLegend(title = "Census Tract Median Income Quintile", 
                pal = inc_pal, 
                values = log(CA_tracts_shp$median_inc_est), 
                position = "topleft", 
                opacity = 10)
  })
}



###'######################################################################
###' 
###' Run the application
###' 
###'   

shinyApp(ui, server)