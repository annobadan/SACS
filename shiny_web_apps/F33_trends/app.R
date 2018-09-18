
###'######################################################################
###'
###' Shiny Web Application
###' 
###' F33 Survey data: Annual Survey of School System Finances
###' 
###' 
###' 20180826 JoonHo Lee
###' 
###' 

###'######################################################################
###'
###' Basic settings
###'
###'

### Load libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)


### Load functions
list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Data preparation
###'
###'

# ### Set temporary working directory for testing
# setwd("~/SACS/shiny_web_apps/F33_trends")


### Load the main data
load(file = "data/indv_units_df_wtd_means.rda")
df <- indv_units_df_wtd_means


### Load variable name and description
vars <- read.csv(file = "data/F33_Variable_Description.csv", as.is = TRUE)


### Variables to use
idx_PP <- grep("PP", vars$Data.Item)
vars_perpupil <- paste0(vars$Data.Item[idx_PP], "_16") 
vars_perpupil_label <- gsub("Per Pupil - ", "", vars$Description[idx_PP])



###'######################################################################
###'
###' Preset ggplot themes & Define functions  
###'    
###'        

### Theme settings
theme_trend <- 
  theme_bw() + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.title = element_blank())


### Temporary labels
temp_labels <- labs(title = "Enter title here", 
                    subtitle = "Enter subtitle here", 
                    caption = "Enter caption here", 
                    y = "Enter ylabel here",  
                    x = "Enter xlabel here")


### Define manual palettes
color_palette <- c("firebrick1", "dodgerblue1", "forestgreen", "darkorchid1", "darkgoldenrod1", 
                   "blue", "green", "purple", "gold", "red")    
shape_palette <- c(16, 17, 15, 18, 1, 2, 0, 5, 6, 4, 3, 8, 10, 7, 9) 


### Function for automatically setting ylim
auto_ylim <- function(value_vec = NULL, tweak = 5){
  
  ### The optimal y-limits
  bottom <- min(value_vec) - (min(value_vec) - 0)/tweak
  ceiling <- max(value_vec) + (min(value_vec) - 0)/tweak
  
  ### Return objects
  auto_ylim <- c(bottom, ceiling)
  return(auto_ylim)
}



###'######################################################################
###'
###' User interface
###'
###'

ui <- fluidPage(
  
  ### Shiny theme
  theme = shinytheme("united"), 
  
  
  ### Application title
  titlePanel("Trends in School System Finances, 2003 - 2016", 
             windowTitle = "School Finances Survey"),
  
  
  ### Sidebar layout with a input and output definitions
  
  sidebarLayout(
    
    ###'############
    ###' Inputs
    ###'############ 
    
    sidebarPanel(
      
      ###########################################
      # First input section: Plotting
      ###########################################
      
      h3("Plotting"),      
      
      
      # Select variable for states 
      selectInput(inputId = "state", 
                  label = "Select (Multiple) States:",
                  choices = unique(df$STATE_NAME), 
                  selected = c("California", "North Carolina"), 
                  multiple = TRUE),
      
      
      # Select variable for outcome variable
      selectInput(inputId = "yvar", 
                  label = "Variable(per-pupil):",
                  choices = vars_perpupil_label, 
                  selected = "Total Current Spending (Elementary-secondary)"),
      
      hr(),
      

      ###########################################
      # Second input section: Subsetting
      ###########################################
      
      h3("Subsetting"),    # Third level header: Subsetting
      
      
      # Select which types of school districts to plot
      checkboxGroupInput(inputId = "selected_type", 
                         label = "Select District Type(s):",
                         choices = c("Elementary School System Only", 
                                     "Secondary School System Only", 
                                     "Elementary-Secondary School System"),
                         selected = c("Elementary-Secondary School System")),
      
      hr(),
      
      
      # Show data table
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = FALSE),
      
      
      # Built with Shiny by JoonHo Lee
      br(), br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", 
             height = "30px"),
         "by JoonHo Lee (joonho@berkeley.edu)"
         )
      
    , width = 3),
    
    
    ###'############
    ###' Outputs
    ###'############ 

    mainPanel(
      
      tabsetPanel(type = "tabs",
                  id = "tabsetpanel",
                  tabPanel(title = "Plot", 
                           plotOutput(outputId = "plot_trend", width = "100%")),
                  tabPanel(title = "Data", 
                           br(),
                           DT::dataTableOutput(outputId = "data_table"))
      )
    , width = 9)
  )
)
  


###'######################################################################
###'
###' Server logic
###'
###'

server <- function(input, output, session) {
  
  ### (1) Generate data to plot
  
  df_plot <- reactive({
    
    # Ensure availablity of value before proceeding
    req(input$state) 
    req(input$selected_type) 
    req(input$yvar)
    
    
    # Get a variable index based on input$yvar
    idx <- which(vars_perpupil_label == input$yvar)
    var_name <- vars_perpupil[idx]
    
    
    # Generate data
    df %>% 
      filter(STATE_NAME %in% input$state) %>%
      filter(School_Level %in% input$selected_type) %>% 
      filter(variable %in% var_name)
  })
  
  
  ### (2) Characters as reactive expressions
  title <- reactive({ input$yvar })
  
  
  ### (3) Create scatterplot object the plotOutput function is expecting 
  output$plot_trend <- renderPlot({
    
    ggplot(data = df_plot(), aes(x = Fiscalyear, y = mean_value, group = School_Level)) +
      geom_point(aes(shape = School_Level, color = School_Level), size = 3.0) +
      geom_path(aes(linetype = School_Level, color = School_Level), size = 1.0) + 
      geom_text(aes(label = comma(mean_value)), size = 4, hjust = 0.5, vjust = 2.0) + 
      geom_vline(aes(xintercept = 2008), color = "red", linetype = "dashed") + 
      geom_vline(aes(xintercept = 2013), color = "red", linetype = "dashed") + 
      facet_wrap(~ STATE_NAME, ncol = 2) + 
      scale_x_continuous(breaks = seq(min(df_plot()$Fiscalyear), max(df_plot()$Fiscalyear), 
                                      by = 1)) + 
      scale_y_continuous(labels = comma, 
                         limits = auto_ylim(df_plot()$mean_value)) + 
      labs(title = paste0("Per-pupil - ", title()), 
           caption = "Source: Annual Survey of School System Finances", 
           y = "Amount in real 2016 dollars",   
           x = "Fiscal Years") + 
      theme_trend + 
      theme(text = element_text(size = 15)) + 
      scale_color_manual(values = color_palette[seq(unique(df_plot()$School_Level))]) + 
      scale_shape_manual(values = shape_palette[seq(unique(df_plot()$School_Level))])

    
  }, height = 600)
  
  
  ### (4) Print data table if checked
  output$data_table <- DT::renderDataTable(
    
    DT::datatable(data = df_plot(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  )
  
  
  ### (5) Display data table tab only if show_data is checked
  observeEvent(input$show_data, {
   
     if(input$show_data){
       
      showTab(inputId = "tabsetpanel", target = "Data", select = TRUE)
       
    } else {
      
      hideTab(inputId = "tabsetpanel", target = "Data")
      
    }
  })
  
  
}



###'######################################################################
###' 
###' Run the application
###' 
###'   

shinyApp(ui = ui, server = server)

