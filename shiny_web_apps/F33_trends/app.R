
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

### Set temporary working directory for testing
setwd("~/SACS/shiny_web_apps/F33_trends")


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
                  label = "Select States:",
                  choices = unique(df$STATE_NAME), 
                  selected = "California"),
      
      
      # Select variable for outcome variable
      selectInput(inputId = "yvar", 
                  label = "Variable:",
                  choices = vars_perpupil, 
                  selected = "PPCSTOT_16"),
      
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
                         selected = "Elementary-Secondary School System"),
      
      hr(),
      
      
      # Show data table
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      
      # Built with Shiny by JoonHo Lee
      br(), br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by JoonHo Lee (joonho@berkeley.edu)", 
         ".")
      
    ),
    
    
    ###'############
    ###' Outputs
    ###'############ 

    mainPanel(
      
      tabsetPanel(type = "tabs",
                  id = "tabsetpanel",
                  tabPanel(title = "Plot", 
                           plotOutput(outputId = "plot_trend")),
                  tabPanel(title = "Data", 
                           br(),
                           DT::dataTableOutput(outputId = "data_table"))
      )
    )
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
    
    # Generate data
    df %>% 
      filter(STATE_NAME %in% input$state) %>%
      filter(School_Level %in% input$selected_type) %>% 
      filter(variable %in% input$yvar)
  })
  
  
  ### (2) Characters as reactive expressions
  title <- reactive({ input$state })
  subtitle <- reactive({ input$yvar })
  
  
  ### (3) Create scatterplot object the plotOutput function is expecting 
  output$plot_trend <- renderPlot({
    
    ggplot(data = df_plot(), aes(x = Fiscalyear, y = mean_value, group = School_Level)) +
      geom_point() +
      geom_path() + 
      labs(title = title(), 
           subtitle = subtitle(), 
           caption = "Source: Annual Survey of School System Finances", 
           y = "Amount in real 2016 dollars",   
           x = "Fiscal Years") 
    
  })
  
  
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

