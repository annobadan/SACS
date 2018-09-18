
###'######################################################################
###'
###' Shiny Web Application
###' 
###' SACS: California School Finance 2003-2016
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
library(ggthemes)
library(scales)
library(tidyverse)
library(DT)
library(plotly)


# ### Load functions
# list.files("functions", full.names = TRUE) %>% walk(source)


###'######################################################################
###'
###' Data preparation
###'
###'

# ### Set temporary working directory for testing
# setwd("~/SACS/shiny_web_apps/California_School_Finance")


### Per-pupil expenditures & revenues
load(file = "data/list_expenditures_def_1_2.RData")
load(file = "data/list_revenues.rda")


### FY1314 Simuated IV
load(file = "data/df_1314_SimIV.rda")


### The State of California's overall and local revenues/expenditures data
state_df <- read.csv(file = "data/May_Revision_Budget_2000_to_2016.csv")


### State-level predictors
load(file = "data/state_predictors.rda")


### Prepare conditional county/district list
load(file = "data/years_of_operation.rda")

opr14 <- years_of_operation %>%
  filter(opr_years == 14)

opr_yrs <- years_of_operation %>%
  select(Ccode, Dcode, opr_years)



###'######################################################################
###'
###' Preset ggplot themes & Define functions  
###'    
###'        

### Theme settings
theme_preset <- 
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
  theme = shinytheme("lumen"), 
  
  
  ### Application title
  titlePanel(" Revenues and Expenditures in California's School Districts, 2003 - 2016", 
             windowTitle = "California School Finance"),
  
  
  ### Sidebar layout with a input and output definitions
  
  sidebarLayout(
    
    ###'############
    ###' Inputs
    ###'############ 
    
    sidebarPanel(
      
      ###'#######################################
      ###' First input section: 1) Factor
      ###'
      
      h3("Variable"),      
      
      # Revenue vs. Expenditure
      radioButtons(inputId = "rev_or_exp", 
                   label = "Select Revenue or Expenditure:", 
                   choices = c("Revenues" = "rev", 
                               "Expenditures - All Funds" = "exp1", 
                               "Expenditures - General Fund Only" = "exp2")), 
      
      
      # Conditional panel 1) Revenues: Factor
      conditionalPanel(
        condition = "input.rev_or_exp == 'rev'", 
        selectInput(inputId = "rev_factor", 
                    label = "Define Subcategories:", 
                    choices = c("Total Revenues" = "total_rev", 
                                "Unrestricted vs. Restricted" = "unres_vs_res", 
                                "Restricted Revenues: Subcategories" = "restricted_sub", 
                                "Revenue Sources" = "revenue_by_object",
                                "Other Revenue Categories" = "revenue_interest"), 
                    selected = "unres_vs_res")
      ),
      
      
      # Conditional panel 2) Expenditures: Factor
      conditionalPanel(
        condition = "input.rev_or_exp == 'exp1' || input.rev_or_exp == 'exp2'", 
        selectInput(inputId = "exp_factor", 
                    label = "Define Subcategories:",
                    choices = c("Total Expenditures" = "total_exp",
                                "Student vs. Non-Student Spending" = "std_vs_nonstd", 
                                "Non-Student Spending" = "nonstd_sub", 
                                "Student Spending: Subcategories by Object Codes" = "std_sub", 
                                "Salaries" = "salaries", 
                                "Employee Benefits" = "benefits", 
                                "Student Spending: Subcategories by Goal Codes" = "goals", 
                                "General Education, K-12" = "general_Ed", 
                                "Supplemental Education, K-12" = "supp_Ed", 
                                "Special Education, K-12" = "SPED", 
                                "Student Spending: Subcategories by Function Codes" = "functions", 
                                "Special Education Instruction" = "SPED_inst", 
                                "Pupil Services" = "pupil_services"), 
                     selected = "std_vs_nonstd")
      ),
      
      
      ###'#######################################
      ###' Second input section: 2) Subgroup
      ###'

      h3("Data Summary"),  
      
      # Level of data summary
      radioButtons(inputId = "summary_level", 
                   label = "The level of data summary:", 
                   choices = c("State-level" = "state", 
                               "District-level" = "district")
      ),
      
      
      # Conditional panel 1) Summary-level: State
      conditionalPanel(
        condition = "input.summary_level == 'state'",
        checkboxGroupInput(inputId = "state_dist_type", 
                           label = "Select District Type(s) to summarize:", 
                           choices = c("ELEMENTARY", "HIGH", "UNIFIED"), 
                           selected = c("ELEMENTARY", "HIGH", "UNIFIED"))
      ), 
      
      
      # Conditional panel 2) Summary-level: District
      conditionalPanel(
        condition = "input.summary_level == 'district'", 
        selectizeInput(inputId = "district", 
                       label = "Search and Select District Name(s):", 
                       choices = opr14$Dname, 
                       options = list(maxOptions = 10), 
                       selected = "Los Angeles Unified")
      ), 
      
      hr(),
      
      
      ###'#######################################
      ###' Data Table
      ###' 
      
      h3("Data Table"),  
      
      
      # Show data table
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      
      ###'#######################################
      ###' Miscellaneous Inputs
      ###' 
      
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
                           plotOutput(outputId = "plot_proportions", 
                                      width = "100%")),
                  
                  # tabPanel(title = "Proportions", 
                  #          plotOutput(outputId = "plot_proportions", width = "100%")),
                  
                  tabPanel(title = "Table", 
                           br(),
                           DT::dataTableOutput(outputId = "data_table"), 
                           downloadButton(outputId = "download_data", 
                                          label = "Download data"))
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
  
  ###'#######################################
  ###' Generate data to summarize
  ###' 

  df_opr14 <- reactive({
    
    if (input$rev_or_exp == "rev") {   ### (1) Revenues
      
      ### Assign data frame
      df <- list_revenues[[input$rev_factor]]
      
      
      ### Filter only districts with 14 years of continuous operation
      df_opr14 <- df %>%
        left_join(opr_yrs, by = c("Ccode", "Dcode")) %>% 
        filter(opr_years == 14)
      
      
      ### Add/Rename factor variable
      if (input$rev_factor == "total_rev"){
        
        df_opr14$factor <- "Total Revenues"
        
      } else if (input$rev_factor != "total_rev"){
        
        idx <- which(names(df_opr14) == input$rev_factor)
        names(df_opr14)[idx] <- "factor"

      }
  
    } else if (input$rev_or_exp == "exp1") {   ### (2) Expenditures: Definition 1
      
      ### Assign data frame
      df <- list_expenditures_def_1[[input$exp_factor]]
      
      
      ### Filter only districts with 14 years of continuous operation
      df_opr14 <- df %>%
        left_join(opr_yrs, by = c("Ccode", "Dcode")) %>% 
        filter(opr_years == 14)
      
      
      ### Add/Rename factor variable
      if (input$exp_factor == "total_exp"){
        
        df_opr14$factor <- "Total Expenditures"
        
      } else if (input$exp_factor != "total_exp"){
        
        idx <- which(names(df_opr14) == input$exp_factor)
        names(df_opr14)[idx] <- "factor"
        
      }
      
    } else if (input$rev_or_exp == "exp2") {   ### (3) Expenditures: Definition 2
      
      ### Assign data frame
      df <- list_expenditures_def_2[[input$exp_factor]]
      
      
      ### Filter only districts with 14 years of continuous operation
      df_opr14 <- df %>%
        left_join(opr_yrs, by = c("Ccode", "Dcode")) %>% 
        filter(opr_years == 14)
      
      
      ### Add/Rename factor variable
      if (input$exp_factor == "total_exp"){
        
        df_opr14$factor <- "Total Expenditures"
        
      } else if (input$exp_factor != "total_exp"){
        
        idx <- which(names(df_opr14) == input$exp_factor)
        names(df_opr14)[idx] <- "factor"
        
      }
      
    }
    
    return(df_opr14)
    
  })
  
  
  ###'###################################################
  ###' Summarize data at state or district level 
  ###' 

  df_opr14_summary <- reactive({
    
    if (input$summary_level == "state") {  ### (1) State-level summary
      
      df_opr14() %>%
        filter(Dtype %in% input$state_dist_type) %>%
        group_by(Fiscalyear, factor) %>%
        summarise(value = round(weighted.mean(sum_value_PP_16, K12ADA_C, na.rm = TRUE), 0)) %>% 
        ungroup() %>%
        group_by(Fiscalyear) %>%
        mutate(group_sum = sum(value, na.rm = TRUE), 
               percent = value/group_sum * 100, 
               label_text = paste0(sprintf("%.1f", percent), "%"))

    } else if (input$summary_level == "district") {   ### (2) District-level summary
      
      df_opr14() %>%
        filter(Dname == input$district) %>%
        rename(value = sum_value_PP_16) %>%
        select(Fiscalyear, factor, value) %>%
        mutate_at(.vars = c("value"), .funs = round) %>%
        group_by(Fiscalyear) %>%
        mutate(group_sum = sum(value, na.rm = TRUE), 
               percent = value/group_sum * 100, 
               label_text = paste0(sprintf("%.1f", percent), "%"))
      
    } 
    
  })  
  
  
  ###'###################################################
  ###' Prepare group total dataframe 
  ###' To plot values at the tip of bar graphs
  ###' 
  
  group_total <- reactive({
    
    df_opr14_summary() %>%
      group_by(Fiscalyear) %>%
      summarise(group_sum = first(group_sum))
    
  })
  
  
  ###'###################################################
  ###' Prepare labels
  ###'
  
  ### Create same named vector globally
  rev_or_exp_Vec <- c("Revenues" = "rev", 
                      "Expenditures - All Funds" = "exp1", 
                      "Expenditures - General Fund Only" = "exp2")
  
  rev_Vec <- c("Total Revenues" = "total_rev", 
               "Unrestricted vs. Restricted" = "unres_vs_res", 
               "Restricted Revenues: Subcategories" = "restricted_sub", 
               "Revenue Sources" = "revenue_by_object", 
               "Other Revenue Categories" = "revenue_interest")
  
  exp_Vec <- c("Total Expenditures" = "total_exp",
               "Student vs. Non-Student Spending" = "std_vs_nonstd", 
               "Non-Student Spending" = "nonstd_sub", 
               "Student Spending: Subcategories by Object Codes" = "std_sub", 
               "Salaries" = "salaries", "Employee Benefits" = "benefits", 
               "Student Spending: Subcategories by Goal Codes" = "goals", 
               "General Education, K-12" = "general_Ed", 
               "Supplemental Education, K-12" = "supp_Ed", 
               "Special Education, K-12" = "SPED", 
               "Student Spending: Subcategories by Function Codes" = "functions", 
               "Special Education Instruction" = "SPED_inst", 
               "Pupil Services" = "pupil_services")
  
  summary_level_Vec <- c("State-level" = "state", 
                         "District-level" = "district")
  
  Dtype_Vec <- c("ELEMENTARY", "HIGH", "UNIFIED")

  
  ### Define reactive lables
  lab_title <- reactive({
    
    if (input$rev_or_exp == "rev"){
      
      names(rev_Vec)[rev_Vec == input$rev_factor]
      
    } else if (input$rev_or_exp %in% c("exp1", "exp2")) {
      
      names(exp_Vec)[exp_Vec == input$exp_factor]
  
    }
  })
  
  lab_subtitle <- reactive({
    
    if (input$summary_level == "state"){
      
      paste0("Proportions Based on ", 
             names(summary_level_Vec)[summary_level_Vec == input$summary_level], 
             " Per-pupil Averages Weighted by ADA")
      
    } else if (input$summary_level == "district") {
      
      paste0("Proportions Based on ", 
             names(summary_level_Vec)[summary_level_Vec == input$summary_level], 
             " Averages: ", 
             input$district)
      
    }
  })
    
 
  
  ###'###################################################
  ###' Create Output: plot_proportions 
  ###' 
  
  output$plot_proportions <- renderPlot({
    
    ggplot(data = df_opr14_summary(), 
           aes(x = Fiscalyear, y = value, fill = factor)) + 
      geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = 0.7) +
      geom_text(aes(label = label_text), 
                position = position_stack(vjust = 0.5, reverse = TRUE), size = 4.2) + 
      geom_text(data = group_total(),  
                aes(x = Fiscalyear, y = group_sum + mean(df_opr14_summary()$group_sum)/30, 
                    label = comma(group_sum), fill = NULL), 
                size = 4.5) + 
      scale_x_continuous(breaks = seq(min(df_opr14_summary()$Fiscalyear), 
                                      max(df_opr14_summary()$Fiscalyear), 
                                      by = 1)) + 
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) + 
      scale_fill_brewer(palette = "Paired") + 
      theme_preset + 
      theme(text = element_text(size = 16)) + 
      labs(title = lab_title(),
           subtitle = lab_subtitle(), 
           caption = "Source: SACS unaudited data", 
           y = "Amount in real 2016 dollars",   
           x = "Fiscal Years") 
    
  }, height = 600)
  
  
  ###'###################################################
  ###'  Create Output: Data table
  ###'  

  ### Define reactive data table
  table <- reactive({
    
    df_opr14_summary() %>% 
      select(Fiscalyear, factor, value, label_text) %>%
      rename(percent = label_text) %>%
      arrange(factor)
    
  })
  
  
  ### Print data table if checked
  output$data_table <- DT::renderDataTable(
    
    DT::datatable(data = table(), 
                  options = list(pageLength = 14), 
                  rownames = FALSE)
  )
  
  
  ### Display data table tab only if show_data is checked
  observeEvent(input$show_data, {
    
    if(input$show_data){
      
      showTab(inputId = "tabsetpanel", target = "Table", select = FALSE)
      
    } else {
      
      hideTab(inputId = "tabsetpanel", target = "Table")
      
    }
  })

  
  ### Download file
  output$download_data <- downloadHandler(
    
    filename = function() {
      
      paste0("data_table.csv")
      
    },
    
    content = function(file) { 
      
      write_csv(table(), file) 
        
    }
  )
  
}   ### End of server() function



###'######################################################################
###' 
###' Run the application
###' 
###'   

shinyApp(ui = ui, server = server)
