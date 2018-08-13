
###'######################################################################
###'
###' Helper functions for plotting
###' 
###' Plots summarizing longitudinal trends
###' 
###' (1) plot_trend_xy
###' (2) plot_trend_grp
###' (3) plot_trend_grp_facet
###' 
###'  
###' 20180726 JoonHo Lee
###' 
###' 

### Package dependency
library(tidyverse)
library(scales)



###'######################################################################
###'
###' Common settings for theme and temporary labels
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


###'######################################################################
###'
###' (1) plot_trend_xy() 
###'     - with only x-y variables
###'     - No groups
###'     - No facets
###'     

### Define function
plot_trend_xy <- function(dataframe, 
                          x, 
                          y,
                          yline = NULL, 
                          ylim = NULL, 
                          xinterval = 1){
  
  ###' Enquote x-y variables
  ###' Renamed variables because scale::comma() didn't work with !!yvar
  xvar <- enquo(x)
  yvar <- enquo(y)
  dataframe <- dataframe %>%
    rename(xvar = !!xvar, yvar = !!yvar)
  
  ### Assign data and aesthetic mappings
  p <- ggplot(dataframe) + 
      aes(x = xvar, y = yvar)
  
  ###' Add point, path, and value label layers
  p <- p + geom_point(size = 3.0) + 
      geom_path(size = 1.0) + 
      geom_text(aes(label = comma(yvar)), size = 3, hjust = 0.5, vjust = 2.0)

  ### Add vertical line layer
  if (!is.null(yline)){
    p <- p + geom_vline(aes(xintercept = yline), color = "red", linetype = "dashed")
  }
  
  ### Scales
  p <- p + 
    scale_x_continuous(breaks = seq(min(dataframe$xvar), max(dataframe$xvar), 
                                    by = xinterval)) + 
    scale_y_continuous(labels = comma, limits = ylim)
  
  ### Themes and temporary labels
  p + theme_trend + temp_labels 

}

# ### Test the code
# plot_trend_xy(df_plot, Fiscalyear, mean_value)



###'######################################################################
###'
###' (2) plot_trend_grp() 
###'     - x-y variables
###'     - With one group (factor)
###'     - No facets
###'     

### Define function 'plot_trend_0fac'
plot_trend_grp <- function(dataframe, 
                           x, 
                           y,
                           group, 
                           yline = NULL, 
                           ylim = NULL, 
                           xinterval = 1){
  
  ###' Enquote x, y, and group variables
  ###' Renamed variables because scale::comma() didn't work with !!yvar
  xvar <- enquo(x)
  yvar <- enquo(y)
  groupvar <- enquo(group)
  dataframe <- dataframe %>%
    rename(xvar = !!xvar, yvar = !!yvar, groupvar = !!groupvar)
  
  ### Assign data and aesthetic mappings
  p <- ggplot(dataframe) + 
    aes(x = xvar, y = yvar, group = groupvar)
  
  ###' Add point, path, and value label layers
  p <- p + geom_point(aes(shape = groupvar, color = groupvar), size = 3.0) + 
    geom_path(aes(linetype = groupvar, color = groupvar), size = 1.0) + 
    geom_text(aes(label = comma(yvar)), size = 3, hjust = 0.5, vjust = 2.0)
  
  ### Add vertical line layer
  if (!is.null(yline)){
    p <- p + geom_vline(aes(xintercept = yline), color = "red", linetype = "dashed")
  }
  
  ### Scales
  p <- p + 
    scale_x_continuous(breaks = seq(min(dataframe$xvar), max(dataframe$xvar), 
                                    by = xinterval)) + 
    scale_y_continuous(labels = comma, limits = ylim)
  
  ### Themes, temporary labels, and manual colors
  p + theme_trend + temp_labels + 
    scale_color_manual(values = color_palette[seq(unique(dataframe$groupvar))]) + 
    scale_shape_manual(values = shape_palette[seq(unique(dataframe$groupvar))])
}

# ### Test the code
# plot_trend_grp(df_plot, Fiscalyear, mean_value, key, ylim = c(8000, 18000))



###'######################################################################
###'
###' (3) plot_trend_grp_facet() 
###'     - x-y variables
###'     - With one group (factor)
###'     - With facet_grid()
###'     

### Define function 'plot_trend_0fac'
plot_trend_grp_facet <- function(dataframe, 
                                 x, 
                                 y,
                                 group, 
                                 facet_formula, 
                                 facet_scales = "fixed", 
                                 yline = NULL, 
                                 ylim = NULL, 
                                 xinterval = 1){
  
  ###' Enquote x, y, and group variables
  ###' Renamed variables because scale::comma() didn't work with !!yvar
  xvar <- enquo(x)
  yvar <- enquo(y)
  groupvar <- enquo(group)
  dataframe <- dataframe %>%
    rename(xvar = !!xvar, yvar = !!yvar, groupvar = !!groupvar)
  
  ### Assign data and aesthetic mappings
  p <- ggplot(dataframe) + 
    aes(x = xvar, y = yvar, group = groupvar)
  
  ###' Add point, path, and value label layers
  p <- p + geom_point(aes(shape = groupvar, color = groupvar), size = 3.0) + 
    geom_path(aes(linetype = groupvar, color = groupvar), size = 1.0) + 
    geom_text(aes(label = comma(yvar)), size = 3, hjust = 0.5, vjust = 2.0)
  
  ### Add vertical line layer
  if (!is.null(yline)){
    p <- p + geom_vline(aes(xintercept = yline), color = "red", linetype = "dashed")
  }
  
  ### Facetting
  p <- p + facet_grid(facet_formula, scales = facet_scales)
  
  ### Scales
  p <- p + 
    scale_x_continuous(breaks = seq(min(dataframe$xvar), max(dataframe$xvar), 
                                    by = xinterval)) + 
    scale_y_continuous(labels = comma, limits = ylim)
  
  ### Themes, temporary labels, and manual colors
  p + theme_trend + temp_labels + 
    scale_color_manual(values = color_palette[seq(unique(dataframe$groupvar))]) + 
    scale_shape_manual(values = shape_palette[seq(unique(dataframe$groupvar))])
}

# ### Test the code
# plot_trend_grp_facet(df_plot, Fiscalyear, mean_value, key,
#                      Dtype~., ylim = c(8000, 18000))
# 
# plot_trend_grp_facet(df_plot, Fiscalyear, mean_value, Dtype,
#                      key~., "free_y")



###'######################################################################
###'
###' operation14(): Remove districts with insufficient years of data
###' 
###' => Analyze only traditional schools in elementary, high, and unified 
###'    school districts that have been in continuous operation (14 years) 
###'    in California from 2003 through 2017

setwd(work_dir)
load(file = "processed_data/years_of_operation.rda")   # Data dependency: years_of_operation.csv

operation14 <- function(df){
  df %>%
    left_join(years_of_operation[, !names(years_of_operation) %in% c("Dname", "Dtype")], 
              by = c("Ccode", "Dcode")) %>%
    filter(opr_years == 14)
}



###'######################################################################
###'
###' get_weighted_mean(): Get weighted district averages
###' 
###' 

get_weighted_mean <- function(df, 
                              x = Fiscalyear, 
                              y = sum_value_PP_16, 
                              weight = K12ADA_C, 
                              ...){
  
  ### Enquote variables
  x <- enquo(x)
  y <- enquo(y)
  weight <- enquo(weight)
  group_var <- quos(...)
  
  df %>% 
    group_by(!!x, !!!group_var) %>%
    summarise(mean_value = round(weighted.mean(!!y, !!weight, na.rm = TRUE), 0))
}



###'######################################################################
###'
###' Calculate the y-limits and height for the PDF file
###' 
###' 

auto_ylim <- function(value_vec = NULL, tweak = 5){
  
  ### The optimal y-limits
  bottom <- min(value_vec) - (min(value_vec) - 0)/tweak
  ceiling <- max(value_vec) + (min(value_vec) - 0)/tweak
  
  ### Return objects
  auto_ylim <- c(bottom, ceiling)
  return(auto_ylim)
}

auto_height <- function(factor_vec = NULL, tweak = 3){
  
  ### The height for the PDF file
  num_factor <- length(levels(factor_vec))
  height <- ifelse(num_factor <= 4, 6, num_factor + tweak)
  
  ### Return objects
  return(height)
}



###'######################################################################
###'
###' Calculate percentages based on groups
###'
###'

group_percent <- function(df, 
                          value = mean_value, 
                          ...){
  
  # Enquote variables
  value <- enquo(value)
  group <- quos(...)

  
  #' (1) Calculate the percentages
  #' (2) Format the labels and calculate their positions
  df %>%
    group_by(!!!group) %>%
    mutate(group_sum = sum(!!value, na.rm = TRUE), 
           percent = !!value/group_sum * 100, 
           # don't need to calculate the label positions from ggplot 2.1.0 
           # position = cumsum(amount) - 0.5 * amount,  
           label_text = paste0(sprintf("%.0f", percent), "%")) -> df
  return(df)
}

