
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
        legend.title = element_blank())

### Temporary labels
temp_labels <- labs(title = "Enter title here", 
                    subtitle = "Enter subtitle here", 
                    caption = "Enter caption here", 
                    y = "Enter ylabel here",  
                    x = "Enter xlabel here")

### Color palette
mypalette <- c("firebrick1", "dodgerblue1", "forestgreen", "darkorchid1", "darkgoldenrod1", 
               "red", "blue", "green", "purple", "gold")    


###'######################################################################
###'
###' (1) plot_trend_xy() 
###'     - with only x-y variables
###'     - No groups
###'     - No facets
###'     

### Define function 'plot_trend_0fac'
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
    scale_color_manual(values = mypalette[seq(unique(dataframe$groupvar))])
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
    scale_color_manual(values = mypalette[seq(unique(dataframe$groupvar))])
}

# ### Test the code
# plot_trend_grp_facet(df_plot, Fiscalyear, mean_value, key,
#                      Dtype~., ylim = c(8000, 18000))
# 
# plot_trend_grp_facet(df_plot, Fiscalyear, mean_value, Dtype,
#                      key~., "free_y")


