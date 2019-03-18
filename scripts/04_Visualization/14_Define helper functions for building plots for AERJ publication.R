
###'######################################################################
###'
###' Helper functions for building publication plots 
###'
###' 
###' 20190106 JoonHo Lee
###'
###'

###'######################################################################
###'
###' tau_recoded_df(): 
###' 
###' Convert tau50 => 0.5 qunatile
###'
###'

tau_recoded_df <- function(df){
  
  df %>%
    mutate(quantile = paste0(as.numeric(gsub("tau", "", element))/100, " quantile"))
  
}



###'######################################################################
###'
###' event_study_xy(): 
###' 
###' Generate a single event study plot
###'
###'

event_study_xy <- function(dataframe, 
                           x = year, 
                           y = estimate,
                           conf.low = conf.low, 
                           conf.high = conf.high, 
                           title = NULL){
  
  ###' Enquote x-y variables
  ###' Renamed variables because scale::comma() didn't work with !!yvar
  xvar <- enquo(x)
  yvar <- enquo(y)
  lower_lim <- enquo(conf.low)
  upper_lim <- enquo(conf.high)
  
  dataframe <- dataframe %>%
    rename(xvar = !!xvar, yvar = !!yvar) %>%
    mutate(year_since = xvar - 2012)
  
  p <- ggplot(aes(x = year_since, y = yvar), 
              data = dataframe) +
    
    # Add point, line, and text for coefficients
    geom_line(size = 0.5) + 
    geom_point(size = 1.5) + 
    geom_text(aes(label = sprintf("%.1f", round(yvar, 1)), hjust = 0.5, vjust = 2.0), size = 4) +
    
    # Add confidence interval band
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15) +
    
    # Add horizontal line at zero
    geom_hline(aes(yintercept = 0), color = "black", linetype = "dashed") + 
    
    # Add vertical line at event
    geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed") + 
    
    # Scales
    scale_x_continuous(breaks = seq(-10, 5, by = 1)) + 
    scale_y_continuous(labels = comma) + 
    
    # Theme
    theme_classic() + 
    theme(panel.background = element_blank(),
          panel.grid = element_blank(), 
          legend.position = "bottom", 
          legend.direction = "horizontal", 
          legend.title = element_blank(), 
          plot.title = element_text(hjust = 0.0)) + 
    
    # Labels
    labs(title = title, 
         subtitle = NULL, 
         caption = NULL, 
         y = "Difference-in-difference estimates",  
         x = "Year minus Initial year of LCFF reform")
  
  p
}



###'######################################################################
###'
###' plot_LCFF_means()
###' : Generate descriptive plots for Q1 and Q5 across 2003-2017
###'
###'

plot_LCFF_means <- function(dataframe, y, bin, 
                            ylim = NULL, yref = 0, 
                            title = NULL){
  
  # Enquote variables
  yvar <- enquo(y)
  
  # Generate a dataframe to plot
  df_plot <- dataframe %>% 
    filter(bin %in% c(1, 5)) %>% 
    filter(Charter == 0) %>%
    mutate(bin = factor(bin, levels = c(1, 5), 
                        labels = c("Q1 mean (Low-poverty schools)", 
                                   "Q5 mean (High-poverty schools)")), 
           year_2000 = AcademicYear - 2000) %>%
    group_by(year_2000, bin) %>%
    summarise(mean_value = mean(!!yvar, na.rm = TRUE))
  
  # Drop NA to make a linear interpolation
  df_plot <- df_plot[complete.cases(df_plot), ]
  
  
  # Define shade range
  rec_y_incr <- (ylim[2] - ylim[1])/17
  

  # Plot!
  p <- ggplot(aes(x = year_2000, y = mean_value, group = bin), 
              data = df_plot) + 
    
    # Add point, line, and text for coefficients
    geom_path(aes(linetype = bin, color = bin), size = 0.5) + 
    geom_point(aes(shape = bin, color = bin), size = 1.5) + 
    geom_text(aes(label = sprintf("%.1f", round(mean_value, 1)), 
                  hjust = 0.5, vjust = 2.0), size = 2.3) +
    
    # Scales
    scale_x_continuous(breaks = seq(3, 17, by = 1)) + 
    scale_y_continuous(limits = ylim) + 
    
    # Add x-axis reference lines and annotate periods
    geom_vline(xintercept = c(7.5, 12.5), color = "gray", linetype = "dashed") + 
    annotate("text", x = c(5, 10, 14.75), y = yref, 
             label = c("Pre-recession", 
                       "Recession", 
                       "LCFF"), 
             color = "black", size = 4) + 
    annotate("rect", xmin = 12.5, xmax = 17, 
             ymin = yref - rec_y_incr, 
             ymax = yref + rec_y_incr,
             alpha = .1) + 
    
    # Theme
    theme_classic() + 
    theme(panel.background = element_blank(),
          panel.grid = element_blank(), 
          legend.position = "bottom", 
          legend.direction = "horizontal", 
          legend.title = element_blank(), 
          plot.title = element_text(hjust = 0.0)) + 
    
    # Labels
    labs(title = title, 
         subtitle = NULL, 
         caption = NULL, 
         y = NULL,  
         x = "Year - 2000") + 
    
    # Mannual color and shapes
    scale_color_manual(values = c("dodgerblue1", "firebrick1")) + 
    scale_shape_manual(values = c(16, 17)) + 
    
    # Legend
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  p
}



###'######################################################################
###'
###' plot_LCFF_means2()
###' : Generate descriptive plots for Q1 and Q5 across 2012-2017
###'
###'

plot_LCFF_means2 <- function(dataframe, y, bin, 
                            ylim = NULL, yref = 0, 
                            title = NULL, 
                            subtitle = NULL){
  
  # Enquote variables
  yvar <- enquo(y)
  
  # Generate a dataframe to plot
  df_plot <- dataframe %>% 
    filter(bin %in% c(1, 5)) %>% 
    filter(Charter == 0) %>%
    mutate(bin = factor(bin, levels = c(1, 5), 
                        labels = c("Q1 mean (Low-poverty schools)", 
                                   "Q5 mean (High-poverty schools)"))) %>%
    group_by(AcademicYear, bin) %>%
    summarise(mean_value = mean(!!yvar, na.rm = TRUE))
  
  
  # Drop NA to make a linear interpolation
  df_plot <- df_plot[complete.cases(df_plot), ]
  
  
  # Define shade range
  rec_y_incr <- (ylim[2] - ylim[1])/17
  
  
  # Plot!
  p <- ggplot(aes(x = AcademicYear, y = mean_value, group = bin), 
              data = df_plot) + 
    
    # Add point, line, and text for coefficients
    geom_path(aes(linetype = bin, color = bin), size = 0.5) + 
    geom_point(aes(shape = bin, color = bin), size = 1.5) + 
    geom_text(aes(label = sprintf("%.1f", round(mean_value, 1)), 
                  hjust = 0.5, vjust = 2.0), size = 2.3) +
    
    # Scales
    scale_x_continuous(breaks = seq(2012, 2017, by = 1)) + 
    scale_y_continuous(limits = ylim) + 
    
    # Add x-axis reference lines and annotate periods
    geom_vline(xintercept = c(2012.5), color = "gray", linetype = "dashed") + 
    # annotate("label", x = c(2013), y = yref, 
    #          label = c("LCFF"), 
    #          color = "black", size = 4) + 
    # annotate("segment", x = 2012.5, xend = 2012.5, y = yref, yend = yref - 2) +
    # annotate("rect", xmin = 2012.5, xmax = 2017,
    #          ymin = yref - rec_y_incr,
    #          ymax = yref + rec_y_incr,
    #          alpha = .1) +
    
    # Theme
    theme_classic() + 
    theme(panel.background = element_blank(),
          panel.grid = element_blank(), 
          legend.position = "bottom", 
          legend.direction = "horizontal", 
          legend.title = element_blank(), 
          plot.title = element_text(hjust = 0.0)) + 
    
    # Labels
    labs(title = title, 
         subtitle = subtitle, 
         caption = NULL, 
         y = NULL,  
         x = "Year") + 
    
    # Mannual color and shapes
    scale_color_manual(values = c("dodgerblue1", "firebrick1")) + 
    scale_shape_manual(values = c(16, 17)) + 
    
    # Legend
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  p
}







