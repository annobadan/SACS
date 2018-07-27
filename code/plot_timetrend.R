
###'######################################################################
###'
###' Helper function for plotting
###' 
###' Plot summarizing longitudinal trends
###' 
###' => Plot point, path, and values to easily visualize time trend
###'  
###' 20180726 JoonHo Lee
###' 
###' 

### Package dependency
library(tidyverse)
library(scales)


### Define function 'plot_timetrend'
plot_timetrend <- function(dataframe, 
                           x, 
                           y,
                           group = NULL, 
                           facet = NULL,
                           ref_line = TRUE, 
                           ref_line_pos = 2013, 
                           ylim = NULL, 
                           xbreak = NULL){
  
  ### Enquote variables
  xvar <- enquo(x)
  yvar <- enquo(y)
  groupvar <- enquo(group)
  facetvar <- enquo(facet)
  
  ###' Assign data and aesthetic mappings
  ###' Add point, path, and value label layers
  if(!is.null(group)){
    p <- ggplot(dataframe) + 
      aes(x = !!xvar, y = !!yvar) + 
      geom_point(size = 3.0) + 
      geom_path(size = 1.0) + 
      geom_text(aes(label = comma(!!yvar)), size = 3, hjust = 0.5, vjust = 2.0)
      
  }else{
    p <- ggplot(dataframe) + 
      aes(x = !!xvar, y = !!yvar, group = !!groupvar) + 
      geom_point(aes(shape = !!groupvar, color = !!groupvar), size = 3.0) + 
      geom_path(aes(linetype = !!groupvar, color = !!groupvar), size = 1.0) + 
      geom_text(aes(label = comma(!!yvar)), size = 3, hjust = 0.5, vjust = 2.0)
  }

  ## Add vertical line layer
  if (ref_line){
    p <- p +
      geom_vline(aes(xintercept = ref_line_pos), color = "red", linetype = "dashed")
    }
  
  ### Faceting
  if (!is.null(facet)){
    p <- p +
      facet_grid(cols = vars(!!facetvar))
  }

  ### Theme settings
  p <- p +
    theme_bw() + 
    theme(panel.background = element_blank(),
          panel.grid = element_blank(), 
          legend.position = "bottom", 
          legend.title = element_blank())
  
  ### Scales
  p + 
    scale_x_continuous(breaks = pretty_breaks(n = xbreak)) + 
    scale_y_continuous(labels = comma, limits = ylim)
}
 
plot_timetrend(data = df_plot, 
               x = Fiscalyear, 
               y = mean_value, 
               group = key, 
               facet = Dtype,
               ref_line = TRUE, 
               ref_line_pos = 2013, 
               ylim = c(8000, 18000), 
               xbreak = 14)

print(p)

p + scale_color_manual(values = c("firebrick1", "dodgerblue1")) + 
  scale_linetype_manual(values = c("solid", "dashed"))
   
  



  ### Plot
  p <- ggplot(data = df_plot, aes(x = Fiscalyear, y = mean_value, group = key)) + 
    # Point
    geom_point(aes(shape = key, color = key), size = 3.0) + 
    scale_linetype_manual(values = c("solid", "dashed")) + 
    # Path
    geom_path(aes(linetype = key, color = key), size = 1.0) + 
    scale_color_manual(values = c("firebrick1", "dodgerblue1")) + 
    # Value label
    geom_text(aes(label = comma(mean_value)), size = 3, hjust = 0.5, vjust = 2.0) +
    # Reference line
    geom_vline(aes(xintercept = 2013), color = "red", linetype = "dashed") + 
    # Facetting
    facet_grid(~Dtype) + 
    # Settings
    theme_bw() + 
    theme(panel.background = element_blank(),
          panel.grid = element_blank(), 
          legend.position = "bottom", 
          legend.title = element_blank()) + 
    # Labels
    scale_x_continuous(breaks = pretty_breaks(n = 14)) +
    scale_y_continuous(labels = comma, limits = c(8000, 18000)) +  
    labs(title = "District per-pupil expenditures by district type and definitions",
         subtitle = "Inflation adjusted using the CPI-U deflator, Weighted averages by ADA",
         caption = "Source: Annual Financial Data, California Department of Education", 
         y = "Average total expenditures per student (in real 2016 dollars)",  
         x = "Fiscal Year")
  
  
  
  

    
    # Labels
    scale_x_continuous(breaks = pretty_breaks(n = 14)) +
    scale_y_continuous(labels = comma, limits = c(8000, 18000)) +  
    labs(title = "Total expenditures per student (Not inflation adjusted)",
         subtitle = "Definition 1 (All funds), Weighted averages by ADA", 
         caption = "Source: Annual Financial Data, California Department of Education", 
         y = "Average total expenditures per student (in dollars)",  
         x = "Fiscal Year")
  
  filename <- c("Total Expenditure perADA_01_Definition1_Not inflation adjusted.pdf")
  ggsave(paste0("figure/", filename, ".pdf"), p, width = 9, height = 5)
  
  
  
  
  
  
  
  
  
  data <- mcmc_intervals_data(x, pars, regex_pars, transformations,
                              prob = prob, prob_outer = prob_outer,
                              point_est = point_est, rhat  = rhat)
  
  color_by_rhat <- rlang::has_name(data, "rhat_rating")
  no_point_est <- all(data$point_est == "none")
  
  x_lim <- range(c(data$ll, data$hh))
  x_range <- diff(x_lim)
  x_lim[1] <- x_lim[1] - 0.05 * x_range
  x_lim[2] <- x_lim[2] + 0.05 * x_range
  
  # faint vertical line at zero if zero is within x_lim
  layer_vertical_line <- if (0 > x_lim[1] && 0 < x_lim[2]) {
    vline_0(color = "gray90", size = 0.5)
  } else {
    geom_ignore()
  }
  
  args_outer <- list(
    mapping = aes_(x = ~ ll, xend = ~ hh, y = ~ parameter, yend = ~ parameter),
    color = get_color("mid")
  )
  args_inner <- list(
    mapping = aes_(x = ~ l, xend = ~ h, y = ~ parameter, yend = ~ parameter),
    size = 2,
    show.legend = FALSE
  )
  args_point <- list(
    mapping = aes_(x = ~ m, y = ~ parameter),
    data = data,
    size = 4,
    shape = 21
  )
  
  if (color_by_rhat) {
    args_inner$mapping <- args_inner$mapping %>%
      modify_aes_(color = ~ rhat_rating)
    args_point$mapping <- args_point$mapping %>%
      modify_aes_(color = ~ rhat_rating,
                  fill = ~ rhat_rating)
  } else {
    args_inner$color <- get_color("dark")
    args_point$color <- get_color("dark_highlight")
    args_point$fill <- get_color("light")
  }
  
  point_func <- if (no_point_est) geom_ignore else geom_point
  
  layer_outer <- do.call(geom_segment, args_outer)
  layer_inner <- do.call(geom_segment, args_inner)
  layer_point <- do.call(point_func, args_point)
  
  # Do something or add an invisible layer
  if (color_by_rhat) {
    scale_color <- scale_color_diagnostic("rhat")
    scale_fill <- scale_fill_diagnostic("rhat")
  } else {
    scale_color <- geom_ignore()
    scale_fill <- geom_ignore()
  }
  
  ggplot(data) +
    layer_vertical_line +
    layer_outer +
    layer_inner +
    layer_point +
    scale_color +
    scale_fill +
    scale_y_discrete(limits = unique(rev(data$parameter))) +
    xlim(x_lim) +
    bayesplot_theme_get() +
    legend_move(ifelse(color_by_rhat, "top", "none")) +
    yaxis_text(face = "bold") +
    yaxis_title(FALSE) +
    yaxis_ticks(size = 1) +
    xaxis_title(FALSE)
}


