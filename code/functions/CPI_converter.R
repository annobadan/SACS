
###'######################################################################
###'
###' Function for transfroming into real 2016 dollars
###' 
###' Using the consumer price index (CPI)
###' 
###'  
###' 20180731 JoonHo Lee
###' 
###' 

### Package dependency
library(tidyverse)


###' Import CPI-U Indices from 1913-2017
###' Include the CPI data in this R code 
###' to make it unnecessary to import external dataset
###' (Use dput() function to create vectors

Year <- 1913:2017

CPI <- c(9.9, 10, 10.1, 10.9, 12.8, 15.1, 17.3, 20, 17.9, 16.8, 17.1, 
         17.1, 17.5, 17.7, 17.4, 17.1, 17.1, 16.7, 15.2, 13.7, 13, 13.4, 
         13.7, 13.9, 14.4, 14.1, 13.9, 14, 14.7, 16.3, 17.3, 17.6, 18, 
         19.5, 22.3, 24.1, 23.8, 24.1, 26, 26.5, 26.7, 26.9, 26.8, 27.2, 
         28.1, 28.9, 29.1, 29.6, 29.9, 30.2, 30.6, 31, 31.5, 32.4, 33.4, 
         34.8, 36.7, 38.8, 40.5, 41.8, 44.4, 49.3, 53.8, 56.9, 60.6, 65.2, 
         72.6, 82.4, 90.9, 96.5, 99.6, 103.9, 107.6, 109.6, 113.6, 118.3, 
         124, 130.7, 136.2, 140.3, 144.5, 148.2, 152.4, 156.9, 160.5, 
         163, 166.6, 172.2, 177.1, 179.9, 184, 188.9, 195.3, 201.6, 207.3, 
         215.303, 214.537, 218.056, 224.939, 229.594, 232.957, 236.736, 
         237.017, 240.007, 245.12)

CPI <- data.frame(Year, CPI)


###' Define CPI Converter
###' ex) Calculate the conversion factors based on 2016 dollars
###'     $1.00 (2006) : $x (2016) = 201.6 : 240.0
###'     $x (2016) = 240.0 / 201.6 => conversion factor
###'     Multiply this conversion factor to 2006 dollars  

CPI_converter <- function(data,
                          year_to = 2016, 
                          year_from = NULL, 
                          variable_to_convert = NULL){
  
  # Enquote variables
  year_from <- enquo(year_from)
  var <- enquo(variable_to_convert)
  
  # Generate CPI conversion factor
  CPI_year_to <- CPI$CPI[which(CPI$Year == year_to)]
  CPI <- mutate(CPI, cvt_factor = CPI_year_to/CPI)
  
  # Merge to the target dataset and muliply the conversion factor
  var_converted <- paste0(quo_name(var), "_", substr(year_to, 3, 4))
  
  data <- data %>%
    rename(Year = !!year_from) %>%
    left_join(CPI, by = c("Year")) %>%
    mutate(!!var_converted := round(!!var*cvt_factor, 2)) %>%
    select(-CPI, -cvt_factor) 
  
  # Rename back the "Year" which is temporarily used for merging
  names(data)[which(names(data) == "Year")] <- quo_name(year_from)
  
  return(data)
}    

# ###' Test the code
# df <- df_cur_exp 
# df <- CPI_converter(df, 2016, FiscalYear, TotalExp_C)
# df <- CPI_converter(df, 2016, FiscalYear, TotalExp_K12_C)

