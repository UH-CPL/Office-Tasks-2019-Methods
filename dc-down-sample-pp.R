library(tidyverse) 
library(lubridate) 

# @PARAM: A dataframe that contains columns Time, Timestamp, and `col_names` 
# @RETURN: A dataframe with incrementing Time starting from 0s, one unique Timestamp per row, and the average `col_names` per Timestamp 
downsample_using_mean <- function(df, col_names) { 
  ## check if datafram has values 
  if(nrow(df) == 0) { 
    return(NULL) 
  } 
  
  ## get rid of "Frame #" attribute 
  df <- df[ , (colnames(df) %in% c("Time", "Timestamp", col_names))] 
  
  ## convert Timestamp to POSIX time values 
  # df$Timestamp <- as.POSIXct(strptime(df$Timestamp, format="%a %b %d %H:%M:%S", tz = "GMT"), origin="1970-01-01") 
  min_time <- head(df$Timestamp, 1) 
  max_time <- tail(df$Timestamp, 1) 
  
  if(!is.POSIXct(min_time) || !is.POSIXct(max_time) || length(min_time) == 0 || length(max_time) == 0) { 
    return(NULL) 
  } 
  
  ## this for loop goes through each Timestamp and calculates the average `col_names` and adds it to the per_sec_df 
  per_sec_df <- data.frame() 
  for(cur_time in c(min_time : max_time)) { 
    one_sec_data = filter(df, Timestamp == cur_time) 
    time <- as.POSIXct(cur_time, origin="1970-01-01") 
    
    if(nrow(one_sec_data) > 0) { 
      one_sec_data$Time[1] <- as.numeric(time - min_time, units="secs") 
      for (column in col_names) { 
        if(all(is.na(one_sec_data[, column]))) { 
          ################ 
          # val = NA/'' 
          ################ 
          one_sec_data[1, column] <- '' 
        } else { 
          one_sec_data[1, column] <- mean(one_sec_data[[column]], na.rm = TRUE) 
        } 
      } 
      per_sec_df = rbind(per_sec_df, one_sec_data[1, ]) 
    } 
    else { 
      ## if no `col_names` value for Timestamp, it will add an '' value 
      data = data.frame("Time" = as.numeric(time - min_time, units="secs"), "Timestamp" = time) 
      for (column in col_names) { 
        ################ 
        # val = NA/'' 
        ################ 
        val = '' 
        d = data.frame(column = val, row.names = NULL) 
        colnames(d) = c(column) 
        data = cbind(data, d) 
      } 
      per_sec_df = rbind(per_sec_df, data) 
    } 
  } 
  return(per_sec_df) 
} 
