#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
# install.packages('XLConnect', dependencies=T)
# install.packages('lubridate', dependencies=T)
# install.packages('readr', dependencies=T)
library(XLConnect)
library(scales)
library(ggplot2)
library(dplyr)
library(readr)

require(xlsx)
library(readxl)
library(lubridate)


#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)


source('dc-filter-pp.R')
source('dc-down-sample-pp.R')


re_extract_pp_dir <- 're-extracted-signals'
data_dir <- 'raw-dataset'
log_dir <- 'log-files'


# s_interface_date_format <- '%a %b %d %H:%M:%OS %Y'
s_interface_date_format <- '%a %b %d %H:%M:%S'
subj_interface_date_format <- '%H:%M:%S'
zephyr_date_format <- '%d/%m/%Y %H:%M:%S'

super_session_pattern <- '^SuperSession$'
one_hour_sec <- 60*60

common_file_pattern <- '.*_pp.csv|.*_ks.csv'
pp_file_pattern <- '.*_pp.csv'
pp_new_file_pattern <- '.*_pp_new.csv'
nr_pp_file_pattern <- '.*_nr.csv'
marker_file_pattern <- '.*_sessionmarkers.csv'
marker_new_file_pattern <- '.*_sessionmarkers_new.csv'
summary_file_pattern <- '.*_Summary.csv'
e4_file_pattern <- 'HR.csv|EDA.csv'

bad_eda_subj_list <- c('T051', 'T145')
discarded_subj_list <- list('T067', 'T023')
new_subj_list <- list()
# subject_list_first_phase <- list()
subject_list_first_phase <- tibble()

data_error <- ''

discarded_df <- tibble()
non_processed_df <- tibble()




runnable_subj_list <- read.csv("data/subj_good_df.csv")$Subject
# runnable_subj_list <- read.csv("nsf-stress-study-scripts/@Datasets/subj_full_df.csv")$Subject


if (length(discarded_subj_list) > 0) { 
  for (discarded_subj in discarded_subj_list) { 
    if (nrow(discarded_df) == 0) { 
      discarded_df <- tibble("Subject" = discarded_subj, 
                             "Session" = c("RestingBaseline", "BaselineWriting", "StressCondition", "DualTask", "Presentation"), 
                             "Measure" = "ALL", 
                             "Explaination" = "Stopped halfway through") 
    } else { 
      discarded_df_temp <- tibble("Subject" = discarded_subj, 
                             "Session" = c("RestingBaseline", "BaselineWriting", "StressCondition", "DualTask", "Presentation"), 
                             "Measure" = "ALL", 
                             "Explaination" = "Stopped halfway through") 
      discarded_df <- rbind(discarded_df, discarded_df_temp) 
    } 
  } 
} 



#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
convert_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ',')
}

convert_date <- function(data, date_format) {
  return(as.POSIXct(data, format=date_format))
}

convert_s_interface_date <- function(data) {
  convert_date(data, s_interface_date_format)
}

isMatchedString <- function(pattern, str) {
  return(grepl(pattern, str, perl=TRUE))
}

getNextSessionRows <- function(session_name, df, start_time, end_time) {
  return(rbind(df, data.frame(Session=session_name, StartTime=start_time, EndTime=end_time)))
}

isEmpty <- function(list_dir) {
  return(length(list_dir)==0)
}

isEmptyDataFrame <- function(df) {
  return(is.data.frame(df) && nrow(df)==0)
}

getMatchedFileNames <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=F))
}

getMatchedFileNamesRecursively <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=T))
}

getAllDirectoryList <- function(directory) {
  return(list.dirs(path=directory, full.names=F, recursive=F))
}

getMatchedFileNamesFullPath <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=F, full.names=T))
}

getKnownError <- function(subj_name) {
  if(subj_name %in% list('T062')) {
    return('No data at Session Marker File')
  }
  return('')
}

checkTotalSessions <- function(df, subj_name) {
  if (nlevels(droplevels(df)$Session) < 5) {
    write(paste0('--- ', subj_name, ' has less than 5 sessions ---'), file=log.file, append=TRUE)
    message(paste0('--- ', subj_name, ' has less than 5 sessions ---'))
  }
}

copyReExtractedDataToNsfDir <- function() {
  subj_list <- getAllDirectoryList(re_extract_pp_dir)
  
  sapply(subj_list, function(subj_name) {
    subj_dir <- file.path(getwd(), re_extract_pp_dir, subj_name)
    subj_serial <- as.numeric(substr(subj_name, 2, 4))
    
    pp_file_name <- getMatchedFileNames(subj_dir, pp_file_pattern)
    pp_file_full_path <- getMatchedFileNamesFullPath(subj_dir, pp_file_pattern)
    
    if(!isEmpty(pp_file_name)) {
      grp_name <- paste0('Group', toString(subj_serial%/%60 + 1))
      pp_new_file_name <- paste0(substr(pp_file_name, 1, nchar(pp_file_name)-4), '_new.csv')
      pp_new_dest_path <- file.path(getwd(), data_dir, grp_name, subj_name, 'SuperSession', pp_new_file_name)
      file.copy(from = pp_file_full_path, to = pp_new_dest_path)
    }
  })
}

getFileNameByNewOldPattern <- function(directory, file_pattern, new_file_pattern) {
  file_name <- getMatchedFileNames(directory, new_file_pattern)
  if(isEmpty(file_name)) {
    file_name <- getMatchedFileNames(directory, file_pattern)
  }
  
  return(file_name)
}

getSignalWithSession <- function(marker_time_df, signal_df) {
  signal_with_session_df <- data.frame()
  for(m_idx in 1 : nrow(marker_time_df)) {
    cur_row <- marker_time_df[m_idx, ]
    temp_df <- signal_df %>% filter(cur_row$StartTime<=CovertedTime & CovertedTime<=cur_row$EndTime)
    temp_df <- cbind(temp_df, Session=rep(cur_row$Session, nrow(temp_df)))
    signal_with_session_df <- rbind(signal_with_session_df, temp_df)
  }
  
  return(signal_with_session_df)
}

getHourMinSec <- function(date_to_convert) {
  if (!is(date_to_convert, "character")) {
    date_to_convert <- strftime(date_to_convert, format="%H:%M:%S")
  }
  return(hms(date_to_convert))
}

convertTimestamp <- function(df, intermittent_df, timestamp='Timestamp') { 
  
  df[[timestamp]] <- as.POSIXct(strptime(df[[timestamp]], format=s_interface_date_format)) 
  intermittent_time <- getHourMinSec(intermittent_df$Baseline.Stress.Timestamp)
  
  min_time <- head(df[[timestamp]], 1) 
  max_time <- tail(df[[timestamp]], 1) 
  
  ## First check if time starts in AM then goes to PM - we need to convert later times to 24-hour format 
  if (min_time > max_time) { 
    df[df[[timestamp]] < min_time, ][[timestamp]] = df[df[[timestamp]] < min_time, ][[timestamp]] + (12 * one_hour_sec) 
    return(list(df, TRUE))
    
    ## Now check if time is all PM - we need to convert ALL times to 24-hour format  
  } else if (hour(intermittent_time) > hour(max_time)) { 
    df[[timestamp]] = df[[timestamp]] + (12 * one_hour_sec) 
    return(list(df, TRUE)) 
  } 
  
  return(list(df, FALSE)) 
}

convertTimestampSessionMarkers <- function(df, intermittent_df, subj_name, timestamp=c('startTimestamp', 'EndTimestamp')) { 
  
  df[[timestamp[1]]] <- as.POSIXct(strptime(df[[timestamp[1]]], format=s_interface_date_format)) 
  df[[timestamp[2]]] <- as.POSIXct(strptime(df[[timestamp[2]]], format=s_interface_date_format))
  
  intermittent_time <- getHourMinSec(intermittent_df$Baseline.Stress.Timestamp)

  min_time <- head(df[[timestamp[1]]], 1) 
  max_time <- tail(df[[timestamp[2]]], 1) 
  
  if (min_time > max_time) { 
    for (i in 1:nrow(df)) { 
      if (df[i, timestamp[1]] < min_time) { 
        df[i, timestamp[1]] <- df[i, timestamp[1]] + (12 * one_hour_sec) 
      } 
      if (df[i, timestamp[2]] < min_time) { 
        df[i, timestamp[2]] <- df[i, timestamp[2]] + (12 * one_hour_sec) 
      } 
    } 
    message(paste0('Timestamps changed for subject ', subj_name, '. ')) 
    flush.console() 
    return(df) 
    
  } else if (hour(intermittent_time) > hour(max_time)) { 
    df[[timestamp[1]]] <- df[[timestamp[1]]] + (12 * one_hour_sec) 
    df[[timestamp[2]]] <- df[[timestamp[2]]] + (12 * one_hour_sec) 
    message(paste0('Timestamps changed for ', subj_name, '. ')) 
    flush.console() 
    return(df) 
  }
  
  return(df) 
}

splitSessions <- function(session_dir, subj_name) {
  merged_df <- data.frame()
  file_name <- getMatchedFileNames(session_dir, common_file_pattern)[[1]][1]
  
  pp_file_name <- getMatchedFileNames(session_dir, pp_file_pattern)
  nr_pp_file_name <- getMatchedFileNames(session_dir, nr_pp_file_pattern)
  
  # subj_interface_file_pattern <- paste0('.*-', subj_name, '.xlsx')
  subj_interface_file_pattern <- paste0('^[^~].*-', subj_name, '.xlsx') 
  subj_interface_file_name <- getMatchedFileNames(session_dir, subj_interface_file_pattern)
  marker_file_name <- getMatchedFileNames(session_dir, marker_file_pattern)
  
  # pp_file_name <- getFileNameByNewOldPattern(session_dir, pp_file_pattern, pp_new_file_pattern)
  # marker_file_name <- getMatchedFileNames(session_dir, marker_file_pattern, marker_new_file_pattern)
  
  data_error <<- getKnownError(subj_name)
  if(isEmpty(subj_interface_file_name)) {
    data_error <<- 'Excel File Unavailable'
  } else if(isEmpty(marker_file_name)) {
    data_error <<- 'Session Marker File Unavailable'
  }
  
  subj_interface_df <- readWorksheet(XLConnect::loadWorkbook(file.path(session_dir, subj_interface_file_name)), sheet = 'Sheet1')
  
  marker_df <- read.csv(file.path(session_dir, marker_file_name))
  marker_df <- convertTimestampSessionMarkers(marker_df, subj_interface_df, subj_name) 
  
  rb_start_time <- convert_s_interface_date(marker_df$startTimestamp[1])
  rb_end_time <- convert_s_interface_date(marker_df$EndTimestamp[1])
  
  presentation_start_time <- convert_s_interface_date(marker_df$startTimestamp[2])
  presentation_end_time <- convert_s_interface_date(marker_df$EndTimestamp[2])
  
  baseline_essay_start_time <- convert_date(subj_interface_df$Baseline.Essay.Timestamp[1], subj_interface_date_format)
  stress_cond_start_time <- convert_date(subj_interface_df$Stress.Condition.Timestamp[1], subj_interface_date_format)
  dual_task_start_time <- convert_date(subj_interface_df$Dual.Essay.Timestamp[1], subj_interface_date_format)
  
  date(baseline_essay_start_time) <- as.Date(rb_start_time)
  date(stress_cond_start_time) <- as.Date(rb_start_time)
  date(dual_task_start_time) <- as.Date(rb_start_time)
  
  baseline_essay_end_time <- baseline_essay_start_time + 5*60
  stress_cond_end_time <- stress_cond_start_time + 5*60
  dual_task_end_time <- dual_task_start_time + 50*60
  
  marker_time_df <- data.frame()
  marker_time_df <- getNextSessionRows('RestingBaseline', marker_time_df, rb_start_time, rb_end_time)
  marker_time_df <- getNextSessionRows('BaselineWriting', marker_time_df, baseline_essay_start_time, baseline_essay_end_time)
  marker_time_df <- getNextSessionRows('StressCondition', marker_time_df, stress_cond_start_time, stress_cond_end_time)
  marker_time_df <- getNextSessionRows('DualTask', marker_time_df, dual_task_start_time, dual_task_end_time)
  marker_time_df <- getNextSessionRows('Presentation', marker_time_df, presentation_start_time, presentation_end_time)
  
  marker_time_df$StartTime <- with(marker_time_df, StartTime - years(1))
  marker_time_df$EndTime <- with(marker_time_df, EndTime - years(1))
  
  convert_to_csv(marker_time_df, file.path(session_dir, paste0(substr(file_name, 1, 11), '_marker.csv')))
  
  if(!isEmpty(pp_file_name)) {
    if(isEmpty(nr_pp_file_name)) {
      pp_df <- read.csv(file.path(session_dir, pp_file_name))
      names(pp_df) <- c("Frame#",	"Time",	"Timestamp", "Perspiration") 
      pp_df <- data.frame(convertTimestamp(pp_df, subj_interface_df)[1])
      
      pp_df$NR_Perspiration <- remove_noise(pp_df$Perspiration)
      downsampled_pp_df <- downsample_using_mean(pp_df, c('Perspiration', 'NR_Perspiration'))
      
      downsampled_pp_df <- downsampled_pp_df[, c(1, 4, 2, 3)]
      names(downsampled_pp_df)[names(downsampled_pp_df) == 'Timestamp'] <- 'CovertedTime'
      convert_to_csv(downsampled_pp_df, file.path(session_dir, paste0(substr(file_name, 1, nchar(file_name)-7), '_pp_nr.csv')))
      
    } else {
      downsampled_pp_df <- read.csv(file.path(session_dir, nr_pp_file_name))
      downsampled_pp_df$CovertedTime <- as.POSIXct(downsampled_pp_df$CovertedTime)
    }
    
    pp_with_session_df <- getSignalWithSession(marker_time_df, downsampled_pp_df)
    convert_to_csv(pp_with_session_df, file.path(session_dir, paste0(substr(file_name, 1, nchar(file_name)-7), '_session.csv')))
    
    merged_df <- pp_with_session_df
    # print(nrow(merged_df))
  }
  
  ##########################################################################
  ######################## Merging Sensor Signals ##########################
  ##########################################################################
  #-------------------------------------------------------------------------------------------- ZEPHYR
  zephyr_file_name <- getMatchedFileNamesRecursively(session_dir, summary_file_pattern)
  
  if(!isEmpty(zephyr_file_name)) {
    ### Read only Time, HR and BR 
    
    ##### THIS IS WITH HEART RATE CONFIDENCE #####
    # zephyr_df <- read.csv(file.path(session_dir, zephyr_file_name))[, c(1, 2, 3, 15)]
    zephyr_df <- read.csv(file.path(session_dir, zephyr_file_name))[, c(1, 2, 3)]
    zephyr_df$CovertedTime <- as.POSIXct(zephyr_df$Time, format=zephyr_date_format)
    # print(colnames(zephyr_df))
    
    if(!isEmpty(pp_file_name)) {
      merged_df <- merge(merged_df, zephyr_df, by='CovertedTime', all.x=T)[, c(1, 2, 3, 4, 7, 8, 5)]
      ##### THIS IS WITH HEART RATE CONFIDENCE #####
      # merged_df <- merge(merged_df, zephyr_df, by='CovertedTime', all.x=T)[, c(1, 2, 3, 4, 7, 8, 9, 5)]
    } else {
      merged_df <- getSignalWithSession(marker_time_df, zephyr_df)[, c(1, 4, 2, 3, 5)]
    }
    
    ###############################################################################################################################
    #### CHANGE THIS - Data Filtering - V.V.I. - ***
    #### NOW WE FILTER OUT HEART RATE SIGNALS FOR SESSIONS WHO HAVE HEART RATE CONFIDENCE LESS THAN 85 
    # heart_rate_confidence_threshold <- 95
    # should_we_filter <- merged_df %>%
    #   group_by(Session) %>%
    #   summarize(meanHR = mean(HRConfidence, na.rm = TRUE))
    # 
    # session_list <- c("RestingBaseline", "BaselineWriting", "StressCondition", "DualTask", "Presentation")
    # bad_sessions <- NULL
    # for (session in session_list) {
    #   avg <- (should_we_filter %>%
    #             filter(Session == session))[1, "meanHR"]
    # 
    #   if (is.na(avg) | avg < heart_rate_confidence_threshold) {
    #     bad_sessions <- c(bad_sessions, session)
    #   }
    # }
    # 
    # if (!is.null(bad_sessions)) {
    #   merged_df[merged_df$Session %in% bad_sessions, "HR"] <- NA
    #   specific_discarded_df <- tibble("Subject" = subj_name, "Session" = bad_sessions, "Measure" = "HR", Explaination = "HR Confidence")
    # 
    #   if (nrow(discarded_df) == 0) {
    #     discarded_df <<- specific_discarded_df
    #   } else {
    #     discarded_df <<- rbind(discarded_df, specific_discarded_df)
    #   }
    # 
    #   message(paste0(subj_name, " HR signal for ", bad_sessions, " has been removed due to HR Confidence less than ",
    #                  heart_rate_confidence_threshold, ". // "))
    #   write(paste0(subj_name, " HR signal for ", bad_sessions, " has been removed due to HR Confidence less than ",
    #                heart_rate_confidence_threshold, ". // "), file=log.file, append=TRUE)
    # }
    ###############################################################################################################################
    
    
    
    ##### THIS IS WITH HEART RATE CONFIDENCE #####
    # merged_df <- merged_df[ , (names(merged_df) != "HRConfidence")] 
    
    ## DONT DELETE
    colnames(merged_df)[2] <- 'Time'
    names(merged_df)[names(merged_df) == 'HR'] <- 'HR_z'
    names(merged_df)[names(merged_df) == 'BR'] <- 'BR_z'
    
    # print(nrow(merged_df))
  }
  
  
  #-------------------------------------------------------------------------------------------- EMPATICA
  e4_file_list <- getMatchedFileNamesRecursively(session_dir, e4_file_pattern)
  for(e4_file_name in e4_file_list) {
    # merged_df <- sapply(e4_file_list, function(e4_file_name) { 
    e4_df <- read_csv(file.path(session_dir, e4_file_name), col_names=F, col_types = cols())
    
    # if(ncol(df) > 1) {
    #   message('Expected one column starting with UTC time then sample rate then values - returning original dataframe.')
    #   flush.console()
    #   return(df)
    # }
    
    timestamp <- as.numeric(e4_df[1, 1]) 
    rate <- as.numeric(e4_df[2, 1]) 
    e4_df <- e4_df[-(1:2), ]
    
    vector <- c(timestamp)
    if (rate==1) { 
      ## Sample rate is 1 - just add a CovertedTime timestamp
      for (i in 2:nrow(e4_df)) { 
        vector <- c(vector, timestamp + i - 1) 
      } 
      
    } else { 
      ## Sample rate is not 1 - take mean for every `rate` values, then add a CovertedTime timestamp 
      for (i in 1:round(nrow(e4_df)/rate)) { 
        e4_df[i, ] <- sum(e4_df[(rate * (i - 1) + 1):(rate * i), ])/rate 
        vector <- c(vector, timestamp + i) 
      } 
      
      vector <- vector[-length(vector)] 
      ## list is one element too large after the loop, so we delete the final element 
      e4_df <- e4_df[1:round(nrow(e4_df)/rate), ] 
    } 
    
    colnames(e4_df) <- c(sub('.csv', '', sub('.*/', '', e4_file_name)) )
    e4_df$CovertedTime <- as.POSIXct(vector, origin='1970-01-01', tz='America/Chicago') # timestamp with Houston timezone 
    e4_df <- data.frame(e4_df)
    # e4_df$Time <- as.numeric(e4_df$CovertedTime - head(df$CovertedTime, n=1), units='secs') # elapsed time
    
    ## Change the column names so they are more meaningful: 
    if (grepl('phone', e4_file_name)) { 
      if (grepl('HR', e4_file_name)) { 
        names(e4_df)[names(e4_df) == 'HR'] <- 'HR_i' 
      } else if (grepl('EDA', e4_file_name)){ 
        names(e4_df)[names(e4_df) == 'EDA'] <- 'EDA_i' 
      } 
    } else if (grepl('laptop', e4_file_name)) { 
      if (grepl('HR', e4_file_name)) { 
        names(e4_df)[names(e4_df) == 'HR'] <- 'HR_l' 
      } else if (grepl('EDA', e4_file_name)){ 
        names(e4_df)[names(e4_df) == 'EDA'] <- 'EDA_l' 
      } 
    }
    
    ## TIMEZONE BUG FIX 
    if (!subj_name %in% bad_eda_subj_list) {
      if (as.numeric(e4_df$CovertedTime[1] - merged_df$CovertedTime[1]) > 1) { 
        e4_df$CovertedTime <- e4_df$CovertedTime - 2 * one_hour_sec 
      } 
    }
    
    merged_df <- merge(merged_df, e4_df, by='CovertedTime', all.x=T)
  }
  
  checkTotalSessions(merged_df, subj_name)
  convert_to_csv(merged_df, file.path(session_dir, paste0(substr(file_name, 1, nchar(file_name)-7), '_merged.csv')))
  
  
  # merged_df <- sapply(merged_df, as.character) # since the values are `factor` ## DELETE THIS LINE IF SUBJECTBOOK IS GOOD ##
  merged_df[is.na(merged_df)] <- "" # replacing NA with ""
  convert_to_csv(as.data.frame(merged_df), file.path(session_dir, paste0(substr(file_name, 1, nchar(file_name)-7), '_all_signals.csv')))
}

splitSessionsForPP <- function() {
  grp_list <- getAllDirectoryList(data_dir)
  sapply(grp_list, function(grp_name) {
  # sapply(grp_list[1], function(grp_name) {

    grp_dir <- file.path(data_dir, grp_name)
    subj_list <- getAllDirectoryList(grp_dir)
    
    sapply(subj_list, function(subj_name) {
    # sapply(subj_list[3], function(subj_name) {
      subj_dir <- file.path(grp_dir, subj_name)
      session_list <- getAllDirectoryList(subj_dir)
      session_list <- session_list[isMatchedString(super_session_pattern, session_list)]
      
      sapply(session_list, function(session_name) {
        session_dir <- file.path(getwd(), subj_dir, session_name)
        tryCatch({
          if(subj_name %in% runnable_subj_list & !(subj_name %in% discarded_subj_list) & !(subj_name %in% new_subj_list)) {
            splitSessions(session_dir, subj_name)
            
            write(paste0(grp_name, '-', subj_name, '-', session_name, ': SUCCESSFUL'), file=log.file, append=TRUE)
            message(paste0(grp_name, '-', subj_name, '-', session_name, ': SUCCESSFUL'))
            
          } else {
            write(paste0(grp_name, '-', subj_name, '-', session_name, ': NOT PROCESSED'), file=log.file, append=TRUE)
            message(paste0(grp_name, '-', subj_name, '-', session_name, ': NOT PROCESSED'))
            
            if(subj_name %in% discarded_subj_list) {
              write('Reason: Subject is discarded subject list!', file=log.file, append=TRUE)
              message('Reason: Subject is discarded subject list!')
            } else if (subj_name %in% new_subj_list) {
              write('Reason: Subject is entered after 08-26-18', file=log.file, append=TRUE)
              message('Reason: Subject is entered after 08-26-18')
            }
          }
        },
        error=function(cond) {
          
          temp_non_processed_df <- tibble("Subject" = subj_name, "Error" = data_error)
          
          if (nrow(non_processed_df) == 0) {
            non_processed_df <<- temp_non_processed_df
          } else {
            non_processed_df <<- rbind(non_processed_df, temp_non_processed_df)
          }
          
          write('----------------------------------------------------------', file=log.file, append=TRUE)
          write(paste0(grp_name, '-', subj_name, '-', session_name, ': ERROR!'), file=log.file, append=TRUE)
          write(paste0(cond, '\n'), file=log.file, append=TRUE)
          
          message('----------------------------------------------------------')
          message(paste0(grp_name, '-', subj_name, '-', session_name, ': ERROR!'))
          message(paste0(cond, '\n'))
        })
      })
    })
  })
}


#-------------------------#
#-------Main Program------#
#-------------------------#
log.file <- file.path(log_dir, paste0('session-split-log-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
file.create(log.file)

# copyReExtractedDataToNsfDir()
splitSessionsForPP() 


