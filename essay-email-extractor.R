#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(XLConnect)
library(xlsx)
library(tibble)



#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path) 
setwd(current_dir)

source('common-functions.R')

raw_data_dir <- 'raw-dataset'
# script_dir <- 'nsf-data-paper-scripts'
data_dir <- 'data'
performance_data_dir <- 'performane-data'
final_data_dir <- 'final-data-set'
textual_data_dir <- 'Textual Data'

super_session_pattern <- '^SuperSession$'

# testing_df_file_path <- 'nsf-stress-study-scripts/@Datasets/testing_df.csv'
essay_df <- tibble()


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
convert_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ',')
}

getAllDirectoryList <- function(directory) {
  return(list.dirs(path=directory, full.names=F, recursive=F))
}

isMatchedString <- function(pattern, str) {
  return(grepl(pattern, str, perl=TRUE))
}

isEmptyDataFrame <- function(df) {
  return(is.data.frame(df) && nrow(df)==0)
}

getMatchedFileNames <- function(directory, file_pattern) {
  return(list.files(path=directory, pattern=file_pattern, recursive=F))
}

# getEssayTibble <- function(subj_name, session, essay_content) {
#   return(tibble('Subject'=subj_name, 
#                 'Session'=session, 
#                 'Essay'=essay_content))
# }

get_data_if_col_exists <- function(subj_name, df, col_name){
  # message(col_name)
  if(col_name %in% colnames(df)) {
    return(df[[col_name]])
  }
  
  message(paste0('For Subject ', subj_name, ', ', col_name, ' does not exists.'))
  return('')
}

getEssayTibble <- function(subj_name, df) {
  return(tibble('Participant_ID'=subj_name, 
                'ST_Report'=df$Essay.Baseline.Content,
                'DT_Report'=df$Essay.Dualtask.Content,
                'Email1'=get_data_if_col_exists(subj_name, df, 'Email.1.Content'),
                'Email2'=get_data_if_col_exists(subj_name, df, 'Email.2.Content'),
                'Email3'=get_data_if_col_exists(subj_name, df, 'Email.3.Content'),
                'Email4'=get_data_if_col_exists(subj_name, df, 'Email.4.Content'),
                'Email5'=get_data_if_col_exists(subj_name, df, 'Email.5.Content'),
                'Email6'=get_data_if_col_exists(subj_name, df, 'Email.6.Content'),
                'Email7'=get_data_if_col_exists(subj_name, df, 'Email.7.Content'),
                'Email8'=get_data_if_col_exists(subj_name, df, 'Email.8.Content')
                ))
}

addRowForEssay <- function(df, tibble_df) {
  if (isEmptyDataFrame(df)) {
      return(tibble_df)
    } else {
      return(rbind(df, tibble_df))
    }
}

extractEssayForEachSubject <- function(session_dir, subj_name) {
  # subj_interface_file_pattern <- paste0('.*-', subj_name, '.xlsx')
  subj_interface_file_pattern <- paste0('^[^~].*-', subj_name, '.xlsx')
  subj_interface_file_name <- getMatchedFileNames(session_dir, subj_interface_file_pattern)
  subj_interface_df <- readWorksheet(XLConnect::loadWorkbook(file.path(session_dir, subj_interface_file_name)), sheet = 'Sheet1')
  
  # essay_df <<- addRowForEssay(essay_df, getEssayTibble(subj_name, 'WB', subj_interface_df$Essay.Baseline.Content))
  # essay_df <<- addRowForEssay(essay_df, getEssayTibble(subj_name, 'DT', subj_interface_df$Essay.Dualtask.Content))
  
  essay_df <<- addRowForEssay(essay_df, getEssayTibble(subj_name, subj_interface_df))
}


extractEssays <- function() {
  grp_list <- getAllDirectoryList(raw_data_dir)
  good_subj_list <- read.csv(file.path(data_dir, 'subj_good_df.csv'))$Subject
  
  sapply(grp_list, function(grp_name) {
  # sapply(grp_list[1], function(grp_name) {
    
    grp_dir <- file.path(raw_data_dir, grp_name)
    subj_list <- getAllDirectoryList(grp_dir)
    
    sapply(subj_list, function(subj_name) {
    # sapply(subj_list[3], function(subj_name) {
      # good_subj_list <- list('T003')
      subj_dir <- file.path(grp_dir, subj_name)
      session_list <- getAllDirectoryList(subj_dir)
      session_list <- session_list[isMatchedString(super_session_pattern, session_list)]
      
      sapply(session_list, function(session_name) {
        # sapply(session_list[3], function(session_name) {
        session_dir <- file.path(getwd(), subj_dir, session_name)
        
        tryCatch({
          if(subj_name %in% good_subj_list) {
            extractEssayForEachSubject(session_dir, subj_name)
            # message(paste('Good Subject: ', subj_name))
          } else {
            # message(paste('Not good Subject: ', subj_name))
          }
          
        },
        error=function(cond) {
          
          # write('----------------------------------------------------------', file=log.file, append=TRUE)
          # write(paste0(grp_name, '-', subj_name, '-', session_name, ': ERROR!'), file=log.file, append=TRUE)
          # write(paste0(cond, '\n'), file=log.file, append=TRUE)
          
          message('----------------------------------------------------------')
          message(paste0(grp_name, '-', subj_name, '-', session_name, ': ERROR!'))
          message(paste0(cond, '\n'))
        })
      })
    })
  })

  
  # write.xlsx(as.data.frame(essay_df), file.path(getwd(), script_dir, data_dir, performance_data_dir, 'Reports and Emails.xlsx'))
  write.xlsx(as.data.frame(essay_df), file.path(getwd(), data_dir, final_data_dir, textual_data_dir, 'Reports and Emails.xlsx'))
} 



#-------------------------#
#-------Main Program------#
#-------------------------#
extractEssays() 


