#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(readr)
library(dplyr)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

source('common-functions.R')

data_dir <- 'data'
survey_data_dir <- 'survey-data'
performance_data_dir <- 'performance-data'
final_data_dir <- 'final-data-set'
quantitative_data_dir <- 'Quantitative Data'
supplementary_data_dir <- 'Supplementary Data'

qc2_filtered_file_name <- 'full_df_second_phase_filtered.csv'
qc2_mean_file_name <- 'result_df_second_phase.csv'
filtered_subj_file_name <- 'filtered_subject_list.csv'
performance_file_name <- 'ets_score_final.csv'
rr_file_name <- 'rr_df_filtered_qc1.csv'
key_data_file_name <- 'key_str.csv'
index_file_name <- 'index.csv'

index_df_row_range <- c(1:315)

physiological_signal_list <- c('PP', 'N.EDA', 'BR', 'HR', 'N.HR')
physiological_col_order <- c('Participant_ID',
                             'Group',
                             'Treatment',
                             'Time',
                             'Treatment_Time',
                             'Task',
                             'PP_QC',
                             'EDA_QC',
                             'BR_QC',
                             'Chest_HR_QC',
                             'Wrist_HR_QC')

key_str_col_order <- c('Participant_ID',
                   'Group',
                   'Treatment',
                   'Time',
                   'Task',
                   'Is_Key_Up',
                   'Key')

index_col_order <- c('Participant_ID',
                     'Group',
                     'Treatment',
                     'PP',
                     'N.EDA', 
                     'BR', 
                     'HR', 
                     'N.HR',
                     'KeyStroke',
                     'RR')



status_missing <- 0
status_valid <- 1
status_discarded <- -1


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
convert_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ',')
}

is_null <- function(cell) {
  # return(length(cell)==0 | is.na(cell))
  if (length(cell)==0) {
    return(T)
  } else if (is.na(cell)) {
    return(T)
  }

  return(F)
}

make_physiological_df <- function() {
  qc2_filtered_df <- read_csv(file.path(data_dir, qc2_filtered_file_name))
  print(str(qc2_filtered_df))
  
  final_physiological_df <- qc2_filtered_df %>% 
    select(-D.EDA, -D.HR) %>% 
    rename(Participant_ID=Subject,
           Group=Condition,
           Treatment=Session,
           Time=CovertedTime,
           Treatment_Time=TimeElapsed,
           # TaskMarkers=Task,
           PP_QC=PP,
           BR_QC=BR,
           EDA_QC=N.EDA,
           Chest_HR_QC=HR,
           Wrist_HR_QC=N.HR) %>% 
    mutate(Treatment = recode(Treatment,
                            'RestingBaseline' = 'RB',
                            'BaselineWriting' = 'ST',
                            'StressCondition' = 'PM',
                            'DualTask' = 'DT',
                            'Presentation' = 'PR'),
           Group = recode(Group,
                          'IH' = 'CH',
                          'IL' = 'CL'),
           Task = recode(Task, 'Essay' = 'Report'))
  
  # print(str(final_physiological_df[, physiological_col_order)
  convert_to_csv(final_physiological_df[, physiological_col_order], 
                 file.path(data_dir, final_data_dir, quantitative_data_dir, 'Physiological Data.csv'))
}

make_performance_df <- function() {
  performance_df <- read_csv(file.path(data_dir, performance_data_dir, performance_file_name)) %>% 
    rename(Participant_ID=Subject,
           Group=Condition,
           Treatment=Session,
           Word_Count=WordCount,
           Character_Count=CharCount,
           Criterion_Score=CriterionScore,
           Mechanics_Errors=MechanicErrors,
           Grammar_Errors=GrammarErrors,
           Usage_Errors=UsageErrors,
           Style_Errors=StyleErrors,
           Delete_Key_Count=Key_No,
           'Mechanic_Errors/WC'=MechanicErrorsRelative,
           'Grammar_Errors/WC'=GrammarErrorsRelative,
           'Usage_Errors/WC'=UsageErrorsRelative,
           'Style_Errors/WC'=StyleErrorsRelative,
           'Delete_Key/CC'=DeleteKeyRelative) %>% 
    mutate(Group = recode(Group,
                          'IH' = 'CH',
                          'IL' = 'CL')) %>% 
    select(Participant_ID,
           Group,
           Treatment,
           Word_Count,
           Character_Count,
           Criterion_Score,
           Mechanics_Errors,
           Grammar_Errors,
           Usage_Errors,
           Style_Errors,
           Delete_Key_Count,
           'Mechanic_Errors/WC',
           'Grammar_Errors/WC',
           'Usage_Errors/WC',
           'Style_Errors/WC',
           'Delete_Key/CC')
  print(str(performance_df))
  convert_to_csv(performance_df, file.path(data_dir, final_data_dir, quantitative_data_dir, 'Report Data.csv'))
}


make_rr_df <- function() {
  rr_df <- read.csv(file.path(data_dir, rr_file_name)) %>% 
    rename(Participant_ID=Subject,
           Treatment=Session,
           Task=TaskMarkers,
           Treatment_Time=TreatmentTime,
           RR_QC=RR) %>% 
    mutate(Group = recode(Group,
                          'IH' = 'CH',
                          'IL' = 'CL')) %>% 
    select(Participant_ID,
           Group,
           Treatment,
           Task,
           Time,
           Treatment_Time,
           RR_QC)

  # print(str(rr_df))
  convert_to_csv(rr_df, file.path(data_dir, final_data_dir, supplementary_data_dir, 'HRV.csv'))
}

make_key_str_df <- function() {
  key_str_df <- read.csv(file.path(data_dir, key_data_file_name)) %>%
    mutate(Group = recode(Group,
                          'IH' = 'CH',
                          'IL' = 'CL'))
  convert_to_csv(key_str_df, file.path(data_dir, final_data_dir, quantitative_data_dir, 'Keyboard Data.csv'))
}


make_initial_index_df <- function() {
  index_df <- read.csv(file.path(data_dir, 'index_base.csv')) %>%
    mutate(Session = recode_factor(Session,
                                   'RestingBaseline'='RB',
                                   'BaselineWriting'='ST',
                                   'StressCondition'='PM',
                                   'DualTask'='DT',
                                   'Presentation'='PR')) %>%
    arrange(Subject, match(Session, c('RB', 'ST', 'PM', 'DT', 'PR'))) %>% 
    rename(Participant_ID=Subject,
           Treatment=Session,
           Group=Condition) %>% 
    select(Participant_ID, Group, Treatment, KeyStroke, RR)
  convert_to_csv(index_df, file.path(data_dir, index_file_name))
}

make_physiological_index_df <- function() {
  index_df <<- read.csv(file.path(data_dir, index_file_name))
  filtered_subj_df <<- read.csv(file.path(data_dir, filtered_subj_file_name)) %>% 
    mutate(Session = recode_factor(Session,
                                   'RestingBaseline'='RB',
                                   'BaselineWriting'='ST',
                                   'StressCondition'='PM',
                                   'DualTask'='DT',
                                   'Presentation'='PR'))
  mean_df <<- read.csv(file.path(data_dir, qc2_mean_file_name)) %>%
    mutate(Session = recode_factor(Session,
                                   'RestingBaseline'='RB',
                                   'BaselineWriting'='ST',
                                   'StressCondition'='PM',
                                   'DualTask'='DT',
                                   'Presentation'='PR'))
  
  
  
  for (signal in physiological_signal_list) {
    for (subj in levels(mean_df$Subject)) {
      for (session in levels(mean_df$Session)) {
        ## If the mean signal is missing or NA. 
        ## We will check if we filtered out that signal or not
        if (is_null(mean_df[mean_df$Subject==subj & mean_df$Session==session, signal])) {
          
          ## If the signal is not in filtered_subj_df, it means it was not there at raw dataset
          if (is_null(filtered_subj_df[filtered_subj_df$Subject==subj & 
                                       filtered_subj_df$Session==session, 'Signal'])) {
            status <- status_missing
            
          ## If the signal is not in filtered_subj_df, it means we discarded it
          } else if (filtered_subj_df[filtered_subj_df$Subject==subj & 
                                      filtered_subj_df$Session==session, 'Signal']==signal) {
            status <- status_discarded
          }
          
        } else {
          status <- status_valid
        }
        
        index_df[index_df$Participant_ID==subj & index_df$Treatment==session, signal] <<- status
      }
    }
  }
  
  # index_col_order <- c('Participant_ID',
  #                      'Group',
  #                      'Treatment',
  #                      'PP',
  #                      'N.EDA', 
  #                      'BR', 
  #                      'HR', 
  #                      'N.HR',
  #                      'KeyStroke',
  #                      'RR')
  index_df <<- rbind(index_df[index_df_row_range,], tibble("Participant_ID" = paste0("TOTAL: ", length(unique(index_df[index_df_row_range,]$Participant_ID))),
                                     "Group"	= "-----", 
                                     "Treatment"	= "----", 
                                     "PP" = sum(index_df$PP == 1), 
                                     "N.EDA"	= sum(index_df$N.EDA == 1), 
                                     "BR" = sum(index_df$BR == 1), 
                                     "HR" = sum(index_df$HR == 1), 
                                     "N.HR" = sum(index_df$N.HR == 1),
                                     "KeyStroke" = sum(index_df$KeyStroke == 1),
                                     "RR" = sum(index_df$RR == 1)
                                     )) 
  convert_to_csv(index_df[, index_col_order], file.path(data_dir, index_file_name))
}

make_index_df <- function() {
  # make_initial_index_df()
  make_physiological_index_df()
}


#-------------------------#
#-------Main Program------#
#-------------------------#
# make_physiological_df()
# make_performance_df()
# make_key_str_df()
# make_rr_df()

make_index_df()




### make_questionnaire_df() ## This is done in questionnaire data analysis




