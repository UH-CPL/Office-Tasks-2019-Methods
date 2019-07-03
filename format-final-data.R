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
performance_file_name <- 'ets_score_final.csv'
rr_file_name <- 'rr_df_filtered_qc1.csv'



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





#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
convert_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ',')
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


#-------------------------#
#-------Main Program------#
#-------------------------#
make_physiological_df()
make_performance_df()
make_rr_df()




### make_questionnaire_df() ## This is done in questionnaire data analysis




