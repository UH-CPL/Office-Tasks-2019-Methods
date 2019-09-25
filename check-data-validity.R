library(readr)


current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)



#---------------------- CHECK full_data.csv
full_df_non_filtered <- read_csv(file.path(current_dir, "data/full_df_non_filtered.csv"))
full_df_qc1_filtered <- read_csv(file.path(current_dir, "data/full_df_first_phase_filtered.csv"))
full_df_qc2_filtered <- read_csv(file.path(current_dir, "data/full_df_second_phase_filtered.csv"))
final_df <- read_csv(file.path(current_dir, "data/final-data-set/Quantitative Data/Physiological Data.csv"))




length(unique(full_df_non_filtered$Subject))
length(unique(full_df_qc1_filtered$Subject))
length(unique(full_df_qc2_filtered$Subject))
length(unique(final_df$Participant_ID))



unique(full_df_non_filtered$Subject) == unique(full_df_qc1_filtered$Subject)
unique(full_df_non_filtered$Subject) == unique(full_df_qc2_filtered$Subject)
unique(full_df_non_filtered$Subject) == unique(final_df$Participant_ID)
#-------------------------------------------------------#
#---------------- MAKE SURE THIS IS  63 ----------------#
#-------------------------------------------------------#


 

#---------------------- EXPORT to csv
# good_subj_list <- list('T003', 'T005', 'T009', 'T011', 'T016', 'T019', 'T021', 'T031',
#                        'T032', 'T035', 'T037', 'T046', 'T047', 'T051', 'T061', 'T063',
#                        'T064', 'T065', 'T066', 'T068', 'T077', 'T078', 'T079', 'T082',
#                        'T083', 'T084', 'T085', 'T091', 'T092', 'T093', 'T094', 'T096', 
#                        'T098', 'T099', 'T106', 'T112', 'T113', 'T114', 'T121', 'T122', 
#                        'T124', 'T126', 'T128', 'T130', 'T132', 'T138', 'T139', 'T141', 
#                        'T144', 'T145', 'T151', 'T152', 'T154', 'T156', 'T157', 'T162', 
#                        'T166', 'T172', 'T173', 'T174', 'T175', 'T176', 'T178')
# 
# good_subj_df <- t(as.data.frame(good_subj_list))
# write.table(good_subj_df, file = "@Datasets/subj_good_df.csv", col.names=c("Subject"), row.names=F)






