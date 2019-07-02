#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(readr)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(grid)
library(gridExtra)
library(directlabels)
library(scales)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

source('common-functions.R')

log_dir <- file.path(dirname(current_dir), 'log-files')
log.file <- file.path(log_dir, paste0('hrv-missing-subj-', format(Sys.Date(), format='%m-%d-%y'), '.txt'))
file.create(log.file)


data_dir <- 'data'
plots_dir <- 'plots'

filtered_subj_file_name <- 'filtered_subject_list.csv'
qc2_full_df_file_name <- 'full_df_second_phase_filtered.csv'
hrv_file_name <- 'hrv.csv'
rr_file_name <- 'rr.csv'

rr_df <- tibble()

plot_list <- list()


comparison_levels <- c( "WB.RB", "SC.RB", "DT.RB", "P.RB")
session_list <- c('RestingBaseline', 'BaselineWriting', 'StressCondition', 'Presentation', 'DualTask')
# session_list <- c('StressCondition')



#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
print_msg <- function(df) {
  print(df)
  message(df)
}

save_plot <- function(plot_name, plot, width=14, height=10) {
  plot_path <- file.path(current_dir, paste0(plot_name, '.png'))
  ggsave(plot_path, plot, width=width, height=height)
  
  plot_path <- file.path(current_dir, paste0(plot_name, '.pdf'))
  ggsave(plot_path, plot, device=cairo_pdf, width=width, height=height)
}

convert_to_csv <- function(df, file_name) {
  write.table(df, file = file.path(file_name), row.names=F, sep = ',')
}

figure_out_title <- function(group) { 
  if (group == 'BH') {
    return('Batch High Group | BH')
  } else if (group == 'BL') {
    return('Batch Low Group | BL')
  } else if (group == 'IH') {
    return('Continual High Group | CH')
  } else if (group == 'IL') {
    return('Continual Low Group | CL')
  }  
} 

figure_out_labels <- function(col_name) { 
  if (isMatch(col_name, 'rr')) { 
    # return(expression(Delta~bar('RR')~' [ms]'))
    return(expression(Delta~'NN [ms]'))
  } else if (col_name=='rmssd') {
    return(expression(Delta~'RMSSD [ms]'))
  } 
  return('Unknown axis.') 
} 

get_significance <- function(p_value) { 
  if (p_value > 0.05) { 
    return(" ") 
  } else if (p_value <= 0.001) { 
    return("***") 
  } else if (p_value <= 0.01) { 
    return("**") 
  } else if (p_value <= 0.05) { 
    return("*") 
  } 
} 

give.n <- function(x) { 
  return(c(y=-Inf, vjust = -1, label=length(x))) 
} 

isMatch <- function(string_we_have, pattern_we_are_looking_for) { 
  return(grepl(pattern_we_are_looking_for, string_we_have)) 
}

is_normal <- function(data) {
  return(shapiro.test(data)$p.value >= 0.05)
}

get_normality <- function(data) {
  # data=data[complete.cases(data), ]
  if (is_normal(data)) {
    return('normal')
  }
  return('non-normal')
}

draw_validation_plot <- function(df, signal) {
  
  plot_list <<- list()
  significance_df <- data.frame()
  
  for (group in c('BH', 'BL', 'IH', 'IL')) {
    
    temp_df <- df %>% 
      filter(Group==group)
    
    if (group == "IL" || group == "BL") {
      break_cond <- "RV-RB"
      plot_margin_left <- 1.5
    } else {
      break_cond <- "Stroop-RB"
      plot_margin_left <- 0.5
    }
    
    labels_vec <- gsub(".", "-", comparison_levels, fixed = TRUE)
    # print(labels_vec)
    # print(levels(factor(temp_df$Comparison)))
    labels_vec <- replace(labels_vec, labels_vec=="SC-RB", break_cond)
    labels_vec <- replace(labels_vec, labels_vec=="WB-RB", "ST-RB")
    labels_vec <- replace(labels_vec, labels_vec=="P-RB", "PR-RB")
    
    for (comparison in comparison_levels) {
      temp_comparison_df <- temp_df %>% 
        filter(Comparison==comparison)
      
      if (is_normal(temp_comparison_df$Value)) {
        test_type <- 't-test'
        test <- t.test(temp_comparison_df$Value)
      } else {
        test_type <- 'wilcoxon'
        test <- wilcox.test(temp_comparison_df$Value)
      }
      temp_significance_df <- data.frame( "Group" = group, 
                                          "Treatment" = labels_vec[which(comparison == comparison_levels)],
                                          "Test Type" = test_type,
                                          "Significance" = get_significance(test$p.value),
                                          "Subj_No" = nrow(temp_comparison_df))
      significance_df <- rbind(significance_df, temp_significance_df)
      
      # print('')
      # print(comparison)
      # print(get_normality(temp_comparison_df$Value))
      # print(test_type)
      # print('----------------------------------------------------')
    }
    
    group_significance_df <- significance_df %>% 
      filter(Group==group)
    
    gg <- ggplot(temp_df, aes(x = Comparison, y = Value)) + 
      geom_boxplot() + 
      labs(title = figure_out_title(group), y = figure_out_labels(signal)) +
      theme_bw(base_size = 18) +
      theme(axis.title.x = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x=element_text(size=16, face='bold'),
            plot.margin = unit(c(0.5, 0.5, 0.5, plot_margin_left), "lines"),  ##top, right, bottom, left
            axis.line = element_line(colour = "black")) +
      geom_hline(yintercept=0, linetype="dashed", color = "red", alpha = 0.6, size=1) +
      scale_x_discrete(labels=labels_vec) +
      stat_summary(fun.y = mean, color = "darkred", geom = "point", shape = 3, size = 4, show_guide = FALSE) +
      stat_summary(fun.data = give.n, geom = "text", size = 6) +
      scale_y_continuous(expand = c(0.15, 0, 0.15, 0)) +
      annotate("text", x=1, y=Inf, label= group_significance_df$Significance[1], vjust = 1.2, size = 10) +
      annotate("text", x=2, y=Inf, label= group_significance_df$Significance[2], vjust = 1.2, size = 10) +
      annotate("text", x=3, y=Inf, label= group_significance_df$Significance[3], vjust = 1.2, size = 10) +
      annotate("text", x=4, y=Inf, label= group_significance_df$Significance[4], vjust = 1.2, size = 10)
    
    plot_list[[length(plot_list) + 1]] <<- gg
  }
  
  grid_plot <- do.call("grid.arrange", c(plot_list, ncol=2))
  plot_path <- file.path(plots_dir, paste0(signal, '-validation-plot'))
  save_plot(plot_path, grid_plot)
  
  # convert_to_csv(significance_df, file.path(data_dir, 'rr_significance.csv'))
}

plot_rmssd <- function() {
  hrv_df <- read.csv(file.path(data_dir, hrv_file_name)) %>% 
    rename(Group=Treatment)
  # print(str(hrv_df))
  
  hrv_df <- hrv_df %>% 
    spread(Session, RMSSD) %>% 
    mutate(WB.RB = ST - RB, 
           SC.RB = PM - RB, 
           DT.RB = DT - RB, 
           P.RB = PR - RB) %>% 
    gather(Comparison, Value, -Participant, -Group,
           -RB, -ST, -PM, -DT, - PR) %>%
    mutate(Comparison = factor(Comparison, levels = comparison_levels)) %>% 
    filter(!is.na(Comparison))
  
  
  # print(str(hrv_df))
  # convert_to_csv(hrv_df, file.path(data_dir, 'hrv_validation_plot_df.csv'))
  
  
  
  
  ########################################################
  draw_validation_plot(hrv_df, 'rmssd')
  ######################################################## 
  
  
  ########################################################
  # for (group in c('BH', 'BL', 'IH', 'IL')) {
  #   
  #   temp_hrv_df <- hrv_df %>% 
  #     filter(Group==group)
  #   
  #   if (group == "IL" || group == "BL") {
  #     break_cond <- "RV-RB"
  #     plot_margin_left <- 1.5
  #   } else {
  #     break_cond <- "Stroop-RB"
  #     plot_margin_left <- 0.5
  #   }
  #   
  #   labels_vec <- gsub(".", "-", comparison_levels, fixed = TRUE)
  #   # print(levels(factor(hrv_df$Comparison)))
  #   labels_vec <- replace(labels_vec, labels_vec=="SC-RB", break_cond)
  #   labels_vec <- replace(labels_vec, labels_vec=="WB-RB", "ST-RB")
  #   labels_vec <- replace(labels_vec, labels_vec=="P-RB", "PR-RB")
  #   
  #   sign <- c()
  #   for (comparison in comparison_levels) {
  #     temp_comparison_df <- temp_hrv_df %>% 
  #       filter(Comparison==comparison)
  #     print(get_significance(t.test(temp_comparison_df$Value)$p.value))
  #     sign <- c(sign, get_significance(t.test(temp_comparison_df$Value)$p.value))
  #   }
  #   
  #   gg <- ggplot(temp_hrv_df, aes(x = Comparison, y = Value)) + 
  #     geom_boxplot() + 
  #     labs(title = figure_out_title(group), y = expression(Delta~'RMSSD [ms]')) +
  #       theme_bw(base_size = 18) +
  #       theme(axis.title.x = element_blank(),
  #             panel.grid.major = element_blank(),
  #             panel.grid.minor = element_blank(),
  #             axis.text.x=element_text(size=16, face='bold'),
  #             plot.margin = unit(c(0.5, 0.5, 0.5, plot_margin_left), "lines"),  ##top, right, bottom, left
  #             axis.line = element_line(colour = "black")) +
  #       geom_hline(yintercept=0, linetype="dashed", color = "red", alpha = 0.6, size=1) +
  #       scale_x_discrete(labels=labels_vec) +
  #       stat_summary(fun.y = mean, color = "darkred", geom = "point", shape = 3, size = 4, show_guide = FALSE) +
  #       stat_summary(fun.data = give.n, geom = "text", size = 6) +
  #       scale_y_continuous(expand = c(0.15, 0, 0.15, 0)) +
  #       annotate("text", x=1, y=Inf, label= sign[1], vjust = 1.2, size = 10) +
  #       annotate("text", x=2, y=Inf, label= sign[2], vjust = 1.2, size = 10) +
  #       annotate("text", x=3, y=Inf, label= sign[3], vjust = 1.2, size = 10) +
  #       annotate("text", x=4, y=Inf, label= sign[4], vjust = 1.2, size = 10)
  #   
  #   plot_list[[length(plot_list) + 1]] <<- gg
  # }
  # 
  # grid_plot <- do.call("grid.arrange", c(plot_list, ncol=2))
  # plot_path <- file.path(plots_dir, 'hrv-validation-plot')
  # save_plot(plot_path, grid_plot)
  ########################################################
  
}

#########################################################
## Discard the data outside of 2SD of the distribution ##
#########################################################
discard_outliers <- function(subj, sess, temp_rr_df) {
  mean <- mean(temp_rr_df$RR)
  sd <- sd(temp_rr_df$RR)
  sd_val <- 2
  
  print(paste('Filtering extremest points: ', subj, ', ', sess))
  # write(paste('Filtering extremest points: ', subj, ', ', sess), file=log.file, append=TRUE)
  # print(paste0('mean: ', mean, ', sd: ', sd))
  
  outliers <- temp_rr_df %>%
    filter(temp_rr_df$RR<mean-sd_val*sd | temp_rr_df$RR>mean+sd_val*sd) %>%
    select(RR) %>%
    unlist()
  
  rr_df[rr_df$Subject==subj &
          rr_df$Session==sess &
          rr_df$RR %in% as.list(outliers),
        'RR'] <<- NA
}

discard_full_session_rr <- function(subj, sess) {
  print(paste('Filtering whole session: ', subj, ', ', sess))
  # write(paste('Filtering whole session: ', subj, ', ', sess), file=log.file, append=TRUE)
  rr_df[rr_df$Subject==subj & rr_df$Session==sess, 'RR'] <<- NA
}

draw_nn_validation_plot <- function() {
  rr_df <<- read.csv(file.path(data_dir, 'rr_df_filtered_qc1.csv'))
  mean_rr_df <- rr_df %>% 
    select(Subject, Group, Session, RR) %>% 
    group_by(Subject, Group, Session) %>% 
    summarize(NN = mean(RR, na.rm = TRUE)) %>% 
    spread(Session, NN) %>% 
    mutate(WB.RB = ST - RB, 
           SC.RB = PM - RB, 
           DT.RB = DT - RB, 
           P.RB = PR - RB) %>% 
    gather(Comparison, Value, -Subject, -Group,
           -RB, -ST, -PM, -DT, - PR) %>% 
    mutate(Comparison = factor(Comparison, levels = comparison_levels)) %>% 
    filter(!is.na(Comparison))
  
  # convert_to_csv(mean_rr_df, file.path(data_dir, 'mean_rr_df_filtered.csv'))
  draw_validation_plot(mean_rr_df, 'rr')
}

clean_invalid_rr <- function() {
  subjects <- levels(factor(qc2_filtered_subj_df$Subject))
  sessions <- levels(factor(qc2_filtered_subj_df$Session))
  
  for (subj in subjects) {
    for (sess in sessions) {
      temp_rr_df <- rr_df %>%
        filter(Subject == subj, Session == sess)
      temp_qc2_filtered_subj_df <- qc2_filtered_subj_df %>% 
        filter(Subject == subj, Session == sess)
      
      if (nrow(temp_qc2_filtered_subj_df)==0 & nrow(temp_rr_df)>0) { 
        discard_full_session_rr(subj, sess)
        # write(paste('Invalid rr data for: ', subj, ', ', sess), file=log.file, append=TRUE)
      }
    }
  }
  
  rr_df <<- rr_df %>%
    group_by(Subject, Group, Session) %>%
    mutate(TreatmentTime=as.integer(Time)-as.integer(head(Time, 1)))
  convert_to_csv(rr_df, file.path(data_dir, 'rr_df_filtered_qc0.csv'))
}

filter_extreme_rr <- function() {
  subjects <- levels(factor(qc2_filtered_subj_df$Subject))
  sessions <- levels(factor(qc2_filtered_subj_df$Session))
  
  for (subj in subjects) {
    for (sess in sessions) {
      temp_rr_df <- rr_df %>%
        filter(Subject == subj, Session == sess)
      temp_qc2_filtered_subj_df <- qc2_filtered_subj_df %>% 
        filter(Subject == subj, Session == sess)
      
      if (nrow(temp_qc2_filtered_subj_df)>0 & nrow(temp_rr_df)==0) { 
        write(paste('Missing rr data for: ', subj, ', ', sess), file=log.file, append=TRUE)
      } else if (nrow(temp_qc2_filtered_subj_df)==0 & nrow(temp_rr_df)>0) { 
        # discard_full_session_rr(subj, sess)
        write(paste('Invalid rr data for: ', subj, ', ', sess), file=log.file, append=TRUE)
      } else {
        discard_outliers(subj, sess, temp_rr_df)
      }
    }
  }
  
  # rr_df <<- rr_df %>%
  #   group_by(Subject, Group, Session) %>%
  #   mutate(TreatmentTime=as.integer(Time)-as.integer(head(Time, 1)))
  convert_to_csv(rr_df, file.path(data_dir, 'rr_df_filtered_qc1.csv'))
}



plot_nn <- function() {
  ##########################################################################
  # subjects <- levels(factor(qc2_filtered_subj_df$Subject))
  # sessions <- levels(factor(qc2_filtered_subj_df$Session))
  # 
  # for (subj in subjects) {
  #   for (sess in sessions) {
  #     temp_rr_df <- rr_df %>%
  #       filter(Subject == subj, Session == sess)
  #     temp_qc2_filtered_subj_df <- qc2_filtered_subj_df %>% 
  #       filter(Subject == subj, Session == sess)
  #     
  #     if (nrow(temp_qc2_filtered_subj_df)>0 & nrow(temp_rr_df)==0) { 
  #       write(paste('Missing rr data for: ', subj, ', ', sess), file=log.file, append=TRUE)
  #     } else if (nrow(temp_qc2_filtered_subj_df)==0 & nrow(temp_rr_df)>0) { 
  #       discard_full_session_rr(subj, sess)
  #       write(paste('Invalid rr data for: ', subj, ', ', sess), file=log.file, append=TRUE)
  #     } else {
  #       discard_outliers(subj, sess, temp_rr_df)
  #     }
  #   }
  # }
  # convert_to_csv(rr_df, file.path(data_dir, 'rr_df_filtered.csv'))
  ##########################################################################
  
  
  clean_invalid_rr()
  filter_extreme_rr()
  draw_nn_validation_plot()
}



read_data <- function() {
  rr_df <<- read.csv(file.path(data_dir, rr_file_name)) %>%
    # rename(Group=Treatment)
    rename(Group=Treatment,
           Subject=Participant)
  
  # print(str(rr_df))
  
  filtered_subj_df <<- read.csv(file.path(data_dir, filtered_subj_file_name)) %>%
    mutate(Session = recode(Session,
                            'RestingBaseline' = 'RB',
                            'BaselineWriting' = 'ST',
                            'StressCondition' = 'PM',
                            'DualTask' = 'DT',
                            'Presentation' = 'PR')) %>%
    filter(Signal=='HR')
  
  qc2_filtered_subj_df <<- read.csv(file.path(data_dir, qc2_full_df_file_name)) %>% 
    select(Subject, Session, HR) %>% 
    mutate(Session = recode(Session,
                            'RestingBaseline' = 'RB',
                            'BaselineWriting' = 'ST',
                            'StressCondition' = 'PM',
                            'DualTask' = 'DT',
                            'Presentation' = 'PR')) %>% 
    na.omit()
}


#-------------------------#
#-------Main Program------#
#-------------------------#
read_data()
plot_nn()



# plot_rmssd()



