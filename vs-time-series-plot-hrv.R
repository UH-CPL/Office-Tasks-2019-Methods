#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(readr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr) 
library(directlabels)
library(gsubfn)
library(scales)




#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

source('common-functions.R')

data_dir <- 'data'
plots_dir <- 'plots'

non_filtered_file_name <- 'rr_df_filtered_qc0.csv'
filtered_file_name <- 'rr_df_filtered_qc1.csv'

col_list <- c('Subject', 'Group', 'Session', 'TreatmentTime', 'RR')

######################
##       ****       ##
######################
axis_type <- 'normal'
y_axis_label <- 'RR [ms]'


######################
##       ****       ##
######################
# axis_type <- 'log-based'
# y_axis_label <- bquote(paste('log'[10], '(RR [ms])'))
# y_axis_threshold <- 0

plot_list <- list()


session_atr <- 'all-session'
session_list <- c('RB', 'ST', 'PM', 'PR', 'DT')
# session_list <- c('RB')





#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
print_msg <- function(df) {
  print(df)
  message(df)
}

save_plot <- function(plot_name, plot) {
  plot_path <- file.path(current_dir, paste0(plot_name, '.png'))
  ggsave(plot_path, plot, width=10, height=10)
  
  plot_path <- file.path(current_dir, paste0(plot_name, '.pdf'))
  ggsave(plot_path, plot, device=cairo_pdf, width=10, height=10)
}

isMatch <- function(str, pattern) { 
  return(grepl(pattern, str)) 
} 

#---- Removing NA values and extracting data for only the valid sessions ----#
extract_session_data <- function(df) {
  # return(df %>% filter(Session %in% session_list))
  return(df[complete.cases(df), ] %>% filter(Session %in% session_list))
}

get_total_subj_no <- function(df) {
  # df <- extract_session_data(df)
  return(length(levels(factor(df$Subject))))
}

get_subj_no_label <- function(subj_no) {
  return(paste("n =", subj_no))
}

replace_dots <- function(str) {
  gsubfn(".", list("." = "_", " " = "_"), tolower(str))
  # gsub("\\.", "-", str)
}

#---- Add one space if it finds any CamelCase ----#
# get_session_name <- function(session_name) {
#   if (session_name == 'BaselineWriting') {
#     return('Single Task')
#   } else if (session_name == 'StressCondition') {
#     return('Priming')
#   }
#   return(gsub("([a-z])([A-Z])", "\\1 \\2", session_name))
# }
# 
# get_abbr_session_name <- function(session_name) {
#   if (session_name=='RestingBaseline') {
#     return('RB')
#   } else if (session_name=='BaselineWriting') {
#     return('ST')
#   } else if (session_name == 'StressCondition') {
#     return('PM')
#   } else if (session_name == 'DualTask') {
#     return('DT')
#   } else if (session_name == 'Presentation') {
#     return('PR')
#   }
#   return(gsub("([a-z])([A-Z])", "\\1 \\2", session_name))
# }

read_data <- function() {
  raw_df <<- read.csv(file.path(data_dir, non_filtered_file_name))[, col_list]
  filtered_df <<- read.csv(file.path(data_dir, filtered_file_name))[, col_list]
  
  # print(str(raw_df))
  # print(str(filtered_df))
}

generate_time_series_plot <- function() {
  
  #####################################################
  #                    *********                      #
  #####################################################
  raw_df <- extract_session_data(raw_df)
  filtered_df <- extract_session_data(filtered_df)
  #####################################################
  
  
  #---- We do not want to calculate the max of x for all sessions again & again ----#
  non_dual_raw_df <- raw_df %>% filter(Session != 'DT')
  max_x <- max(non_dual_raw_df$TreatmentTime)
  
  
  for(sess_idx in 1 : length(session_list)) {
    session_name <- session_list[sess_idx]
    
    session_raw_df <- raw_df %>% 
      filter(Session == session_name) 
    # %>% 
    #   slice(1:10)
    
    session_filtered_df <- filtered_df %>% 
      filter(Session == session_name) 
    # %>% 
    #   slice(1:10)
    
    
    if (session_name == 'DT') {
      max_x <- max(session_raw_df$TreatmentTime)
    }
    # print(session_name)
    # print(max_x)
    
    
    x_axis_label <- ''
    
    #---- PUTTING X-LABEL FOR THE LAST PLOT ONLY ----#
    if (sess_idx == length(session_list)) {
      x_axis_label <- 'Time [s]'
    }
    
    
    if (nrow(session_raw_df) != 0) {
      raw_data_plot <- ggplot(data=session_raw_df,
                              aes(x=TreatmentTime, y=RR, group=Subject)) +
        geom_linerange(ymin=0, ymax=session_raw_df$RR, alpha = 0.3) +
        # geom_line(alpha = 0.7) +
        annotate("text",
                 x=Inf,
                 y=Inf,
                 hjust=1.2,
                 vjust=1.5,
                 size=4.5,
                 label=get_subj_no_label(get_total_subj_no(session_raw_df)),
                 fontface = 'italic')
      
      if (session_name != 'DT') {
        raw_data_plot <- raw_data_plot + 
          theme_bw() +
          theme(axis.line = element_line(colour = "black"))
      }
      
      raw_data_plot <- raw_data_plot + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y.right=element_blank(),
              axis.ticks.y.right=element_blank(),
              plot.title = element_text(hjust = 0.5),
              text=element_text(size=14),
              axis.text.x=element_text(size=16),
              axis.text.y=element_text(size=12),
              legend.position='none'
        ) +
        xlim(0, max_x) +
        xlab(x_axis_label) +
        ylab(y_axis_label)
      
      
      if (axis_type=='log-based') {
        raw_data_plot <- raw_data_plot + 
          scale_y_continuous(trans='log10',
                             limits=c(
                               min(raw_df$RR) + y_axis_threshold, 
                               max(raw_df$RR)
                             ),
                             breaks=c(0.01, 1, 5, 20)) ## ****
      } else {
        raw_data_plot <- raw_data_plot + 
          scale_y_continuous(limits=c(min(raw_df$RR),
                                      max(raw_df$RR)))
        
      }
      
      if (sess_idx==1) {
        raw_data_plot <- raw_data_plot + 
          # ggtitle('Original RR sets')
          ggtitle('RR matched to Chest HR sets')
      }
      
      #---- SAVING THE PLOTS IN A LIST TO MAKE A GRID GRAPH ----#
      plot_list[[length(plot_list)+1]] <- raw_data_plot
    }
    
    if (nrow(session_filtered_df) != 0) {
      filtered_data_plot <- ggplot(data=session_filtered_df,
                                   aes(x=TreatmentTime, y=RR, group=Subject)) +
        geom_linerange(ymin=0, ymax=session_filtered_df$RR, alpha = 0.3) +
        # geom_line(alpha = 0.7) +
        annotate("text", 
                 x=Inf,
                 y=Inf,
                 hjust=1.2,
                 vjust=1.5,
                 size=4.5,
                 label=get_subj_no_label(get_total_subj_no(session_filtered_df)),
                 fontface = 'italic')
      
      if (session_name != 'DT') {
        filtered_data_plot <- filtered_data_plot + 
          theme_bw() +
          theme(axis.line = element_line(colour = "black"))
      }
      
      filtered_data_plot <- filtered_data_plot + 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.text.y.right=element_blank(),
              axis.ticks.y.right=element_blank(),
              axis.title.y.right = element_text(angle=0, vjust=0.5, face='bold'),
              plot.title = element_text(hjust=0.5),
              text=element_text(size=14),
              axis.text.x=element_text(size=16),
              axis.text.y=element_text(size=12),
              legend.position='none'
        ) +
        xlim(0, max_x) +
        xlab(x_axis_label) +
        ylab('')
      
      if (axis_type=='log-based') {
        filtered_data_plot <- filtered_data_plot + 
          scale_y_continuous(trans='log10',
                             limits=c(
                               min(raw_df$RR) + y_axis_threshold, 
                               max(raw_df$RR)
                             ),
                             breaks=c(0.01, 1, 5, 20), ## ****
                             sec.axis=sec_axis(~.+1, name=session_name))
      } else {
        filtered_data_plot <- filtered_data_plot + 
          scale_y_continuous(limits=c(min(raw_df$RR),  ## filtered_df[col_name]
                                      max(raw_df$RR)),  ## filtered_df[col_name]
                             # position='right',
                             sec.axis=sec_axis(~.+1, name=session_name))
      }
      
      if (sess_idx==1) {
        filtered_data_plot <- filtered_data_plot + 
          ggtitle('QC1 RR sets')
      }
    
      #---- SAVING THE PLOTS IN A LIST TO MAKE A GRID GRAPH ----#
      plot_list[[length(plot_list)+1]] <- filtered_data_plot
    }
    
  }
  
  
  grid_plot <- do.call('grid.arrange', c(plot_list, ncol=2))
  grid_plot <- grid.arrange(grid_plot)
  
  # print(grid_plot)
  
  plot_path <- file.path(plots_dir, 'rr-time-series')
  save_plot(plot_path, grid_plot)
}




#-------------------------#
#-------Main Program------#
#-------------------------#
read_data()
generate_time_series_plot()





