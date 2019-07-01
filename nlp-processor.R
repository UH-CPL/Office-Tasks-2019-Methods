# install.packages(c('devtools','curl'))
# install.packages(c('coreNLP','curl'))
# devtools::install_github("statsmaths/coreNLP")
# install.packages('rJava', dependencies = T)
# install.packages('ngram', dependencies = T)





# current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(getwd()) 

options(java.parameters = "-Xmx4096m") 
library(rJava) 
library(tidyverse) 
# coreNLP::downloadCoreNLP()
library(coreNLP) 
library(XLConnect) 
library(scales) 
library(readr) 
library(grid) 
library(gridExtra) 
library(cowplot) 
library(ngram) 

# RUN THIS ONCE! 
# IF YOU RUN OUT OF MEMORY, THEN RESTART R AND RUN AGAIN 
# IF THIS DOESN'T WORK, INCREASE `mem` TO MORE RAM 
# https://cran.r-project.org/web/packages/coreNLP/coreNLP.pdf 
initCoreNLP(mem = "4g")


raw_data_dir <- 'raw-dataset' 
data_dir <- 'data' 
performance_data_dir <- 'performance-data' 
super_session_pattern <- '^SuperSession$' 
plots <- list() 
result_df <- tibble() # IMPORTANT: global tibble here for scope purposes :) 

good_subj_list <- read.csv(file.path(getwd(), data_dir, "subj_good_df.csv"))$Subject


getAllDirectoryList <- function(directory) { 
  return(list.dirs(path=directory, full.names=F, recursive=F)) 
} 

getMatchedFileNames <- function(directory, file_pattern) { 
  return(list.files(path=directory, pattern=file_pattern, recursive=F)) 
} 

isMatchedString <- function(pattern, str) { 
  return(grepl(pattern, str, perl=TRUE)) 
} 

isMatch <- function(pattern, str) { 
  return(grepl(str, pattern)) 
} 

convert_to_csv <- function(df, file_path) { 
  write.table(df, file = file_path, row.names=F, sep = ',') 
} 


find_subjects <- function(cond) { 
  plots <<- list() 
  
  grp_list <- getAllDirectoryList(raw_data_dir) 
  sapply(grp_list, function(grp_name) {
  # sapply(grp_list[1], function(grp_name) {
    
    grp_dir <- file.path(raw_data_dir, grp_name) 
    subj_list <- getAllDirectoryList(grp_dir) 
    
    sapply(subj_list, function(subj_name) {
    # sapply(subj_list[3], function(subj_name) {
      
      subj_dir <- file.path(grp_dir, subj_name) 
      #session_list <- list.dirs(path=subj_dir, full.names=F, recursive=F) 
      session_list <- getAllDirectoryList(subj_dir) 
      session_list <- session_list[isMatchedString(super_session_pattern, session_list)] 
      
      sapply(session_list, function(session_name) { 
      #sapply(session_list[3], function(session_name) { 
        
        session_dir <- file.path(getwd(), subj_dir, session_name) 
        
        tryCatch({ 
          if(subj_name %in% good_subj_list) {
            doCoreNLP(session_dir, subj_name, cond) 
          }
        }, 
        warning=function(cond) { 
          message('----------------------------------------------------------') 
          message(paste0(grp_name, '-', subj_name, '-', session_name, ': WARNING')) 
          message(paste0(cond, '\n')) 
        }, 
        error=function(cond) { 
          message('----------------------------------------------------------') 
          message(paste0(grp_name, '-', subj_name, '-', session_name, ': ERROR!!')) 
          message(paste0(cond, '\n')) 
        }) 
      }) 
    }) 
  }) 
} 





doCoreNLP <- function(session_dir, subj_name, cond) { 
  # subj_interface_file_pattern <- paste0('.*-', subj_name, '.xlsx') 
  subj_interface_file_pattern <- paste0('^[^~].*-', subj_name, '.xlsx')
  subj_interface_file_name <- getMatchedFileNames(session_dir, subj_interface_file_pattern) 
  
  ## FIGURE OUT THE CONDITION 
  condition <- NA 
  if (isMatch(subj_interface_file_name, "intermittent-high")) { 
    condition <- "IH" 
  } else if (isMatch(subj_interface_file_name, "batch-high")) { 
    condition <- "BH" 
  } else if (isMatch(subj_interface_file_name, "intermittent-low")) { 
    condition <- "IL" 
  } else if (isMatch(subj_interface_file_name, "batch-low")) { 
    condition <- "BL" 
  } 
  
  if (condition != cond) { 
    return() 
  } 
  
  subj_interface_df <- readWorksheet(XLConnect::loadWorkbook( 
    file.path(session_dir, subj_interface_file_name)), sheet = 'Sheet1') 
  
  baseline_essay <- subj_interface_df$Essay.Baseline.Content 
  dual_task_essay <- subj_interface_df$Essay.Dualtask.Content 
  
  ## DO THE NLP 
  baseline_annotated = annotateString(baseline_essay) 
  dual_task_annotated = annotateString(dual_task_essay) 
  
  baseline_words <- wordcount(baseline_essay) 
  dual_task_words <- wordcount(dual_task_essay) 
  
  baseline_sent <- getSentiment(baseline_annotated) 
  baseline_sent$sentiment[baseline_sent$sentiment == "Verypositive"] <- "Positive" 
  baseline_sent$sentiment[baseline_sent$sentiment == "Verynegative"] <- "Negative" 
  
  dual_task_sent <- getSentiment(dual_task_annotated) 
  dual_task_sent$sentiment[dual_task_sent$sentiment == "Verypositive"] <- "Positive" 
  dual_task_sent$sentiment[dual_task_sent$sentiment == "Verynegative"] <- "Negative" 
  
  baseline_sentences <- nrow(baseline_sent) 
  dual_task_sentences <- nrow(dual_task_sent) 
  
  ## FIGURE OUT COUNTS 
  baseline_counts <- baseline_sent %>% 
    group_by(sentiment) %>% 
    summarize(sentiment_count = n()) 
  
  baseline_negative_count <- 0 
  if (nrow(baseline_counts %>% filter(sentiment == "Negative")) == 1) { 
    baseline_negative_count <- (baseline_counts %>% 
                                   filter(sentiment == "Negative"))$sentiment_count 
  } 
  baseline_neutral_count <- 0 
  if (nrow(baseline_counts %>% filter(sentiment == "Neutral")) == 1) { 
    baseline_neutral_count <- (baseline_counts %>% 
                                  filter(sentiment == "Neutral"))$sentiment_count 
  } 
  baseline_positive_count <- 0 
  if (nrow(baseline_counts %>% filter(sentiment == "Positive")) == 1) { 
    baseline_positive_count <- (baseline_counts %>% 
                                   filter(sentiment == "Positive"))$sentiment_count 
  } 
  
  result_df <<- rbind(result_df, tibble("Subject" = subj_name, 
                                        "Condition" = condition, 
                                        "Essay" = "WB", 
                                        "Positive" = baseline_positive_count, 
                                        "Neutral" = baseline_neutral_count, 
                                        "Negative" = baseline_negative_count, 
                                        "CharCount" = nchar(baseline_essay),
                                        "WordCount" = baseline_words,
                                        "SentenceCount" = baseline_sentences)) 
  
  dual_task_counts <- dual_task_sent %>% 
    group_by(sentiment) %>% 
    summarize(sentiment_count = n()) 
  
  dual_task_negative_count <- 0 
  if (nrow(dual_task_counts %>% filter(sentiment == "Negative")) == 1) { 
    dual_task_negative_count <- (dual_task_counts %>% 
      filter(sentiment == "Negative"))$sentiment_count 
  } 
  dual_task_neutral_count <- 0 
  if (nrow(dual_task_counts %>% filter(sentiment == "Neutral")) == 1) { 
    dual_task_neutral_count <- (dual_task_counts %>% 
                                   filter(sentiment == "Neutral"))$sentiment_count 
  } 
  dual_task_positive_count <- 0 
  if (nrow(dual_task_counts %>% filter(sentiment == "Positive")) == 1) { 
    dual_task_positive_count <- (dual_task_counts %>% 
                                   filter(sentiment == "Positive"))$sentiment_count 
  } 
  
  result_df <<- rbind(result_df, tibble("Subject" = subj_name, 
                                        "Condition" = condition, 
                                        "Essay" = "DT", 
                                        "Positive" = dual_task_positive_count, 
                                        "Neutral" = dual_task_neutral_count, 
                                        "Negative" = dual_task_negative_count, 
                                        "CharCount" = nchar(dual_task_essay),
                                        "WordCount" = dual_task_words, 
                                        "SentenceCount" = dual_task_sentences)) 
  
  ## PLOTTING 
  g1 <- baseline_sent %>% 
    mutate(sentiment = factor(sentiment, levels = c("Positive", "Neutral", "Negative"))) %>% 
    ggplot(aes(x = sentiment)) + 
      geom_bar() + 
      labs(title = paste0("WB Essay for ", subj_name), 
           subtitle = paste0("Wordcount: ", baseline_words), 
           x = "Sentiment", 
           y = "Sentence Count") + 
      theme_bw() + 
      theme(panel.background=element_rect(fill = "NA"), 
            panel.grid.minor = element_line(colour = "#E0E0E0"), 
            axis.text.x = element_text(angle = 90, hjust = 1), 
            plot.title = element_text(size=11), 
            plot.subtitle = element_text(size=9, face="italic"), 
            legend.position="none") + 
      scale_x_discrete(drop=FALSE) + 
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) 
  
  g2 <- dual_task_sent %>% 
    mutate(sentiment = factor(sentiment, levels = c("Positive", "Neutral", "Negative"))) %>% 
    ggplot(aes(x = sentiment)) + 
      geom_bar() + 
      labs(title = paste0("DT Essay for ", subj_name), 
           subtitle = paste0("Wordcount: ", dual_task_words), 
           x = "Sentiment") + 
      theme_bw() + 
      theme(panel.background=element_rect(fill = "NA"), 
            panel.grid.minor = element_line(colour = "#E0E0E0"), 
            axis.text.x = element_text(angle = 90, hjust = 1), 
            plot.title = element_text(size=11), 
            plot.subtitle = element_text(size=9, face="italic"), 
            legend.position="none", 
            axis.title.y = element_blank()) + 
      scale_x_discrete(drop=FALSE) + 
      scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) 
  
  g3 <- plot_grid(g1, g2, ncol = 2, align = "v") 
  
  plots[[length(plots)+1]] <<- g3 
  
} 


## IH 
# find_subjects("IH")
# title <- ggdraw() + draw_label("Intermittent Filler (IF) Essays", fontface='bold')
# gg <- plot_grid(plotlist = plots, ncol = 3, align = "v", scale = 0.9)
# gg <- plot_grid(title, gg, ncol = 1, align = "v", rel_heights=c(0.04, 1))
# save_plot("nsf-stress-study-scripts/IH_essays.pdf", gg, ncol = 2, base_height = 20, base_width = 7)
# 
# ## IL
# find_subjects("IL")
# title <- ggdraw() + draw_label("Intermittent Nothing (IN) Essays", fontface='bold')
# gg <- plot_grid(plotlist = plots, ncol = 3, align = "v", scale = 0.9)
# gg <- plot_grid(title, gg, ncol = 1, align = "v", rel_heights=c(0.04, 1))
# save_plot("nsf-stress-study-scripts/IL_essays.pdf", gg, ncol = 2, base_height = 20, base_width = 7)
# 
# ## BH 
# find_subjects("BH") 
# title <- ggdraw() + draw_label("Batch Filler (BF) Essays", fontface='bold')
# gg <- plot_grid(plotlist = plots, ncol = 3, align = "v", scale = 0.9)
# gg <- plot_grid(title, gg, ncol = 1, align = "v", rel_heights=c(0.04, 1))
# save_plot("nsf-stress-study-scripts/BH_essays.pdf", gg, ncol = 2, base_height = 20, base_width = 7)
# 
# ## BL 
# find_subjects("BL")
# title <- ggdraw() + draw_label("Batch Nothing (BN) Essays", fontface='bold')
# gg <- plot_grid(plotlist = plots, ncol = 3, align = "v", scale = 0.9)
# gg <- plot_grid(title, gg, ncol = 1, align = "v", rel_heights=c(0.04, 1))
# save_plot("nsf-stress-study-scripts/BL_essays.pdf", gg, ncol = 2, base_height = 20, base_width = 7)





## WRITE THE GRAND CSV 
find_subjects("IH")
find_subjects("IL")
find_subjects("BH")
find_subjects("BL")
# convert_to_csv(result_df, file.path('nsf-stress-study-scripts/@Datasets/essay_nlp_results.csv')) 
convert_to_csv(result_df, file.path(data_dir, performance_data_dir, 'essay_nlp_results.csv')) 




# ## ESSAY PROMPTS 
# baseline_prompt <- "The best way for a society to prepare its young people for leadership in 
#                     government, industry, or other fields is by instilling in them a sense of '
#                     cooperation, not competition. Do you agree or disagree? Explain your 
#                     reasoning in detail. Note: copying and pasting in the essay window are 
#                     disabled." 
# dual_task_prompt <- "You will have 50 minutes to write. You will be given a topic. you can 
#                     research the topic on the web. Please do not watch videos on this topic, 
#                     but rather use written documents on the subject. Your performance will be 
#                     based on both the content of your essay and the relevance and sufficiency of 
#                     your responses to emails. Paragraph 1: Explain what The Technological 
#                     Singularity is in your own words. Paragraph 2: Explain and discuss the view 
#                     of your first Singularity theorist. Paragraph 3: Explain and discuss the 
#                     view of your second Singularity theorist. Paragraph 4: Explain and discuss 
#                     your own view of the Singularity. Paragraph 5: Conclude your essay 
#                     summarizing the views of the theorists and your own. Note: copying and 
#                     pasting in the essay window are disabled." 
# 
# baseline_annotated = annotateString(baseline_prompt) 
# dual_task_annotated = annotateString(dual_task_prompt) 
# 
# baseline_words <- wordcount(baseline_prompt) 
# dual_task_words <- wordcount(dual_task_prompt) 
# 
# baseline_sent <- getSentiment(baseline_annotated) 
# dual_task_sent <- getSentiment(dual_task_annotated) 
# 
# g1 <- baseline_sent %>% 
#   mutate(sentiment = factor(sentiment, levels = c("Positive", "Neutral", "Negative"))) %>% 
#   ggplot(aes(x = sentiment)) + 
#   geom_bar() + 
#   labs(title = paste0("WB Prompt"), 
#        subtitle = paste0("Wordcount: ", baseline_words), 
#        x = "Sentiment", 
#        y = "Sentence Count") + 
#   theme_bw() + 
#   scale_x_discrete(drop=FALSE) + 
#   theme(panel.background=element_rect(fill = "NA"), 
#         panel.grid.minor = element_line(colour = "#E0E0E0"), 
#         axis.text.x = element_text(angle = 90, hjust = 1), 
#         plot.title = element_text(size=11), 
#         plot.subtitle = element_text(size=9, face="italic")) + 
#   scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) + 
#   scale_color_manual(values = c("Verypositive" = "#3bff00", 
#                                 "Positive" = "green", 
#                                 "Neutral" = "yellow", 
#                                 "Negative" = "red", 
#                                 "Verynegative" = "#910a0a")) 
# 
# g2 <- dual_task_sent %>% 
#   mutate(sentiment = factor(sentiment, levels = c("Positive", "Neutral", "Negative"))) %>% 
#   ggplot(aes(x = sentiment)) + 
#   geom_bar() + 
#   labs(title = paste0("DT Essay Prompt"), 
#        subtitle = paste0("Wordcount: ", dual_task_words), 
#        x = "Sentiment", 
#        y = "Sentence Count") + 
#   theme_bw() + 
#   scale_x_discrete(drop=FALSE) + 
#   theme(panel.background=element_rect(fill = "NA"), 
#         panel.grid.minor = element_line(colour = "#E0E0E0"), 
#         axis.text.x = element_text(angle = 90, hjust = 1), 
#         plot.title = element_text(size=11), 
#         plot.subtitle = element_text(size=9, face="italic"), 
#         legend.position="none") + 
#   scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) + 
#   scale_color_manual(values = c("Verypositive" = "#3bff00", 
#                                 "Positive" = "green", 
#                                 "Neutral" = "yellow", 
#                                 "Negative" = "red", 
#                                 "Verynegative" = "#910a0a")) 
# 
# title <- ggdraw() + draw_label("Sentimental Analysis of Essay Prompts", fontface='bold') 
# g <- plot_grid(g1, g2, ncol = 2, align = "v") 
# g <- plot_grid(title, g, ncol = 1, align = "v", rel_heights=c(0.1, 1)) 
# save_plot("nsf-stress-study-scripts/essay_prompts.pdf", g) 
