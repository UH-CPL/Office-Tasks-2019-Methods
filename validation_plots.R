library(tidyverse) 
library(knitr) 
library(grid) 
library(gridExtra) 
#install.packages("devtools") 
#devtools::install_github("kassambara/ggpubr") 
library(ggpubr) 
library(kableExtra) 
library(cowplot) 
library(gsubfn)


# signal_name_list <- c('PP')
signal_name_list <- c('PP', 'N.EDA', 'BR', 'HR', 'N.HR')


## CHANGE THIS!!!!
check_outlier = F


## CHANGE THIS!!!!
hr_data_only <- F
file_name_part_sensor_comparison <- ''
col_names_final <- c("Subject", "Condition", "Session", "PP", "HR", "BR", "D.EDA", "N.EDA", "D.HR", "N.HR")


## CHANGE THIS!!!!
## HR Comparison 
# hr_data_only = T
# file_name_part_sensor_comparison <- '-sensor-comparison'
# col_names_final <- c("Subject", "Condition", "Session", "CovertedTime", "TimeElapsed", "HR", "N.HR")


project_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(project_dir)



data_dir <- 'data'
plots_dir <- 'plots'



filtered_file_name <- 'full_df_second_phase_filtered.csv'

file_name_part_outlier <- ''

sensor_comparicon_col <- c('HR', 'N.HR')
condition_list <- c('BH', 'BL', 'IH', 'IL')


full_df <<- read_csv(file.path(data_dir, filtered_file_name))[, col_names_final]
# full_df$HR <- as.numeric(full_df$HR) 
if (hr_data_only) {
  full_df <<- full_df[complete.cases(full_df), ]
}



# Setting up three (that's right, three!) whole global tibbles. Wow! 
result_df <- tibble() 
t_test_df <- tibble() 
temp_df <- tibble()
session_compare_df <- tibble() 
final_testing_df <- tibble()
plot_list <- list()



isMatch <- function(string_we_have, pattern_we_are_looking_for) { 
  return(grepl(pattern_we_are_looking_for, string_we_have)) 
} 

replace_dots <- function(str) {
  gsubfn(".", list("." = "_", " " = "_"), tolower(str))
  # gsub("\\.", "-", str)
}

save_plot <- function(plot_name, plot, default_width=15, default_height=10) {
  plot_path <- file.path(project_dir, paste0(plot_name, '.png'))
  ggsave(plot_path, plot, width=default_width, height=default_height)
  
  plot_path <- file.path(project_dir, paste0(plot_name, '.pdf'))
  ggsave(plot_path, plot, device=cairo_pdf, width=default_width, height=default_height)
}


# Like "Where's Waldo?" but way less fun. 
find_subjects <- function() { 
  for (subj_name in levels(factor(full_df$Subject))) { 
    df <- full_df %>% 
      filter(Subject == subj_name) 
    
    tryCatch({ 
      condition <- df$Condition[1] 
      df <- df[ , !(names(df) %in% c("Subject", "Condition", "Task"))] 
      
      process_subject(df, subj_name, condition) 
    }, warning = function(w) { 
      message(paste0("Warnings present for subject ", subj_name, ": ", w, " Continuing process anyway.")) 
      flush.console() 
      return() 
    }, error = function(e) { 
      message(paste0("Error present for subject ", subj_name, ": ", e, " ")) 
      flush.console() 
      return() 
    }) 
  } 
} 



# This function takes a subject and computes the proper dataframe. 
# This involves renaming some columns, adding in the subject's condition, etc., then `rbinds()` it to the global `result_df`. 
# NOTE that this does NOT edit the global dataframe result_df, refer to `mess_with_columns` below for that. 
# This is like `mess_with_columns`'s little brother - close, but not quite there yet. 
process_subject <- function(df, subj_name, condition) { 
  # Test to see if the dataframe df actually has some rows in it and it is not just empty (more than five is good enough for me)! 
  if (nrow(df) <= 5) { 
    message(paste0("Dataframe for subject ", subj_name, " is empty. ")) 
    flush.console() 
    return() 
  } 
  
  # Convert each of the Sessions into a factor so R knows there is an order to these! 
  all_sessions <- c("RestingBaseline", "BaselineWriting", "StressCondition", "DualTask", "Presentation") 
  df$Session <- factor(df$Session, levels = all_sessions) 
  
  # We now group by session and find the mean values for ALL of the columns for that subject with the whole fancy thing. Cool, right? 
  temp_df <- df %>% 
    group_by(Session) %>% 
    summarize_all(mean, na.rm = TRUE) %>% 
    mutate(Subject = subj_name, Condition = condition) 
  
  # We now add this subject's mean df on to the global dataframe and move on to the next one. NEXT! 
  result_df <<- rbind(result_df, temp_df)
} 





# This function adjusts the global dataframe, result_df, after all of the subjects' own mean dataframes have been added. 
mess_with_the_columns <- function(signal_name) { 
  
  # We now get rid of columns we don't need any more and adjust the order of our final dataframe, result_df. 
  result_df <<- result_df[, (colnames(result_df) %in% col_names_final)] 
  result_df <<- result_df[col_names_final] 
  
  
  
  ## CHANGE THIS for OUTLIER
  if (check_outlier) {
    if (signal_name %in% sensor_comparicon_col) {
      file_name_part_outlier <<- '-no-outlier'
      for (col in sensor_comparicon_col) {
        new_result_df <- tibble()
  
        for (cond in condition_list) {
          temp_df <<- result_df[result_df$Condition == cond, ]
  
          if (!is_normal(temp_df[[col]])) {
            print_msg(paste0("Removing outliers for ", cond, " for signal ", col, ": ", boxplot.stats(temp_df[[col]])$out))
            temp_df[temp_df[[col]] %in% boxplot.stats(temp_df[[col]])$out, col] <<- NA
  
            if (is_normal(temp_df[[col]])) {
              print_msg(paste0("After removing outliers ", col, " signal for ", cond, " is normal"))
            } else {
              print_msg(paste0("After removing outliers ", col, " signal for ", cond, " is NOT normal!!!!"))
            }
          }
          new_result_df <- rbind(new_result_df, temp_df)
        }
        print('')
        result_df <<- new_result_df
      }
    }
  }
  

  ### print_msg(boxplot.stats(temp_col)$out)
  ### temp_col <- temp_col[!temp_col %in% boxplot.stats(temp_col)$out]
  ### print_msg(paste0("After removing outliers the data is ", check_normality(temp_col)))
} 


# Function for annotating boxplot with n. It is a very secret formula I cannot disclose at this time. 
# ... 
# Just kidding, here it is. Note how fancy and sneaky it is! 
give.n <- function(x) { 
  # print(length(x))
  # print(x)
  return(c(y=-Inf, vjust = -1, label=length(x))) 
} 


## Wow, we made it though plot 1s! Great job! Go take a break. 
## ... 
## Are you back? Ready to work now, slacker? Good! 
## Let's go on to the next round of plots! 
plot2 <- function(testing_df, test_type, col, cond, pdf_file_name, plot_label) { 
  ## This is pretty important, per Professor Pavlidis' request, we note that no matter what, these columns 
  ## need to be logged and applied as a t-test. So we do just that by setting their test-type to "transformed t-test." 
  if (col == "PP" || col == "D.EDA" || col == "N.EDA") {
    test_type <- "tt"
  }
  
  ## Simple mode: activated. 
  label <- figure_out_labels(col, test_type) 
  title <- figure_out_title(col, cond, test_type) 
  
  ## We set up the intended order for displaying the differences. 
  # comparison_levels <- c("WB.RB", "SC.RB", "SC.WB", "DT.RB", "DT.WB", "DT.SC",
  #                        "P.RB", "P.WB", "P.SC", "P.DT")
  
  comparison_levels <- c("WB.RB", "SC.RB", "DT.RB", "P.RB")
  
  ## We like '*' characters in our graphs, so we do this! Yay! 
  ## EDIT: Note that `t_test_df` is the very last table you see generated in the PDF. 
  do_we_have_anything <- t_test_df %>% 
      filter(Measure == col & Condition == cond) 
  
  # Wow, what a fancy line of code, eh! Replacing '.' with '-' has never been easier this way! 
  labels_vec <- gsub(".", "-", comparison_levels, fixed = TRUE)
  if (cond == "IL" || cond == "BL") {
    break_cond <- "RV-RB"
    plot_margin_left <- 1.5
  } else {
    break_cond <- "Stroop-RB"
    plot_margin_left <- 0.5
  }
  labels_vec <- replace(labels_vec, labels_vec=="SC-RB", break_cond)
  labels_vec <- replace(labels_vec, labels_vec=="WB-RB", "ST-RB")
  labels_vec <- replace(labels_vec, labels_vec=="P-RB", "PR-RB")
  
  # If we have symbols for each session difference above, then we can get them all ready! 
  # if (nrow(do_we_have_anything) == length(comparison_levels)) { 
  #   sign <- do_we_have_anything$Significance 
  # }
  sign <- do_we_have_anything$Significance 
  
  
  # Gross! We have to use `gather` to make the dataframe nice, then factor the Comparison values into our intended order. 
  # This part is gross - just figure out how it works. 
  middle_df <- tibble() 
  middle_df <- testing_df %>% 
          gather(Comparison, Value, -Subject, -Condition, -Measurement, 
                 -RestingBaseline, -BaselineWriting, -StressCondition, -DualTask, - Presentation) %>% 
          mutate(Comparison = factor(Comparison, levels = comparison_levels)) %>%
          filter(!is.na(Comparison))
  
  #### FOR CHECKING DATA
  # write.table(testing_df, file = file.path(project_dir, data_dir, "@testing_df.csv"), row.names=F, sep = ',')
  # write.table(middle_df, file = file.path(project_dir, data_dir, "@middle_df.csv"), row.names=F, sep = ',')
  
  # Fun plotting time now! Everything here SHOULD be pretty straight-forward, especially after reading the code for `plot1`... 
  # print(middle_df$Subject)
  
  gg <- ggplot(middle_df, aes(x = Comparison, y = Value)) + 
          geom_boxplot() + 
          labs(title = title, y = label) + 
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
          annotate("text", x=1, y=Inf, label= sign[1], vjust = 1.2, size = 10) +
          annotate("text", x=2, y=Inf, label= sign[2], vjust = 1.2, size = 10) +
          annotate("text", x=3, y=Inf, label= sign[4], vjust = 1.2, size = 10) +
          annotate("text", x=4, y=Inf, label= sign[7], vjust = 1.2, size = 10)
  
  # print(gg) 
  # save_plot(pdf_file_name, gg, base_height = 10, base_width = 12)
  
  gg <- arrangeGrob(gg, top = textGrob(toupper(plot_label),
                                        x = unit(0, "npc"),
                                        y = unit(1, "npc"),
                                        # vjust = 2, 
                                        # hjust = -3,
                                        vjust = 2, 
                                        hjust = -1.5,
                                        just = c("left","top"),
                                        gp = gpar(col="black", fontsize=22, fontface="bold")
                                       ))
  # save_plot(pdf_file_name, gg, base_height = 10, base_width = 12)
  plot_list[[length(plot_list) + 1]] <<- gg
} 

print_msg <- function(msg) {
  print(msg)
  message(msg)
}

is_normal <- function(data) {
  return(shapiro.test(data)$p.value >= 0.05)
}

check_normality <- function(data) {
  if (is_normal(data)) {
    return('normal.')
  }
  return('not normal.')
}


# We finally made it! Now we can finally compute all the tests. 
# Wasn't that exhausting to go through just to get here? 
# Yeah, I know. 
# I had to write it and then re-write it to get it here. 
# I hope you can feel my pain. 
test_time <- function(condition, col, full = TRUE) {
  
  # Set a global variable to determine which test we do: 
  # 't' = t-test 
  # 'tt' = transformed t-test 
  # 'w' = Wilcoxon test 
  test_type <- "t" 
  end_str <- NA 
  
  # Firstly, get rid of missing values. Duh! 
  temp <- result_df[!is.na(result_df[[col]]), ] 
  
  # Ensure that n >= 7 for these conditions, or else we can run any tests on it since there aren't enough subjects. 
  # Note that I compute n in a really fancy, dynamic way that makes sense once you read it. 
  # Why do I do this? Because there is one row for each session, so that means if T0XX has a `PP` value, there is a row for 
  # RestingBaseline, WritingBaseline, ..., and Presentation, making it 5 rows for a single subject. Because we want n >= 7, 
  # we really want to test that there n is greater than the number of rows for that subject, meaning if each subject has 5 rows, 
  # one for each session, n has to be >= 35. 
  # Aren't I smart! 
  n <- length(levels(factor(temp$Session))) * 7 
  
  # Now we can filter by condition and ensuring that we have enough subjects for that condition based on the `n` computed right above. 
  temp <- temp %>% 
    group_by(Condition) %>% 
    # filter(n() >= n) %>% 
    filter(Condition == condition)

  # I guess we didn't have enough subjects for this condition! 
  if (nrow(temp) == 0) { 
    # THIS IS IMPORTANT! 
    # Because this function gets called twice (you will see why later), we don't want to print out the same thing twice! 
    # So, I very GENIUSLY use a variable to dictate when to print something to the console and when to print it to the screen. Ha ha! 
    if (!full) { 
      message(paste0(condition, " has LESS than 7 subjects for ", col, ". Cannot continue with test. ")) 
      flush.console() 
    } else { 
      cat(paste0(condition, " has LESS than 7 subjects for ", col, ". Cannot continue with test. \n-----\n")) 
    } 
  } else { # This means that we have 7 or more subjects, so we can actually do a test now! :) 
    
    # Ensure that we always log for PP and EDAs. 
    # Of course to do that, we have to make sure all values are above 0 and shift up by the minimum value (plus a bit more) if not! 
    # We make this a permanent change! 
    # EDIT: Professor wanted these measurements logged no matter what - regardless of whether it is inherently Normal or not. 
    if (col == "PP" || col == "D.EDA" || col == "N.EDA") { # we don't want to do it for these columns AGAIN, now do we? 
      shift_val <- 0 
      if (min(temp[[col]]) <= 0) { 
        shift_val <- min(temp[[col]]) + 0.001 
      } 
      temp[[col]] <- log(temp[[col]]) + shift_val 
    } 
    
    # We now perform the Shapiro-Wilks test to test if our data is naturally Normal! 
    # EDIT: Here we make a variable `temp_col` that points to the actual dataframe's column (temp[[col]]). We do this so that 
    # `temp_col` acts as a practice dummy in a way - we first log `temp_col` to see if we like the results. If we do, then 
    # we know it is safe and can permanently make the change for `temp[[col]]`. If we log it and hate it, `temp_col` is great 
    # because we never made that change permanently, so we can just continue on with the calculations with the unmodified column. 
    temp_col <- temp[[col]] 
    # print(str(temp_col))
    shapiro_test_results <- shapiro.test(temp_col) 
    # This line below will make much more sense in just a bit... 
    temp_for_plotting <- temp 
    
    
    
    ###############################################################################
    # Dang, it wasn't Normal
    # if (shapiro_test_results$p.value < 0.05) { 
    #   if (!full) { 
    #     print_msg(paste0(col, " is NOT normal (p = ", round(shapiro_test_results$p.value, 4), ") for ", condition, ". Adjusting with `log` now. ")) 
    #     flush.console() 
    #   } 
    #   # Dang, it wasn't Normal, so try correcting with a shift and then ln. 
    #   if (col != "PP" && col != "D.EDA" && col != "N.EDA") { 
    #     shift_val <- 0 
    #     if (min(temp_col) <= 0) { 
    #       shift_val <- min(temp_col) + 0.001 
    #       if (!full) { 
    #         print_msg(paste0(col, " values less than 0, shifted each value up by ", shift_val, "for ", condition, ". ")) 
    #         flush.console() 
    #       } 
    #     } 
    #     temp_col <- log(temp_col) + shift_val 
    #   } 
    #   
    #   shapiro_test_results <- shapiro.test(temp_col) 
    #   
    #   # We preserve the current status of our dataframe by saving it for plotting later without any log adjustments to it. 
    #   # Why? Because I was told to make it like this. 
    #   temp_for_plotting <- temp 
    #   
    #   # Okay, the moment we have all been waiting for: do we have Normal data? 
    #   if (shapiro_test_results$p.value < 0.05) { 
    #     # if (full) { 
    #     #   end_str <- paste0("In the following tests, we applied ln(", col, "). \n\n") 
    #     # } else { 
    #     #   message(paste0("Transformed data for ", col, " is still NOT normal (p = ", round(shapiro_test_results$p.value, 4), 
    #     #                "). Continuing with Wilcoxn test. ")) 
    #     #   flush.console() 
    #     # } 
    #     
    #     # Dang, STILL not normal, so correct with ln anyway (per Professor Pavlidis' request), but this time permanently. 
    #     test_type <- "w" 
    #     # if (col != "PP" && col != "D.EDA" && col != "N.EDA") { 
    #     #   shift_val <- 0 
    #     #   if (min(temp[[col]]) <= 0) { 
    #     #     shift_val <- min(temp[[col]]) + 0.001 
    #     #   } 
    #     #   temp[[col]] <- log(temp[[col]]) + shift_val 
    #     # } 
    #   } else { 
    #     if (!full) { 
    #       print_msg(paste0("Transformed data for ", col, " is now normal (p = ", round(shapiro_test_results$p.value, 4), 
    #                      "). Continuing with t-test. ")) 
    #       flush.console() 
    #     } 
    #     
    #     # We are doing a transformed t-test, so we still have to permanently log our data for this column! 
    #     test_type <- "tt" 
    #     # if (col != "PP" && col != "D.EDA" && col != "N.EDA") { 
    #     #   shift_val <- 0 
    #     #   if (min(temp[[col]]) <= 0) { 
    #     #     shift_val <- min(temp[[col]]) + 0.001 
    #     #   } 
    #     #   temp[[col]] <- log(temp[[col]]) + shift_val 
    #     # } 
    #   } 
    # } else { 
    #   # Wow, it was actually Normal first try? Neat! 
    #   if (!full) { 
    #     print_msg(paste0(col, " is normal (p = ", round(shapiro_test_results$p.value, 4), ") for ", condition, ". Continuing with t-test. ")) 
    #     flush.console() 
    #   } 
    #   test_type <- "t" 
    # } 
    ###############################################################################
    
    
    
    
    ###############################################################################
    if (shapiro_test_results$p.value < 0.05) {
      test_type <- "w"
    } else {
      test_type <- "t"
    }
    ###############################################################################
    
    
    
    

    conditions <- levels(factor(temp$Condition)) 
    for (cond in conditions) { 
      
      # One condition at a time, please. 
      temp_cond <- temp %>% 
        filter(Condition == cond) 
      
      # This is it - the most complicated code segment of the whole script. This is just gross. 
      # I won't even try explaining it here. Just trust that it works or figure it out on your own through 
      # several rounds of printing, experimenting, and messing up before finally getting it. 
      testing_df <- temp_cond %>% 
        gather(Measurement, Value, -Subject, -Condition, -Session) %>% 
        spread(Session, Value) %>% 
        filter(Measurement == col) %>% 
        filter(!is.na(col)) %>% 
        mutate(WB.RB = BaselineWriting - RestingBaseline, 
               SC.RB = StressCondition - RestingBaseline, 
               SC.WB = StressCondition - BaselineWriting, 
               DT.RB = DualTask - RestingBaseline, 
               DT.WB = DualTask - BaselineWriting, 
               DT.SC = DualTask - StressCondition, 
               P.RB = Presentation - RestingBaseline, 
               P.WB = Presentation - BaselineWriting, 
               P.SC = Presentation - StressCondition, 
               P.DT = Presentation - DualTask) 
      # print(cond)
      # print(str(testing_df))
      
      # We set up a tibble, `t_df`, to put our test results into. 
      t_df <- tibble() 
      # If the column is one listed below, per Professor Pavlidis, we just continue with a t-test anyway since the 
      # format of the data inherently doesn't work with a Wilcoxon. We just force it as a t-test because we have all 
      # the power here and can do whatever we want. 
      if (col == "PP" || col == "D.EDA" || col == "N.EDA") { 
        if (!full) { 
          print_msg(paste0(col, " is close enough to normal. Continuing with t-test. ")) 
          flush.console() 
        } 
        test_type = "tt" 
      } 
      
      # This code is for a t-test or a transformed t-test (basically the same thing, but with a log transformation). 
      if (test_type == "t" | test_type == "tt") { 
        
        name <- "t-test" 
        if (test_type == "tt") { 
          name <- "Transformed t-test" 
        } 
        
        # We document the name of the test. 
        test <- "Writing Baseline - Resting Baseline" 
        # We calculate the p-value for this session difference. 
        p <- t.test(testing_df[["WB.RB"]])$p.value 
        # We find the sign of our results (this is the '*' thing we put in our two plots way up above). 
        sign <- interpret_results(p) 
        # Then we add EVERYTHING to that tibble we made earlier to hold it for us (wow, how nice!). 
        t_df <- rbind(t_df, tibble(Difference = "WB.RB", Measure = col, Condition = cond, p = p, 
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(WB.RB))), Significance = sign))
        print(paste("Session:", test, "Test Type:", test_type, "Condition:", cond, "p-value:", p, "Significance:", sign))
        
        # And now just rinse and repeat a million times. 
        test <- "Stress Condition - Resting Baseline" 
        p <- t.test(testing_df[["SC.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "SC.RB", Measure = col, Condition = cond, p = p,                                   
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(SC.RB))), Significance = sign)) 
        print(paste("Session:", test, "Test Type:", test_type, "Condition:", cond, "p-value:", p, "Significance:", sign))
        
        test <- "StressCondition - Writing Baseline" 
        p <- t.test(testing_df[["SC.WB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "SC.WB", Measure = col, Condition = cond, p = p,                                   
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(SC.WB))), Significance = sign)) 
        
        test <- "Dual Task - Resting Baseline" 
        p <- t.test(testing_df[["DT.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "DT.RB", Measure = col, Condition = cond, p = p,                                   
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(DT.RB))), Significance = sign)) 
        print(paste("Session:", test, "Test Type:", test_type, "Condition:", cond, "p-value:", p, "Significance:", sign))
        
        test <- "Dual Task - Writing Baseline" 
        p <- t.test(testing_df[["DT.WB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "DT.WB", Measure = col, Condition = cond, p = p,                                 
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(DT.WB))), Significance = sign)) 
        
        test <- "Dual Task - Stress Condition" 
        p <- t.test(testing_df[["DT.SC"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "DT.SC", Measure = col, Condition = cond, p = p,                                   
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(DT.SC))), Significance = sign)) 
        
        test <- "Presentation - Resting Baseline" 
        p <- t.test(testing_df[["P.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "P.RB", Measure = col, Condition = cond, p = p,                                    
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(P.RB))), Significance = sign)) 
        print(paste("Session:", test, "Test Type:", test_type, "Condition:", cond, "p-value:", p, "Significance:", sign))
        
        test <- "Presentation - Writing Baseline" 
        p <- t.test(testing_df[["P.WB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "P.WB", Measure = col, Condition = cond, p = p,                                    
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(P.WB))), Significance = sign)) 
        
        test <- "Presentation - Stress Condition" 
        p <- t.test(testing_df[["P.SC"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "P.SC", Measure = col, Condition = cond, p = p,                                    
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(P.SC))), Significance = sign)) 
        
        test <- "Presentation - Dual Task" 
        p <- t.test(testing_df[["P.DT"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "P.DT", Measure = col, Condition = cond, p = p,                                     
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(P.DT))), Significance = sign)) 
      
      # Not a t-test or a transformed t-test? Don't worry - we got you covered with our friend, Wilcoxon! 
      } else if (test_type == "w" ) { 

        name <- "Wilcoxon" 
        
        test <- "Writing Baseline - Resting Baseline" 
        p <- wilcox.test(testing_df[["WB.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "WB.RB", Measure = col, Condition = cond, p = p,                                   
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(WB.RB))), Significance = sign))
        print(paste("Session:", test, "Test Type:", test_type, "Condition:", cond, "p-value:", p, "Significance:", sign))
        print(paste("t-test p-value : ", t.test(testing_df[["WB.RB"]])$p.value))
        
        test <- "Stress Condition - Resting Baseline" 
        p <- wilcox.test(testing_df[["SC.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "SC.RB", Measure = col, Condition = cond, p = p,                                   
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(SC.RB))), Significance = sign))
        print(paste("Session:", test, "Test Type:", test_type, "Condition:", cond, "p-value:", p, "Significance:", sign))
        print(paste("t-test p-value : ", t.test(testing_df[["SC.RB"]])$p.value))
        
        test <- "StressCondition - Writing Baseline" 
        p <- wilcox.test(testing_df[["SC.WB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "SC.WB", Measure = col, Condition = cond, p = p,                                   
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(SC.WB))), Significance = sign)) 
        
        test <- "Dual Task - Resting Baseline" 
        p <- wilcox.test(testing_df[["DT.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "DT.RB", Measure = col, Condition = cond, p = p,                                   
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(DT.RB))), Significance = sign))
        print(paste("test_type: ", test_type, cond, test, p, sign))
        print(paste("t-test p-value : ", t.test(testing_df[["DT.RB"]])$p.value))
        
        test <- "Dual Task - Writing Baseline" 
        p <- wilcox.test(testing_df[["DT.WB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "DT.WB", Measure = col, Condition = cond, p = p,                                   
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(DT.WB))), Significance = sign))
        
        test <- "Dual Task - Stress Condition" 
        p <- wilcox.test(testing_df[["DT.SC"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "DT.SC", Measure = col, Condition = cond, p = p,                                   
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(DT.SC))), Significance = sign)) 
        
        test <- "Presentation - Resting Baseline" 
        p <- wilcox.test(testing_df[["P.RB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "P.RB", Measure = col, Condition = cond, p = p,                                   
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(P.RB))), Significance = sign))
        print(paste("Session:", test, "Test Type:", test_type, "Condition:", cond, "p-value:", p, "Significance:", sign))
        print(paste("t-test p-value : ", t.test(testing_df[["P.RB"]])$p.value))
        
        test <- "Presentation - Writing Baseline" 
        p <- wilcox.test(testing_df[["P.WB"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "P.WB", Measure = col, Condition = cond, p = p,                                   
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(P.WB))), Significance = sign)) 
        
        test <- "Presentation - Stress Condition" 
        p <- wilcox.test(testing_df[["P.SC"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "P.SC", Measure = col, Condition = cond, p = p,                                    
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(P.SC))), Significance = sign)) 
        
        test <- "Presentation - Dual Task" 
        p <- wilcox.test(testing_df[["P.DT"]])$p.value 
        sign <- interpret_results(p) 
        t_df <- rbind(t_df, tibble(Difference = "P.DT", Measure = col, Condition = cond, p = p,                                    
                                   Name = test, Test = name, n = nrow(testing_df %>% filter(!is.na(P.DT))), Significance = sign)) 
        
      } 
      
      # We create a list with this tibble we just made, the test test, column, condition, and special end string to print out if we need to 
      # and then we return that, which ends up going towards the code to create the plots! 
      # Pretty genius and excessive, no? 
      if (full) { 
        if (nrow(final_testing_df) > 0) { 
          final_testing_df <<- rbind(final_testing_df, testing_df) 
        } else { 
          final_testing_df <<- testing_df 
        } 
        lst <- list("testing_df" = testing_df, "test_type" = test_type, "col" = col, "cond" = cond, "end_str" = end_str) 
        # We also append this tibble to our big ol' global tibble from the beginning. 
        if (nrow(session_compare_df) > 0) { 
          session_compare_df <<- rbind(session_compare_df, testing_df) 
        } else { 
          session_compare_df <<- testing_df 
        } 
        return(lst) 
      } 
      
      # We also append this smaller tibble to our smaller ol' global tibble from the beginning. This is the one that gets 
      # printed out at the very end. 
      t_test_df <<- rbind(t_test_df, t_df) 
      return(NA) 
      
    } 
  } 
  return(NA) 
} 


# Whew! We made it! Now let's go on to the easier functions that are WAY more self-explanatory! 
interpret_results <- function(p_value) { 
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

# Pretty much the same thing as above but much more verbose and talkative. 
# interpret_results_after <- function(cond, col) { 
#   comparison_levels <- c("WB.RB", "SC.RB", "SC.WB", "DT.RB", "DT.WB", "DT.SC", 
#                          "P.RB", "P.WB", "P.SC", "P.DT") 
#   
#   for (test in comparison_levels) { 
#     do_we_have_anything <- t_test_df %>% 
#       filter(Measure == col & Condition == cond & Difference == test) 
#     
#     if (nrow(do_we_have_anything) == 1) { 
#       p_value <- do_we_have_anything$p 
#       test <- do_we_have_anything$Name 
#       name <- do_we_have_anything$Test 
#       if (p_value > 0.05) { 
#         cat(paste0(test, "\n", name, " p = ", round(p_value, 4), " > 0.05\n\n")) 
#       } else if (p_value <= 0.001) { 
#         cat(paste0(test, "\n", name, " p = ", round(p_value, 4), " < 0.001  ***\n\n")) 
#       } else if (p_value <= 0.01) { 
#         cat(paste0(test, "\n", name, " p = ", round(p_value, 4), " < 0.01  **\n\n")) 
#       } else if (p_value <= 0.05) { 
#         cat(paste0(test, "\n", name, " p = ", round(p_value, 4), " < 0.05  *\n\n")) 
#       } 
#     } 
#   } 
# } 

# You should be used to this by now... 
figure_out_labels <- function(col_name, test_type) { 
  if (isMatch(col_name, 'PP')) { 
    return(expression(Delta~"ln("~bar("PP")~paste('[',''^'o','C',''^2,'])')))
  } else if (col_name=='HR') {
    # if (test_type == "tt") {
    #   return(expression(Delta~"ln("~bar("Heart Rate")~') [BPM]'))
    # }
    return(expression(Delta~bar('Chest HR')~' [BPM]'))
  } else if (col_name=='N.HR') {
    # if (test_type == "tt") {
    #   return(expression(Delta~"ln("~bar("Heart Rate")~') [BPM]'))
    # }
    return(expression(Delta~bar('Wrist HR')~' [BPM]'))
  } else if (isMatch(col_name, 'BR')) { 
    # if (test_type == "tt") {
    #   return(expression(Delta~"ln("~bar("Breathing Rate")~') [BPM]'))
    # }
    # return(expression(Delta~bar('Breathing Rate')~' [BPM]')) 
    return(expression(Delta~bar('BR')~' [BPM]')) 
  } else if (isMatch(col_name, 'EDA')) {
    if (test_type == "tt") {
      return(expression(Delta~"ln("~bar("EDA")~paste('[', mu, 'S])')))
    }
    return(expression(Delta~bar('EDA')~' ['~mu~'S]')) 
  } 
  return('Unknown axis.') 
} 

# ... and this... 
figure_out_special_title <- function(difference, condition) { 
  return(paste0(condition, ": ", gsub(".", " - ", difference, fixed = TRUE))) 
} 

# ... and even this. 
figure_out_title <- function(col_name, condition, test_type) { 
  # col_name <- gsub("N.EDA", "EDA", col_name, fixed = TRUE)
  # condition <- gsub("I", "C", condition)
  # if (test_type != "tt") { 
  #   return(paste0(condition, ": ", col_name)) 
  # } 
  # return(paste0(condition, ": ln(", col_name, ") ")) 
  
  if (condition == 'BH') {
    # return('Batch High Group')
    return('Batch High Group | BH')
    # return('Batch High Group (BH)')
  } else if (condition == 'BL') {
    # return('Batch Low Group')
    return('Batch Low Group | BL')
    # return('Batch Low Group (BL)')
  } else if (condition == 'IH') {
    # return('Continual High Group')
    return('Continual High Group | CH')
    # return('Continual High Group (CH)')
  } else if (condition == 'IL') {
    # return('Continual Low Group')
    return('Continual Low Group | CL')
    # return('Continual Low Group (CL)')
  }  
} 



# Wow, it's like every script starts out the exact same and then halfway through, takes a crazy, 
# wild turn for the worse! 
find_subjects()



for (signal_name in signal_name_list) {
  plot_list <- list()
  
  mess_with_the_columns(signal_name) 
  measure_vec <- c(signal_name)
  
  cond <- "BH"
  for (col in measure_vec) {
    lst <- test_time(cond, col, FALSE)
  }
  
  
  for (col in measure_vec) {
    lst <- test_time(cond, col)
    if (!is.na(lst)) {
      plot2(as.tibble(lst[[1]]), as.character(lst[[2]]), as.character(lst[[3]]), as.character(lst[[4]]), 'BH_boxplot.pdf', 'A')
      # interpret_results_after(cond, col)
      # plot.new()
    }
  }
  
  
  
  
  cond <- "BL"
  for (col in measure_vec) {
    lst <- test_time(cond, col, FALSE)
  }
  
  
  for (col in measure_vec) {
    lst <- test_time(cond, col)
    if (!is.na(lst)) {
      plot2(as.tibble(lst[[1]]), as.character(lst[[2]]), as.character(lst[[3]]), as.character(lst[[4]]), 'BL_boxplot.pdf', 'B')
      # interpret_results_after(cond, col)
      # plot.new()
    }
  }
  
  
  
  
  
  cond <- "IH"
  
  ## THIS IS EXTREMELY IMPORTANT TO UNDERSTANDING THIS SCRIPT:
  # So as you might know from looking at the end-PDF from this script, we need to run the plots BEFORE we print out the test results.
  # BUT, we can't print out the plots until we figure out the test results for each different column and plot the '*' character for each.
  # And we can't just figure these out in the plots on the spot since each column / condition could potentially run with a totally different
  # test than the one next to it, so...
  # ...
  # We have to run `test_time` twice: once to calculate the '*'s for us, then one to actually print out the results.
  # I know, it is a pain and surely not very efficient, but it is the only way I could think of meeting these pretty ridiculous requests
  # without making something way too complicated.
  # Make sure this all makes sense to you before continuing, or else this whole script will have made no sense.
  # If you do understand this, then the whole script will suddenly make a whole lot more sense in its very seemingly poort organization.
  for (col in measure_vec) {
    # We compute the test results for each column first.
    lst <- test_time(cond, col, FALSE)
  }
  
  # Okay, great. So we know that `test_time` will return a list when specified to, so now we finally get to see what this list does!
  # cond <- "IH"
  for (col in measure_vec) {
    lst <- test_time(cond, col)
    if (!is.na(lst)) {
      # That's right, it all gets fed into the Plot 2 function, then printed out via the `interpret_results_after` function call below!
      plot2(as.tibble(lst[[1]]), as.character(lst[[2]]), as.character(lst[[3]]), as.character(lst[[4]]), 'IH_boxplot.pdf', 'C')
      if (!is.na(as.character(lst[[5]]))) {
        cat(as.character(lst[[5]]))
        lst[[5]] <- NA
      }
      # interpret_results_after(cond, col)
      # plot.new()
      # Wow, wasn't that just neat-o? And also extremely and incredibly complicated...
    }
  }
  
  # Now we can kind of sit back and relax while we rinse and repeat for each condition.
  cond <- "IL"
  for (col in measure_vec) {
    lst <- test_time(cond, col, FALSE)
  }
  
  for (col in measure_vec) {
    lst <- test_time(cond, col)
    if (!is.na(lst)) {
      plot2(as.tibble(lst[[1]]), as.character(lst[[2]]), as.character(lst[[3]]), as.character(lst[[4]]), 'IL_boxplot.pdf', 'D')
      # interpret_results_after(cond, col)
      # plot.new()
    }
  }
  
  grid_plot <- do.call("grid.arrange", c(plot_list, ncol=2))
  
  # plot_path <- file.path(plots_dir, paste0(tolower(signal_name), '-validation-plot-', format(Sys.Date(), format='%m-%d-%y'), '.pdf'))
  plot_path <- file.path(plots_dir, paste0(replace_dots(tolower(signal_name)), '-validation-plot', file_name_part_sensor_comparison, file_name_part_outlier))
  save_plot(plot_path, grid_plot)
  

  
  # write.table(final_testing_df, file = file.path(data_set_dir, "testing_df.csv"), row.names=F, sep = ',')
  # write.table(result_df, file = file.path(data_set_dir, "result_df_first_phase.csv"), row.names=F, sep = ',')
  # write.table(result_df, file = file.path(project_dir, data_dir, "result_df_first_phase.csv"), row.names=F, sep = ',')
}

