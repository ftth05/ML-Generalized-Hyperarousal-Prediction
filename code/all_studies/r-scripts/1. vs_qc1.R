#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(ggplot2)
library(grid)
library(gridExtra)
# library(plyr)      ## for func rbind.fill()
# library(readr)
# library(magrittr)  ## for func set_colnames()
# library(gsubfn)    ## for func read.pattern()
# library(zoo)       ## for func na.locf()
library(data.table)
library(dplyr)     ## load it after loading all other libraries


#-------------------------#
#-----GLOBAL VARIABLES----#
#-------------------------#
all_sudies_code_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
project_dir <- dirname(dirname(dirname(all_sudies_code_dir)))
setwd(project_dir)

source(file.path(all_sudies_code_dir, 'us_common_functions.R'))

all_studies_data_dir <- file.path(project_dir, data_dir, all_studies)
all_studies_figure_dir <- file.path(project_dir, figure_dir, all_studies)


# df_v2 <- tibble()
# df_v2_1 <- tibble()
# df_v2_2 <- tibble()
# df_v2_5 <- tibble()
### df_v3 <- tibble()

plot_list <- list()


bins <- 1000





#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_and_mutate <- function(file_name) {
  df <- custom_read_csv(file.path(all_studies_data_dir, file_name))
  
  # df <- df %>% 
  #   dplyr::group_by(Subject, Study_Name) %>% 
  #   dplyr::mutate(Time = 1:n()) %>% 
  #   dplyr::ungroup()
  
  df <- df %>% 
    dplyr::group_by(Subject, Treatment) %>% 
    mutate(Subj_Treatment=paste(Subject, '_', Treatment)) 
  
  df
}


read_data <- function() {
  # df_v2 <<- read_and_mutate('data_2.csv')
  df_v2_1 <<- read_and_mutate('data_2.1.csv')
  df_v2_2 <<- read_and_mutate('data_2.2.csv')
  # df_v2_3 <<- read_and_mutate('data_2.3.csv')
  # df_v2_5 <<- read_and_mutate('data_2.5.csv')
}





get_study_name <- function(study_name) {
  if (study_name %in% c('email_study', 'office_task_study')) { 
    'OFFICE TASKS'
  }
  
  study_name
}


get_plot_title <- function(signal) {
  if (signal == 'Perinasal') { 
    return('Perinasal Perspiration')
  } else if (signal == 'Heart') { 
    return('Heart Rate')
  } else if (signal == 'Breathing') { 
    return('Breathing Rate') 
  }
  
  return('')
}


get_titles <- function(signal) {
  # left_title <- paste0(get_plot_title(signal), ' - Noised Removed')
  # right_title <- paste0(get_plot_title(signal), ' - Log Transformed')
  left_title <- ""
  right_title <- ""
  
  if (signal == 'Perinasal') { 
    left_title <- bquote(paste('QC - PP [',''^'o','C',''^2,']'))
    right_title <- bquote(paste('QC - ln(PP) [ln',''^'o','C',''^2,']'))
    
  } else if (signal == 'Heart') {
    left_title <- 'Raw - HR [BPM]'
    right_title <- 'QC - HR [BPM]'
    
  } else if (signal == 'Breathing') {
    left_title <- 'Raw - BR [BPM]'
    right_title <- 'QC - BR [BPM]'
    
  }
  
  list(left_title=left_title, right_title=right_title)
}


get_colors <- function(signal) {
  # color <- ""
  # fill <- ""
  color <- "darkblue"
  fill <- "lightblue"
  
  if (signal == 'Perinasal') {
    color <- "coral1"
    fill <- "chocolate4"
  }
  
  if (signal == 'Heart') {
    color <- "darkslategray"
    fill <- "darkseagreen2"
  }

  if (signal == 'Breathing') {
    color <- "darkcyan"
    fill <- "aquamarine3"
  }
  
  list(color=color, fill=fill)
}


add_common_themes <- function(plot) {
  plot <- plot +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          
          axis.title.x=element_blank(),
          axis.text.y.right=element_blank(),
          axis.ticks.y.right=element_blank(),
          
          plot.title = element_text(hjust = 0.5),
          text=element_text(size=14),
          
          axis.text.x=element_text(size=12),
          axis.text.y=element_text(size=12)
    )
  
  plot
}


generate_common_density_plot <- function(df, col_name) {
  
  colors <- get_colors(col_name)
  
  plot <- ggplot(data=df, aes_string(x=col_name)) +
    geom_density(color="darkblue", fill="lightblue") 
    # geom_density(color=colors$color, fill=colors$fill) 
    # geom_histogram(bins = bins, color="darkblue", fill="white") 
    
  plot <- add_common_themes(plot)
  
  plot
}


generate_qc1_plot <- function(signal) {
  qc1_col_name <- signal
  if (signal=='Perinasal') {
    qc1_col_name <- 'Perinasal_Log'
  }
  
  #---- Extracting & plotting only for the specific column name and nothing extra ----#
  qc0_df <- df_v2_1[, c("Subject", "Study_Name", signal)] 
  qc1_df <- df_v2_2[, c("Subject", "Study_Name", qc1_col_name)]
  
  titles <- get_titles(signal)
  
  if (signal %in% c('Perinasal', 'Breathing')) {
    studies <- unique(qc0_df$Study_Name)
  } else if (signal == 'Heart') {
    studies <- c('sim2', 'tt1')
  }
  
  for(study_idx in 1 : length(studies)) {
    
    
    study_name <- studies[study_idx]
    
    study_qc0_df <- qc0_df %>% filter(Study_Name == study_name)
    study_qc1_df <- qc1_df %>% filter(Study_Name == study_name)

    
    # x_axis_label <- ''
    # #---- PUTTING X-LABEL FOR THE LAST PLOT ONLY ----#
    # if (study_idx == length(studies)) {
    #   # x_axis_label <- 'Time [s]'
    # }
    
    
    
    # ####################################################################
    # qc0_plot <- add_common_themes(ggplot()) +
    #   xlim(0, 140) +
    #   ylab('PDF')
    # ####################################################################
    
    
    
    
    ####################################################################
    qc0_plot <- generate_common_density_plot(study_qc0_df, signal) +
      ylab('PDF')
    # ylab('Count')
    ####################################################################
    
    
    qc1_plot <- generate_common_density_plot(study_qc1_df, qc1_col_name) + 
      theme(axis.text.y.left=element_blank(),
            axis.ticks.y.left=element_blank(),
            axis.title.y.left=element_blank()
      ) +
      scale_y_continuous(sec.axis=sec_axis(~.+1, name=toupper(get_study_name(study_name))))
    
    if (study_name==studies[1]) {
      qc0_plot <- qc0_plot + ggtitle(titles$left_title)
      qc1_plot <- qc1_plot + ggtitle(titles$right_title)
    }
  
    
    plot_list[[length(plot_list)+1]] <- qc0_plot
    plot_list[[length(plot_list)+1]] <- qc1_plot
  }
  
  #----------------------------------------------------------------#
  #----  MAKING GRID GRAPH WITH ALL THE PLOTS FROM EACH STUDY  ----#
  #----------------------------------------------------------------#
  grid_plot <- do.call('grid.arrange', c(plot_list, ncol=2))
  grid_plot <- grid.arrange(grid_plot)

  #---- SAVING GRID PLOT ----#
  save_plot(paste0(tolower(signal), '-qc1'), grid_plot)
}


generate_qc1_plots <- function() {
  for (signal in c('Perinasal', 'Heart', 'Breathing')) {
  # for (signal in c('Perinasal', 'Breathing')) {
  # for (signal in c('Heart')) {
    generate_qc1_plot(signal)
  }
}






get_subj_no_label <- function(subj_no) {
  return(paste("n =", subj_no))
}


get_total_subj_no <- function(df) {
  # df <- extract_session_data(df)
  return(length(levels(factor(df$Subj_Treatment))))
}


generate_common_time_series_plot <- function(df, col_name) {
  # plot <- ggplot(data=df, aes_string('TimeElapsed', col_name, group=interaction('Subject','Treatment'))) + 
  #   geom_line(alpha = 0.4, lwd=0.2)
  
  plot <- ggplot(data=df, aes_string('TimeElapsed', col_name, group='Subj_Treatment')) +
    geom_line(alpha = 0.8) + 
    annotate("text",
             x=Inf,
             y=Inf,
             hjust=1.2,
             vjust=1.5,
             size=4.5,
             label=get_subj_no_label(get_total_subj_no(df)),
             fontface = 'italic')
  
  plot <- add_common_themes(plot)
  plot
}


generate_time_series_plot <- function(signal) {
  qc1_col_name <- signal
  if (signal=='Perinasal') {
    qc1_col_name <- 'Perinasal_Log'
  }
  
  #---- Extracting & plotting only for the specific column name and nothing extra ----#
  qc0_df <- df_v2_1[, c("Subject", "Study_Name", "TimeElapsed", 'Treatment', 'Subj_Treatment', signal)] 
  qc1_df <- df_v2_2[, c("Subject", "Study_Name", "TimeElapsed", 'Treatment', 'Subj_Treatment', qc1_col_name)]
  
  # View(qc0_df)
  # View(qc1_df)
  
  titles <- get_titles(signal)
  
  if (signal %in% c('Perinasal', 'Breathing')) {
    studies <- unique(qc0_df$Study_Name)
  } else if (signal == 'Heart') {
    studies <- c('sim2', 'tt1')
  }
  
  for(study_idx in 1 : length(studies)) {
    study_name <- studies[study_idx]
    
    study_qc0_df <- qc0_df %>% filter(Study_Name == study_name)
    study_qc1_df <- qc1_df %>% filter(Study_Name == study_name)
    
    
    # x_axis_label <- ''
    # #---- PUTTING X-LABEL FOR THE LAST PLOT ONLY ----#
    # if (study_idx == length(studies)) {
    #   # x_axis_label <- 'Time [s]'
    # }
    
    
    
    # ####################################################################
    # x_max <- if(study_name=='sim1') 1000 else 3000
    # 
    # qc0_plot <- add_common_themes(ggplot()) + xlim(0, x_max)
    # ####################################################################
    
    
    
    ####################################################################
    qc0_plot <-  generate_common_time_series_plot(study_qc0_df, signal) +
      ylim(0, 140) +
      ylab('HR [BPM]')
    ####################################################################
    
    qc1_plot <- generate_common_time_series_plot(study_qc1_df, qc1_col_name) + 
      theme(axis.text.y.left=element_blank(),
            axis.ticks.y.left=element_blank(),
            axis.title.y.left=element_blank()
      ) + 
      # ylim(0, 140) +
      scale_y_continuous(limits=c(0, 140),
                         sec.axis=sec_axis(~.+1, name=toupper(get_study_name(study_name))))
    
    if (study_name==studies[1]) {
      qc0_plot <- qc0_plot + ggtitle(titles$left_title)
      qc1_plot <- qc1_plot + ggtitle(titles$right_title)
    }
    
    
    plot_list[[length(plot_list)+1]] <- qc0_plot
    plot_list[[length(plot_list)+1]] <- qc1_plot
  }
  
  #----------------------------------------------------------------#
  #----  MAKING GRID GRAPH WITH ALL THE PLOTS FROM EACH STUDY  ----#
  #----------------------------------------------------------------#
  grid_plot <- do.call('grid.arrange', c(plot_list, ncol=2))
  grid_plot <- grid.arrange(grid_plot)
  
  #---- SAVING GRID PLOT ----#
  save_plot(paste0(tolower(signal), '-time-series'), grid_plot)
}


generate_time_series_plots <- function() {
  # for (signal in c('Perinasal', 'Heart', 'Breathing')) {
  for (signal in c('Heart')) {
    generate_time_series_plot(signal)
  }
}


#----------------------------#
#------  Main Program  ------#
#----------------------------#
read_data()
generate_qc1_plots()
generate_time_series_plots()



