#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)
library(plyr)      ## for func rbind.fill()
library(magrittr)  ## for func set_colnames()
library(gsubfn)    ## for func read.pattern()
library(zoo)       ## for func na.locf()
library(epiR)
library(nortest)
library(readr)
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


# studies <- c('sim1', 'sim2', 'tt1', 'office_tasks') #deadline_study
signals <<- c('PP', 'HR', 'BR')
plot_list <<- list()
qq_plot_list <<- list()

qq_y_min <<- 999
qq_y_max <<- -999



#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
read_data <- function() {
  df <- custom_read_csv(file.path(all_studies_data_dir, 'data_2_5_study_combined.csv')) %>% 
    filter(PP_Normalized>-1.5 & PP_Normalized<1.5 & BR_Normalized<35 & HR_Normalized>-40) 
  # %>% 
  #   dplyr::rename(
  #     PP = Perinasal_Log,
  #     HR = Heart,
  #     BR = Breathing,
  #   )
  
  # View(df)
  df
}


generate_data_for_dr_p <- function() {
  df <- read_data() %>% 
    dplyr::select(Study_Name,
           Subject,
           PP_Normalized,
           HR_Normalized,
           BR_Normalized,
           # PP_ecdf,
           # HR_ecdf,
           # BR_ecdf,
           PP_Arousal_ecdf,
           HR_Arousal_ecdf,
           BR_Arousal_ecdf
    ) %>% 
    dplyr::rename(Study = Study_Name,
           Participant_ID = Subject,
           PP_Arousal_Label = PP_Arousal_ecdf,
           HR_Arousal_Label = HR_Arousal_ecdf,
           BR_Arousal_Label = BR_Arousal_ecdf)
  # View(df)
  convert_to_csv(df, file.path(all_studies_data_dir, 'ground_truth_data_2.5.csv'))
}

generate_standardized_data <- function() {
  standardized_df <- tibble()
  
  df <- read_data() %>% 
    dplyr::select(Study_Name,
           Subject,
           
           Perinasal_Log,
           Heart,
           Breathing,
           
           PP_Normalized,
           HR_Normalized,
           BR_Normalized,
           
           PP_Arousal_ecdf,
           HR_Arousal_ecdf,
           BR_Arousal_ecdf
    ) %>% 
    dplyr::rename(
      PP = Perinasal_Log,
      HR = Heart,
      BR = Breathing,
    )
  
  for (study in unique(df$Study_Name)) {
    
    study_df <<- df %>% filter(Study_Name == study) 
    
    for (signal in signals) {
      x_col <- paste0(signal, '_Normalized')
      x_col_sn <- paste0(x_col, '_SN')
      
      study_df <<- study_df %>%
        dplyr::mutate(!!x_col_sn := scale(study_df[[x_col]]),
                      !!paste0(signal, '_SN') := scale(study_df[[signal]]))
        # dplyr::mutate(!!x_col_sn := scale_this(study_df[[x_col]])) # built-in scale() and custom function scale_this() is the same
    }
    
    standardized_df <- rbind.fill(standardized_df, study_df)
  }
  
  convert_to_csv(standardized_df, file.path(all_studies_data_dir, 'data_2_5_standardized.csv'))
}

generate_custom_data <- function() {
  # generate_data_for_dr_p()
  generate_standardized_data()
}










generate_rb_mean_dist <- function() {
  rb_df <- custom_read_csv(file.path(all_studies_data_dir, 'rb_data.csv')) %>% 
    dplyr::mutate(Study_Name=case_when(Study_Name=='deadline_study' ~ paste0('DS_', Treatment), 
                                       TRUE~Study_Name))
  
  print(rb_df$Study_Name)
  
  rb_plot_list <<- list()
  
  for (study in unique(rb_df$Study_Name)) {
    
    rb_plot_df <- rb_df %>% filter(Study_Name == study) 
    
    for (signal in signals) {
      
      print(paste0(study, signal))
      x_col <- paste0(signal, '_RB_Mean')
      rb_plot <- ggplot(rb_plot_df, aes_string(x=x_col)) +
        # geom_density() +
        # geom_histogram(fill='white', alpha=0.1, binwidth=0.001, position='identity') +
        geom_histogram(fill='blue', alpha=0.8, position="identity") +
        xlim(min(rb_df[x_col]), max(rb_df[x_col])) +
        ylim(0, 15)
      
      
      rb_plot <- add_common_themes(rb_plot)
      
      rb_plot <- rb_plot + 
        theme(
          axis.text.x=element_text(size=12),
          axis.title.y=element_text(size=14),
        )
      
      if (study=='sim1') {
        rb_plot <- rb_plot + ggtitle(signal)
      }
      
      
      if (signal=='PP') {
        rb_plot <- rb_plot + ylab(get_study_y_lab(study))
      } else {
        rb_plot <- rb_plot + theme(axis.title.y=element_blank())
      }
      
      if (study=='deadline_study' && signal=='BR') {
        rb_plot <- ggplot() +
          xlim(min(rb_df[x_col]), max(rb_df[x_col]))
        
        rb_plot <- add_common_themes(rb_plot)
      }
      
      rb_plot_list[[length(rb_plot_list)+1]] <<- rb_plot
    }
  }
  
  
  grid_plot <- plot_grid(plotlist=rb_plot_list,
                         ncol=length(signals))
  save_plot('rb_mean_dist', grid_plot, width=20, height=12)
  
}










add_common_themes <- function(plot) {
  plot <- plot +
    scale_fill_manual(values=c('relaxed'='chartreuse3', 
                               'neutral'='gray71', 
                               'stressed'='red')) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          
          axis.title.x=element_blank(),
          
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank(),
          
          # axis.text.y.right=element_blank(),
          # axis.ticks.y.right=element_blank(),
          
          plot.title = element_text(hjust = 0.5),
          text=element_text(size=22),
          
          axis.text.x=element_text(size=18),
          axis.title.y=element_text(size=16),
          
          legend.position='none',
    )
  
  plot
}

# # (X – μ) / σ
scale_this <- function(x) {
  # print(x)
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}


# {'SS': ['sim1', 'sim2'],
#   'SST': ['sim1', 'sim2', 'tt1'],
#   'SSO': ['sim1', 'sim2', 'office_tasks'],
#   'SSTO': ['sim1', 'sim2', 'tt1', 'office_tasks']}
get_study_y_lab <- function(study) {
  print(study)
  if (study=='sim1___sim2') {
    return('SS')
  } else if (study=='sim1___sim2___tt1') {
    return('SST')
  } else if (study=='sim1___sim2___office_tasks') {
    return('SSO')
  } else if (study=='sim1___sim2___tt1___office_tasks') {
    return('SSTO')
  } else if (study=='sim1___sim2___tt1___office_tasks') {
    return('SSTO')
  } else if (study=='office_tasks') {
    return('OT')
  } else if (study=='office_tasks') {
    return('OT')
  } else if (study=='deadline_study') {
    return('DS')
  } else if (study=='DS_Day3_Day4_Min') {
    return('DS_3_4_Min')
  } else if (study=='DS_Day3_Day4_Mean') {
    return('DS_3_4_Mean')
  } else if (study=='DS_Four_Day_Mean') {
    return('DS_4D_Mean')
  } else if (study=='DS_Corresponding_Day_RB') {
    return('D_RB')
  }
  
  toupper(study)
}
















generate_arousal_labeling_plot <- function(df, study) {
  
  study_df <- df %>% 
    filter(Study_Name == study) 

  
  if (test==T) {
    study_df <- study_df %>% 
      slice(1:100)
    }
  
  for (signal in signals) {
    
    ##########################################################################################################
    # x_col <- if(data_type=='sn') paste0(signal, '_SN') else signal
    # col_name <- if(data_type=='sn') '_Normalized_SN' else '_Normalized'
    # x_col <- paste0(signal, col_name)
    
    
    x_col <- if(data_type=='sn') '_Normalized_SN' else '_Normalized'
    
    if (data_type=='sn') {
      col_name <- '_Normalized_SN'
      
    } else if (data_type=='not_sn') {
      col_name <- '_Normalized'
      
    } else if (data_type=='10_sec') {
      col_name <- ''
      
    }
    
    x_col <- paste0(signal, col_name)
    ##########################################################################################################
    
    
    ##########################################################################################################
    # if (data_type=='sn') {
    #   study_df <- study_df %>% filter(.[[x_col]]>-5 & .[[x_col]]<5)
    # }
    # print(nrow(study_df[x_col]))
    
    # print(nrow(study_df[x_col]))
    normalized_col_vals <- as.numeric(unlist(study_df[x_col]))
    normalized_col_vals <- normalized_col_vals[!is.na(normalized_col_vals)]
    # print(length(normalized_col_vals))
    norm_data <- rnorm(n = length(normalized_col_vals), mean = 0, sd = 1)
    # norm_data <- rnorm(n = nrow(study_df[x_col]), mean = 0, sd = 1)
    # print(length(norm_data))

    # print(class(norm_data))
    # print(class(normalized_col_vals))
    
    # print(norm_data)
    # print(normalized_col_vals)
    
    rval.ccc01 <- epi.ccc(norm_data, 
                          normalized_col_vals,
                          ci = "z-transform", 
                          conf.level = 0.95, 
                          rep.measure = FALSE)
    cc_val <- round(rval.ccc01$rho.c[,1], digits = 2)
    # print(cc_val)
    
    ad_val <- ad.test(normalized_col_vals)$p.value
    # print(ad_val)
    
    # shapiro_val <- shapiro.test(normalized_col_vals)
    # print(shapiro_val)
    
    # cc_shapiro <- paste("CCC: ", cc_val, "Shapiro: ", shapiro_val$p.value)
    # print(cc_shapiro)
    
    cc_ad <- paste("n: ", length(norm_data), "   CCC: ", cc_val, "  AD: ", ad_val)
    # print(cc_ad)
    ##########################################################################################################
    
    
    
    
    if (signal=='PP') {
      bin_width = 0.01
    } else {
      bin_width = 0.06
    }
    
    # bin_width = 0.01
    
    plot <- ggplot(study_df, aes_string(x=x_col, fill=paste0(signal, '_Arousal_ecdf'))) +
      # geom_density(alpha=0.4) +
      # geom_histogram(fill='white', alpha=0.1, binwidth=0.001, position='identity') +
      geom_histogram(alpha=0.6, binwidth=bin_width, position="identity") +
      xlim(min(df[x_col]), max(df[x_col]))
      
      
      
    
    qq_plot <- ggplot(study_df, aes_string(sample=x_col)) +
      stat_qq() + 
      stat_qq_line() +
      xlim(min(df[x_col]), max(df[x_col])) +
      ylim(-8, 10) 
    # + geom_text(x = -Inf, y = Inf, hjust = 0, vjust = 1, label = cc_ad, size=7) 
    
    
    plot <- add_common_themes(plot) + 
      theme(axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
    qq_plot <- add_common_themes(qq_plot)
    
    if (study=='sim1') {
      title = paste0("\u0394", signal)
      
      plot <- plot + ggtitle(title)
      qq_plot <- qq_plot + ggtitle(title)
    }
    
      
    if (signal=='PP') {
      plot <- plot + ylab(get_study_y_lab(study))
      qq_plot <- qq_plot + ylab(get_study_y_lab(study))
      
    } else {
      plot <- plot + theme(axis.title.y=element_blank())
      qq_plot <- qq_plot + theme(axis.title.y=element_blank())
    }
    
    if (study=='deadline_study' && signal=='BR') {
      plot <- ggplot() +
        xlim(min(df[x_col]), max(df[x_col]))
      
      plot <- add_common_themes(plot)
    }
    
    plot_list[[length(plot_list)+1]] <<- plot
    qq_plot_list[[length(qq_plot_list)+1]] <<- qq_plot
    
    # if (data_type=='sn') {
    #   #### qq_plot <- qq_plot + ggtitle(paste0(toupper(study), ' - ', signal))
    #   qq_plot <- qq_plot + ggtitle(signal)
    #   save_plot(paste0('qq_', study, '_', signal), qq_plot, width=20, height=12)
    # }
  }
}

generate_arousal_labeling_plots <- function() {
  
  plot_list <<- list()
  qq_plot_list <<- list()
  
  plot_name <- 'arousal_labeling_plot'
  qq_plot_name <- 'arousal_labeling_qq_plot'
  
  if (data_type=='sn') {
    df <- custom_read_csv(file.path(all_studies_data_dir, 'data_2_5_standardized.csv')) %>%
      filter(PP_Normalized_SN>-6 & PP_Normalized_SN<8
             # & HR_Normalized_SN>-5 & HR_Normalized_SN<5 
             # & BR_Normalized_SN>-5 & BR_Normalized_SN<5
             )
    
    plot_name <- paste0(plot_name, '_sn')
    qq_plot_name <- paste0(qq_plot_name, '_sn')
    
  } else if (data_type=='not_sn') {
    df <- read_data()
    
    plot_name <- paste0(plot_name, '_per_sec')
    qq_plot_name <- paste0(qq_plot_name, '_per_sec')
    
  } else if (data_type=='10_sec') {

    df <- custom_read_csv(file.path(all_studies_data_dir, 'data_3_study_combined.csv')) %>% 
      dplyr::rename(
        PP = Perinasal_Mean,
        HR = HR_Mean,
        BR = BR_Mean,
        
        PP_Arousal_ecdf = PP_Arousal_Mode,
        HR_Arousal_ecdf = HR_Arousal_Mode,
        BR_Arousal_ecdf = BR_Arousal_Mode,
      )
    
    plot_name <- paste0(plot_name, '_10sec')
    qq_plot_name <- paste0(qq_plot_name, '_10sec')

  }
  
  studies <- unique(df$Study_Name)
  # studies <- studies[! studies %in% c('deadline_study')]

  if (test==T) {
    studies <- c('sim1')
    signals <<- c('PP')
  }
  
  for (study in studies) {
    print(study)
    generate_arousal_labeling_plot(df, study)
  }

  grid_plot <- plot_grid(plotlist=plot_list,
                         # rel_heights = c(1.5, 1, 1, 1, 1, 1, 1, 1),
                         ncol=length(signals))
  save_plot(plot_name, grid_plot, width=20, height=12)
  # print(grid_plot)
  
  # if (data_type=='sn') {
  #   qq_grid_plot <- plot_grid(plotlist=qq_plot_list,
  #                             # rel_heights = c(1.5, 1, 1, 1, 1, 1, 1, 1),
  #                             ncol=length(signals))
  #   save_plot(qq_plot_name, qq_grid_plot, width=20, height=12)
  # }
}






#----------------------------#
#------  Main Program  ------#
#----------------------------#
test <<- F

##############################################
# generate_custom_data()
# generate_rb_mean_dist()
##############################################


##############################################
# c('sn', 'not_sn', '10_sec')
for (dt in c('not_sn')) {
  tryCatch({
    data_type <<- dt
    generate_arousal_labeling_plots()
  },
  error=function(e) {
    print(e)
  })
}
##############################################



