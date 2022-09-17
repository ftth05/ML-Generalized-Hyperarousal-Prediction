  #-------------------------#
  #--------LIBRARIES--------#
  #-------------------------#
  library(ggplot2)
  library(grid)
  library(gridExtra)
  library(cowplot)
  library(stringr)
  library(data.table)
  library(reshape2)
  # library(relevel)
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
  
  
  # deadline <- F
  # all_studies <<- F
  
  
  
  signals <<- NA
  train_studies <<- NA
  train_study_levels <<- NA
  test_study_levels <<- NA
  plot_name <<- NA
  
  
  
  office_tasks <- 'OT'
  deadline_study <- 'DS'
  sim1_sim2 <- 'SS'
  sim1_sim2_tt1 <- 'SST'
  sim1_sim2_ot <- 'SSO'
  sim1_sim2_tt1_ot <- 'SSTO'
  
  
  
  round_value <- 2  # 0, 2
  
  
  
  
  # metrics_ml_hist_v2.csv  --> Hist
  # metrics_ml_ecdf_v4.csv  --> Empirical
  # metrics_ml_ecdf_deadline_v5.csv --> Empirical Deadline
  # file_name <- 'metrics_ml_hist_v2.csv'
  
  # ml_30_5.csv
  file_name <- 'lstm_30_5.csv'
  
  
  #-------------------------#
  #---FUNCTION DEFINITION---#
  #-------------------------#
  read_data <- function() {
    metrics_df <<- custom_read_csv(file.path(all_studies_data_dir, 'metrics', file_name))
    # View(metrics_df)
  }
  
  add_common_themes <- function(plot) {
    plot <- plot +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            
            axis.title.x=element_blank(),
            # axis.text.y.right=element_blank(),
            # axis.ticks.y.right=element_blank(),
  
            plot.title = element_text(size=20, hjust = 0.5),
            # text=element_text(12),
  
            axis.text.x=element_text(size=12),
            axis.text.y=element_text(size=12)
      )
    
    plot
  }
  
  add_bottom_legend <- function() {
    theme(legend.position="bottom",
          legend.spacing.x = unit(0.2, 'cm'), ## Distance between legend key and legend text
          legend.text = element_text(size=12,
                                     margin=margin(r=15, unit="pt")), ## Distance between legends
          legend.direction = "horizontal",
          legend.justification = "center",
          legend.title = element_blank()) 
  }
  
  remove_legend <- function() {
    theme(legend.position="none")
  }
  
  get_model_name <- function() {
    if (grepl('ml', file_name, fixed=TRUE)) {
      'ml'
    } else if (grepl('lstm', file_name, fixed=TRUE)) {
      'lstm'
    }
  }
  
  
  # generate_bar_plot <- function(train_study) {
  #   train_study_df <- metrics_df %>% filter(Train_Study == train_study)
  #   
  #   train_study_df <- melt(setDT(train_study_df), id.vars = c('Model', 'Arousal_Signal', 'Train_Study', 'Test_Study'), variable.name = 'Metrics') %>%
  #     mutate(Test_Study=case_when(grepl('Train', Metrics)~'Train', TRUE~Test_Study),
  #            Test_Study=case_when(Test_Study=='office_tasks'~office_tasks, 
  #                                 TRUE~Test_Study)) %>%
  #     group_by(Arousal_Signal, Train_Study, Test_Study,  Metrics) %>%
  #     slice(1) %>%
  #     mutate(Metrics = str_split(Metrics, '_', simplify = T)[, 2],
  #            Test_Study = toupper(Test_Study),
  #            Train_Study = toupper(Train_Study)) 
  # 
  #     # %>% 
  #     # mutate(Test_Study = factor(Test_Study, levels = c(office_tasks, 'TT1',  'SIM2', 'SIM1')),
  #     #        Train_Study = factor(Train_Study, levels = c('SIM1', 'SIM2',  'TT1', office_tasks)))
  #     
  #   
  #   
  #   # signals <- c('PP', 'PP_2', 'HR', 'BR', 'PP_BR', 'PP_HR', 'HR_BR', 'PP_HR_BR')
  #   signals <- unique(train_study_df$Arousal_Signal)
  #   # signals <- c('PP', 'PP_2')
  #   # signals <- c('PP')
  # 
  #   for (signal in signals) {
  #     signal_df <- train_study_df %>% 
  #       filter(Arousal_Signal == signal)%>%
  #       na.omit()
  #     # View(signal_df)
  #     
  #     plot <- ggplot(signal_df, aes(fill=Metrics, y=value, x=Test_Study)) + 
  #       geom_bar(position='dodge', stat='identity') +
  #       ggtitle(signal) +
  #       ylab('') +
  #       xlab('') + 
  #       ylim(0, 1) 
  # 
  #     
  #     plot_legend <<- get_legend(plot + add_bottom_legend())
  #     
  #     plot <- add_common_themes(plot) + remove_legend()
  # 
  #     plot_list[[length(plot_list)+1]] <- plot
  #   }
  #   
  #   #----------------------------------------------------------------#
  #   #----  MAKING GRID GRAPH WITH ALL THE PLOTS FROM EACH STUDY  ----#
  #   #----------------------------------------------------------------#
  #   grid_plot_title <- ggdraw() + 
  #     draw_label(
  #       paste0('Train Study: ', toupper(train_study)),
  #       fontface = 'bold',
  #       size = 30,
  #     ) 
  #   
  #   grid_plot <- plot_grid(plotlist=plot_list,
  #                          # rel_heights = c(2.6, 2, 2, 2, 2),
  #                          ncol=ceiling(sqrt(length(plot_list))))
  #   
  #   grid_plot <- plot_grid(grid_plot_title,
  #                          grid_plot,
  #                          plot_legend,
  #                          rel_heights = c(1, 10.6, 1),
  #                          ncol=1)
  #   
  #   
  #   
  #   #---- SAVING GRID PLOT ----#
  #   save_plot(paste0(get_model_name(), '_', tolower(train_study), '_bar_plot'), grid_plot, width=16, height=12)
  #   
  # }
  # 
  # generate_bar_plots <- function() {
  #   # train_studies <- unique(metrics_df$Train_Study)
  #   train_studies <- c('sim1___sim2___tt1___office_tasks')
  #   # train_studies <- c('sim1', 'sim2')
  #   # train_studies <- c('sim1')
  #   
  #   for (train_study in train_studies) {
  #     generate_bar_plot(train_study)
  #   }
  # }
  # 
  
  
  
  
  draw_heat_map_plot <- function(heat_map_df, plot_title) {
    
    # View(heat_map_df)
    heatmap_plot <- ggplot(heat_map_df, aes(x=Test_Study, y=Train_Study)) +
    # heatmap_plot <- ggplot(heat_map_df, aes(x=Train_Study, y=Test_Study)) +  
      geom_tile(aes(fill=round(value, round_value))) +
      
      # scale_fill_brewer(palette = "YlOrRd") +
      
      scale_fill_gradientn(colours = c("white", "snow", "azure2", "azure4", "yellow", "chocolate3", "darkred"),
                           name = "",
                            limits = c(0, 1)) +
      
      # # scale_fill_gradientn(colours = c("azure4", "yellow", "darkorange", "pink", "brown", "darkred"), 
      # #                      name = "", 
      # #                      limits = c(.4, 1)) +
      
      
      ggtitle(plot_title) +
      xlab("") +
      ylab("") +
      theme_bw() +
      theme(text = element_text(size=20),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(c(1.2, 1.2, 1.2, 1.2), "lines"),
            plot.title = element_text(hjust = 0.5, size=18, margin=margin(0,0,-15,0))) +
      labs(fill="") +
      scale_x_discrete(position = "top") +
      geom_text(aes(label=round(value, round_value)), size=4.5)
    
    heatmap_plot
  }
  
  
  set_studies_signals <- function(i) {
    
    all_studies <<- F
    signals <<- c('PP', 'HR', 'BR', 'PP_BR', 'PP_HR', 'HR_BR', 'PP_HR_BR')
    # signals <<- c('PP')
    
    
    if (i==1) {
      train_studies <<- c('sim1', 'sim2', 'tt1', 'office_tasks', 'deadline_study')
      train_study_levels <<- c(deadline_study, office_tasks, 'TT1', 'SIM2', 'SIM1')
      test_study_levels <<- c('SIM1', 'SIM2', 'TT1', office_tasks, deadline_study)
      signals <<- c('PP', 'HR', 'PP_HR')
      plot_name <<- 'single_studies_and_deadline'
      
    } else if (i==2) {
      train_studies <<- c('sim1', 'sim2', 'tt1', 'office_tasks')
      train_study_levels <<- c(office_tasks, 'TT1', 'SIM2', 'SIM1')
      test_study_levels <<- c('SIM1', 'SIM2', 'TT1', office_tasks)
      plot_name <<- 'single_studies'
      
    } else if (i==3) {
      train_studies <<- c('sim1___sim2', 'tt1', 'office_tasks')
      train_study_levels <<- c(office_tasks, 'TT1', sim1_sim2)
      test_study_levels <<- c(sim1_sim2, 'TT1', office_tasks)
      plot_name <<- 'sim1___sim2'
      
    } else if (i==4) {
      train_studies <<- c('sim1___sim2___tt1', 'office_tasks')
      train_study_levels <<- c(office_tasks, 'TT1', sim1_sim2_tt1)
      test_study_levels <<- c(sim1_sim2_tt1, 'TT1', office_tasks)
      plot_name <<- 'sim1___sim2___tt1'
      
    } else if (i==5) {
      train_studies <<- c('sim1___sim2___office_tasks', 'tt1')
      train_study_levels <<- c('TT1', sim1_sim2_ot)
      test_study_levels <<- c(sim1_sim2_ot, 'TT1')
      plot_name <<- 'sim1___sim2___office_tasks'
      
    } else if (i==6) {
      train_studies <<- c('sim1___sim2___tt1___office_tasks')
      train_study_levels <<- c(sim1_sim2_tt1_ot)
      test_study_levels <<- c(sim1_sim2_tt1_ot)
      plot_name <<- 'sim1___sim2___tt1___office_tasks'
      all_studies <<- T
    }
  }
  
  generate_2d_plot <- function(metric) {
    # 1:1 --> Deadline
    # 2:6 --> All other studies
    if (grepl("deadline", file_name, fixed=TRUE)) {
      plot_range <- c(1:1)
    } else {
      plot_range <- c(2:6)
    }
    
    for (i in plot_range) {
      # print(i)
      set_studies_signals(i)
      print(plot_name)
      
      plot_list <- list()
      
      # View(metrics_df)
      # print(train_studies)
      
      train_study_df <- metrics_df %>% 
        dplyr::mutate(Test_Study = tolower(Test_Study)) %>% 
        dplyr::filter(Train_Study %in% train_studies) 
      
      # View(train_study_df)
      
      if (all_studies == F) {
        print('not all studies')
        train_study_df <- train_study_df %>% 
          dplyr::filter(Test_Study %in% train_studies) 
      } else {
        train_study_df <- train_study_df %>%
          dplyr::mutate(Test_Study=Train_Study)
      }
      
      # View(train_study_df)
      
      train_study_df <- melt(setDT(train_study_df), id.vars = c('Model', 'Arousal_Signal', 'Train_Study', 'Test_Study'), variable.name = 'Metrics') %>%
        dplyr::mutate(Train_Study=case_when(Train_Study=='office_tasks' ~ office_tasks, 
                                     Train_Study=='deadline_study' ~ deadline_study,
                                     Train_Study=='sim1___sim2' ~ sim1_sim2,
                                     Train_Study=='sim1___sim2___tt1' ~ sim1_sim2_tt1,
                                     Train_Study=='sim1___sim2___office_tasks' ~ sim1_sim2_ot,
                                     Train_Study=='sim1___sim2___tt1___office_tasks' ~ sim1_sim2_tt1_ot,
                                     # Train_Study=='X'~'X',
                                     TRUE~Train_Study),
               Test_Study=case_when(Test_Study=='office_tasks' ~ office_tasks, 
                                    Test_Study=='deadline_study' ~ deadline_study,
                                    Test_Study=='sim1___sim2' ~ sim1_sim2,
                                    Test_Study=='sim1___sim2___tt1' ~ sim1_sim2_tt1,
                                    Test_Study=='sim1___sim2___office_tasks' ~ sim1_sim2_ot,
                                    Test_Study=='sim1___sim2___tt1___office_tasks' ~ sim1_sim2_tt1_ot,
                                    # Test_Study=='X'~'X',
                                    TRUE~Test_Study),
               Test_Study=case_when(grepl('Train', Metrics)~Train_Study, TRUE~Test_Study)) %>%
        dplyr::group_by(Arousal_Signal, Train_Study, Test_Study,  Metrics) %>%
        slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Metrics = str_split(Metrics, '_', simplify = T)[, 2],
               Test_Study = toupper(Test_Study),
               Train_Study = toupper(Train_Study)) %>% 
        dplyr::filter(Metrics == metric) %>% 
        dplyr::mutate(Train_Study = factor(Train_Study, levels = train_study_levels),
               Test_Study = factor(Test_Study, levels = test_study_levels),
               value=as.numeric(as.character(value)))
      
      # View(train_study_df)
      
      
      
      
      for (signal in signals) {
        print(signal)
        signal_df <- train_study_df %>% 
          filter(Arousal_Signal == signal) %>%
          na.omit()
        # View(signal_df)
        
        heatmap_plot <- draw_heat_map_plot(signal_df, signal)
        plot_list[[length(plot_list)+1]] <- heatmap_plot
      }
      
      #----------------------------------------------------------------#
      #----  MAKING GRID GRAPH WITH ALL THE PLOTS FROM EACH STUDY  ----#
      #----------------------------------------------------------------#
      grid_plot <- plot_grid(plotlist=plot_list,
                             # rel_heights = c(2.6, 2, 2, 2, 2),
                             ncol=ceiling(sqrt(length(plot_list))))
      
      
      #---- SAVING GRID PLOT ----#
      # save_plot(paste0(get_model_name(), '_', tolower(metric), '_', plot_name), grid_plot, width=16, height=12)
      save_plot(paste0(str_sub(file_name, 1, -5), '_', tolower(metric), '_', plot_name), grid_plot, width=16, height=12)
      # save_plot(paste0(str_sub(file_name, 1, -5), '_', tolower(metric), '_', paste(train_studies, collapse = '_')), grid_plot, width=16, height=12)
      # save_plot(paste0(get_model_name(), '_', tolower(metric), '_2d_plot'), grid_plot, width=16, height=12)
      
    }
  }
  
  generate_2d_plots <- function() {
    # for (metric in c('AUC', 'F1')) {
    for (metric in c('AUC')) {
      generate_2d_plot(metric)
    }
  }
  
  
  
  
  
  #----------------------------#
  #------  Main Program  ------#
  #----------------------------#
  # c('ml_classification.csv', 'ml_classification_deadline.csv', 'lstm_15_5_v3.csv', 'lstm_30_5_v3.csv', 'lstm_60_5_v3.csv')
  # knn_classification.csv
  # linear_svc_classification.csv
  for (file in c('knn_classification.csv', 'linear_svc_classification.csv')) {
    tryCatch({
      file_name <<- file
      read_data()
      generate_2d_plots()
    },
    error=function(e) {
      print(paste0(file_name, ':   ', e))
    })
  }
  
  
  
  # generate_bar_plots()
  
  
  
  
  
  
