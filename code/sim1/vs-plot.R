#----------------------------------------------------------------------------------------
# Plot the results of the predictions in their physical form
#   1. Compute and store evaluation metric for each session
#   2. Plot the true positives, false positives and false negatives with different colors
#     in their physical form ( 10 second windows of each subject's recordings)
#----------------------------------------------------------------------------------------
# sd_arousal=""
# sd_arousal="_sd"


libs = c("dplyr", "ggplot2", "caret", "ROCR", "cowplot", "stringr")
lapply(libs, require, character.only = TRUE)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("../../data/sim1")

figure_dir <- "../../figure/sim1"

test_subjs <- ""
windows_size <- 10



get_sec_axis_label <- function(col_name) { 
  if (col_name=='pp') { 
    # return(bquote(paste('log'[10], '(PP [',''^'o','C',''^2,'])'))) 
    # return(bquote('PP '[~italic~('n')]))
    return(bquote(italic('PP '['N'])))
    
  } else if (col_name=='hr') { 
    # return('Chest HR [BPM]') 
    return(bquote(italic('HR '['N'])))
    
  }  else if (col_name=='br') { 
    # return('Chest BR [BPM]') 
    return(bquote(italic('BR '['N'])))
    
  } 
  
  
  # else if (col_name=='pp_2') { 
  #   # return('Chest HR [BPM]') 
  #   return(bquote(italic('PP '['N'])))
  #   
  # } 
  
  return('Unknown axis') 
} 


get_drive_name <- function(d) {
  if (d=="Sensorimotor") {
    return("Physical")
  }
  
  d
}


save_plot <- function(plot_name, plot, width=default_plot_width, height=default_plot_height) {
  plot_path <- file.path(figure_dir, paste0(plot_name, '.png'))
  ggsave(plot_path, plot, width=width, height=height)
  
  plot_path <- file.path(figure_dir, paste0(plot_name, '.pdf'))
  ggsave(plot_path, plot, device=cairo_pdf, width=width, height=height)
}


generate_all_signal_arousal_plots <- function(windows_size=10, only_test_subjects=F) {
  final_plot_list <- list()
  
  for (signal in c("pp", "hr", "br")) {
  
    if (windows_size==10) {
      file_name = "data_3.csv"
      arousal_col_name = "Arousal_Mode"
      alpha_val = 0.8
      
    } else {
      file_name = "data_2.5.csv"
      arousal_col_name = "Arousal"
      alpha_val = 0.3
    }
    
    
    # dat = read.csv(paste0(signal, file_name))
    dat = read.csv(file_name)
    nasa_dat = read.csv("nasa_cluster_df.csv") %>% 
      select(Subject, Drive, Nasa_Cluster)
    
    # View(dat)
    # View(nasa_dat)
    
    dat = dat %>% 
      dplyr::left_join(nasa_dat, by=c('Subject', 'Drive'))
    
    # View(dat)
    
    dat <- dat %>%
      dplyr::rename(drive = Drive,
                    subject = Subject,
                    arousal = paste0(toupper(signal), "_", arousal_col_name)) %>%
      filter(drive %in% c(2, 3, 4)) %>%
      select(subject, drive, arousal, Nasa_Cluster) %>%
      group_by(subject, drive) %>%
      dplyr::mutate(Frame = 1:n())
    # print(head(dat))
    
    if (only_test_subjects) {
      result_df = read.csv(paste0(signal, "_pred_result_df.csv"))
      test_subjs = unique(result_df$Subject)
      
      dat <- dat %>%
        filter(subject %in% test_subjs)
      
      test_subjs = "_test_subjs"
    }
    
    
    dat$drive[dat$drive==1]  = "Normal"
    dat$drive[dat$drive==2]  = "Cognitive"
    dat$drive[dat$drive==3]  = "Sensorimotor"
    dat$drive[dat$drive==4]  = "Emotional"
    dat$drive[dat$drive==5]  = "Failure"
    
    
    # print(head(dat, 2))
    # View(dat)
    # print(unique(dat$drive))
    
    
    # get_axis_color <- ifelse(dat$Nasa_Cluster == 'High', "red", "green")
    # print(get_axis_color)
    
    # get_axis_color <- function(data) {
    #   View(data)
    #   # if (data$Nasa_Cluster == 'High') {
    #   #   'red'
    #   # } else if (data$Nasa_Cluster == 'Low') {
    #   #   'green'
    #   # }
    # }
    
    
    
    plot_list <- list()
    
    for(d in unique(dat$drive)) {
      # d = unique(dat$drive)[4]
      # print(d)
      
      plot_dat <- dat %>%
        filter(drive == d)
      
      # get_axis_color <- ifelse(plot_dat$Nasa_Cluster == 'High', "red", "green")
      # print(get_axis_color)
      
      axis_colors = c()
      for (subj in sort(unique(plot_dat$subject))) {
        
        subj_df <- plot_dat %>% 
          filter(subject == subj)
        
        # print(paste(subj, subj_df$Nasa_Cluster[1]))
        
        if (!is.na(subj_df$Nasa_Cluster[1])) {
          if (subj_df$Nasa_Cluster[1] == 'High') {
            axis_color = 'red'
          } else if (subj_df$Nasa_Cluster[1] == 'Low') {
            axis_color = 'green'
          } else if (subj_df$Nasa_Cluster[1] == 'Medium') {
            axis_color = 'grey'
          } 
          
        } else {
          axis_color = 'black'
        }
        
        axis_colors <- c(axis_colors, axis_color)
      }
      
      
      plot <-  plot_dat %>%
        ggplot(. ,aes(x=factor(subject), y=Frame, color=factor(arousal))) +
        geom_point(alpha = alpha_val) +
        
        theme_bw() +
        theme(
              plot.title = element_text(hjust = 0.5, size = 40),
              axis.title.x = element_text(size = 30, margin = margin(t = 18, r = 0, b = 0, l = 0)),
              axis.title.y =  element_text(size = 28),
              # axis.text.x = element_text(size = 15, angle = 45, vjust = 0.5, hjust=1, colour=axis_colors),
              axis.text.x = element_text(size = 15, angle = 45, vjust = 0.5, hjust=1),
              axis.text.y = element_text(size = 20)
              ) +

        scale_color_manual(values = c("relaxed" = "green",
                                      "stressed" = "red"))
      
      # scale_color_manual(values = c("relaxed" = "green",
      #                               "normal" = "gray81",
      #                               "unknown" = "white",
      #                               "stressed" = "red"))
      
      # if (signal=="hr_") {
      #   plot <- plot +
      #     scale_color_manual(values = c("relaxed" = "green",
      #                                   "normal" = "gray81",
      #                                   "unknown" = "white",
      #                                   "stressed" = "red"))
      # } else if (signal=="pp_") {
      #   plot <- plot +
      #     scale_color_manual(values = c("relaxed" = "green",
      #                                   "normal" = "gray81",
      #                                   "stressed" = "red"))
      # }
      
      
      if (d=="Cognitive") {
          plot <- plot + ylab(paste0("Time [", windows_size, " s]"))
          
      } else if (d=="Emotional") {
        plot <- plot + 
          theme(
            axis.title.y.left=element_blank(),
            axis.text.y.left=element_blank(),
            axis.ticks.y.left=element_blank(),
            axis.text.y.right=element_blank(),
            axis.ticks.y.right=element_blank(),
            axis.title.y.right = element_text(size=28, 
                                              angle=90,
                                              vjust=0.25, 
                                              face='bold')
            ) +
          # scale_y_continuous(sec.axis=sec_axis(~.+1, name=toupper(signal)))
          scale_y_continuous(sec.axis=sec_axis(~.+1, name=get_sec_axis_label(signal)))
        
      } else {
        plot <- plot + theme(
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
      }
      
    
      
      if (signal=="pp") {
        plot <- plot + ggtitle(get_drive_name(d))
      }
      
      # if (signal=="pp") {
      #   plot <- plot + 
      #     ggtitle(d) +
      #     theme(legend.title = element_blank(),
      #           legend.position = "top",
      #           legend.text = element_text(size = 24)) +
      #     guides(colour = guide_legend(override.aes = list(size=10)))
      #   
      # } else {
      #   plot <- plot +
      #     theme(legend.position = "none")
      # }
      
      if (signal=="br") {
        plot <- plot + 
          xlab("Subjects") +
          theme(legend.title = element_blank(),
                legend.position = "bottom",
                legend.text = element_text(size=20)) +
          guides(colour = guide_legend(override.aes = list(size=10)))
        
      } else {
        plot <- plot + theme(
          legend.position = "none",
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
      }
      
      # ggsave(paste0("figures/", signal, "_arousal_", windows_size, "_sec_window_", d, test_subjs, ".pdf"), plot, device=cairo_pdf, width = 7, height = 5.5)
      plot_list[[length(plot_list) + 1]] <- plot
    }
    
    final_plot_list[[length(final_plot_list) + 1]] <- plot_grid(plotlist=plot_list,
                                                                # rel_heights = c(1, 0.1, 1),
                                                                ncol=3)
  }
    
  grid_plot <- plot_grid(plotlist=final_plot_list,
                         rel_heights = c(1.1, 1, 1, 1.3),
                         ncol=1)
  
  ggsave(paste0("../../figure/sim1/sim1_combined_arousal_", windows_size, "_sec_window", test_subjs, ".pdf"),
         grid_plot,
         device=cairo_pdf,

         #--------- For ncol=2
         # width=20,
         # height=14,

         #--------- For ncol=3
         # width=21,
         # height=6,

         #--------- For ncol=1
         width=21,
         height=20,
  )
}

generate_all_signal_prediction_plots <- function(signal) {
  final_plot_list <- list()
  
  for (signal in c("pp", "hr", "br")) {
    
    dat = read.csv(paste0(signal, "_pred_result_df.csv"))
    # print(signal)
    # print(head(dat))
    
    dat <- dat %>%
      dplyr::rename(drive = Drive,
                    subject = Subject,
                    arousal = paste0(toupper(signal), "_Arousal_Mode"),
                    prediction = Prediction) %>% 
      # filter(drive!=1, drive!=5) %>%
      filter(drive %in% c(2, 3, 4, 5)) %>%
      select(subject, drive, arousal, prediction) %>%
      group_by(subject, drive) %>% 
      dplyr::mutate(Frame = 1:n())
    # print(head(dat))
    
    
    dat$drive[dat$drive==1]  = "Normal"  
    dat$drive[dat$drive==2]  = "Cognitive"
    dat$drive[dat$drive==3]  = "Sensorimotor"
    dat$drive[dat$drive==4]  = "Emotional"
    dat$drive[dat$drive==5]  = "Failure"
    # print(head(dat, 2))
    # View(dat)
    
    
    dat$prediction_dot = "TN"
    dat$prediction_dot[dat[, "arousal"]=="stressed" & dat[, "prediction"]=="stressed"] = "TP"
    dat$prediction_dot[dat[, "arousal"]=="stressed" & dat[, "prediction"]=="relaxed"] = "FN"
    dat$prediction_dot[dat[, "arousal"]=="relaxed" & dat[, "prediction"]=="stressed"] = "FP"
    
    dat$prediction_dot <- factor(dat$prediction_dot, levels=c("TP", "TN", "FP", "FN"), labels=c("TP", "TN", "FP", "FN"))
    
    
    # dat = dat[-which(dat[, "arousal"]==0 & dat[, "prediction"]==0),]
    # View(dat)
    
    #### colors=c("#999999", "#56B4E9","#E69F00")
    #### colors=c("red", "black", "gray81", "green")
    # print(unique(dat$drive))
    
    plot_list <- list()
    
    for(d in unique(dat$drive)) {
      # d = unique(dat$drive)[4]
      # print(d)
      
      plot <- dat %>%
        filter(drive == d) %>%
        ggplot(. ,aes(x=factor(subject), y=Frame, color=factor(prediction_dot))) +
        geom_point() +
        xlab("Subjects") +
        ylab("Time Windows (10 sec each)") +
        # ggtitle(d) +
        
        theme_bw() +
        theme(
              plot.title = element_text(hjust = 0.5, size = 40),
              axis.title.x = element_text(size = 30, margin = margin(t = 18, r = 0, b = 0, l = 0)),
              axis.title.y =  element_text(size = 28),
              axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust=1),
              axis.text.y = element_text(size = 20)
              
              # legend.title = element_blank(),
              # legend.position = "top",
              # legend.text = element_text(size = 12),
              
              # axis.title =  element_text(size = 14),
              # axis.text = element_text(size = 14)
              )+
        guides(colour = guide_legend(override.aes = list(size=6))) +
        # scale_color_manual(values=colors) +
        # scale_color_manual(values = c("TN" = "gray81",
        #                               "TP" = "green",
        #                               "FN" = "red",
        #                               "FP" = "black"))
        scale_color_manual(values = c("TN" = "gray81",
                                      "TP" = "cornflowerblue",
                                      "FN" = "darkorange",
                                      "FP" = "black"))
      
      
      if (d=="Cognitive") {
        plot <- plot + ylab(paste0("Time [", windows_size, " s]"))
        
      } else if (d=="Emotional") {
        plot <- plot + 
          theme(
            axis.title.y.left=element_blank(),
            axis.text.y.left=element_blank(),
            axis.ticks.y.left=element_blank(),
            axis.text.y.right=element_blank(),
            axis.ticks.y.right=element_blank(),
            axis.title.y.right = element_text(size=28, 
                                              angle=90,
                                              vjust=0.25, 
                                              face='bold')
          ) +
          # scale_y_continuous(sec.axis=sec_axis(~.+1, name=toupper(signal)))
          scale_y_continuous(sec.axis=sec_axis(~.+1, name=get_sec_axis_label(signal)))
        
      } else {
        plot <- plot + theme(
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
      }
      
      
      if (signal=="pp") {
        plot <- plot + ggtitle(get_drive_name(d))
      }
      
      if (signal=="br") {
        plot <- plot + 
          theme(legend.title = element_blank(),
                legend.position = "bottom",
                legend.text = element_text(size=20)) +
          guides(colour = guide_legend(override.aes = list(size=10)))
        
      } else {
        plot <- plot +
          theme(legend.position = "none",
                axis.title.x=element_blank(),
                # axis.text.x=element_blank(),
                # axis.ticks.x=element_blank()
                )
      }
      
      
      plot <- plot + xlab("Subjects")
      #######################################
      # if (signal=="br") {
      #   plot <- plot + xlab("Subjects")
      #   
      # } else {
      #   plot <- plot + theme(
      #     axis.title.x=element_blank(),
      #     axis.text.x=element_blank(),
      #     axis.ticks.x=element_blank())
      # }
      #######################################
      
      
      # ggsave(paste0("figures/", signal, "_arousal_prediction_", d, ".pdf"), plot, device=cairo_pdf, width = 7, height = 5.5)
      plot_list[[length(plot_list) + 1]] <- plot
    }
    
    final_plot_list[[length(final_plot_list) + 1]] <- plot_grid(plotlist=plot_list, 
                                                                # rel_heights = c(1, 1, 1, 1.5),
                                                                ncol=3)
  }
  
  
  grid_plot <- plot_grid(plotlist=final_plot_list,
                         rel_heights = c(1.1, 1, 1, 1.3),
                         ncol=1)
  
  ggsave(paste0("../../figure/sim1/sim1_combined_arousal_prediction", ".pdf"), 
         grid_plot,
         device=cairo_pdf,
         
         #--------- For ncol=2
         # width=20,
         # height=14,
         
         #--------- For ncol=3
         # width=21,
         # height=6,
         
         #--------- For ncol=1
         width=21,
         height=20,
  )
}




generate_all_signal_ecdf_arousal_plots <- function(windows_size=10, only_test_subjects=F, threshold='') {
  final_ecdf_plot_list <- list()
  
  for (signal in c("pp", "hr", "br")) {
    
    if (windows_size==10) {
      file_name = "data_3.csv"
      arousal_col_name = "Arousal_Mode"
      alpha_val = 0.8
      
    } else {
      file_name = "data_2.5.csv"
      arousal_col_name = "Arousal"
      alpha_val = 0.3
    }
    
    if (threshold=='1sd') {
      arousal_col_name = "Arousal_Mode_1sd"
    }
    
    
    # dat = read.csv(paste0(signal, file_name))
    dat = read.csv(file_name)
    # nasa_dat = read.csv("nasa_cluster_df.csv") %>% 
    #   select(Subject, Drive, Nasa_Cluster)
    
    # View(dat)
    # View(nasa_dat)
    
    # dat = dat %>% 
    #   dplyr::left_join(nasa_dat, by=c('Subject', 'Drive'))
    
    # View(dat)
    
    # new_arousals <- list("relaxed" = "non-arousal",
    #                      "neutral" = "neutral",
    #                      "stressed" = "arousal")
    
    dat <- dat %>%
      dplyr::rename(drive = Treatment,
                    subject = Subject,
                    arousal = paste0(toupper(signal), "_", arousal_col_name)) %>%
      dplyr::mutate(arousal = case_when(arousal == 'relaxed' ~ 'non-arousal',
                                        arousal == 'stressed' ~ 'arousal',
                                        TRUE ~ 'neutral')) %>% 
      # dplyr::mutate(arousal = new_arousals[old_arousal]) %>% 
      filter(drive %in% c('CD', 'MD', 'ED')) %>% 
      # filter(drive %in% c(2, 3, 4)) %>%
      select(subject, drive, arousal) %>%
      group_by(subject, drive) %>%
      dplyr::mutate(Frame = 1:n())
    # print(head(dat))
    # View(dat)
    
    
    if (only_test_subjects) {
      result_df = read.csv(paste0(signal, "_pred_result_df.csv"))
      test_subjs = unique(result_df$Subject)
      
      dat <- dat %>%
        filter(subject %in% test_subjs)
      
      test_subjs = "_test_subjs"
    }
    
    
    # dat$drive[dat$drive==1]  = "Normal"
    dat$drive[dat$drive=='CD']  = "Cognitive"
    dat$drive[dat$drive=='MD']  = "Sensorimotor"
    dat$drive[dat$drive=='ED']  = "Emotional"
    # dat$drive[dat$drive==5]  = "Failure"
    
    
    # dat$drive[dat$drive=='relaxed']  = "non-arousal"
    # dat$drive[dat$drive=='neutral']  = "undefined"
    # dat$drive[dat$drive=='stressed']  = "arousal"
    # scale_color_manual(values = c("relaxed" = "green",
    #                               "neutral" = "grey",
    #                               "stressed" = "red"))
    
    
    # print(head(dat, 2))
    # View(dat)
    # print(unique(dat$drive))
    
    
    # get_axis_color <- ifelse(dat$Nasa_Cluster == 'High', "red", "green")
    # print(get_axis_color)
    
    # get_axis_color <- function(data) {
    #   View(data)
    #   # if (data$Nasa_Cluster == 'High') {
    #   #   'red'
    #   # } else if (data$Nasa_Cluster == 'Low') {
    #   #   'green'
    #   # }
    # }
    
    
    
    plot_list <- list()
    
    for(d in c("Sensorimotor", "Cognitive", "Emotional")) {
    # for(d in unique(dat$drive)) {
      # d = unique(dat$drive)[4]
      # print(d)
      
      plot_dat <- dat %>%
        filter(drive == d)
      
      # get_axis_color <- ifelse(plot_dat$Nasa_Cluster == 'High', "red", "green")
      # print(get_axis_color)
      

      
      plot <-  plot_dat %>%
        ggplot(. ,aes(x=factor(subject), y=Frame, color=factor(arousal))) +
        geom_point(alpha = alpha_val) +
        
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5, size = 40),
          axis.title.x = element_text(size = 30, margin = margin(t = 18, r = 0, b = 0, l = 0)),
          axis.title.y =  element_text(size = 28),
          # axis.text.x = element_text(size = 15, angle = 45, vjust = 0.5, hjust=1, colour=axis_colors),
          axis.text.x = element_text(size = 15, angle = 45, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size = 20)
        ) +
        
        # scale_color_manual(values = c("relaxed" = "green",
        #                               "neutral" = "grey",
        #                               "stressed" = "red"))
      
        scale_color_manual(values = c("non-arousal" = "green",
                                      "neutral" = "grey",
                                      "arousal" = "red"))
      
      
      
      
      
      
      
      
      
      ##############################################################################      
      # scale_color_manual(values = c("relaxed" = "green",
      #                               "normal" = "gray81",
      #                               "unknown" = "white",
      #                               "stressed" = "red"))
      
      # if (signal=="hr_") {
      #   plot <- plot +
      #     scale_color_manual(values = c("relaxed" = "green",
      #                                   "normal" = "gray81",
      #                                   "unknown" = "white",
      #                                   "stressed" = "red"))
      # } else if (signal=="pp_") {
      #   plot <- plot +
      #     scale_color_manual(values = c("relaxed" = "green",
      #                                   "normal" = "gray81",
      #                                   "stressed" = "red"))
      # }
      ##############################################################################
      
      if (d=="Sensorimotor") {
        plot <- plot + ylab(paste0("Time [", windows_size, " s]"))
        
      } else if (d=="Emotional") {
        plot <- plot + 
          theme(
            axis.title.y.left=element_blank(),
            axis.text.y.left=element_blank(),
            axis.ticks.y.left=element_blank(),
            axis.text.y.right=element_blank(),
            axis.ticks.y.right=element_blank(),
            axis.title.y.right = element_text(size=28, 
                                              angle=90,
                                              vjust=0.25, 
                                              face='bold')
          ) +
          # scale_y_continuous(sec.axis=sec_axis(~.+1, name=toupper(signal)))
          scale_y_continuous(sec.axis=sec_axis(~.+1, name=get_sec_axis_label(signal)))
        
      } else {
        plot <- plot + theme(
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
      }
      
      
      
      if (signal=="pp") {
        plot <- plot + ggtitle(get_drive_name(d))
      }
      
      # if (signal=="pp") {
      #   plot <- plot + 
      #     ggtitle(d) +
      #     theme(legend.title = element_blank(),
      #           legend.position = "top",
      #           legend.text = element_text(size = 24)) +
      #     guides(colour = guide_legend(override.aes = list(size=10)))
      #   
      # } else {
      #   plot <- plot +
      #     theme(legend.position = "none")
      # }
      
      if (signal=="br") {
        plot <- plot + 
          xlab("Subjects") +
          theme(legend.title = element_blank(),
                legend.position = "bottom",
                legend.text = element_text(size=20)) +
          guides(colour = guide_legend(override.aes = list(size=10)))
        
      } else {
        plot <- plot + theme(
          legend.position = "none",
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
      }
      
      # ggsave(paste0("figures/", signal, "_arousal_", windows_size, "_sec_window_", d, test_subjs, ".pdf"), plot, device=cairo_pdf, width = 7, height = 5.5)
      plot_list[[length(plot_list) + 1]] <- plot
      # print(plot)
    }
    
    final_ecdf_plot_list[[length(final_ecdf_plot_list) + 1]] <- plot_grid(plotlist=plot_list,
                                                                # rel_heights = c(1, 0.1, 1),
                                                                ncol=3)
  }
  
  
  grid_ecdf_plot <- plot_grid(plotlist=final_ecdf_plot_list,
                         rel_heights = c(1.1, 1, 1.3),
                         ncol=1)

  # ggsave(paste0("../../figure/sim1/sim1_combined_ecdf_arousal_", windows_size, "_sec_window", test_subjs, threshold, ".pdf"),
  #        grid_ecdf_plot,
  #        device=cairo_pdf,
  # 
  #        #--------- For ncol=2
  #        # width=20,
  #        # height=14,
  # 
  #        #--------- For ncol=3
  #        # width=21,
  #        # height=6,
  # 
  #        #--------- For ncol=1
  #        width=21,
  #        height=20,
  # )
  plot_name <- paste0("sim1_combined_ecdf_arousal_", windows_size, "_sec_window", test_subjs, threshold)
  save_plot(plot_name, grid_ecdf_plot, 21, 20)
  
}








# xlabel color factor on data -->
# https://stackoverflow.com/questions/38862303/customize-ggplot2-axis-labels-with-different-colors




# ########################################################################
# ************************
# ########################################################################
# # generate_all_signal_arousal_plots(only_test_subjects=T)
# generate_all_signal_arousal_plots()
# generate_all_signal_prediction_plots()
# ########################################################################







########################################################################
#### generate_all_signal_ecdf_arousal_plots(only_test_subjects=T)
generate_all_signal_ecdf_arousal_plots()
generate_all_signal_ecdf_arousal_plots(threshold='1sd')
#### generate_all_signal_ecdf_prediction_plots()
########################################################################


