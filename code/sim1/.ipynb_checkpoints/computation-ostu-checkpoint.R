libs = c("dplyr", "plyr", "ggplot2", "EBImage")
lapply(libs, require, character.only = TRUE)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("../../data/sim1")


otsu_df <<- data.frame()


get_otsu_threshold <- function() {
  
  signals <- c("pp", "pp_2", "hr", "br", "pp_hr_br")
  
  for (signal in signals) {
    
    # 'Relaxed_Prob': 'relaxed', 'Stress_Prob': 'stressed'
    df = read.csv(paste0(signal, "_pred_result_df.csv")) %>% 
      dplyr::rename('relaxed'='Relaxed_Prob',
             'stressed'='Stress_Prob')
    
    print(signal)
    for (label in sort(unique(df$Arousal_Mode))) {
      
      # print(label)
      # print(class(array(df[[label]])))
      # print(as.vector(df[[label]]))
      
      temp_df <- df %>% 
        filter(Arousal_Mode==label)
      
      otsu_val <- otsu(array(temp_df[[label]], dim=c(2, length(df[[label]])/2)), range = c(0, 1))
      print(otsu_val)
      
      temp_otsu_df <- data.frame(signal = c(signal), 
                       label = c(label), 
                       ostu_val = c(otsu_val))
      
      otsu_df <<- rbind.fill(otsu_df, temp_otsu_df)
    }
  }
  
  View(otsu_df)
  write.csv(otsu_df, "otsu_data.csv", row.names = FALSE)
}

get_otsu_threshold()

# get_otsu_threshold(signal="pp")
# get_otsu_threshold(signal="pp_2")
# get_otsu_threshold(signal="hr")
# get_otsu_threshold(signal="br")
# get_otsu_threshold(signal="pp_hr_br")
