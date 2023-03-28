  if (!require("pacman")) install.packages("pacman"); library(pacman)
  p_load(tidyverse, janitor)
  setwd("C:/Users/steff/Documents/Universität/Master Psychologie/SS 2022/Masterarbeit/Experiment/Daten")
  df <- list.files(pattern = "*.csv") %>% 
    map_df(~read_csv(.))
  #to-dos: count Bildershuffle Serie und count Bildershuffle Tutorial löschen
  keeplist <- c("accuracy","anzahl_gruppen","avg_rt","blauetaste","block", "correct", "correct1", "correct2", "correct3",
  "correct_colour", "correct_response", "count_Bildstimulus_3_1", "count_Blockwerte_zuruecksetzten",
   "cutoff", "datetime",                                       
   "difficulty","erstebedingung", "experiment_file", "filename","height","id_geburtsort", "id_jahr",                                    
   "id_vater",               
   "linketaste", "logfile", "loose1",                                       
   "loose2", "loose3", "orangenetaste","punkte1", "punkteimblock", "response", "response_Tasteresponse_3_1",       
   "response_time","response_time_Tasteresponse_3_1","subject_nr", 
   "time_Bildershuffle",               "time_Bildstimulus_3_1",           
   "time_Block", "time_Block_Tut",                  
  "time_Blockwerte_zuruecksetzten","time_CutoffWerteUndPunkte_3_1",
  "time_Eingabemaske_Kuerzel_python","time_EndeExperimentierungsphase",
  "time_Ende_Tutorial_1","time_FeedbackTutorial",
  "time_Feedback_Serie","time_Fixationcross_3_1",
  "time_Gruppe_Block","time_Gruppe_Tutorial",
  "time_Instruktionen_Block","time_Instruktionen_Tutorial_1",
  "time_Introduction_1","time_Introduction_2_1",
  "time_Introduction_3_Punkte","time_Introduction_3_Speed",
  "time_Introduction_4_Accuracy","time_Introduction_5_Feedback",
  "time_Leertaste_fuer_naechsten_Bildschirm","time_Loop_Block",
  "time_Loop_Block_Tut","time_Minifeedbacknegativ_acc",
  "time_Minifeedbacknegativ_speed_slow","time_Minifeedbacknegativ_speed_slowandwrong",
  "time_Minifeedbacknegativ_speed_wrong","time_Minifeedbackpositiv_acc",
  "time_Minifeedbackpositiv_speed","time_Pause_zwischen_Serie_1",
  "time_Pause_zwischen_Serie_2","time_Sequenz_Block",
  "time_Sequenz_Block_Tut","time_Sequenz_Block_Tut_1",
  "time_Sequenz_Gruppe","time_Sequenz_Gruppe_Tut",
  "time_Sequenz_Serie","time_Serie",
  "time_Start","time_Tasteresponse_3_1",
  "time_Testloopprepareglitchtutorial","time_Trial_3_1",
  "time_Tutorial","time_Tutorial_Wiederholungscheck",
  "time_Vorgeplaenkel","time_Warnhinweis_3Sek",
  "time_Warnhinweis_5Sek","time_WarnungTutorial_1",
  "time_definecorrectcolour_3_1","time_experiment",
  "time_grauesbild_3_1","time_logger_3_1",
  "time_new_inline_script","time_reset_feedback",
  "time_reset_feedback_1","time_set_to_zero",
  "time_tutorial_Ende","total_correct",
  "total_response_time","total_responses",
  "tutorial","tutorialpunkte")
  df <- df[ , names(df) %in% keeplist] %>% 
    mutate(
      difficulty = 3 - difficulty,
      difficulty_f = as.factor(difficulty),
      correct = as.factor(correct),
      correct1 = as.factor(correct1),
      correct2 = as.factor(correct2),
      correct3 = as.factor(correct3),
      code = paste0(.$id_vater,.$id_jahr, .$id_geburtsort),
      count_Block = 1 + count_Blockwerte_zuruecksetzten
    ) %>% 
    select(!count_Blockwerte_zuruecksetzten) %>% 
    clean_names()
  df <- df %>%  select(order(colnames(df)))
  levels(df$difficulty_f) <-
    c("sehr leicht", "eher leicht", "eher schwer", "sehr schwer")
  
  df_list <- split(df, df$code)
  
  j=1
  # loop over list of data frames and write each to a CSV file
  for (i in seq_along(df_list)) {
    subj_nr <- as.numeric(unique(df["code" == names(df_list)[i],"subject_nr"]))
    
    filename <- paste0("subject_", i, ".csv")
    write.csv(df_list[[i]], file = filename, row.names = FALSE)
  }
  
  
  
  # df_clean <-  df[,!variabledellist]
  # variable.names(df_clean)
  # unique_count <- apply(df, 2, function(x) length(unique(x)))
  # only_one_unique <- which(unique_count == 1)
  # 
  # # print the column names with only 1 unique value
  # test <- (colnames(df)[only_one_unique])
  # dellist = c("Dummy",        
  # "background",   
  # "canvas_backend",      "clock_backend",       "color_backend",      
  # "compensation", "coordinates",  "correct_Leertaste_fuer_naechsten_Bildschirm" ,
  # "count_Block_Tut",     "count_Eingabemaske_Kuerzel_python" ,           "count_EndeExperimentierungsphase"            ,
  # "count_Ende_Tutorial_1", "count_Introduction_1",  "count_Introduction_2_1"                      ,
  # "count_Introduction_3_Punkte"      ,            "count_Introduction_3_Speed"    ,               "count_Introduction_4_Accuracy"    ,           
  # "count_Introduction_5_Feedback"   ,             "count_Sequenz_Block_Tut"        ,              "count_Serie", 
  # "count_Start",  "count_Testloopprepareglitchtutorial"   ,       "count_Tutorial",     
  # "count_Tutorial_Wiederholungscheck" ,           "count_Vorgeplaenkel", "count_WarnungTutorial_1" ,                    
  # "count_experiment",    "count_tutorial_Ende",     
  # "description",  "disable_garbage_collection"   ,               
  # "empty_column", "erstefarbedeutsch",  
  # "font_bold",   
  # "font_family",  "font_italic",  "font_size",   
  # "font_underline",      "form_clicks", 
  # "form_response",       "fullscreen",         
  # "keyboard_backend",    
  # "linketaste2",  "live_row_Testloopprepareglitchtutorial",     
  # "mouse_backend",       "opensesame_codename", "opensesame_version", 
  # "psychopy_waitblanking"  ,                     
  # "randomwalk",
  # "repeat_cycle",
  # "round_decimals",      "sampler_backend",     "sound_buf_size",     
  # "sound_channels",      "sound_freq",   "sound_sample_size",  
  # "start", 
  # "title",        "uniform_coordinates", "warning1",
  #  "warning2",     "width",       "zweitefarbe", 
  #  "zweitefarbedeutsch")