  if (!require("pacman")) install.packages("pacman"); library(pacman)
  p_load(tidyverse, janitor)
  setwd("C:/Users/steff/Documents/Universität/Master Psychologie/SS 2022/Masterarbeit/Experiment/Daten")
  options(encoding = "UTF-8")
  ds = read.delim( #Befehl für Sosci-Survey-Daten
    file="Demographie\\rdata_sozialdemographiefragebogenPSYHD_2023-04-20_15-20.csv", encoding="UTF-8", fileEncoding="UTF-8",
      header = FALSE, sep = "\t", quote = "\"",
      dec = ".", row.names = NULL,
      col.names = c(
        "CASE","SERIAL","REF","QUESTNNR","MODE","STARTED","CO01_01","CO01_02","CO01_03",
        "SD01","SD02_01","SD03","SD03_01","SD03_01a","SD04","SD05_01_CN","SD05_01_1",
        "SD05_01_2","SD05_01_3","SD05_02_CN","SD05_02_1","SD05_02_2","SD05_02_3",
        "SD05_03_CN","SD05_03_1","SD05_03_2","SD05_03_3","SD05_04_CN","SD05_04_1",
        "SD05_04_2","SD05_04_3","TIME001","TIME002","TIME003","TIME004","TIME_SUM",
        "MAILSENT","LASTDATA","FINISHED","Q_VIEWER","LASTPAGE","MAXPAGE","MISSING",
        "MISSREL","TIME_RSI","DEG_TIME"
      ),
      as.is = TRUE,
      colClasses = c(
        CASE="numeric", SERIAL="character", REF="character", QUESTNNR="character",
        MODE="factor", STARTED="POSIXct", CO01_01="character", CO01_02="character",
        CO01_03="character", SD01="numeric", SD02_01="numeric", SD03="numeric",
        SD03_01="logical", SD03_01a="character", SD04="numeric",
        SD05_01_CN="numeric", SD05_01_1="logical", SD05_01_2="logical",
        SD05_01_3="logical", SD05_02_CN="numeric", SD05_02_1="logical",
        SD05_02_2="logical", SD05_02_3="logical", SD05_03_CN="numeric",
        SD05_03_1="logical", SD05_03_2="logical", SD05_03_3="logical",
        SD05_04_CN="numeric", SD05_04_1="logical", SD05_04_2="logical",
        SD05_04_3="logical", TIME001="integer", TIME002="integer",
        TIME003="integer", TIME004="integer", TIME_SUM="integer",
        MAILSENT="POSIXct", LASTDATA="POSIXct", FINISHED="logical",
        Q_VIEWER="logical", LASTPAGE="numeric", MAXPAGE="numeric",
        MISSING="numeric", MISSREL="numeric", TIME_RSI="numeric", DEG_TIME="numeric"
      ),
      skip = 1,
      check.names = TRUE, fill = TRUE,
      strip.white = FALSE, blank.lines.skip = TRUE,
      comment.char = "",
      na.strings = "")
  ##Demographiefragebogen wird eingefügt
  ds <- ds %>% 
 #   slice(-1) %>% #erster Fragebogen fehlerhaft
  mutate(
    haendig1 = ifelse(ds$SD05_01_1, 1, # aus 3 Variablen wird 1 gemacht. 1 für links, 2 für beides, 3 für rechts.
                      ifelse(ds$SD05_01_2, 2,
                             ifelse(ds$SD05_01_3, 3, NA))),
    haendig2 = ifelse(ds$SD05_02_1, 1,
                      ifelse(ds$SD05_02_2, 2,
                             ifelse(ds$SD05_02_3, 3, NA))),
    haendig3 = ifelse(ds$SD05_03_1, 1,
                      ifelse(ds$SD05_03_2, 2,
                             ifelse(ds$SD05_03_3, 3, NA))),
    haendig4 = ifelse(ds$SD05_04_1, 1,
                      ifelse(ds$SD05_04_2, 2,
                             ifelse(ds$SD05_04_3, 3, NA))),
    code = tolower(paste0(CO01_01, CO01_02, CO01_03)) #code zum zusammenführen der Daten
    ) %>% 
    slice(-1) %>%  #leere Codezeile
  rowwise() %>% 
    mutate(haendig_mean = mean(c(haendig1, haendig2, haendig3, haendig4))) %>% #avg. score
    select(c("SD01",   
                                "SD02_01",#Alter
                                "SD03_01a", # Studiengang
                                "SD04", #Bildungsabschluss
                                "haendig1",
                                "haendig2",
                                "haendig3",
             "haendig4",
             "haendig_mean",
                                "code"))
  colnames(ds) <-  c(" Geschlecht", "Alter", "Studiengang", "Bildungsabschluss", "haendig1", "haendig2", "haendig3", "haendig4", "haendig_mean", "code")

  
  
  df <- list.files(pattern = "*.csv") %>% #alle Datensätze werden in einen großen Datensatz geladen
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
      difficulty = 3 - difficulty, #umcodierung Schwierigkeit
      difficulty_f = as.factor(difficulty),
      correct = as.factor(correct),
      correct1 = as.factor(correct1),
      correct2 = as.factor(correct2),
      correct3 = as.factor(correct3),
      code = paste0(.$id_vater,.$id_jahr, .$id_geburtsort), #code zum Zusammenführen
      count_Block = 1 + count_Blockwerte_zuruecksetzten #im wiewvielten Block ist man?
    ) %>% 
    select(!count_Blockwerte_zuruecksetzten)
  df <- df %>%  select(order(colnames(df))) #alphabetisch
  levels(df$difficulty_f) <-
    c("sehr leicht", "eher leicht", "eher schwer", "sehr schwer")
  df <- left_join(df,ds, by = "code")  %>% #zusammenführen nach code
    clean_names() 
    
  df_list <- split(df, df$code) #Wieder Aufteilen in mehrere Datensätze
  # loop over list of data frames and write each to a CSV file
  for (i in seq_along(df_list)) {
    #subj_nr <- 
    #  as.numeric(unique(df["code" == names(df_list)[i],"subject_nr"]))
    
    filename <- paste0("subject_", unique(df_list[[i]]$subject_nr), ".csv") #Name nach SUbject-Nr.
    write.csv(df_list[[i]], file = paste0("Daten_cleaned\\", filename), row.names = FALSE)
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