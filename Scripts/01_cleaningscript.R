if (!require("pacman"))
  install.packages("pacman")
library(pacman)
p_load(tidyverse, janitor, httr, jsonlite)

#cdd <- read_csv("C:/Users/steff/Documents/Universität/Master Psychologie/Masterarbeit/Skripte/Daten_cleaned/df_min.csv") #TODO
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets Path to where file is downloaded
dir.create(path = "Data_cleaned")
options(encoding = "UTF-8")
ds = read.delim(#boilerplate for data from soscisurvey, the raw file is downloaded from github.
  
  file = "https://raw.githubusercontent.com/steffenernst96/master-thesis/main/data_raw/rdata_sozialdemographiefragebogenPSYHD_2023-04-20_15-20.csv", #this loads the demographic variables
  encoding = "UTF-8",
  fileEncoding = "UTF-8",
  header = FALSE,
  sep = "\t",
  quote = "\"",
  dec = ".",
  row.names = NULL,
  col.names = c(
    "CASE",
    "SERIAL",
    "REF",
    "QUESTNNR",
    "MODE",
    "STARTED",
    "CO01_01",
    "CO01_02",
    "CO01_03",
    "SD01",
    "SD02_01",
    "SD03",
    "SD03_01",
    "SD03_01a",
    "SD04",
    "SD05_01_CN",
    "SD05_01_1",
    "SD05_01_2",
    "SD05_01_3",
    "SD05_02_CN",
    "SD05_02_1",
    "SD05_02_2",
    "SD05_02_3",
    "SD05_03_CN",
    "SD05_03_1",
    "SD05_03_2",
    "SD05_03_3",
    "SD05_04_CN",
    "SD05_04_1",
    "SD05_04_2",
    "SD05_04_3",
    "TIME001",
    "TIME002",
    "TIME003",
    "TIME004",
    "TIME_SUM",
    "MAILSENT",
    "LASTDATA",
    "FINISHED",
    "Q_VIEWER",
    "LASTPAGE",
    "MAXPAGE",
    "MISSING",
    "MISSREL",
    "TIME_RSI",
    "DEG_TIME"
  ),
  as.is = TRUE,
  colClasses = c(
    CASE = "numeric",
    SERIAL = "character",
    REF = "character",
    QUESTNNR = "character",
    MODE = "factor",
    STARTED = "POSIXct",
    CO01_01 = "character",
    CO01_02 = "character",
    CO01_03 = "character",
    SD01 = "numeric",
    SD02_01 = "numeric",
    SD03 = "numeric",
    SD03_01 = "logical",
    SD03_01a = "character",
    SD04 = "numeric",
    SD05_01_CN = "numeric",
    SD05_01_1 = "logical",
    SD05_01_2 = "logical",
    SD05_01_3 = "logical",
    SD05_02_CN = "numeric",
    SD05_02_1 = "logical",
    SD05_02_2 = "logical",
    SD05_02_3 = "logical",
    SD05_03_CN = "numeric",
    SD05_03_1 = "logical",
    SD05_03_2 = "logical",
    SD05_03_3 = "logical",
    SD05_04_CN = "numeric",
    SD05_04_1 = "logical",
    SD05_04_2 = "logical",
    SD05_04_3 = "logical",
    TIME001 = "integer",
    TIME002 = "integer",
    TIME003 = "integer",
    TIME004 = "integer",
    TIME_SUM = "integer",
    MAILSENT = "POSIXct",
    LASTDATA = "POSIXct",
    FINISHED = "logical",
    Q_VIEWER = "logical",
    LASTPAGE = "numeric",
    MAXPAGE = "numeric",
    MISSING = "numeric",
    MISSREL = "numeric",
    TIME_RSI = "numeric",
    DEG_TIME = "numeric"
  ),
  skip = 1,
  check.names = TRUE,
  fill = TRUE,
  strip.white = FALSE,
  blank.lines.skip = TRUE,
  comment.char = "",
  na.strings = ""
)

ds <- ds %>%
  mutate(
    haendig1 = ifelse(ds$SD05_01_1, 1, # converts 3 binary variables to a single one. Question about left/righthandedness. 1 = left, 2 = both, 3 = right
                      ifelse(
                        ds$SD05_01_2, 2,
                        ifelse(ds$SD05_01_3, 3, NA)
                      )),
    haendig2 = ifelse(ds$SD05_02_1, 1,
                      ifelse(
                        ds$SD05_02_2, 2,
                        ifelse(ds$SD05_02_3, 3, NA)
                      )),
    haendig3 = ifelse(ds$SD05_03_1, 1,
                      ifelse(
                        ds$SD05_03_2, 2,
                        ifelse(ds$SD05_03_3, 3, NA)
                      )),
    haendig4 = ifelse(ds$SD05_04_1, 1,
                      ifelse(
                        ds$SD05_04_2, 2,
                        ifelse(ds$SD05_04_3, 3, NA)
                      )),
    code = tolower(paste0(CO01_01, CO01_02, CO01_03)) #code to combine datasets.
  ) %>%
  slice(-1) %>%  #empty row
  rowwise() %>%
  mutate(haendig_mean = mean(c(haendig1, haendig2, haendig3, haendig4))) %>% #avg. score for handedness variables
  select( #select variables that are of interest
    c(
      "SD01",
      "SD02_01",
      #Alter
      "SD03_01a",
      # Studiengang
      "SD04",
      #Bildungsabschluss
      "haendig1",
      "haendig2",
      "haendig3",
      "haendig4",
      "haendig_mean",
      "code"
    )
  )
colnames(ds) <-
  c(
    "d_geschlecht",
    "d_alter",
    "d_studiengang",
    "d_bildungsabschluss",
    "d_haendig1",
    "d_haendig2",
    "d_haendig3",
    "d_haendig4",
    "haendig_mean",
    "code"
  )


response <- GET("https://api.github.com/repos/steffenernst96/master-thesis/contents/data_raw") #get urls of raw data
files <- fromJSON(content(response, "text")) #get files from github

download_url <- files %>%
  filter(grepl("^subject", name)) %>% #get all data except sociodemographic raw data
  pull(download_url)
df <- download_url %>% 
  map_df(~ read.csv(.))


keeplist <- #variables of interest
  c(
    keeplist <-
      c(
        "blauetaste",
        "block",
        "correct",
        "correct1",
        "correct2",
        "correct3",
        "correct_colour",
        "correct_response",
        "count_Blockwerte_zuruecksetzten",
        "cutoff",
        "difficulty",
        "erstebedingung",
        "id_geburtsort",
        "id_jahr",
        "id_vater",
        "linketaste",
        "loose1",
        "loose2",
        "loose3",
        "punkte1",
        "punkteimblock",
        "response",
        "response_time",
        "subject_nr",
        "tutorial"
      )
  )
df <- df[, names(df) %in% keeplist] %>% #only keep variables of interest
  mutate( #some cleaning
    difficulty = 3 - difficulty,
    difficulty_f = as.factor(difficulty),
    correct = as.numeric(correct),
    correct1 = as.factor(correct1),
    correct2 = as.factor(correct2),
    correct3 = as.factor(correct3),
    code = paste0(.$id_vater, .$id_jahr, .$id_geburtsort), #individual code to link demographic variables to data from experiment
    count_Block = 1 + count_Blockwerte_zuruecksetzten,
    rt = response_time / 1000,
    rt_in_ms = response_time,
    speed_condition = ifelse(block == 'speed', 1, 0),
    resp = if_else(response == "a", 1, 0),
    correct_resp = if_else(correct_response == "a", 1, 0),
    id = dense_rank(factor(subject_nr, levels = unique(subject_nr)))
  )  %>%
  select(!c( #remove no longer needed variables
    count_Blockwerte_zuruecksetzten,
    id_vater,
    id_jahr,
    id_geburtsort,
    correct_response,
    response,
    response_time
  ))

df <- df %>%  select(order(colnames(df))) #order alphabetically / according to subject-nr
levels(df$difficulty_f) <-
  c("sehr leicht", "eher leicht", "eher schwer", "sehr schwer")
df <- left_join(df, ds, by = "code")  %>% #combine demographic variables with data from experiment
  clean_names()

df_list <-
  split(df, df$code) #split into multiple dataframes


# loop over list of data frames and write each to a local CSV file
for (i in seq_along(df_list)) {
  filename <-
    paste0("subject_", unique(df_list[[i]]$subject_nr), ".csv") #Name to Subject-Nr.
  write.csv(df_list[[i]],
            file = paste0("Data_cleaned\\", filename),
            row.names = FALSE)
}
write.csv(df, file = "Data_cleaned\\df_all.csv")
df_min <- df %>%
  select(rt,
         rt_in_ms,
         correct,
         speed_condition,
         difficulty,
         code,
         tutorial,
         subject_nr,
         id)
write.csv(df_min, file = "Data_cleaned\\df_min.csv")


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
