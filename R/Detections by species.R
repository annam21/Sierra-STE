# Occasions with detections
# Anna Moeller
# 6/2/2021

library(tidyverse)
library(spaceNtime)

# Read in data
dep <- read_csv("data/deploywithstudyarea.csv") %>% 
  mutate(cam = paste(Camera, Rotation, sep = "_"),
         start = as.POSIXct(Start, format = "%m/%d/%y", tz = "GMT"),
         end = as.POSIXct(End, format = "%m/%d/%y", tz = "GMT")) %>%
  rename(area = Area) %>%
  select(cam, area, start, end, study_area)

datung <- read_csv("data/MOOSE AND DEER DATA FINAL.csv") %>% 
  mutate(cam = paste(cam_N, rotation_N, sep = "_"),
         dt  = paste(as.character(date), as.character(time)),
         datetime = as.POSIXct(dt, format = "%m/%d/%y %H:%M:%S", tz = "GMT"),
         count = 1) %>%
  select(cam, datetime, species, count)

datpred <- read_csv("data/threepredators_fixedseconds.csv")  %>%
  mutate(cam = paste(CAM_ID, ROT_N, sep = "_"),
         dt = paste(DATE, `TIME (HH:MM:SS)`),
         datetime = as.POSIXct(dt, format = "%m/%d/%Y %H:%M:%S", tz = "GMT"),
         species = case_when(DET_TYPE == "BB" ~ "black bear",
                             DET_TYPE == "ML" ~ "mountain lion",
                             DET_TYPE == "W" ~ "wolf"))  %>%
  select(cam, datetime, species, count)

# Instead of fixing prob cameras, take them out instead. 
prob <- c("1_1", "3_1", "4_1", "5_1", "6_1")
dat_nopred <- datpred %>% 
  filter(!(cam %in% prob))
dep_nopred <- dep %>% 
  filter(!(cam %in% prob))

# Function to run a bunch of stuff at once
geteh <- function(df, deploy, sp, st_area = "both", occ){
  # Flaws in this function: 
  # 1. dplyr got angry at function arguments being called the same thing as the
  #   column names in df and deploy. I had to change function arguments to 
  #   sp and st_area instead of species and study_area. Could instead use dplyr 
  #   tools to make it recognize name vs. object 
  sdat <- df %>% 
    filter(species == sp)
  
  # If separated by study_area, filter deploy
  if(!(st_area == "both")){
    deploy <- deploy %>% 
      filter(study_area == st_area)
    
    sdat <- sdat %>% 
      filter(cam %in% deploy$cam)
  }
  
  ste_eh <- ste_build_eh(sdat, deploy, occ, quiet = T)
  out <- ste_eh %>% 
    mutate(species = sp, 
           study_area = st_area)
 
  return(out)
}

# New study dates
study_dates <- as.POSIXct(c("2015-08-27 00:00:00", 
                            "2018-02-20 00:00:00"), 
                          tz = "GMT")

# For just one set of occasions
# This took ~17 minutes for 5 minute intervals
occas <- build_occ(samp_freq = 300, # seconds between the start of each sampling occasion
                   samp_length = 10, # duration of each sampling occasion (seconds)
                   study_start = study_dates[1],
                   study_end = study_dates[2])

xung <- crossing(
  sp = c("deer", "moose"),
  st_area = c("N", "S", "both")
  # st_area = "both"
)

xpred <- crossing(
  sp = c("black bear", "mountain lion", "wolf"),
  st_area = c("N", "S", "both")
)

# Run all estimates
ehung <- xung %>%
  purrr::pmap_dfr(
    .l = .,
    .f = geteh,
    df = datung,
    deploy = dep,
    occ = occas
  )
ehpred <- xpred %>%
  purrr::pmap(
    .l = .,
    .f = geteh,
    df = dat_nopred,
    deploy = dep_nopred,
    occ = occas
  )

ehs <- bind_rows(ehung, ehpred)
# saveRDS(ehs, "results/STE_ehs_300_removeprobcams.rds")

# How many detections for each? 
table2 <- ehs %>% 
  filter(!is.na(STE)) %>% 
  count(species, study_area) %>%
  pivot_wider(names_from = study_area, 
              values_from = n)
write_csv(table2, "results/Table 2.csv")  

