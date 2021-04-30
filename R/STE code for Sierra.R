# STE by Anna 
# 3/12/2021
# Anna Moeller 

library(spaceNtime)
library(tidyverse)

# Read in data and clean up names, dates, create cam_rotation
deploy <- read_csv("deploywithstudyarea.csv") %>% 
  mutate(cam = paste(Camera, Rotation, sep = "_"),
         start = as.POSIXct(Start, format = "%m/%d/%y", tz = "GMT"),
         end = as.POSIXct(End, format = "%m/%d/%y", tz = "GMT")) %>%
  rename(area = Area) %>%
  select(cam, area, start, end, study_area)

dat1 <- read_csv("MOOSE AND DEER DATA FINAL.csv") %>% 
  mutate(cam = paste(cam_N, rotation_N, sep = "_"),
         dt  = paste(as.character(date), as.character(time)),
         datetime = as.POSIXct(dt, format = "%m/%d/%y %H:%M:%S", tz = "GMT"),
         count = 1) %>%
  select(cam, datetime, species, count)

dat2 <- read_csv("threepredators.csv")  %>%
  mutate(cam = paste(CAM_ID, ROT_N, sep = "_"),
         dt = paste(DATE, `TIME (HH:MM:SS)`),
         datetime = as.POSIXct(dt, format = "%m/%d/%Y %H:%M:%S", tz = "GMT"),
         species = case_when(DET_TYPE == "BB" ~ "black bear",
                             DET_TYPE == "ML" ~ "mountain lion",
                             DET_TYPE == "W" ~ "wolf"))  %>%
  select(cam, datetime, species, count)

# Combine ungulates with predators 
df <- bind_rows(dat1, dat2)

# Prep stuff for STE
study_dates <- as.POSIXct(c("2015-08-27 00:00:01", 
                            "2018-02-20 00:00:01"), 
                          tz = "GMT")

occ <- build_occ(samp_freq = 300, # seconds between the start of each sampling occasion
                 samp_length = 10, # duration of each sampling occasion (seconds)
                 study_start = study_dates[1],
                 study_end = study_dates[2])

# Create a function that filters df and deploy to study area and species, then 
#  builds that EH then runs STE. It prints NA if there were no observations
runests <- function(sp, st_area = "both"){
  sdat <- df %>% 
    filter(species == sp)
  
  # If separated by study_area, filter deploy
  if(!(st_area == "both")){
    deploy <- deploy %>% 
      filter(study_area == st_area)
    
    sdat <- sdat %>% 
      filter(cam %in% deploy$cam)
  }
  
  ste_eh <- ste_build_eh(sdat, deploy, occ)
  if(all(is.na(ste_eh$STE))){
    out <- tibble::tibble(N = NA, SE = NA, LCI = NA, UCI = NA)
  } else {
    out <- ste_estN_fn(ste_eh, study_area = 1e8)
  }
  return(out)
}

# Write out all the combinations we want to run
x <- expand_grid(
  sp = c("deer", "moose", "black bear", "mountain lion", "wolf"),
  st_area = c("N", "S", "both")
)

# Run the function on all the combinations 
# This took ~17 minutes for 5 minute intervals 
Nhat <- x %>%
  bind_cols(
    purrr::pmap_dfr(.l = ., 
                    .f = runests)
  )

# Rearrange results so we can look at them in a nice table
Nhat %>% 
  select(sp, st_area, N) %>% 
  pivot_wider(names_from = st_area, values_from = N)
