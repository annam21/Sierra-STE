# STE by Anna 
# 3/12/2021
# Anna Moeller 

library(spaceNtime)
library(tidyverse)
# library(assertr)

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

df_0s <- bind_rows(datung, datpred)

# Assign random seconds to our problem cameras 
## Visually decided the problem cameras are 
prob <- c("1_1", "3_1", "4_1", "5_1", "6_1")
# Fix problem ones
set.seed(23)
fixed <- df_0s %>% 
  filter(cam %in% prob,
         lubridate::second(datetime) == 0) 
lubridate::second(fixed$datetime) <- runif(nrow(fixed), 0, 59)
# Add back to old ones
dfnew <- df_0s %>% 
  filter(!(cam %in% prob & lubridate::second(datetime) == 0)) %>% 
  bind_rows(., fixed)

# Instead of fixing prob cameras, take them out instead. 
dat_nopred <- datpred %>% 
  filter(!(cam %in% prob))
dep_nopred <- dep %>% 
  filter(!(cam %in% prob))
# Going to have to run 2 diff deploys: 1 for ungulate, 1 for predators

# Function to run a bunch of stuff at once
runests <- function(df, deploy, sp, st_area = "both", occ){
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
  if(all(is.na(ste_eh$STE))){
    out <- tibble::tibble(N = NA, SE = NA, LCI = NA, UCI = NA)
  } else {
    out <- ste_estN_fn(ste_eh, study_area = 1e8)
  }
  return(out)
}

# # Old study dates
# study_dates <- as.POSIXct(c("2015-01-1 00:00:01", 
#                             "2018-02-01 00:00:01"), 
#                           tz = "GMT")
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
x <- crossing(
  # sp = c("deer", "moose", "black bear", "mountain lion", "wolf"),
  sp = c("black bear", "mountain lion", "wolf"),
  st_area = c("N", "S", "both")
)

# Run all estimates
Nhat <- x %>%
  bind_cols(
    purrr::pmap_dfr(
      .l = .,
      .f = runests,
      # df = dfnew,
      df = dat_nopred,
      # deploy = dep,
      deploy = dep_nopred,
      occ = occas)
  )

# Visualize data in wide format
Nhat %>%
  select(sp, st_area, N) %>%
  pivot_wider(names_from = st_area, values_from = N)

# saveRDS(Nhat, "results/STE_estimates300_correctprobcams_20210422.rds")
# saveRDS(Nhat, "results/STE_estimates300_removeprobcams_20210426.rds")

Nhat1 <- readRDS("results/STE_estimates300_correctprobcams_20210422.rds")
Nhat2 <- readRDS("results/STE_estimates300_removeprobcams_20210426.rds")

# # For multiple sets of occasions
# sf <- 600
# # Each set of occasions begins *by = 60 seconds* later
# st <- study_dates[1] + seq(0, sf-1, by = 60)
# occas <- tibble(
#   occ = lapply(st, build_occ, samp_freq = sf, samp_length = 10, 
#                study_end = study_dates[2])
# )
# 
# # Make all combinations of species/study area for the function above
# x <- crossing(
#   sp = c("deer", "moose", "black bear", "mountain lion", "wolf"),
#   st_area = c("N", "S", "both"),
#   occas
# )
# 
# # Run all estimates 
# s <- Sys.time()
# Nhat <- x %>%
#   bind_cols(
#     purrr::pmap_dfr(
#       .l = ., 
#       .f = runests,
#       df = dfnew,
#       deploy = dep)
# )
# Sys.time() - s
# 
# # Calculate 
# Nhat %>% 
#   group_by(sp, st_area) %>% 
#   mutate(var = SE^2) %>% 
#   summarize(
#     N = mean(N, na.rm = T),
#     # If independent, 
#     SE_N = (1/n())*sqrt(sum(var, na.rm = T))
#   ) %>% 
#   # Calculate logCIs
#   bind_cols(., logCI(.$N, .$SE_N)) %>% 
#   # See how many of the occasion sets got estimates
#   left_join(., Nhat %>% filter(!is.na(N)) %>% count(sp, st_area))
# 
# # Visualize data in wide format
# Nhat %>% 
#   group_by(sp, st_area) %>% 
#   summarize(N = mean(N, na.rm = T)) %>%
#   pivot_wider(names_from = st_area, values_from = N)
# 
# # saveRDS(Nhat, "results/STE_estimates_multioccasion_20210408.rds")
# Nhat <- readRDS("results/STE_estimates_multioccasion_20210408.rds")

# Code for Density
Nh <- Nhat %>% 
  group_by(sp, st_area) %>% 
  mutate(var = SE^2) %>% 
  summarize(
    N = mean(N, na.rm = T),
    # If independent, 
    SE_N = (1/n())*sqrt(sum(var, na.rm = T))
  ) %>% 
  # Calculate logCIs
  bind_cols(., logCI(.$N, .$SE_N)) %>% 
  # See how many of the occasion sets got estimates
  left_join(., Nhat %>% filter(!is.na(N)) %>% count(sp, st_area))

Nh %>% 
  rename(D = N, SE_D = SE_N) %>%
  mutate(
    A = case_when(
      st_area == "N" ~ 775e6,
      st_area == "S" ~ 477e6,
      st_area == "both" ~ 1.252e9
    ),
    N = D/1e8 * A,
    SE_N = SE_D/1e8 * A
  ) %>%
  select(-n, -LCI, -UCI)

# # Split everything up by species 
# deer <- df %>%
#   filter(species == "deer")
# elk <- df %>% 
#   filter(species == "elk")
# moose <- df %>% 
#   filter(species == "moose")
# bear <- df %>%
#   filter(species == "black bear")
# lion <- df %>%
#   filter(species == "mountain lion")
# wolf <- df %>%
#   filter(species == "wolf")
# 
# # Run estimates of both study areas combined 
# deer_eh <- ste_build_eh(deer, deploy, occ)
# ste_estN_fn(deer_eh, study_area = 1e8)
#          
# elk_eh <- ste_build_eh(elk, deploy, occ)
# ste_estN_fn(elk_eh, study_area = 1e8)
# 
# moose_eh <- ste_build_eh(moose, deploy, occ)
# ste_estN_fn(moose_eh, study_area = 1e8)
# 
# bear_eh <- ste_build_eh(bear, deploy, occ)
# ste_estN_fn(bear_eh, study_area = 1e8)
# 
# lion_eh <- ste_build_eh(lion, deploy, occ)
# ste_estN_fn(lion_eh, study_area = 1e8)
# 
# wolf_eh <- ste_build_eh(wolf, deploy, occ)
# ste_estN_fn(wolf_eh, study_area = 1e8)


# Data checks/fixes
# # Look at problem cameras in the data that don't match up with deploy
# df %>% 
#   group_by(cam) %>% 
#   summarize(min = min(datetime),
#             max = max(datetime)) %>%
#   filter(min < study_dates[1] | max > study_dates[2])
# 
# # Look at all the problem photos 
# probs <- df %>% 
#   filter(datetime < study_dates[1] | 
#            datetime > study_dates[2]) %>%
#   group_by(cam) %>% 
#   # mutate(n = n())
#   arrange(n())
# f <- file.path("problems")
# dir.create(f)
# write_csv(probs, "problems/photos_outside_of_deploy.csv")

# # Look at bears
# depS <- deploy %>% 
#   filter(study_area == "S")
# bear <- df %>% 
#   filter(species == "black bear",
#          cam %in% depS$cam)
# 
# bear_eh <- ste_build_eh(bear, depS, occ)

# See if there are photos in each rotation

# # Code our "bootstrap" for multiple estimates 
# occ <- build_occ(samp_freq = 3600, # seconds between the start of each sampling occasion
#                  samp_length = 10, # duration of each sampling occasion (seconds)
#                  study_start = study_dates[1],
#                  study_end = study_dates[2])


# # Make code faster - take out effort 
# # I don't know why, but validate_df isn't exporting... 
# validate_df <- function(df){
#   df %>%
#     verify(has_all_names("cam", "datetime", "count")) %>%
#     verify(lubridate::is.POSIXct(datetime)) %>% 
#     verify(is.numeric(count)) %>% 
#     verify(count >= 0)
# }
# ste_build_eh <- function(df, deploy, occ, eff, ...){
#   tictoc::tic("data checks")
#   # Run all my data checks here
#   df <- validate_df(df)
#   deploy <- validate_deploy(deploy)
#   occ <- validate_occ(occ)
#   
#   # Forcing a data subset so I can validate df and deploy together. 
#   # Subset is not technically necessary because everything hinges on occ later.
#   d1 <- min(occ$start)
#   d2 <- max(occ$end)
#   df_s <- study_subset(df, "datetime", NULL, d1, d2)
#   deploy_s <- study_subset(deploy, "start", "end", d1, d2)
#   
#   # Then validate df and deploy together (should really do after subset)
#   validate_df_deploy(df_s, deploy_s) # This one is weird because it doesn't return anything if all good...
#   
#   tictoc::toc(...)
#   
#   # Calculate the censors
#   tictoc::tic("calculate censors")
#   censor <- ste_calc_censor(eff)
#   tictoc::toc(...)
#   
#   # Calculate STE at each occasion
#   tictoc::tic("calculate STE")
#   out <- ste_calc_toevent(df_s, occ, eff)   %>%
#     mutate(censor = censor$censor)
#   tictoc::toc(...)
#   
#   return(out)
# }
# 
# # Build effort for each cam at each occasion
# # Need one for each study area 
# effS <- deploy %>% 
#   filter(study_area == "S") %>% 
#   effort_fn(., occ)
# effN <- deploy %>% 
#   filter(study_area == "N") %>% 
#   effort_fn(., occ)
# effboth <- effort_fn(deploy, occ)
# 
# # ste_build_eh(df, deploy, occ, effboth)
# 
# runests <- function(sp, st_area = "both", eff){
#   sdat <- df %>% 
#     filter(species == sp)
# 
#   # If separated by study_area, filter deploy
#   if(!(st_area == "both")){
#     deploy <- deploy %>% 
#       filter(study_area == st_area)
#     
#     sdat <- sdat %>% 
#       filter(cam %in% deploy$cam)
#   }
#   
#   ste_eh <- ste_build_eh(sdat, deploy, occ)
#   if(all(is.na(ste_eh$STE))){
#     out <- tibble::tibble(N = NA, SE = NA, LCI = NA, UCI = NA)
#   } else {
#     out <- ste_estN_fn(ste_eh, study_area = 1e8)
#   }
#   return(out)
# }
# 
# x <- expand_grid(
#   sp = c("deer", "moose", "black bear", "mountain lion", "wolf"),
#   st_area = c("N", "S", "both")
#   # st_area = "both"
# ) %>%
#   # Not the greatest way to make this happen, but it's all I can come up with
#   mutate(eff = rep(list(effN, effS, effboth), 5) )
# 
# Nhat <- x[1:2,] %>%
#   bind_cols(
#     purrr::pmap_dfr(.l = ., 
#                     .f = runests)
#   )
# 
# Nhat %>% 
#   select(sp, st_area, N) %>% 
#   pivot_wider(names_from = st_area, values_from = N)