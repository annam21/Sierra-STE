# Running a bunch of occasions for the same species
# Anna Moeller 
# 4/7/21

# Try to make a dataframe with different occ types
study_dates <- as.POSIXct(c("2015-08-27 00:00:00", 
                            "2018-02-20 00:00:00"), 
                          tz = "GMT")

sf <- 3600
# occ1 <- build_occ(samp_freq = sf, # seconds between the start of each sampling occasion
#                  samp_length = 10, # duration of each sampling occasion (seconds)
#                  study_start = study_dates[1],
#                  study_end = study_dates[2])
# occ2 <- build_occ(samp_freq = sf, # seconds between the start of each sampling occasion
#                   samp_length = 10, # duration of each sampling occasion (seconds)
#                   study_start = study_dates[1] + 600,
#                   study_end = study_dates[2])

# Calculate every 10 minutes, up to samp_freq
st <- study_dates[1] + seq(0, sf-1, by = 600)
# tst <- lapply(st, build_occ, samp_freq = sf, samp_length = 10, study_end = study_dates[2])
tst2 <- tibble(
  occ = lapply(st, build_occ, samp_freq = sf, samp_length = 10, study_end = study_dates[2])
)
tst3 <- tibble(
  occ = map(
    st, 
    ~build_occ(
      samp_freq = sf, 
      samp_length = 10, 
      study_start = .x, 
      study_end = study_dates[2]
    )
  )
)
# tst2 and tst3 are equivalent 

x <- crossing(
  sp = c("deer","wolf"),
  tst2
)


# This is coming along great. 

# Assign random seconds to our problem cameras 

# Visually decided the problem cameras are 
prob <- c("1_1", "10_1", "11_1", "16_1", "3_1", "4_1", "5_1", "6_1")

# Proof of concept 
# Fix problem ones
# x <- df %>% 
#   filter(cam %in% prob,
#          lubridate::second(datetime)==0) %>% 
#   slice(1:2) %>% 
#   .$datetime
# lubridate::second(x) <- c(14, 15)

# Fix problem ones
x <- df %>% 
  filter(cam %in% prob,
         lubridate::second(datetime) == 0) 
lubridate::second(x$datetime) <- runif(nrow(x), 0, 59)

# Add back to old ones
dfnew <- df %>% 
  filter(!(cam %in% prob & lubridate::second(datetime) == 0)) %>% 
  bind_rows(., x)


