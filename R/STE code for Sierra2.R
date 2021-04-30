# Assign random seconds to our problem cameras 
## Visually decided the problem cameras are 
prob <- c("1_1", "10_1", "11_1", "16_1", "3_1", "4_1", "5_1", "6_1")

# Fix problem ones
# This works if your dataset is called df_0s
set.seed(23)
fixed <- df_0s %>% 
  filter(cam %in% prob,
         lubridate::second(datetime) == 0) 
lubridate::second(fixed$datetime) <- runif(nrow(fixed), 0, 59)
# Add back to old ones
dfnew <- df_0s %>% 
  filter(!(cam %in% prob & lubridate::second(datetime) == 0)) %>% 
  bind_rows(., fixed)

# Take out problem cameras from the predators 
# This is for deploy called dep and data called dfnew 
dep <- dep %>% 
  filter(!(cam %in% prob))
dfnew <- dfnew %>% 
  filter(!(cam %in% prob))

# Once you run your estimates, you'll get out something called NHat
# Calculate abundance and its SE from the densities
Nhat %>% 
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