# Finding lost seconds 

df %>% 
  filter( 
    species %in% c("black bear", "mountain lion", "wolf"),
    lubridate::second(datetime) == 0) %>% 
  arrange(cam) %>%
  View


# Instead...
tosierra <- dat2 %>% 
  filter(
    lubridate::second(datetime) == 0
  ) %>% 
  arrange(ROT_N, CAM_ID) 

write_csv(tosierra, "problems/seconds0.csv")

# Visually, Anna decided the problem cameras are: 
c("1_1", "10_1", "11_1", "14_6", "16_1", "3_1", "4_1", "5_1", "6_1")
# And actually, let's say that 14_6 is some other problem/duplicate
prob <- c("1_1", "10_1", "11_1", "16_1", "3_1", "4_1", "5_1", "6_1")
