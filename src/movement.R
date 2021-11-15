#-------------------------------------------------------------

# Bison movement data.

df_movement <- read_csv("./data/base/bisonMay_Aug_20.csv") %>%
  rename(id = 1) %>%
  select(id, gateway_time, longitude, latitude) %>%
  arrange(id, gateway_time) %>%
  # Only want up to July 25
  filter(gateway_time < "2020-07-26")


# How many observations, generally?
high_obs_animals <- df_movement %>%
  group_by(id) %>%
  tally() %>%
  filter(n > 100) %>%
  select(id) %>%
  pull()

df_movement <- df_movement %>%
  filter(id %in% high_obs_animals) %>%
  group_by(id, gateway_time) %>%
  summarise(longitude = mean(longitude),
            latitude = mean(latitude)) %>%
  ungroup() %>%
  arrange(id, gateway_time)


