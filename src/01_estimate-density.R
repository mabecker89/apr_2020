#-----------------------------------------------------------------------------------------------------------------------

# Title: Estimating Density
# Date: April 3, 2021
# Author: M.Becker

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages (and install if necessary)
pacman::p_load(readr, dplyr, tidyr, janitor)

# Load APR data
df_apr <- read_csv("./data/base/SCBI_2020_camera_trap_survey.csv") %>%
  clean_names()

# Species of interest (most abundant; but we can look at others later, too)
sp <- c("American bison", "Mule deer", "Pronghorn", "White-tailed deer", "Coyote")

# Predicted probabilities of leaving the camera field of view (of various image gap lengths) - from ABMI.
df_leave_prob_pred <- read_csv("./data/lookup/gap-leave-prob_predictions_2020-06-25.csv")
# Species gap groups
df_gap_groups <- read_csv("./data/lookup/species-gap-groups.csv") %>%
  mutate(common_name = case_when(
    common_name == "Bison" ~ "American bison",
    common_name == "White-tailed Deer" ~ "White-tailed deer",
    TRUE ~ common_name
  ))

# Detection distances (from ABMI - we'll need to revise later.)
df_edd <- read_csv("./data/lookup/edd-veghf-season_predictions.csv")
# Groups
df_edd_groups <- read_csv("./data/lookup/species-distance-groups.csv") %>%
  mutate(common_name = case_when(
    common_name == "Bison" ~ "American bison",
    common_name == "White-tailed Deer" ~ "White-tailed deer",
    TRUE ~ common_name
  ))

cam_fov_angle <- 37.7

#-----------------------------------------------------------------------------------------------------------------------

# Identify Series
df_series <- df_apr %>%
  # Relevant fields:
  select(deployment_id, timestamp = timestamp2, species = species_common_name, number = sighting_quantity) %>%
  # Only look at most common species
  filter(species %in% sp) %>%
  # Order observations
  arrange(deployment_id, timestamp, species) %>%
  # Identify series
  mutate(series_num = 0,
         timestamp_previous = lag(timestamp),
         diff_time = as.numeric(timestamp - timestamp_previous),
         species_previous = lag(species),
         diff_sp = ifelse(species != species_previous, TRUE, FALSE),
         deployment_id_previous = lag(deployment_id),
         diff_deployment = ifelse(deployment_id != deployment_id_previous, TRUE, FALSE),
         diff_series = ifelse(diff_deployment == TRUE | diff_sp == TRUE | diff_time > 120, 1, 0),
         series_num = c(0, cumsum(diff_series[-1]))) %>%
  # Join gap group lookup
  left_join(df_gap_groups, by = c("species" = "common_name")) %>%
  # Join predictions
  left_join(df_leave_prob_pred, by = c("gap_group", "diff_time")) %>%
  # Adjust time difference between ordered images that require probabilistic gap assignment
  mutate(pred = replace_na(pred, 0),
         diff_time_adj = round(diff_time * (1 - pred), digits = 2))

#-----------------------------------------------------------------------------------------------------------------------

# Calculate time between images (tbi), by species
df_tbi <- df_series %>%
  mutate(series_num_previous = lag(series_num)) %>%
  # Remove first image from each series
  filter(series_num == series_num_previous) %>%
  group_by(species) %>%
  # Calculate average tbi and n of images from each series
  summarise(tbi = mean(diff_time),
            sample_size = n())

# Calculate total time in front of camera, by series

# Starting with series that have >1 image
df_tts_multiple <- df_series %>%
  # Remove first image from dataframe (as to not include diff_time in time totals for the series)
  mutate(series_num_previous = lag(series_num)) %>%
  filter(series_num_previous == series_num) %>%
  group_by(series_num) %>%
  summarise(n_images = n() + 1,
            total_time = sum(diff_time_adj))

# Next, single-image series'
df_tts_single <- df_series %>%
  group_by(series_num) %>%
  summarise(n_images = n()) %>%
  # Keep only series with 1 image
  filter(n_images == 1) %>%
  mutate(total_time = 0)

# Bind together
df_tts_all <- df_tts_multiple %>%
  bind_rows(df_tts_single) %>%
  arrange(series_num)

# Add time between photos, accounting for the average number of animals in each photo of a series
df_tts_final <- df_series %>%
  # Join in average tbp, by species
  left_join(df_tbi, by = "species") %>%
  select(series_num, species, number, tbi) %>%
  group_by(series_num) %>%
  # Number of individuals in a series - we could do this more accurately (time-weighted)
  mutate(avg_number = mean(number)) %>%
  ungroup() %>%
  select(-number) %>%
  distinct() %>%
  left_join(df_tts_all, by = "series_num") %>%
  # Add time between images, and multiply by average number of individuals in series
  mutate(series_total_time = (total_time + tbi) * avg_number) %>%
  select(series_num, species, n_images, series_total_time)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate total time (tt) in front of camera, by deployment and species

# Operational duration information
df_durations <- df_apr %>%
  # Just by days for now - could do by hours/minutes etc to be more precise.
  select(deployment_id, duration_days) %>%
  distinct() %>%
  mutate(duration_days = round(duration_days, digits = 0))

df_tt <- df_series %>%
  group_by(series_num) %>%
  arrange(timestamp) %>%
  filter(row_number() == 1) %>%
  left_join(df_tts_final, by = c("series_num", "species")) %>%
  select(series_num, deployment_id, timestamp, species, series_total_time) %>%
  ungroup() %>%
  mutate_at(c("deployment_id", "species"), factor) %>%
  group_by(deployment_id, species, .drop = FALSE) %>%
  summarise(total_duration = sum(series_total_time)) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  # Join duration information
  left_join(df_durations, by = "deployment_id")

# For deployments with no images of our species of interest:

# Vector of all deployments
dep <- df_apr %>%
  select(deployment_id) %>%
  distinct() %>%
  pull()

df_tt_none <- df_durations %>%
  # Retrieve only those that had no images of native species
  anti_join(df_tt, by = "deployment_id") %>%
  expand(deployment_id, species = sp) %>%
  # Re-join time-by-day information
  left_join(df_durations, by = "deployment_id") %>%
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0)

df_tt_full <- df_tt %>%
  bind_rows(df_tt_none) %>%
  arrange(deployment_id, species) %>%
  rename(time_in_fov_s = total_duration)

# Write processed results
write_csv(df_tt_full, "./data/processed/time-in-fov-species-deployment.csv")

#-----------------------------------------------------------------------------------------------------------------------

# Vegetation information
df_veg <- df_apr %>%
  select(deployment_id, grass_cover) %>%
  distinct()

# Calculate density
df_density <- df_tt_full %>%
  # Append Effective Detection Distance (EDD) information (from ABMI - we'll need to update this.)
  left_join(df_edd_groups, by = c("species" = "common_name")) %>%
  left_join(df_veg, by = "deployment_id") %>%
  mutate(veg_edd = ifelse(grass_cover > 50, "Grass", "Shrub"),
         season = "summer") %>%
  left_join(df_edd, by = c("dist_group", "season", "veg_edd" = "VegForDetectionDistance")) %>%
  mutate(effort = duration_days * (detdist ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = total_duration / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000) %>%
  select(deployment_id:duration_days, density_km2 = cpue_km2)

#-----------------------------------------------------------------------------------------------------------------------





























