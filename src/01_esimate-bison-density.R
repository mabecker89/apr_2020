#-----------------------------------------------------------------------------------------------------------------------

# American Prairie Reserve - Bison Camera Project

# Joke: What did the father buffalo say to his son when he left for college? Bi-son.

# Date: August 2021
# Author: M.Becker

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages:
pacman::p_load(readr, dplyr, tidyr, janitor, lubridate, stringr)

# Species of interest
sp <- "American bison"

# Camera field of view angle (taken directly from the data)
cam_fov_angle <- 37.7

# Load American Prairie Reserve (APR) data
df_apr <- read_csv("./data/base/apr_revised_data.csv") %>%
  clean_names() %>%
  # Consistent deployment naming
  mutate(deployment_id = toupper(deployment_id))

# Predicted probabilities of leaving the camera field of view (of various image gap lengths) - from ABMI.
df_leave_prob_pred <- read_csv("./data/lookup/gap-leave-prob_predictions_2020-06-25.csv")

# Species gap groups
df_gap_groups <- read_csv("./data/lookup/species-gap-groups.csv") %>%
  mutate(common_name = case_when(
    common_name == "Bison" ~ "American bison",
    common_name == "White-tailed Deer" ~ "White-tailed deer",
    TRUE ~ common_name
  ))

#-----------------------------------------------------------------------------------------------------------------------

# Identify Series
df_series <- df_apr %>%
  # Relevant columns:
  select(deployment_id, date_detected = timestamp, species = species_common_name, number = sighting_quantity,
         frame, sex, life_stage) %>%
  # Only look at species of interest
  filter(species %in% sp) %>%
  # Remove observations that are beyond distance
  filter(frame == TRUE) %>%
  # Only interested in the first two deployment periods (for now)
  filter(str_detect(deployment_id, "_R1$|_R2$")) %>%
  # Remove duplicated tags (per Hila)
  distinct() %>% # 5,596 observations
  # Re-calculate number of individuals when there are different categories (sex, life stage) present in an image
  group_by(deployment_id, date_detected, species) %>%
  mutate(number = sum(number)) %>%
  distinct(deployment_id, date_detected, species, number, .keep_all = TRUE) %>%
  ungroup() %>%
  # Order observations
  arrange(deployment_id, date_detected, species) %>%
  # Identify series
  mutate(series_num = 0,
         date_detected_previous = lag(date_detected),
         diff_time = as.numeric(date_detected - date_detected_previous),
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
  # Adjust time difference between ordered images that require probabilistic gap assignment - minimal impact.
  mutate(pred = replace_na(pred, 0),
         diff_time_adj = round(diff_time * (1 - pred), digits = 2)) %>%
  # Alternative method: 2 seconds per image (per Tal)
  mutate(image_time = 2) %>%
  # Trim down fields
  select(deployment_id, date_detected, species, number, sex, life_stage, series_num, diff_time, diff_time_adj, image_time)

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
            total_time = sum(diff_time_adj),
            total_image_time = sum(image_time) + 2) # Add 2 seconds to account for removal of first image

# Next, single-image series
df_tts_single <- df_series %>%
  group_by(series_num) %>%
  summarise(n_images = n()) %>%
  # Keep only series with 1 image
  filter(n_images == 1) %>%
  mutate(total_time = 0,
         total_image_time = 2)

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
  mutate(series_total_time = (total_time + tbi) * avg_number,
         series_total_image_time = total_image_time) %>%
  select(series_num, species, n_images, series_total_time, series_total_image_time)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate total time (tt) in front of camera, by deployment and species

# Operational duration information
df_durations <- df_apr %>%
  # Just by days for now - could do by hours/minutes etc to be more precise.
  select(deployment_id, duration_days) %>%
  group_by(deployment_id) %>%
  distinct() %>%
  # Just the first two rounds
  filter(str_detect(deployment_id, "_R1$|_R2$"))

df_tt <- df_series %>%
  group_by(series_num) %>%
  arrange(date_detected) %>%
  filter(row_number() == 1) %>%
  left_join(df_tts_final, by = c("series_num", "species")) %>%
  select(series_num, deployment_id, date_detected,
         species, series_total_time, series_total_image_time) %>%
  ungroup() %>%
  mutate_at(c("deployment_id", "species"), factor) %>%
  group_by(deployment_id, species, .drop = FALSE) %>%
  summarise(total_duration = sum(series_total_time),
            total_image_duration = sum(series_total_image_time)) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  # Join duration information
  left_join(df_durations, by = "deployment_id")

# For deployments with no images of our species of interest:

# Vector of all deployments
dep <- df_durations %>%
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
  mutate(total_duration = 0,
         total_image_duration = 0)

df_tt_full <- df_tt %>%
  bind_rows(df_tt_none) %>%
  arrange(deployment_id, species) %>%
  rename(time_in_fov_s = total_duration, image_time_s = total_image_duration) %>%
  mutate(deployment_id = toupper(deployment_id))

#-----------------------------------------------------------------------------------------------------------------------

# Distance information
df_dist <- df_apr %>%
  select(deployment_id, distance_m = distance) %>%
  distinct()

# Calculate density
df_density <- df_tt_full %>%
  mutate(deployment_id = toupper(deployment_id)) %>%
  left_join(df_dist, by = "deployment_id") %>%
  # Calculate density
  mutate(effort = duration_days * (distance_m ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = time_in_fov_s / effort,
         cpue_image_time = image_time_s / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000,
         cpue_image_time_km2 = cpue_image_time / 60 / 60 / 24 * 10000) %>%
  select(deployment_id:duration_days, distance_m, effort, density_km2 = cpue_km2, density_image_time_km2 = cpue_image_time_km2)

#-----------------------------------------------------------------------------------------------------------------------

# Summarise density

source("./src/estimate-confidence-intervals-fn.R")

# Both periods
df_density_apr <- df_density %>%
  filter(!density_km2 == "NaN",
         # Arbitrary threshold of days of operation
         duration_days > 5) %>%
  summarise_density(species_col = species, dens_col = density_km2)

# First period
r1 <- df_density %>%
  filter(str_detect(deployment_id, "_R1$"),
         !density_km2 == "NaN",
         duration_days > 5) %>%
  summarise_density(species_col = species, dens_col = density_km2)

# Second Period
r2 <- df_density %>%
  filter(str_detect(deployment_id, "_R2$"),
         !density_km2 == "NaN",
         duration_days > 5) %>%
  summarise_density(species_col = species, dens_col = density_km2)

# Second period much less?

#-----------------------------------------------------------------------------------------------------------------------


