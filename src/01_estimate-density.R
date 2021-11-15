#-----------------------------------------------------------------------------------------------------------------------

# Title: Estimating Density
# Date: April 3, 2021
# Author: M.Becker

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages (and install if necessary)
pacman::p_load(readr, dplyr, tidyr, janitor, lubridate)

# Load APR data
df_apr <- read_csv("./data/base/SCBI_2020_camera_trap_survey.csv") %>%
  clean_names() %>%
  # Consistent deployment naming
  mutate(deployment_id = toupper(deployment_id))

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

# Source functions
source("./src/00_summarise-density_2021-06-23.R")

sp <- "American bison"

cam_fov_angle <- 37.7

#-----------------------------------------------------------------------------------------------------------------------

# Identify Series
df_series <- df_apr %>%
  # Relevant fields:
  select(deployment_id, timestamp = timestamp2, species = species_common_name, number = sighting_quantity, frame,
         sex, life_stage) %>%
  # Only look at most common species
  filter(species %in% sp) %>%
  # Remove observations that are beyond distance
  filter(frame == TRUE) %>%
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
         diff_time_adj = round(diff_time * (1 - pred), digits = 2)) %>%
  # Alternative method: 2 seconds per image
  mutate(image_time = 2)

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
            total_image_time = sum(image_time))

# Next, single-image series'
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
  distinct()

df_tt <- df_series %>%
  group_by(series_num) %>%
  arrange(timestamp) %>%
  filter(row_number() == 1) %>%
  left_join(df_tts_final, by = c("series_num", "species")) %>%
  select(series_num, deployment_id, timestamp,
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
  mutate(total_duration = 0,
         total_image_duration = 0)

df_tt_full <- df_tt %>%
  bind_rows(df_tt_none) %>%
  arrange(deployment_id, species) %>%
  rename(time_in_fov_s = total_duration, image_time_s = total_image_duration) %>%
  mutate(deployment_id = toupper(deployment_id))

# Write processed results
write_csv(df_tt_full, "./data/processed/time-in-fov-species-deployment.csv")

#-----------------------------------------------------------------------------------------------------------------------

# Aggregate up the periods
df_tt_full_agg <- df_tt_full %>%
  separate(deployment_id, into = c("deployment", "round"), sep = "_(?=[^_]+$)") %>%
  mutate(deployment = toupper(deployment)) %>%
  group_by(deployment, species) %>%
  summarise(total_time_in_fov_s = sum(time_in_fov_s),
            total_image_time_s = sum(image_time_s),
            total_duration_days = sum(duration_days))

# Write processed results
write_csv(df_tt_full_agg, "./data/processed/time-in-fov-species-deployment-agg.csv")

#-----------------------------------------------------------------------------------------------------------------------

# Distance information
df_dist <- df_apr %>%
  select(deployment_id, distance) %>%
  distinct()

# Calculate density
df_density <- df_tt_full %>%
  mutate(deployment_id = toupper(deployment_id)) %>%
  left_join(df_dist, by = "deployment_id") %>%
  # Calculate density
  mutate(effort = duration_days * (distance ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = time_in_fov_s / effort,
         cpue_image_time = image_time_s / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000,
         cpue_image_time_km2 = cpue_image_time / 60 / 60 / 24 * 10000) %>%
  select(deployment_id:duration_days, distance, effort, density_km2 = cpue_km2, density_image_time_km2 = cpue_image_time_km2)

# Aggregate density (by deployment, over time periods)
df_density_agg <- df_density %>%
  separate(deployment_id, into = c("deployment", "round"), sep = "_(?=[^_]+$)") %>%
  mutate(deployment = toupper(deployment)) %>%
  # Remove NaNs, Infs
  filter(!(density_km2 == "NaN" | (density_km2 == "Inf"))) %>%
  group_by(deployment, species) %>%
  summarise(mean_density_km2 = mean(density_km2, na.rm = TRUE),
            mean_density_image_time_km2 = mean(density_image_time_km2, na.rm = TRUE))

write_csv(df_density, "./data/processed/density-by-deployment-round.csv")
write_csv(df_density_agg, "./data/processed/density-by-deployment.csv")

#-----------------------------------------------------------------------------------------------------------------------

# Density for the APR
df_density_apr <- df_density_agg %>%
  mutate(area = "American Prairie Reserve") %>%
  # Use custom function
  summarise_density(group_id = area,
                    agg_samp_per = TRUE,
                    species_col = species,
                    dens_col = mean_density_km2,
                    conflevel = 0.9) # Can adjust confidence level

write_csv(df_density_apr, "./data/processed/density-in-apr.csv")

#-----------------------------------------------------------------------------------------------------------------------

# Alternative Metrics

# The week/yday of each series
df_series_wd <- df_series %>%
  group_by(series_num) %>%
  arrange(timestamp) %>%
  filter(row_number() == 1) %>%
  select(timestamp, series_num, deployment_id, species) %>%
  mutate(week = week(timestamp),
         yday = yday(timestamp))

# Which weeks are included in the deployment periods?
df_weeks <- df_apr %>%
  select(deployment_id, session_start_date, session_end_date) %>%
  distinct() %>%
  mutate(start_week = week(session_start_date),
         end_week = week(session_end_date)) %>%
  # Extraneous early start for one deployment-period? 76_R1
  filter(session_start_date > "2020-01-01") %>%
  rowwise() %>%
  do(data.frame(deployment_id = .$deployment_id, week = .$start_week:.$end_week))

# Which days are included in the deployment period?
df_ydays <- df_apr %>%
  select(deployment_id, session_start_date, session_end_date) %>%
  distinct() %>%
  mutate(start_yday = yday(session_start_date),
         end_yday = yday(session_end_date)) %>%
  # Extraneous early start for one deployment-period? 76_R1
  filter(session_start_date > "2020-01-01") %>%
  rowwise() %>%
  do(data.frame(deployment_id = .$deployment_id, yday = .$start_yday:.$end_yday))

# Total time for each series
df <- df_series %>%
  group_by(series_num) %>%
  arrange(timestamp) %>%
  filter(row_number() == 1) %>%
  left_join(df_tts_final, by = c("series_num", "species")) %>%
  left_join(df_series_wd, by = c("series_num", "deployment_id", "species", "timestamp")) %>%
  select(series_num, deployment_id, timestamp, week, yday,
         species, series_total_time, series_total_image_time) %>%
  ungroup()

# By week
df_tt_week <- df %>%
  mutate_at(c("deployment_id", "species", "week"), factor) %>%
  group_by(deployment_id, species, week, .drop = FALSE) %>%
  summarise(total_duration = sum(series_total_time),
            total_image_duration = sum(series_total_image_time)) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  mutate(week = as.integer(week)) %>%
  # Remove weeks in which the deployment_period was not actually operating
  semi_join(df_weeks, by = c("deployment_id", "week")) %>%
  # Join duration information
  mutate(duration_days = 7)

# By day
df_tt_ydays <- df %>%
  mutate_at(c("deployment_id", "species", "yday"), factor) %>%
  group_by(deployment_id, species, yday, .drop = FALSE) %>%
  summarise(total_duration = sum(series_total_time),
            total_image_duration = sum(series_total_image_time)) %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  mutate(yday = as.integer(yday)) %>%
  # Remove weeks in which the deployment_period was not actually operating
  semi_join(df_ydays, by = c("deployment_id", "yday")) %>%
  # Join duration information
  mutate(duration_days = 1)

# For deployments with no images of our species of interest:

df_tt_week_none <- df_weeks %>%
  # Retrieve only those that had no images of native species
  anti_join(df_tt_week, by = "deployment_id") %>%
  group_by(deployment_id, week) %>%
  expand(deployment_id, week, species = sp) %>%
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0,
         total_image_duration = 0,
         # Wrong to hard code this as 7
         duration_days = 7)

df_tt_week_full <- df_tt_week %>%
  bind_rows(df_tt_week_none) %>%
  arrange(deployment_id, species) %>%
  rename(time_in_fov_s = total_duration, image_time_s = total_image_duration)

df_tt_yday_none <- df_ydays %>%
  # Retrieve only those that had no images of native species
  anti_join(df_tt_ydays, by = "deployment_id") %>%
  group_by(deployment_id, yday) %>%
  expand(deployment_id, yday, species = sp) %>%
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0,
         total_image_duration = 0,
         duration_days = 1)

df_tt_ydays_full <- df_tt_ydays %>%
  bind_rows(df_tt_yday_none) %>%
  arrange(deployment_id, species) %>%
  rename(time_in_fov_s = total_duration, image_time_s = total_image_duration)

# Calculate density
df_density_week <- df_tt_week_full %>%
  left_join(df_dist, by = "deployment_id") %>%
  # Calculate density
  mutate(effort = duration_days * (distance ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Add 2 seconds to image_time - not sure how we lost an image on each series. To check later.
         image_time_s = image_time_s + 2,
         # Catch per unit effort
         cpue = time_in_fov_s / effort,
         cpue_image_time = image_time_s / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000,
         cpue_image_time_km2 = cpue_image_time / 60 / 60 / 24 * 10000) %>%
  select(deployment_id:duration_days, distance, effort, density_km2 = cpue_km2, density_image_time_km2 = cpue_image_time_km2)

df_density_ydays <- df_tt_ydays_full %>%
  left_join(df_dist, by = "deployment_id") %>%
  # Calculate density
  mutate(effort = duration_days * (distance ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = time_in_fov_s / effort,
         cpue_image_time = image_time_s / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000,
         cpue_image_time_km2 = cpue_image_time / 60 / 60 / 24 * 10000) %>%
  select(deployment_id:duration_days, distance, effort, density_km2 = cpue_km2, density_image_time_km2 = cpue_image_time_km2)

write_csv(df_density_week, "./data/processed/density-by-deployment-week.csv")
write_csv(df_density_ydays, "./data/processed/density-by-deployment-day.csv")

#-----------------------------------------------------------------------------------------------------------------------

# Number of Images

df_images <- df_apr %>%
  # Relevant fields:
  select(deployment_id, timestamp = timestamp2, species = species_common_name, frame) %>%
  # Only look at most common species
  filter(species %in% sp,
         frame == TRUE) %>%
  mutate(week = week(timestamp),
         yday = yday(timestamp))

# By period
df_images_period <- df_images %>%
  mutate_at(c("deployment_id", "species"), factor) %>%
  group_by(deployment_id, species, .drop = FALSE) %>%
  tally()

# Weeks
df_images_week <- df_images %>%
  mutate_at(c("deployment_id", "species", "week"), factor) %>%
  group_by(deployment_id, species, week, .drop = FALSE) %>%
  tally(name = "n_images") %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  mutate(week = as.integer(week)) %>%
  semi_join(df_weeks, by = c("deployment_id", "week"))

df_images_week_none <- df_weeks %>%
  # Retrieve only those that had no images of native species
  anti_join(df_images_week, by = "deployment_id") %>%
  group_by(deployment_id, week) %>%
  expand(deployment_id, week, species = sp) %>%
  # Add total_duration column, which is zero in these cases
  mutate(n_images = 0)

df_images_week_full <- bind_rows(df_images_week, df_images_week_none) %>%
  arrange(deployment_id, week, species)

# Days
df_images_yday <- df_images %>%
  mutate_at(c("deployment_id", "species", "yday"), factor) %>%
  group_by(deployment_id, species, yday, .drop = FALSE) %>%
  tally(name = "n_images") %>%
  ungroup() %>%
  mutate_if(is.factor, as.character) %>%
  mutate(yday = as.integer(yday)) %>%
  semi_join(df_ydays, by = c("deployment_id", "yday"))

df_images_yday_none <- df_ydays %>%
  # Retrieve only those that had no images of native species
  anti_join(df_images_yday, by = "deployment_id") %>%
  group_by(deployment_id, yday) %>%
  expand(deployment_id, yday, species = sp) %>%
  # Add total_duration column, which is zero in these cases
  mutate(n_images = 0)

df_images_yday_full <- bind_rows(df_images_yday, df_images_yday_none) %>%
  arrange(deployment_id, yday, species)

write_csv(df_images_week_full, "./data/processed/images-by-deployment-week.csv")
write_csv(df_images_yday_full, "./data/processed/images-by-deployment-day.csv")

# Just bison
df_images_week_full_bison <- df_images_week_full %>%
  filter(species == "American bison",
         str_detect(deployment_id, "_R1$|_R2$"))

#-----------------------------------------------------------------------------------------------------------------------

# August 12, 2021 - Let's just do Bison!

# Note: Issue of 'partial' weeks, i.e. the week that the camera was re-deployed. How to handle? Get rid of?

# All bison - not broken down by age, gender, etc.
df_density_week_bison <- df_density_week %>%
  filter(species == "American bison",
         str_detect(deployment_id, "_R1$|_R2$")) %>%
  # Let's make an indicator to indicate the first and last week of each deployment period; can filter out later
  group_by(deployment_id) %>%
  mutate(bookend = if_else(row_number() == 1 | row_number() == n(), 1, 0)) %>%
  # Don't need duration_days anymore (not correct)
  select(-c(duration_days, effort)) %>%
  # Join number of images
  left_join(df_images_week_full, by = c("deployment_id", "species", "week"))

# Now let's break down by age and gender

check <- df_apr %>%
  select(species = species_common_name, sex, life_stage, frame, deployment_id) %>%
  filter(species == "American bison",
         frame == TRUE,
         str_detect(deployment_id, "_R1$|_R2$")) %>%
  group_by(sex, life_stage) %>%
  tally()


















