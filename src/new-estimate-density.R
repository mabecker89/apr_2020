#-----------------------------------------------------------------------------------------------------------------------

# American Bison

# Joke: What did the father buffalo say to his son when he left for college? Bi-son.

# Date: August 25, 2021

#-----------------------------------------------------------------------------------------------------------------------

# Attach packages:
pacman::p_load(readr, dplyr, tidyr, janitor, lubridate, stringr)

# Species of interest
sp <- "American bison"
cam_fov_angle <- 37.7 # Taken directly from the data

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
root <- "G:/Shared drives/ABMI Camera Mammals/"

source(paste0(root, "src/R/summarise-density_2021-06-23.R"))

df_density_apr <- df_density %>%
  filter(!density_km2 == "NaN",
         duration_days > 5) %>%
  summarise_density(species_col = species, dens_col = density_km2) # 4.19 (3 - 5.64)

r1 <- df_density %>%
  filter(str_detect(deployment_id, "_R1$"),
         !density_km2 == "NaN",
         duration_days > 5) %>%
  summarise_density(species_col = species, dens_col = density_km2) # 7.6

r2 <- df_density %>%
  filter(str_detect(deployment_id, "_R2$"),
         !density_km2 == "NaN",
         duration_days > 5) %>%
  summarise_density(species_col = species, dens_col = density_km2) # 0.75

#-----------------------------------------------------------------------------------------------------------------------

# Alternative Metrics

# The week/yday of each series
df_series_wd <- df_series %>%
  group_by(series_num) %>%
  arrange(date_detected) %>%
  filter(row_number() == 1) %>%
  select(date_detected, series_num, deployment_id, species) %>%
  mutate(week = week(date_detected),
         yday = yday(date_detected))

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
  arrange(date_detected) %>%
  filter(row_number() == 1) %>%
  left_join(df_tts_final, by = c("series_num", "species")) %>%
  left_join(df_series_wd, by = c("series_num", "deployment_id", "species", "date_detected")) %>%
  select(series_num, deployment_id, date_detected, week, yday,
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
  # Remove days in which the deployment_period was not actually operating
  semi_join(df_ydays, by = c("deployment_id", "yday")) %>%
  # Join duration information
  mutate(duration_days = 1)

# For deployments with no images of our species of interest:

df_tt_week_none <- df_weeks %>%
  # Only include the first two rounds of deployments
  filter(str_detect(deployment_id, "_R1$|_R2$")) %>%
  # Retrieve only those that had no images of native species
  anti_join(df_tt_week, by = c("deployment_id", "week")) %>%
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
  # Only include the first two rounds of deployments
  filter(str_detect(deployment_id, "_R1$|_R2$")) %>%
  # Retrieve only those that had no images of native species
  anti_join(df_tt_ydays, by = c("deployment_id", "yday")) %>%
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
  mutate(effort = duration_days * (distance_m ^ 2 * pi * (cam_fov_angle / 360)) / 100,
         # Catch per unit effort
         cpue = time_in_fov_s / effort,
         cpue_image_time = image_time_s / effort,
         # Catch per unit effort in km2
         cpue_km2 = cpue / 60 / 60 / 24 * 10000,
         cpue_image_time_km2 = cpue_image_time / 60 / 60 / 24 * 10000) %>%
  select(deployment_id:duration_days, distance_m, effort, density_km2 = cpue_km2, density_image_time_km2 = cpue_image_time_km2)

df_density_ydays <- df_tt_ydays_full %>%
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

# Let's get rid of the bookend weeks/ydays - not reliable.

df_density_week_mid <- df_density_week %>%
  group_by(deployment_id) %>%
  mutate(bookend = if_else(row_number() == 1 | row_number() == n(), 1, 0)) %>%
  filter(bookend == "0")

df_density_ydays_mid <- df_density_ydays %>%
  arrange(deployment_id, yday) %>%
  group_by(deployment_id) %>%
  mutate(bookend = if_else(row_number() == 1 | row_number() == n(), 1, 0)) %>%
  filter(bookend == "0",
         # Get rid of 0 distance
         distance_m > 0) %>%
  ungroup()

#-----------------------------------------------------------------------------------------------------------------------

# Summarise by week/yday

summarise_week <- df_density_week_mid %>%
  group_by(week) %>%
  summarise(density = mean(density_km2),
            n_deployments = n())

# Leaving in mid days for now.
summarise_day <- df_density_ydays %>%
  # Let's take out SUN_PRAIRIE_25_R1 after ... 158.
  filter(!(deployment_id == "SUN_PRAIRIE_25_R1" & yday > 158)) %>%
  group_by(yday) %>%
  summarise(density = mean(density_km2),
            n_deployments = n()) %>%
  mutate(date = as.Date(paste0("2020-01-01")) + yday - 1) %>%
  ungroup()

# Let's look at day 149 ...
day_149 <- df_density_ydays_mid %>%
  filter(yday == "149")

library(ggplot2)

# Plot
ggplot(data = summarise_day) +
  geom_col(aes(x = date, y = density), fill = "cornflowerblue") +
  labs(y = expression(Bison~density~(animals~per~km^2)),
       x = "",
       title = "Bison density by day (all)") +
  scale_x_date(date_breaks = "5 days", date_labels = "%b %d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-----------------------------------------------------------------------------------------------------------------------

# Bootstrapping by yday

library(infer)
library(ggplot2)

# Mean of days
observed_mean <- summarise_day %>%
  #filter(n_deployments > 20) %>%
  summarise(mean = mean(density)) # 2.30

bootstrap <- summarise_day %>%
  # Remove last two days when virtually all deployments had been removed
  # filter(n_deployments > 20) %>%
  specify(response = density) %>%
  generate(reps = 100000, type = "bootstrap") %>%
  calculate(stat = "mean")

bootstrapped_mean <- mean(bootstrap$stat)
conf_ints <- bootstrap %>%
  summarise(upper = quantile(stat, 0.95),
            lower = quantile(stat, 0.05))

# Plot
ggplot(data = bootstrap) +
  geom_density(aes(x = stat), color = "cornflowerblue", fill = "cornflowerblue", alpha = 0.5) +
  geom_vline(xintercept = observed_mean$mean, color = "darkred", size = 1.5) +
  # geom_vline(xintercept = bootstrapped_mean, color = "darkblue") +
  geom_vline(xintercept = conf_ints$upper, color = "black", linetype = 2, size = 1) +
  geom_vline(xintercept = conf_ints$lower, color = "black", linetype = 2, size = 1) +
  labs(x = expression(Bison~density~(animals~per~km^2)),
       y = "",
       title = "Variation in the mean estimate of bison density",
       subtitle = "With day as the bootstrapped resampling unit (10,000 replications)",
       caption = "Mean bison density: 1.41 (90% CI: 0.76, 2.20)") +
  scale_x_continuous(breaks = seq(0, 5, 0.5), limits = c(0, 5)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# What if we just looked at R1?

summarise_day_R1 <- df_density_ydays %>%
  filter(!(deployment_id == "SUN_PRAIRIE_25_R1" & yday > 158)) %>%
  filter(!str_detect(deployment_id, "_R2$")) %>%
  filter(yday < 172) %>%
  group_by(yday) %>%
  summarise(density = mean(density_km2),
            n_deployments = n()) %>%
  mutate(date = as.Date(paste0("2020-01-01")) + yday - 1)

# Mean of days
observed_mean_R1 <- summarise_day_R1 %>%
  #filter(n_deployments > 20) %>%
  summarise(mean = mean(density))

bootstrap_R1 <- summarise_day_R1 %>%
  # Remove last two days when virtually all deployments had been removed
  # filter(n_deployments > 20) %>%
  specify(response = density) %>%
  generate(reps = 10000, type = "bootstrap") %>%
  calculate(stat = "mean")

bootstrapped_mean <- mean(bootstrap_R1$stat)
conf_ints_R1 <- bootstrap_R1 %>%
  summarise(upper = quantile(stat, 0.95),
            lower = quantile(stat, 0.05))

# Plot
ggplot(data = bootstrap_R1) +
  geom_density(aes(x = stat), color = "cornflowerblue", fill = "cornflowerblue", alpha = 0.5) +
  geom_vline(xintercept = observed_mean_R1$mean, color = "darkred", size = 1.5) +
  geom_vline(xintercept = bootstrapped_mean, color = "darkblue") +
  geom_vline(xintercept = conf_ints_R1$upper, color = "black", linetype = 2, size = 1) +
  geom_vline(xintercept = conf_ints_R1$lower, color = "black", linetype = 2, size = 1) +
  labs(x = expression(Bison~density~(animals~per~km^2)),
       y = "",
       title = "Variation in the mean estimate of bison density",
       subtitle = "With day as the bootstrapped resampling unit (10,000 replications)",
       caption = "Mean bison density: 2.86 (90% CI: 1.55, 4.44)") +
  scale_x_continuous(breaks = seq(0, 8, 0.5), limits = c(0, 8)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

#-----------------------------------------------------------------------------------------------------------------------

# Now what about the different gender/age classes?
# Let's just go with age for now. I think that's what we're most interested in.

# Juvenile

# Identify Series
df_series_juv <- df_apr %>%
  # Relevant columns:
  select(deployment_id, date_detected = timestamp2, species = species_common_name, number = sighting_quantity,
         frame, sex, life_stage) %>%
  # Only look at species of interest
  filter(species %in% sp) %>%
  # Remove observations that are beyond distance
  filter(frame == TRUE) %>%
  # Only interested in the first two deployment periods (for now)
  filter(str_detect(deployment_id, "_R1$|_R2$")) %>%
  # Remove duplicated tags (per Hila)
  distinct() %>% # 5,596 observations
  # Only want juveniles this time
  filter(life_stage == "Juvenile") %>%
  # Re-calculate number of individuals when there are different sex categories present in an image
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
  select(deployment_id, date_detected, species, number, life_stage, series_num, diff_time, diff_time_adj, image_time)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate total time in front of camera, by series

# Starting with series that have >1 image
df_tts_multiple_juv <- df_series_juv %>%
  # Remove first image from dataframe (as to not include diff_time in time totals for the series)
  mutate(series_num_previous = lag(series_num)) %>%
  filter(series_num_previous == series_num) %>%
  group_by(series_num) %>%
  summarise(n_images = n() + 1,
            total_time = sum(diff_time_adj),
            total_image_time = sum(image_time) + 2) # Add 2 seconds to account for removal of first image

# Next, single-image series
df_tts_single_juv <- df_series_juv %>%
  group_by(series_num) %>%
  summarise(n_images = n()) %>%
  # Keep only series with 1 image
  filter(n_images == 1) %>%
  mutate(total_time = 0,
         total_image_time = 2)

# Bind together
df_tts_all_juv <- df_tts_multiple_juv %>%
  bind_rows(df_tts_single_juv) %>%
  arrange(series_num)

# Add time between photos, accounting for the average number of animals in each photo of a series
df_tts_final_juv <- df_series_juv %>%
  # Join in average tbp, by species
  left_join(df_tbi, by = "species") %>%
  select(series_num, species, number, tbi) %>%
  group_by(series_num) %>%
  # Number of individuals in a series - we could do this more accurately (time-weighted)
  mutate(avg_number = mean(number)) %>%
  ungroup() %>%
  select(-number) %>%
  distinct() %>%
  left_join(df_tts_all_juv, by = "series_num") %>%
  # Add time between images, and multiply by average number of individuals in series
  mutate(series_total_time = (total_time + tbi) * avg_number,
         series_total_image_time = total_image_time) %>%
  select(series_num, species, n_images, series_total_time, series_total_image_time)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate total time (tt) in front of camera, by deployment and species

df_tt_juv <- df_series_juv %>%
  group_by(series_num) %>%
  arrange(date_detected) %>%
  filter(row_number() == 1) %>%
  left_join(df_tts_final_juv, by = c("series_num", "species")) %>%
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

df_tt_none_juv <- df_durations %>%
  # Retrieve only those that had no images of native species
  anti_join(df_tt_juv, by = "deployment_id") %>%
  expand(deployment_id, species = sp) %>%
  # Re-join time-by-day information
  left_join(df_durations, by = "deployment_id") %>%
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0,
         total_image_duration = 0)

df_tt_full_juv <- df_tt_juv %>%
  bind_rows(df_tt_none_juv) %>%
  arrange(deployment_id, species) %>%
  rename(time_in_fov_s = total_duration, image_time_s = total_image_duration) %>%
  mutate(deployment_id = toupper(deployment_id))

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density
df_density_juv <- df_tt_full_juv %>%
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

# Alternative Metrics

# The week/yday of each series
df_series_wd_juv <- df_series_juv %>%
  group_by(series_num) %>%
  arrange(date_detected) %>%
  filter(row_number() == 1) %>%
  select(date_detected, series_num, deployment_id, species) %>%
  mutate(week = week(date_detected),
         yday = yday(date_detected))

# Total time for each series
df_juv <- df_series_juv %>%
  group_by(series_num) %>%
  arrange(date_detected) %>%
  filter(row_number() == 1) %>%
  left_join(df_tts_final_juv, by = c("series_num", "species")) %>%
  left_join(df_series_wd_juv, by = c("series_num", "deployment_id", "species", "date_detected")) %>%
  select(series_num, deployment_id, date_detected, week, yday,
         species, series_total_time, series_total_image_time) %>%
  ungroup()

# By day
df_tt_ydays_juv <- df_juv %>%
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

df_tt_yday_none_juv <- df_ydays %>%
  # Only include the first two rounds of deployments
  filter(str_detect(deployment_id, "_R1$|_R2$")) %>%
  # Retrieve only those that had no images of native species
  anti_join(df_tt_ydays_juv, by = c("deployment_id", "yday")) %>%
  group_by(deployment_id, yday) %>%
  expand(deployment_id, yday, species = sp) %>%
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0,
         total_image_duration = 0,
         duration_days = 1)

df_tt_ydays_full_juv <- df_tt_ydays_juv %>%
  bind_rows(df_tt_yday_none_juv) %>%
  arrange(deployment_id, species) %>%
  rename(time_in_fov_s = total_duration, image_time_s = total_image_duration) %>%
  distinct()

df_density_ydays_juv <- df_tt_ydays_full_juv %>%
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

# Let's get rid of the bookend weeks/ydays - not reliable.

df_density_ydays_mid_juv <- df_density_ydays_juv %>%
  arrange(deployment_id, yday) %>%
  group_by(deployment_id) %>%
  mutate(bookend = if_else(row_number() == 1 | row_number() == n(), 1, 0)) %>%
  filter(bookend == "0",
         # Get rid of 0 distance
         distance_m > 0)

# Summarise by day
summarise_day_juv <- df_density_ydays_mid_juv %>%
  group_by(yday) %>%
  summarise(density = mean(density_km2),
            n_deployments = n())

#-----------------------------------------------------------------------------------------------------------------------

# Adults

# Identify Series
df_series_adult <- df_apr %>%
  # Relevant columns:
  select(deployment_id, date_detected = timestamp2, species = species_common_name, number = sighting_quantity,
         frame, sex, life_stage) %>%
  # Only look at species of interest
  filter(species %in% sp) %>%
  # Remove observations that are beyond distance
  filter(frame == TRUE) %>%
  # Only interested in the first two deployment periods (for now)
  filter(str_detect(deployment_id, "_R1$|_R2$")) %>%
  # Remove duplicated tags (per Hila)
  distinct() %>% # 5,596 observations
  # Only want juveniles this time
  filter(life_stage == "Adult") %>%
  # Re-calculate number of individuals when there are different sex categories present in an image
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
  select(deployment_id, date_detected, species, number, life_stage, series_num, diff_time, diff_time_adj, image_time)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate total time in front of camera, by series

# Starting with series that have >1 image
df_tts_multiple_adult <- df_series_adult %>%
  # Remove first image from dataframe (as to not include diff_time in time totals for the series)
  mutate(series_num_previous = lag(series_num)) %>%
  filter(series_num_previous == series_num) %>%
  group_by(series_num) %>%
  summarise(n_images = n() + 1,
            total_time = sum(diff_time_adj),
            total_image_time = sum(image_time) + 2) # Add 2 seconds to account for removal of first image

# Next, single-image series
df_tts_single_adult <- df_series_adult %>%
  group_by(series_num) %>%
  summarise(n_images = n()) %>%
  # Keep only series with 1 image
  filter(n_images == 1) %>%
  mutate(total_time = 0,
         total_image_time = 2)

# Bind together
df_tts_all_adult <- df_tts_multiple_adult %>%
  bind_rows(df_tts_single_adult) %>%
  arrange(series_num)

# Add time between photos, accounting for the average number of animals in each photo of a series
df_tts_final_adult <- df_series_adult %>%
  # Join in average tbp, by species
  left_join(df_tbi, by = "species") %>%
  select(series_num, species, number, tbi) %>%
  group_by(series_num) %>%
  # Number of individuals in a series - we could do this more accurately (time-weighted)
  mutate(avg_number = mean(number)) %>%
  ungroup() %>%
  select(-number) %>%
  distinct() %>%
  left_join(df_tts_all_adult, by = "series_num") %>%
  # Add time between images, and multiply by average number of individuals in series
  mutate(series_total_time = (total_time + tbi) * avg_number,
         series_total_image_time = total_image_time) %>%
  select(series_num, species, n_images, series_total_time, series_total_image_time)

#-----------------------------------------------------------------------------------------------------------------------

# Calculate total time (tt) in front of camera, by deployment and species

df_tt_adult <- df_series_adult %>%
  group_by(series_num) %>%
  arrange(date_detected) %>%
  filter(row_number() == 1) %>%
  left_join(df_tts_final_adult, by = c("series_num", "species")) %>%
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

df_tt_none_adult <- df_durations %>%
  # Retrieve only those that had no images of native species
  anti_join(df_tt_adult, by = "deployment_id") %>%
  expand(deployment_id, species = sp) %>%
  # Re-join time-by-day information
  left_join(df_durations, by = "deployment_id") %>%
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0,
         total_image_duration = 0)

df_tt_full_adult <- df_tt_adult %>%
  bind_rows(df_tt_none_adult) %>%
  arrange(deployment_id, species) %>%
  rename(time_in_fov_s = total_duration, image_time_s = total_image_duration) %>%
  mutate(deployment_id = toupper(deployment_id))

#-----------------------------------------------------------------------------------------------------------------------

# Calculate density
df_density_adult <- df_tt_full_adult %>%
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

# Alternative Metrics

# The week/yday of each series
df_series_wd_adult <- df_series_adult %>%
  group_by(series_num) %>%
  arrange(date_detected) %>%
  filter(row_number() == 1) %>%
  select(date_detected, series_num, deployment_id, species) %>%
  mutate(week = week(date_detected),
         yday = yday(date_detected))

# Total time for each series
df_adult <- df_series_adult %>%
  group_by(series_num) %>%
  arrange(date_detected) %>%
  filter(row_number() == 1) %>%
  left_join(df_tts_final_adult, by = c("series_num", "species")) %>%
  left_join(df_series_wd_adult, by = c("series_num", "deployment_id", "species", "date_detected")) %>%
  select(series_num, deployment_id, date_detected, week, yday,
         species, series_total_time, series_total_image_time) %>%
  ungroup()

# By day
df_tt_ydays_adult <- df_adult %>%
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

df_tt_yday_none_adult <- df_ydays %>%
  # Only include the first two rounds of deployments
  filter(str_detect(deployment_id, "_R1$|_R2$")) %>%
  # Retrieve only those that had no images of native species
  anti_join(df_tt_ydays_adult, by = c("deployment_id", "yday")) %>%
  group_by(deployment_id, yday) %>%
  expand(deployment_id, yday, species = sp) %>%
  # Add total_duration column, which is zero in these cases
  mutate(total_duration = 0,
         total_image_duration = 0,
         duration_days = 1)

df_tt_ydays_full_adult <- df_tt_ydays_adult %>%
  bind_rows(df_tt_yday_none_adult) %>%
  arrange(deployment_id, species) %>%
  rename(time_in_fov_s = total_duration, image_time_s = total_image_duration)

df_density_ydays_adult <- df_tt_ydays_full_adult %>%
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

# Let's get rid of the bookend weeks/ydays - not reliable.

df_density_ydays_mid_adult <- df_density_ydays_adult %>%
  arrange(deployment_id, yday) %>%
  group_by(deployment_id) %>%
  mutate(bookend = if_else(row_number() == 1 | row_number() == n(), 1, 0)) %>%
  filter(bookend == "0",
         # Get rid of 0 distance
         distance_m > 0)

# Summarise by day
summarise_day_adult <- df_density_ydays_mid_adult %>%
  group_by(yday) %>%
  summarise(density = mean(density_km2),
            n_deployments = n())

#-----------------------------------------------------------------------------------------------------------------------

# Plot juvenile and adult results together

df_adult_juv_dens_yday <- summarise_day_adult %>%
  rename(adult_density_km2 = density) %>%
  left_join(summarise_day_juv, by = c("yday", "n_deployments")) %>%
  rename(juvenile_density_km2 = density) %>%
  select(1, 3, 2, 4) %>%
  left_join(summarise_day, by = c("yday", "n_deployments")) %>%
  rename(overall_density_km2 = density) %>%
  select(6, 1, 2, 5, 3, 4)

write_csv(df_adult_juv_dens_yday, "./data/density-by-day-life-stage_wide_2021-08-26.csv")

df_adult_juv_dens_yday %>%
  pivot_longer(4:6, names_to = "life_stage", values_to = "density_km2") %>%
  mutate(life_stage = case_when(
    str_detect(life_stage, "overall") ~ "overall",
    str_detect(life_stage, "juvenile") ~ "juvenile",
    str_detect(life_stage, "adult") ~ "adult"
  )) %>%
  write_csv("./data/density-by-day-life-stage_long_2021-08-26.csv")

# Labels
labels <- as_labeller(c(`adult_density_km2` = "Adults",
                        `juvenile_density_km2` = "Juveniles"))

df_adult_juv_dens_yday %>%
  pivot_longer(4:6, names_to = "life_stage", values_to = "density") %>%
  filter(!life_stage == "overall_density_km2") %>%
  ggplot() +
  geom_col(aes(x = date, y = density, fill = life_stage)) +
  scale_x_date(breaks = "5 days", date_labels = "%b %d") +
  facet_wrap(~ life_stage, nrow = 2, labeller = labels) +
  labs(y = expression(Bison~density~(animals~per~km^2)),
       x = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")


# Prep data
df_density_ydays_mid %>%
  select(deployment_id, yday, species, overall_density_km2 = density_km2) %>%
  write_csv("./data/density-by-day-deployment_long_2021-08-26.csv")























