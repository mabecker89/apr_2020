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
