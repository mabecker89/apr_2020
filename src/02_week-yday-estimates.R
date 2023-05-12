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
