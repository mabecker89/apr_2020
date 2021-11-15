#--------------------------------------------------------

# Identify Series
df_series_check <- df_apr %>%
  # Relevant columns:
  select(deployment_id, date_detected = timestamp2, species = species_common_name, number = sighting_quantity,
         frame, sex, life_stage) %>%
  # Only look at species of interest
  filter(species %in% sp) %>%
  # Remove observations that are beyond distance
  #filter(frame == TRUE) %>%
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
  select(deployment_id, date_detected, species, number, sex, life_stage, frame, series_num, diff_time, diff_time_adj, image_time)


#---------------------------------------------------

# Check images of bison in R1 and R2 vis-a-vis TRUE/FALSE in frame

check <- df_series_check %>%
  separate(deployment_id, into = c("deployment", "period"), sep = "_(?=[^_]+$)", remove = FALSE) %>%
  group_by(period, frame) %>%
  tally()

# In addition to there being more images in R1 (6k vs 4k), the majority were TRUE (in the frame), whereas that was
# not the case in R2 (majority were FALSE). So it stands to reason that densities of bison went down in period 2. Why?

