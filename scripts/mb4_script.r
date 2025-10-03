library(tidyverse)
library(osfr)
library(here)

DATA_LOC <- here("data")
osf_retr <- osf_retrieve_file("sercb") |>
  osf_download(DATA_LOC, conflicts = "skip")

df_data <- read_csv(here(DATA_LOC, "clean_data.csv"))

df_sum <- df_data |>
  filter(
    condition == "social",
    exclude_session == "not_excluded"
  ) |>
  group_by(lab_id) |>
  summarise(
    n_participants = n(),
    mean_age = mean(age_days, na.rm = TRUE),
    n_helper = sum(helper_hinderer_choice == "helper", na.rm = TRUE)
  )

write_csv(df_sum, here(DATA_LOC, "summary_stats.csv"))
