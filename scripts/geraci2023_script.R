library(tidyverse)
library(metaDigitise)
library(here)

# NOTE: assume that error bars are 95%CIs
df_geraci <- metaDigitise(here("plot_extract"))
df <- df_geraci |>
  mutate(
    condition = ifelse(str_starts(group_id, "control"), "control", "experimental"),
    agent = ifelse(str_detect(group_id, "non"), "non-collector", "collector")
  )

# light verification that 95%CIs produces a reasonable correlation
F_agent <- 0.607
F_condition <- 4.288
F_interaction <- 1.085
df_effect <- 1
df_error <- 23
n <- 24

# calculations
pooled_var <- mean(df$sd^2)
grand_mean <- mean(df$mean)

# sum of squares
cond_means <- aggregate(mean ~ condition, df, mean)
SS_condition <- n * 2 * sum((cond_means$mean - grand_mean)^2)
MS_condition <- SS_condition / df_effect

agent_means <- aggregate(mean ~ agent, df, mean)
SS_agent <- n * 2 * sum((agent_means$mean - grand_mean)^2)
MS_agent <- SS_agent / df_effect

SS_total_between <- n * sum((df$mean - grand_mean)^2)
SS_interaction <- SS_total_between - SS_condition - SS_agent
MS_interaction <- SS_interaction / df_effect

# estimate correlation
MS_error_from_condition <- MS_condition / F_condition
MS_error_from_agent <- MS_agent / F_agent
MS_error_from_interaction <- MS_interaction / F_interaction

MS_error <- mean(c(MS_error_from_condition, MS_error_from_agent, MS_error_from_interaction))

estimated_r <- 1 - (MS_error / pooled_var)

if (estimated_r < 0 || estimated_r > 1) {
  print("r outside of [0, 1] range")
}

# reconstruct ANOVA table
results <- data.frame(
  Effect = c("Condition", "Agent", "Condition x Agent", "Error"),
  SS = c(SS_condition, SS_agent, SS_interaction, MS_error * df_error),
  df = c(df_effect, df_effect, df_effect, df_error),
  MS = c(MS_condition, MS_agent, MS_interaction, MS_error),
  F = c(F_condition, F_agent, F_interaction, NA),
  p = c(
    pf(F_condition, df_effect, df_error, lower.tail = FALSE),
    pf(F_agent, df_effect, df_error, lower.tail = FALSE),
    pf(F_interaction, df_effect, df_error, lower.tail = FALSE),
    NA
  )
)

print(results, row.names = FALSE)

# t-test
expt <- df[df$condition == "experimental", ]
mean_col <- expt$mean[expt$agent == "collector"]
mean_non <- expt$mean[expt$agent == "non-collector"]
mean_diff <- mean_col - mean_non

sd_col <- expt$sd[expt$agent == "collector"]
sd_non <- expt$sd[expt$agent == "non-collector"]

sd_pooled <- sqrt((sd_col^2 + sd_non^2) / 2)
se_diff <- sqrt(sd_pooled^2 * 2 * (1 - estimated_r) / n)

t_value <- mean_diff / se_diff
