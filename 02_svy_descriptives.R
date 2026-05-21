## 02_svy_descriptives.R
## Weighted descriptives: income-based inequality in MH service use

library(tidyverse)
library(survey)
library (srvyr)

# 1. Load clean data
mhacs_clean <- read_rds("data_clean/mhacs_clean.rds")

dim(mhacs_clean)
names(mhacs_clean)

# 2. define survey design object (weights only for now)
mhacs_design <- svydesign(
  ids = ~1,
  weights = ~WTS_M,
  data = mhacs_clean
)

# convert to srvyr object for tidy syntax
mhacs_svy <- as_survey(mhacs_design)


# 3. Weighted proportion of MH service use by household income group
svy_inc_service <- mhacs_svy %>%
  group_by(inc_hh) %>%
  summarise(
    prop_service = survey_mean(mh_service_use, na.rm = TRUE),
    n = unweighted(n())
  )

svy_inc_service

mhacs_clean2 <- mhacs_clean %>%
  mutate(
    inc3 = case_when (
      inc_hh %in% 1:5 ~ "low",
      inc_hh %in% 6:10 ~ "middle",
      inc_hh %in% 11:15 ~ "high",
      TRUE ~ NA_character_
    )
  )

mhacs_svy2 <- as_survey(
  svydesign(ids = ~1, weights = ~WTS_M, data = mhacs_clean2)
)

svy_inc3_service <- mhacs_svy2 %>%
  group_by(inc3) %>%
  summarise(
    prop_service = survey_mean(mh_service_use, na.rm = TRUE),
    n = unweighted(n())
  )

svy_inc3_service

svy_inc3_unmet <- mhacs_svy2 %>%
  group_by(inc3) %>%
  summarise(
    prop_unmet = survey_mean(unmet_need, na.rm = TRUE),
    n = unweighted(n())
  )
  
svy_inc3_unmet


mhacs_clean2 <- mhacs_clean %>%
  mutate(
    inc3 = case_when(
      inc_hh %in% 1:5  ~ "low",
      inc_hh %in% 6:10 ~ "middle",
      inc_hh %in% 11:15 ~ "high",
      TRUE ~ NA_character_
    )
  )

mhacs_svy2 <- as_survey(
  svydesign(ids = ~1, weights = ~WTS_M, data = mhacs_clean2)
)

# 4. Overall weighted prevalence of MH service use
svy_overall <- mhacs_svy2 %>%
  summarise(
    prop_service = survey_mean(mh_service_use, na.rm = TRUE),
    n = unweighted(n())
  )
svy_overall

# 5. Sample characteristics table by income group
svy_chars <- mhacs_svy2 %>%
  group_by(inc3) %>%
  summarise(
    n             = unweighted(n()),
    pct_service   = survey_mean(mh_service_use, na.rm = TRUE),
    pct_unmet     = survey_mean(unmet_need, na.rm = TRUE)
  )
svy_chars

# 6. Bar plot: service use % by income group with 95% CIs
plot_data <- svy_inc3_service %>%
  filter(!is.na(inc3)) %>%
  mutate(
    inc3 = factor(inc3, levels = c("low", "middle", "high"),
                  labels = c("Low Income", "Middle Income", "High Income")),
    pct       = prop_service * 100,
    ci_low    = (prop_service - 1.96 * prop_service_se) * 100,
    ci_high   = (prop_service + 1.96 * prop_service_se) * 100
  )

fig1 <- ggplot(plot_data, aes(x = inc3, y = pct, fill = inc3)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.18, linewidth = 0.8) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -1.8, size = 4.2, fontface = "bold") +
  scale_fill_manual(values = c("Low Income"    = "#D73027",
                               "Middle Income" = "#FC8D59",
                               "High Income"   = "#4575B4")) +
  scale_y_continuous(limits = c(0, 50), expand = c(0, 0)) +
  labs(
    title    = "Mental Health Service Use by Household Income Group",
    subtitle = "Canadian Adults, MHACS 2022 (survey-weighted)",
    x        = "Income Group",
    y        = "Weighted Prevalence (%)",
    caption  = "Error bars represent 95% confidence intervals.\nSource: Mental Health and Access to Care Survey (MHACS) 2022 PUMF"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40"),
    axis.line     = element_line(color = "grey70"),
    panel.grid.major.x = element_blank()
  )

fig1

# 7. Save outputs
ggsave("figs/fig1_service_use_by_income.png", fig1,
       width = 7, height = 5, dpi = 300)

write_csv(svy_inc3_service, "outputs/svy_inc3_service.csv")
write_csv(svy_inc3_unmet,   "outputs/svy_inc3_unmet.csv")
write_csv(svy_overall,      "outputs/svy_overall.csv")
write_csv(svy_chars,        "outputs/svy_chars.csv")


table(mhacs_clean$unmet_need)
summary(mhacs_clean$INCDVP20)