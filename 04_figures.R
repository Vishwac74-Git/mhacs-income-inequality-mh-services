library(tidyverse)
library(survey)
library(srvyr)
library(broom)

# 2. Load clean data and rebuild objects
mhacs_clean <- read_rds("data_clean/mhacs_clean.rds")

mhacs_clean2 <- mhacs_clean %>%
  mutate(
    inc3 = case_when(
      inc_hh %in% 1:5   ~ "low",
      inc_hh %in% 6:10  ~ "middle",
      inc_hh %in% 11:15 ~ "high",
      TRUE ~ NA_character_
    ),
    inc3 = factor(inc3) %>% relevel(ref = "high")
  )

mhacs_svy2 <- svydesign(
  ids = ~1,
  weights = ~WTS_M,
  data = mhacs_clean2
)

dim(mhacs_clean2)
table (mhacs_clean2$inc3, useNA = "always")

## 3 PREDICTED PROBABILITIES from Model 3
model3 <- svyglm(
  mh_service_use ~ inc3 + age_grp + gender + any_disorder_12m,
  design = mhacs_svy2,
  family = quasibinomial()
)

# Step 2: Create a "prediction grid"
pred_grid <- expand.grid(
  inc3             = factor(c("low", "middle", "high"),
                            levels = c("high", "low", "middle")),
  age_grp          = median(mhacs_clean2$age_grp, na.rm = TRUE),
  gender           = 1,
  any_disorder_12m = 0
)

# Step 3: Get predicted probabilities (svyglm returns vector directly)
pred_probs <- predict(model3, newdata = pred_grid, type = "response")

# Step 4: Combine into a clean table
pred_df <- pred_grid %>%
  mutate(
    prob    = as.numeric(pred_probs),
    se      = as.numeric(attr(pred_probs, "var")^0.5),
    ci_low  = prob - 1.96 * se,
    ci_high = prob + 1.96 * se,
    inc3    = factor(inc3, levels = c("low", "middle", "high"),
                     labels = c("Low Income", "Middle Income", "High Income"))
  )

pred_df

# 4. FIGURE 3 — Predicted probabilities plot

fig3 <- ggplot(pred_df,
               aes(x = inc3, y = prob, color = inc3)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.15, linewidth = 0.8) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.15)
  ) +
  scale_color_manual(values = c(
    "Low Income"    = "#D73027",
    "Middle Income" = "#FC8D59",
    "High Income"   = "#4575B4"
  )) +
  labs(
    title    = "Predicted Probability of MH Service Use by Income Group",
    subtitle = "Adjusted for age, gender, and 12-month mental disorder status\nMHACS 2022 (n = 9,798)",
    x        = "Household Income Group",
    y        = "Predicted Probability of Service Use",
    caption  = "Error bars represent 95% confidence intervals.\nReference: High Income. Model adjusted for age, gender, disorder status."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 11)
  )

fig3

ggsave("figs/fig3_predicted_probs.png", fig3,
       width = 7, height = 5, dpi = 300)

message("Fig 3 saved ✅")

# 5. FIGURE 4 — Unmet need by income group

svy_inc3_unmet <- read_csv("outputs/svy_inc3_unmet.csv")

# Clean and prepare for plotting
unmet_plot <- svy_inc3_unmet %>%
  filter(!is.na(inc3)) %>%
  mutate(
    inc3    = factor(inc3,
                     levels = c("low", "middle", "high"),
                     labels = c("Low Income", "Middle Income", "High Income")),
    pct     = prop_unmet * 100,
    ci_low  = (prop_unmet - 1.96 * prop_unmet_se) * 100,
    ci_high = (prop_unmet + 1.96 * prop_unmet_se) * 100
  )

fig4 <- ggplot(unmet_plot, aes(x = inc3, y = pct, fill = inc3)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.15, linewidth = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -1.8, fontface = "bold", size = 4) +
  scale_fill_manual(values = c(
    "Low Income"    = "#D73027",
    "Middle Income" = "#FC8D59",
    "High Income"   = "#4575B4"
  )) +
  scale_y_continuous(limits = c(0, 25)) +
  labs(
    title    = "Unmet Mental Health Care Need by Household Income Group",
    subtitle = "Survey-weighted estimates, MHACS 2022",
    x        = "Household Income Group",
    y        = "% Reporting Unmet Need",
    caption  = "Error bars represent 95% confidence intervals.\nUnmet need defined as needing but not receiving mental health care in the past 12 months."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 11)
  )

fig4

ggsave("figs/fig4_unmet_need_by_income.png", fig4,
       width = 7, height = 5, dpi = 300)

message("Fig 4 saved ✅")

library(tidyverse)
library(survey)
library(srvyr)
library(broom)
library(tidyverse)
library(survey)
library(srvyr)

# 6. SUBGROUP ANALYSIS — service use among those WITH a disorder only

mhacs_disorder <- mhacs_clean2 %>%
  filter(any_disorder_12m == 1)  # keep only people with a disorder

nrow(mhacs_disorder)
table(mhacs_disorder$inc3, useNA = "always")

svy_disorder <- svydesign(
  ids     = ~1,
  weights = ~WTS_M,
  data    = mhacs_disorder
) %>% as_survey()

svy_disorder_inc3 <- svy_disorder %>%
  group_by(inc3) %>%
  summarise(
    prop_service = survey_mean(mh_service_use, na.rm = TRUE),
    n = unweighted(n())
  ) %>%
  filter(!is.na(inc3))

svy_disorder_inc3

# 7. FIGURE 5 — Service use among disorder subgroup by income
fig5_data <- svy_disorder_inc3 %>%
  mutate(
    inc3    = factor(inc3,
                     levels = c("low", "middle", "high"),
                     labels = c("Low Income", "Middle Income", "High Income")),
    pct     = prop_service * 100,
    ci_low  = (prop_service - 1.96 * prop_service_se) * 100,
    ci_high = (prop_service + 1.96 * prop_service_se) * 100
  )

fig5 <- ggplot(fig5_data, aes(x = inc3, y = pct, fill = inc3)) +
  geom_col(width = 0.55, show.legend = FALSE) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.15, linewidth = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")),
            vjust = -1.8, fontface = "bold", size = 4) +
  scale_fill_manual(values = c(
    "Low Income"    = "#D73027",
    "Middle Income" = "#FC8D59",
    "High Income"   = "#4575B4"
  )) +
  scale_y_continuous(limits = c(0, 75)) +
  labs(
    title    = "Mental Health Service Use Among Those With a Disorder",
    subtitle = "Restricted to respondents with a 12-month mental disorder, MHACS 2022",
    x        = "Household Income Group",
    y        = "% Using Mental Health Services",
    caption  = "Error bars represent 95% confidence intervals.\nSample sizes: Low n=47, Middle n=298, High n=1,077."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(size = 11)
  )

fig5

ggsave("figs/fig5_service_use_disorder_subgroup.png", fig5,
       width = 7, height = 5, dpi = 300)

write_csv(svy_disorder_inc3, "outputs/svy_disorder_subgroup.csv")

message("Fig 5 saved ✅")

write_csv(pred_df, "outputs/predicted_probs.csv")
message("All Day 5 outputs saved ✅")



    