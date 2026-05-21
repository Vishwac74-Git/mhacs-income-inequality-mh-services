## 03_regression.R
## Survey-weighted logistic regression: income-based inequality in MH service use
## Project: Income-Based Inequality in Mental Health Service Use
## Data: MHACS 2022 PUMF

# ── What this file does ──────────────────────────────────────────────────────
# Model 1: Unadjusted    — service use ~ income only
# Model 2: Age/sex       — service use ~ income + age_grp + gender
# Model 3: Full          — service use ~ income + age_grp + gender + any_disorder_12m
# Then: extract ORs + CIs, build forest plot
# ─────────────────────────────────────────────────────────────────────────────

# 1. Load packages
library(tidyverse)   # data manipulation and ggplot2 for plotting
library(survey)      # svyglm() — survey-weighted regression
library(srvyr)       # tidy survey syntax
library(broom)       # tidy() — converts model output into clean tables

# 2. load clean data
mhacs_clean <- read_rds ("data_clean/mhacs_clean.rds")

# 3. create income variable with correct grouping and reference level
# why factor()? - regression needs a categorical variable with a reference group
# why relevel()? - we set "high" income as reference so ORs show how
# low/middle income compares TO high income (the advantaged group)
mhacs_clean2 <- mhacs_clean %>%
  mutate(
    inc3=case_when(
      inc_hh %in% 1:5 ~ "low", 
      inc_hh %in% 6:10 ~ "middle",
      inc_hh %in% 11:15 ~ "high",
      TRUE~NA_character_
    ),
    inc3=factor(inc3) %>% relevel(ref="high")
    )
#check: confirm levels and distribution
table(mhacs_clean2$inc3, useNA="always")

# 4. define survey design object for regression 
# Why do this again? - We are using mhacs_clean2 (which has the new inc3 variable)
# so we need a new survey design object build from THIS dataset, not the old one
mhacs_svy2 <- svydesign (
  ids = ~1, # ~1 means no clustering (simple random within strata)
  weights = ~WTS_M, #WTS_M is the survey weight from Statistics Canada
  data = mhacs_clean2
)

# quick check - confirm the design is correct 
summary (mhacs_svy2)

# 5. Model 1: Unadjusted - income only. no covariates
# svyglm() = survey-weighted generalized linear model
# family = quasibinomial() = used for binary outcomes with survey weights
# (better than binomial for weighted data- handles variance correctly)
# mh_service_use ~ inc3 means: predict service use FROM income group

model1 <- svyglm(
  mh_service_use ~ inc3,
  design = mhacs_svy2,
  family = quasibinomial()
)

# view raw output
summary(model1)

library(tidyverse)
library(survey)
library(srvyr)
library(broom)

# Convert Model 1 coefficients to Odds Ratios with 95% CI
# exp() converts log-odds to OR
# tidy() from broom package makes output a clean table
# exponentiate = TRUE does the exp() conversion automatically
# conf.int = TRUE adds 95% confidence intervals

or_model1 <- tidy(model1, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%   # remove intercept — not meaningful here
  mutate(model = "Model 1: Unadjusted") # label which model this came from

or_model1

#6. Model 2: Adjusted for age and gender

model2<-svyglm(
  mh_service_use ~ inc3 + age_grp + gender,
  design = mhacs_svy2, 
  family = quasibinomial()
)

or_model2 <- tidy(model2, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(model = "Model 2: + Age & Gender")

or_model2

# 7. MODEL 3: Fully adjusted — adds mental disorder status
model3 <- svyglm(
  mh_service_use ~ inc3 + age_grp + gender + any_disorder_12m,
  design = mhacs_svy2,
  family = quasibinomial()
)

or_model3 <- tidy(model3, exponentiate = TRUE, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(model = "Model 3: Fully Adjusted")

or_model3

# 8. Combine all 3 models into one clean table for export
or_all_models <- bind_rows(or_model1, or_model2, or_model3) %>%
  filter(term %in% c("inc3low", "inc3middle")) %>%
  select(model, term, estimate, conf.low, conf.high, p.value) %>%
  mutate(
    term = recode(term,
                  "inc3low"    = "Low Income vs High",
                  "inc3middle" = "Middle Income vs High"
    ),
    across(c(estimate, conf.low, conf.high), ~round(.x, 2)),
    p.value = round(p.value, 3)
  )

or_all_models

# Save to outputs
write_csv(or_all_models, "outputs/or_all_models.csv")

# 9. Forest plot of income ORs across all 3 models
# A forest plot shows OR as a dot and CI as a horizontal line
# The vertical line at OR=1.0 = "no difference" reference line
# If the CI line crosses 1.0, the result is not significant

forest_data <- or_all_models %>%
  mutate(
    model = factor(model,
                   levels = c("Model 1: Unadjusted",
                              "Model 2: + Age & Gender",
                              "Model 3: Fully Adjusted"))
  )

fig2 <- ggplot(forest_data,
               aes(x = estimate, y = model, color = term, shape = term)) +
  geom_point(size = 3, position = position_dodge(width = 0.4)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2,
                 position = position_dodge(width = 0.4)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c("Low Income vs High"    = "#D73027",
                                "Middle Income vs High" = "#FC8D59")) +
  labs(
    title    = "Odds Ratios for Mental Health Service Use by Income Group",
    subtitle = "Survey-weighted logistic regression, MHACS 2022",
    x        = "Odds Ratio (95% CI)",
    y        = NULL,
    color    = "Income Comparison",
    shape    = "Income Comparison",
    caption  = "Reference group: High Income. Adjusted for age, gender, and disorder status."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position   = "bottom",
    plot.title        = element_text(face = "bold"),
    panel.grid.minor  = element_blank()
  )

fig2

ggsave("figs/fig2_forest_plot_income_OR.png", fig2,
       width = 8, height = 4, dpi = 300)

message("Forest plot saved ✅")

# ── METHODS STUB
# Statistical Analysis:
# Survey-weighted logistic regression was conducted using the svyglm() function
# from the survey package in R (v4.5.0). Three sequential models were built:
# Model 1 (unadjusted) examined the association between household income group
# and mental health service use. Model 2 added age group and gender as covariates.
# Model 3 additionally adjusted for 12-month mental disorder status. Household
# income was categorized into three groups (low: categories 1-5, middle: 6-10,
# high: 11-15) using Statistics Canada's derived variable (INCDVHH), with high
# income as the reference group. Results are reported as odds ratios (OR) with
# 95% confidence intervals. All analyses accounted for survey weights (WTS_M)
# provided in the MHACS 2022 PUMF to produce population-representative estimates.
# ─────────────────────────────────────────────────────────────────────────────