## 03_regression.R
## Survey-weighted logistic regression: income-based inequality in MH service use

# 1. Load packages
library(tidyverse)   
library(survey)      
library(srvyr)
library(broom)       

# 2. load clean data
mhacs_clean <- read_rds ("data_clean/mhacs_clean.rds")

# 3. create income variable with correct grouping and reference level
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
table(mhacs_clean2$inc3, useNA="always")

# 4. define survey design object for regression 
mhacs_svy2 <- svydesign (
  ids = ~1,
  weights = ~WTS_M, 
  data = mhacs_clean2
)

summary (mhacs_svy2)

# 5. Model 1: Unadjusted - income only. no covariates

model1 <- svyglm(
  mh_service_use ~ inc3,
  design = mhacs_svy2,
  family = quasibinomial()
)

summary(model1)

library(tidyverse)
library(survey)
library(srvyr)
library(broom)


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

write_csv(or_all_models, "outputs/or_all_models.csv")

# 9. Forest plot of income ORs across all 3 models

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

