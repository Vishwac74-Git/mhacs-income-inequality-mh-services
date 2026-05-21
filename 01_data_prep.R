## 01_data_prep.R
## Data cleaning for: Income-based inequality in MH service use

# 1. Load packages
library(tidyverse)
library(survey)
library(srvyr)

# 2. Load raw MHACS data
mhacs <- read_csv("data_raw/mhacs_2022.csv", show_col_types = FALSE)

dim(mhacs)
names(mhacs)[1:40]

# 3. Select variables needed for analysis
mhacs_sel <- mhacs %>%
  select(
    WTS_M, # survey weight
    INCDVHH, INCDVP20, # income
    MHPFY, MHPFYM, MHPFYA, # disorder flags
    SR1FPRU, # any professional MH service use 12m
    PNCDNEED, #needed help but did not receive it
    DHHGAGE, GENDER #age and gender
    )

dim(mhacs_sel)
names(mhacs_sel)

# 4. Recode key variables into clean analysis versions
mhacs_clean <- mhacs_sel %>%
  mutate(
    inc_hh = INCDVHH,
    
    mh_service_use = case_when(
      SR1FPRU == 1 ~ 1, 
      SR1FPRU == 2 ~ 0,  
      TRUE ~ NA_real_
    ),
    
    unmet_need = case_when(
      PNCDNEED == 2 ~ 1,
      PNCDNEED == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    any_disorder_12m = case_when(
      MHPFY == 1 ~ 1,
      MHPFY == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    age_grp = DHHGAGE,
    gender  = GENDER
  )

# 5. Save clean dataset for analysis
write_rds(mhacs_clean,
          "data_clean/mhacs_clean.rds")

dim(mhacs_clean)
names(mhacs_clean)
mhacs_clean %>% count(mh_service_use)
mhacs_clean %>% count(unmet_need)