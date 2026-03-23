#-------------------------------------------------------------------------------
# Converting types and preprocessing the data for the did regressions
#-------------------------------------------------------------------------------
rm(list=ls())
all_lfs_data <- read_csv("output/italy_lfs_data.csv")

# we use only yearly data
yearly_data <- all_lfs_data %>%
  filter(
    quarter == "_A" 
  )

yearly_data_decoded <- yearly_data %>%
  mutate(
    
    urban = case_when(
      DEGURBA == "1" ~ 1,
      DEGURBA == "3" ~ 0,
      TRUE ~ NA_real_
    ),
    
    hwusual = case_when(
      HWUSUAL == "99" ~ NA_real_,
      TRUE ~ as.numeric(HWUSUAL)
    ),
    
    hwactual = case_when(
      HWACTUAL == "99" ~ NA_real_,
      TRUE ~ as.numeric(HWACTUAL)
    ),
    
    educlevl = case_when(
      EDUCLEVL == "9" ~ NA_real_,
      TRUE ~ as.numeric(EDUCLEVL)
    ),
    
    hhtype = case_when(
      HHTYPE == "9" ~ NA_real_,
      TRUE ~ as.numeric(EDUCLEVL)
    ),
    
    durune = case_when(
      DURUNE == "9" ~ NA_real_,
      TRUE ~ as.numeric(HHTYPE)
    ),
    
    seekwork = case_when(
      SEEKWORK == "1" ~ 1,
      SEEKWORK == "2" ~ 0,
      TRUE ~ NA_real_
    ),
    wantwork = case_when(
      WANTWORK == "1" ~ 1,
      WANTWORK == "2" ~ 0,
      TRUE ~ NA_real_
    ),
    
    available = case_when(
      AVAILBLE == "1" ~ 1,
      AVAILBLE == "2" ~ 0,
      TRUE ~ NA_real_
    ),
    
    marstat = MARSTAT,
    hhchildren = HHCHILDR,
    
    sizefirm = as.numeric(SIZEFIRM),
    supervisor = as.numeric(SUPVISOR),
    
    industry = NACE1D,
    occupation = ISCO1D,
    
    # further won't be used because of the distribution
    # mostly(>90%) italians
    foreign = case_when( 
      NATIONAL == "000-OWN COUNTRY" ~ 0,
      NATIONAL == "005-EU28" ~ 1,
      TRUE ~ NA_real_
    ),
    
    female = case_when(
      SEX == "1" ~ 0,  # male
      SEX == "2" ~ 1,  # female
      TRUE ~ NA_real_
    ),
    part_time = case_when(
      FTPT == 2 ~ 1,
      FTPT == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    temporary = case_when(
      TEMP == 2 ~ 1,  # temporary contract
      TEMP == 1 ~ 0,  # permanent contract
      TRUE ~ NA_real_
    )
  ) %>%
  select(-SEX, -REGION, -NATIONAL, -EDUCLEVL, -DEGURBA, 
         -HWACTUAL, -HWUSUAL, -TEMP, -FTPT, -DURUNE, -NACE1D, -ISCO1D, -HHTYPE,
         -SEEKWORK, -WANTWORK, -AVAILBLE, -MARSTAT, -HHCHILDR, -SIZEFIRM, 
         -SUPVISOR)

# removing NA mostly columns
yearly_data_decoded <- yearly_data_decoded %>%
  select(where(~ mean(is.na(.)) <= 0.20))

glimpse(yearly_data_decoded)
# simple loop for each column to check the data
for(col in names(yearly_data_decoded)) {
  if(col %in% c("coeff", "weight_norm")) {
    next
  }
  na_pct <- mean(is.na(yearly_data_decoded[[col]])) * 100
  unique_vals <- unique(yearly_data_decoded[[col]])
  
  cat("\n", col, "\n")
  cat("  NA %:", round(na_pct, 2), "%\n")
  cat("  Unique values:", paste(unique_vals, collapse = ", "), "\n")
}

summary_data <- yearly_data_decoded %>%
  filter(year %in% c(2006,2007,2009,2010)) #excluding 2004 and 2005

summary_table <- summary_data %>%
  group_by(treat, post, age_group) %>%
  summarise(
    employment_rate = weighted.mean(employed, w = coeff, na.rm = TRUE),
    n_individuals = n(),
    weighted_n = sum(coeff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    period = if_else(post == 1, "2009-2010", "2006-2007"),
    group = if_else(treat == 1, "young", "prime-age")
  ) %>%
  select(period, group, employment_rate, n_individuals, weighted_n)

print(summary_table)

write_csv(yearly_data_decoded, "output/italy_lfs_did_analysis_data.csv")


