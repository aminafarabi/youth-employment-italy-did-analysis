library(tidyverse)
library(lubridate)

rm(list=ls())
#-------------------------------------------------------------------------------
# step 1: we define file paths to create yearly and quarterly data lists 
# yearly is used for the main regression algorithm
# quarterly for the better depiction of the trends and parallel plot
#-------------------------------------------------------------------------------
all_files <- list.files(path="./lfs_data", pattern = "*.csv", full.names = TRUE)

yearly_files <- all_files[str_detect(all_files, "_Y\\.csv$")]
quarterly_files <- all_files[str_detect(all_files, "Q\\d\\.csv$")]

# for analysing yearly data variables, we need one type for merge and all cols
read_lfs_file <- function(file_path) {
  df <- read_csv(file_path, 
                 col_types = cols(.default = "c"),
                 show_col_types = FALSE)
  return(df)
}

# reading all files into a list
yearly_list <- map(yearly_files, read_lfs_file)

#-------------------------------------------------------------------------------
# step 2: binding rows from all files
# we combine all yearly files and all quarterly files separately first
#-------------------------------------------------------------------------------

# bind all yearly data
yearly_data <- bind_rows(yearly_list)

# rows with all NA
na_percentages <- yearly_data %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "na_percent") %>%
  arrange(desc(na_percent))

# top NAs to understand what vars we can use
print(na_percentages, n = Inf)

necessary_columns <- c(
  "COEFF", "REFYEAR", "QUARTER", "AGE", "SEX", "ILOSTAT", "REGION",
  
  # controls
  "NATIONAL", "MARSTAT", "HHTYPE", "HHCHILDR", "EDUCLEVL", "DEGURBA",
  
  # outcomes
  "HWUSUAL", "HWACTUAL", "TEMP", "FTPT", "DURUNE",
  
  # job characteristics
  "NACE1D", "ISCO1D", "SIZEFIRM", "SUPVISOR",
  
  # unemployment
  "SEEKWORK", "WANTWORK", "AVAILBLE"
)

read_lfs_with_spec_cols <- function(file_path) {
  df <- read_csv(file_path, 
                 col_types = cols(.default = "c"), 
                 col_select = any_of(necessary_columns),
                 show_col_types = FALSE)
  return(df)
}

all_data_list <- map(all_files, read_lfs_with_spec_cols)

# binding
all_lfs_data <- bind_rows(all_data_list)

# ------------------------------------------------------------------------------
# step 3: removing unnecessary rows
# we focus only on individuals in the labor force (employed or unemployed)
# ------------------------------------------------------------------------------
glimpse(all_lfs_data)

all_lfs_data_decoded <- all_lfs_data %>%
  mutate(
    year = as.numeric(REFYEAR),
    quarter = QUARTER,
    age = as.numeric(AGE),
    coeff = as.numeric(COEFF),
    weight_norm = coeff / mean(coeff, na.rm = TRUE),
    ilostat = as.numeric(ILOSTAT),
    
    # AGE in lfs data is aggregated
    age_group = case_when(
      age == 20 ~ "young",
      age %in% c(32, 47) ~ "prime_age",
      TRUE ~ "other"
    ),
    employed = case_when(
      ILOSTAT == 1 ~ 1,
      ILOSTAT == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    treat = case_when(
      age_group == "young" ~ 1,
      age_group == "prime_age" ~ 0,
      TRUE ~ NA_real_
    ),
    # post-crisis indicator
    # we exclude 2008 as the transition year
    post = case_when(
      year %in% c(2009, 2010) ~ 1,
      year %in% c(2004,2005, 2006, 2007) ~ 0,
      TRUE ~ NA_real_
    ),
    # interaction term for did
    did = treat * post
  ) %>%
  select(-REFYEAR, -COEFF, -ILOSTAT, -AGE, -QUARTER) %>%
  filter(
    ilostat %in% c(1, 2),
    !is.na(coeff), coeff > 0,
    age_group %in% c("young", "prime_age"),
    !is.na(employed),
    !is.na(treat)
  )

glimpse(all_lfs_data_decoded)
write_csv(all_lfs_data_decoded, "output/italy_lfs_data.csv")

#-------------------------------------------------------------------------------
# step 4: parallel trends plot
#-------------------------------------------------------------------------------
all_lfs_data_decoded <- read_csv("output/italy_lfs_data.csv")

quarterly_data <- all_lfs_data_decoded %>%
  filter(
    quarter != "_A" 
  )

plot_data <- quarterly_data %>%
  mutate(
    quarter_num = case_when(
      str_detect(quarter, "Q1") ~ 0.25,
      str_detect(quarter, "Q2") ~ 0.50,
      str_detect(quarter, "Q3") ~ 0.75,
      str_detect(quarter, "Q4") ~ 1,
      TRUE ~ 0
    ),
    time_num = year + quarter_num
  ) %>%
  filter(
    age_group %in% c("young", "prime_age"),
    !is.na(employed)
  ) %>%
  group_by(time_num, age_group) %>%
  summarise(
    emp_rate = weighted.mean(employed, w = coeff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group_label = case_when(
      age_group == "young" ~ "young workers (15-24)",
      age_group == "prime_age" ~ "prime-age workers (30-50)"
    )
  )

# black and white theme (looks better for the paper)
theme_aer <- function() {
  theme_minimal(base_size = 11, base_family = "serif") +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_text(
        size = 12, 
        face = "bold", 
        hjust = 0.5,
        margin = margin(t = 15, b = 5)
      ),
      
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      legend.key.width = unit(1.5, "cm"),
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      axis.line = element_line(color = "black", linewidth = 0.3),
      axis.ticks = element_line(color = "black", linewidth = 0.3),
      panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
    )
}

parallel_trends_plot <- ggplot(plot_data, aes(x = time_num, y = emp_rate, color = group_label)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  geom_vline(xintercept = 2008, linetype = "dashed", color = "grey50", alpha = 0.7) +
  geom_vline(xintercept = 2009, linetype = "dashed", color = "grey50", alpha = 0.7) +
  annotate("text", x = 2008.5, y = 0.86, 
           label = "crisis onset", hjust = 0.5, size = 3.5, color = "grey30") +
  scale_x_continuous(breaks = seq(2004, 2011, by = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("young workers (15-24)" = "black", 
                                "prime-age workers (30-50)" = "grey50")) +
  labs(
    x = "Year",
    y = "Employment rate",
    color = NULL
  ) +
  theme_aer()

print(parallel_trends_plot)

ggsave("output/figure1_parallel_trends.png", parallel_trends_plot, width = 7, height = 4, dpi = 300)

