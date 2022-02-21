
# Packages ----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)

# Read xlsx ---------------------------------------------------------------

excel <- read_xlsx(here("Data", "excel", "USBORD_3.xlsx")) %>% 
  janitor::clean_names() 

# Cleaning ----------------------------------------------------------------

migrants_int <- excel %>% 
  rename(
    country = 1
  ) %>% 
  mutate(
    country = ifelse(row_number()==2, "country", country)
  ) %>%
  filter(
    # !is.na(country),
    !str_detect(country, "Page")
  ) %>%
  select(
    where(~!all(is.na(.) | . == TRUE | . == FALSE))
  ) %>% 
  janitor::row_to_names(row_number = 2) %>% 
  rename(
    nationwide = 25
  ) %>% 
  mutate(
    id = case_when(country == "AFGHANISTAN" ~ 2007),
    .before = country
  ) %>% 
  group_by(id) %>% 
  mutate(
    year = id + row_number()-1,
    .before = country
  ) %>% 
  ungroup() %>% 
  select(-id) %>% 
  fill(year)

migrants <- migrants_int %>% 
  filter(
    !str_detect(country, "Border|CITIZENSHIP|TOTAL"),
    year < 2018
  ) %>% 
  janitor::clean_names() %>% 
  select(
    !contains("total"),
    -nationwide
  ) %>% 
  pivot_longer(
    !c(year, country),
    names_to = "border",
    values_to = "migrants"
  ) %>% 
  mutate(
    border = str_to_upper(border),
    region = ifelse(
      border %in% c("BBT", "DRT", "ELC", "EPT", "LRT", "RGV", "SDC", "TCA", "YUM"), "Southwest",
      ifelse(
        border %in% c("BLW", "BUN", "DTM", "GFN", "HLT", "HVM", "SPW", "SWB"), "Northern",
        ifelse(
          border %in% c("MIP", "NLL", "RMY"), "Coastal", NA_character_
        )
      )
    ),
    country = str_to_title(country),
    migrants = as.numeric(migrants)
  ) 

# Save data ---------------------------------------------------------------

write_rds(
  migrants, 
  here("Data", "rds", "apprehensions_2007_2017.rds")
)

# 2018 --------------------------------------------------------------------

excel_2018 <- read_xlsx(here("Data", "excel", "USBORD_3_2018.xlsx")) %>% 
  janitor::clean_names()

migrants_2018 <- excel_2018 %>% 
  rename(
    country = 1
  ) %>% 
  mutate(
    country = ifelse(row_number()==2, "country", country)
  ) %>%
  filter(
    # !is.na(country),
    !str_detect(country, "Page")
  ) %>%
  select(
    where(~!all(is.na(.) | . == TRUE | . == FALSE))
  ) %>% 
  janitor::row_to_names(row_number = 2) %>% 
  rename(
    nationwide = 25
  ) %>% 
  mutate(
    year = 2018,
    .before = country
  ) %>% 
  janitor::clean_names() %>% 
  select(
    !contains("total"),
    -nationwide
  ) %>% 
  pivot_longer(
    !c(year, country),
    names_to = "border",
    values_to = "migrants"
  ) %>% 
  mutate(
    border = str_to_upper(border),
    region = ifelse(
      border %in% c("BBT", "DRT", "ELC", "EPT", "LRT", "RGV", "SDC", "TCA", "YUM"), "Southwest",
      ifelse(
        border %in% c("BLW", "BUN", "DTM", "GFN", "HLT", "HVM", "SPW", "SWB"), "Northern",
        ifelse(
          border %in% c("MIP", "NLL", "RMY"), "Coastal", NA_character_
        )
      )
    ),
    country = str_to_title(country),
    migrants = as.numeric(migrants)
  )

write_rds(
  migrants_2018, 
  here("Data", "rds", "apprehensions_2018.rds")
)

# 2019 --------------------------------------------------------------------

excel_2019 <- read_xlsx(here("Data", "excel", "USBORD_3_2019.xlsx")) %>% 
  janitor::clean_names()

migrants_2019 <- excel_2019 %>% 
  rename(
    country = 1
  ) %>% 
  mutate(
    country = ifelse(row_number()==2, "country", country)
  ) %>%
  filter(
    # !is.na(country),
    !str_detect(country, "Page")
  ) %>%
  select(
    where(~!all(is.na(.) | . == TRUE | . == FALSE))
  ) %>% 
  janitor::row_to_names(row_number = 2) %>% 
  rename(
    nationwide = 25
  ) %>% 
  mutate(
    year = 2019,
    .before = country
  ) %>% 
  janitor::clean_names() %>% 
  select(
    !contains("total"),
    -nationwide
  ) %>% 
  pivot_longer(
    !c(year, country),
    names_to = "border",
    values_to = "migrants"
  ) %>% 
  mutate(
    border = str_to_upper(border),
    region = ifelse(
      border %in% c("BBT", "DRT", "ELC", "EPT", "LRT", "RGV", "SDC", "TCA", "YUM"), "Southwest",
      ifelse(
        border %in% c("BLW", "BUN", "DTM", "GFN", "HLT", "HVM", "SPW", "SWB"), "Northern",
        ifelse(
          border %in% c("MIP", "NLL", "RMY"), "Coastal", NA_character_
        )
      )
    ),
    country = str_to_title(country),
    migrants = as.numeric(migrants)
  )

write_rds(
  migrants_2019, 
  here("Data", "rds", "apprehensions_2019.rds")
)

# 2020 --------------------------------------------------------------------

excel_2020 <- read_xlsx(here("Data", "excel", "USBORD_3_2020.xlsx")) %>% 
  janitor::clean_names()

migrants_2020 <- excel_2020 %>% 
  rename(
    country = 1
  ) %>% 
  mutate(
    country = ifelse(row_number()==2, "country", country)
  ) %>%
  filter(
    # !is.na(country),
    !str_detect(country, "Page")
  ) %>%
  select(
    where(~!all(is.na(.) | . == TRUE | . == FALSE))
  ) %>% 
  janitor::row_to_names(row_number = 2) %>% 
  rename(
    nationwide = 25
  ) %>% 
  mutate(
    year = 2020,
    .before = country
  ) %>% 
  janitor::clean_names() %>% 
  select(
    !contains("total"),
    -nationwide
  ) %>% 
  pivot_longer(
    !c(year, country),
    names_to = "border",
    values_to = "migrants"
  ) %>% 
  mutate(
    border = str_to_upper(border),
    region = ifelse(
      border %in% c("BBT", "DRT", "ELC", "EPT", "LRT", "RGV", "SDC", "TCA", "YUM"), "Southwest",
      ifelse(
        border %in% c("BLW", "BUN", "DTM", "GFN", "HLT", "HVM", "SPW", "SWB"), "Northern",
        ifelse(
          border %in% c("MIP", "NLL", "RMY"), "Coastal", NA_character_
        )
      )
    ),
    country = str_to_title(country),
    migrants = as.numeric(migrants)
  )

write_rds(
  migrants_2020, 
  here("Data", "rds", "apprehensions_2020.rds")
)

# All years ---------------------------------------------------------------

files <- list.files(here("Data", "rds"), full.names = TRUE, pattern = "*.rds")

all_dfs <- map_dfr(files, read_rds) %>% 
  select(
    year, country, region, border, migrants
  )

write_rds(
  all_dfs,
  here("Data", "clean", "apprehensions_2007_2020.rds")
)
