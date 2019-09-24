## code to prepare datasets in this file

library(tidyverse)

sac_salvage <- read_csv("data-raw/sac_sal.csv") %>%
  mutate(log_released = log(released))
usethis::use_data(sac_salvage, overwrite = TRUE)

# Water year definition is shifted to avoid splitting a cohort across two water year types
wy_adjusted <- read_csv("data-raw/WaterYearAdjusted.csv") %>%
  select(Date, WaterYear, WaterYearType = WYT.Existing)
usethis::use_data(wy_adjusted, overwrite = TRUE)

winter_run_size = readxl::read_excel("data-raw/winter run size.xlsx", skip = 1) %>%
  mutate(Month = match(Month, month.name),
         Length = ifelse(midpoint > max(sac_sal$length[sac_sal$winter == 1], na.rm = TRUE), max(sac_sal$length[sac_sal$winter == 1], na.rm = TRUE), midpoint)) %>%
  select(Month, Length)
usethis::use_data(winter_run_size, overwrite = TRUE)
