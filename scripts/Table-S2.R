library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

raw <- read_xlsx("data/raw/C-addition-studies.xlsx", sheet = "screen 3_data")
one.res <- read_xlsx("data/raw/C-addition-studies.xlsx", sheet = "screen 3_data (1 res, nt)")
biocov <- read_xlsx("data/raw/C-addition-studies.xlsx", sheet = "screen 3_data (biocov, ex)") 


# Filter out unused rows from screen 3_data -------------------------------

# Exotic raw
raw.ex <- biocov %>% 
  filter(str_detect(plant_category, "exotic"))

# Native raw
raw.nt <- one.res %>% 
  filter(str_detect(plant_category, "native"))

# Find unused rows
nrow(raw.ex) + nrow(raw.nt)
x <- unique(c(raw.ex$obs_ID, raw.nt$obs_ID))
setdiff(raw$obs_ID, x)
length(setdiff(raw$obs_ID, x)) # length difference comes from Averett and Iannone; 
  # biomass values for native, but native uses density
  # okay to drop these values because there are better native measurements

# obs_ID for Table S2
ex.obs <- raw.ex %>%
  mutate(dataset = rep("exotic", nrow(raw.ex))) 
ex.obs <- ex.obs$obs_ID

nt.obs <- raw.nt %>%
  mutate(dataset = rep("native", nrow(raw.nt)))
nt.obs <- nt.obs$obs_ID

# Combine and reorder
s2.ex <- biocov %>% 
  filter(obs_ID %in% ex.obs) 

s2.nt <- one.res %>% 
  filter(obs_ID %in% nt.obs)

s2 <- rbind(s2.ex, s2.nt)
s2 <- s2 %>% 
  arrange(obs_ID)


# Change native seeding to categorical yes/no -----------------------------

for(i in 1:nrow(s2)) {
  if(is.na(s2$seeding_native[i])) {
    s2$seeding_native[i] <- "native seeded"
  } else if(s2$seeding_native[i] == 0) {
    s2$seeding_native[i] <- "native not seeded"
  } else {
    s2$seeding_native[i] <- "native seeded"
  }
}


# Remove unnecessary columns ----------------------------------------------

s2 <- s2 %>% 
  select(-cntrl_ID, -C_app_py, -C_app_ty, -C_year, -plant_measurement, -presence,
         -prior_veg, -removal_extent, -seeding_exotic, -plot, -factorial)


# Write to CSV ------------------------------------------------------------

write.csv(s2,
          file = "data/raw/Table-S2_meta-analysis.csv")


