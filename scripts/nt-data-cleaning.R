library(tidyverse)
library(readxl)
library(metafor)

# Load data ---------------------------------------------------------------

raw <- read_xlsx("data/raw/C-addition-studies.xlsx", sheet = "screen 3_data (1 res, nt)")

# Data wrangling ----------------------------------------------------------

raw$plant_apgfs <- paste(raw$plant_anper, raw$plant_gfs, sep = " ")
raw$C_app_tm <- raw$duration_first - raw$duration_last
raw$C_app_ma <- raw$C_app_tm / raw$C_app

trt.cntrl.se.sd <- c("biomass_mean_trt", "biomass_SE_trt", "biomass_SD_trt",
                     "biomass_mean_cntrl", "biomass_SE_cntrl", "biomass_SD_cntrl")
to.drop <- c("biomass_SE_trt", "biomass_SE_cntrl", "to_sd_trt", "to_sd_cntrl")
biomass <- raw %>% 
  select(!(contains("cover"))) %>% 
  select(!contains("density")) %>% 
  filter(!is.na(biomass_mean_trt)) %>% 
  mutate_at(trt.cntrl.se.sd, as.numeric) %>% 
  mutate(to_sd_trt = biomass_SE_trt * sqrt(n_trt)) %>% 
  mutate(to_sd_cntrl = biomass_SE_cntrl * sqrt(n_cntrl)) %>% 
  mutate(biomass_SD_trt = paste(biomass_SD_trt, to_sd_trt)) %>% 
  mutate(biomass_SD_trt = str_remove(biomass_SD_trt, pattern = "NA"), .keep = "unused") %>% 
  mutate(biomass_SD_cntrl = paste(biomass_SD_cntrl, to_sd_cntrl)) %>% 
  mutate(biomass_SD_cntrl = str_remove(biomass_SD_cntrl, pattern = "NA"), .keep = "unused") %>% 
  select(-all_of(to.drop)) %>% 
  mutate_at(c("biomass_SD_trt", "biomass_SD_cntrl"), as.numeric) %>% 
  rename(mean_trt = biomass_mean_trt) %>% 
  rename(mean_cntrl = biomass_mean_cntrl) %>% 
  rename(SD_trt = biomass_SD_trt) %>% 
  rename(SD_cntrl = biomass_SD_cntrl)

trt.cntrl.se.sd <- c("cover_mean_trt", "cover_SE_trt", "cover_SD_trt",
                     "cover_mean_cntrl", "cover_SE_cntrl", "cover_SD_cntrl")
to.drop <- c("cover_SE_trt", "cover_SE_cntrl", "to_sd_trt", "to_sd_cntrl")
cover <- raw %>% 
  select(!(contains("biomass"))) %>% 
  select(!contains("density")) %>% 
  filter(!is.na(cover_mean_cntrl)) %>% 
  mutate_at(trt.cntrl.se.sd, as.numeric) %>% 
  mutate(to_sd_trt = cover_SE_trt * sqrt(n_trt)) %>% 
  mutate(to_sd_cntrl = cover_SE_cntrl * sqrt(n_cntrl)) %>% 
  mutate(cover_SD_trt = paste(cover_SD_trt, to_sd_trt)) %>% 
  mutate(cover_SD_trt = str_remove(cover_SD_trt, pattern = "NA"), .keep = "unused") %>% 
  mutate(cover_SD_cntrl = paste(cover_SD_cntrl, to_sd_cntrl)) %>% 
  mutate(cover_SD_cntrl = str_remove(cover_SD_cntrl, pattern = "NA"), .keep = "unused") %>% 
  select(-all_of(to.drop)) %>% 
  mutate_at(c("cover_SD_trt", "cover_SD_cntrl"), as.numeric)  %>% 
  rename(mean_trt = cover_mean_trt) %>% 
  rename(mean_cntrl = cover_mean_cntrl) %>% 
  rename(SD_trt = cover_SD_trt) %>% 
  rename(SD_cntrl = cover_SD_cntrl)

trt.cntrl.se.sd <- c("density_mean_trt", "density_SE_trt", "density_SD_trt",
                     "density_mean_cntrl", "density_SE_cntrl", "density_SD_cntrl")
to.drop <- c("density_SE_trt", "density_SE_cntrl", "to_sd_trt", "to_sd_cntrl")
density <- raw %>% 
  select(!(contains("cover"))) %>% 
  select(!contains("biomass")) %>% 
  filter(!is.na(density_mean_trt)) %>% 
  mutate_at(trt.cntrl.se.sd, as.numeric) %>% 
  mutate(to_sd_trt = density_SE_trt * sqrt(n_trt)) %>% 
  mutate(to_sd_cntrl = density_SE_cntrl * sqrt(n_cntrl)) %>% 
  mutate(density_SD_trt = paste(density_SD_trt, to_sd_trt)) %>% 
  mutate(density_SD_trt = str_remove(density_SD_trt, pattern = "NA"), .keep = "unused") %>% 
  mutate(density_SD_cntrl = paste(density_SD_cntrl, to_sd_cntrl)) %>% 
  mutate(density_SD_cntrl = str_remove(density_SD_cntrl, pattern = "NA"), .keep = "unused") %>% 
  select(-all_of(to.drop)) %>% 
  filter(density_SD_trt != " NA") %>% 
  mutate_at(c("density_SD_trt", "density_SD_cntrl"), as.numeric)  %>% 
  rename(mean_trt = density_mean_trt) %>% 
  rename(mean_cntrl = density_mean_cntrl) %>% 
  rename(SD_trt = density_SD_trt) %>% 
  rename(SD_cntrl = density_SD_cntrl)

all.1res <- rbind(biomass, cover, density)


# Calculating effect size -------------------------------------------------

nt <- all.1res %>%
  filter(str_detect(plant_category, "native"))

nt <- escalc(measure = "SMD",
             n1i = n_trt,
             n2i = n_cntrl,
             m1i = mean_trt,
             m2i = mean_cntrl,
             sd1i = SD_trt,
             sd2i = SD_cntrl,
             data = nt)

nt <- nt %>% 
  filter(!is.na(yi)) # 1125 to 655 obs



# Categorical versions ----------------------------------------------------

nt$dfc <- NA
for(i in 1:nrow(nt)) {
  if(nt$duration_first[i] == 3) {
    nt$dfc[i] <- "3"
  } else if(between(nt$duration_first[i], 5, 6)) {
    nt$dfc[i] <- "5-6"
  } else if(between(nt$duration_first[i], 7, 12)) {
    nt$dfc[i] <- "7-12"
  } else if(between(nt$duration_first[i], 12, 18)) {
    nt$dfc[i] <- "12-18"
  } else if(between(nt$duration_first[i], 19, 24)) {
    nt$dfc[i] <- "19-24"
  } else if(between(nt$duration_first[i], 25, 36)) {
    nt$dfc[i] <- "25-36"
  } else if(between(nt$duration_first[i], 37, 50)) {
    nt$dfc[i] <- "37-50"
  } else if(between(nt$duration_first[i], 100, 200)) {
    nt$dfc[i] <- "100-200"
  } else {
    nt$dfc[i] <- ">200"
  }
}
nt$dfc <- factor(nt$df, levels = c("3", "5-6", "7-12", "12-18", "19-24", "25-36", "37-50", "100-200", ">200"))

nt$dlc <- NA
for(i in 1:nrow(nt)) {
  if(between(nt$duration_last[i], 0, 1.5)) {
    nt$dlc[i] <- "0-1.5"
  } else if(nt$duration_last[i] == 2) {
    nt$dlc[i] <- "2"
  } else if(between(nt$duration_last[i], 3, 3.5)) {
    nt$dlc[i] <- "3-3.5"
  } else if(between(nt$duration_last[i], 4, 6)) {
    nt$dlc[i] <- "4-6"
  } else if(between(nt$duration_last[i], 7, 12)) {
    nt$dlc[i] <- "7-12"
  } else if(between(nt$duration_last[i], 13, 18)) {
    nt$dlc[i] <- "13-18"
  } else if(between(nt$duration_last[i], 19, 24)) {
    nt$dlc[i] <- "19-24"
  } else if(between(nt$duration_last[i], 36, 49)) {
    nt$dlc[i] <- "36-49"
  } else {
    nt$dlc[i] <- ">100"
  }
}
nt$dlc <- factor(nt$dl, levels = c("0-1.5", "2", "3-3.5", "4-6", "7-12", "13-18", "19-24", 
                                   "36-49", ">100"))

nt$cratc <- NA
for(i in 1:nrow(nt)) {
  if(is.na(nt$C_rate[i])) {
    nt$cratc[i] <- "idk"
  } else if(between(nt$C_rate[i], 0, 100)) {
    nt$cratc[i] <- "30-100"
  } else if(between(nt$C_rate[i], 101, 160)) {
    nt$cratc[i] <- "133-160"
  } else if(between(nt$C_rate[i], 161, 200)) {
    nt$cratc[i] <- "174-200"
  } else if(between(nt$C_rate[i], 201, 300)) {
    nt$cratc[i] <- "210-300"
  } else if(between(nt$C_rate[i], 301, 400)) {
    nt$cratc[i] <- "330-400"
  } else if(between(nt$C_rate[i], 401, 500)) {
    nt$cratc[i] <- "420-500"
  } else if(between(nt$C_rate[i], 501, 600)) {
    nt$cratc[i] <- "506-600"
  } else if(between(nt$C_rate[i], 630, 700)) {
    nt$cratc[i] <- "620-700"
  } else if(between(nt$C_rate[i], 714, 999)) {
    nt$cratc[i] <- "714-999"
  } else if(between(nt$C_rate[i], 1000, 1330)) {
    nt$cratc[i] <- "1000-1330"
  } else if(between(nt$C_rate[i], 1600, 2000)) {
    nt$cratc[i] <- "1600-2000"
  } else if(between(nt$C_rate[i], 2001, 3000)) {
    nt$cratc[i] <- "2110-3000"
  } else if(between(nt$C_rate[i], 3001, 5000)) {
    nt$cratc[i] <- "3346-5000"
  } else {
    nt$cratc[i] <- ">5000"
  }
}
nt$cratc <- factor(nt$cratc, levels = c("30-100", "133-160", "174-200", "210-300", "330-400", "420-500", 
                                        "506-600", "620-700", "714-999", "1000-1330", "1600-2000",
                                        "2110-3000", "3346-5000", ">5000"))

nt$capc <- NA
for(i in 1:nrow(nt)) {
  if(nt$C_app[i] == 1) {
    nt$capc[i] <- "1"
  } else if(nt$C_app[i] == 2) {
    nt$capc[i] <- "2"
  } else if(between(nt$C_app[i], 3, 6)) {
    nt$capc[i] <- "3-6"
  } else if(between(nt$C_app[i], 7, 10)) {
    nt$capc[i] <- "7-10"
  } else if(between(nt$C_app[i], 15, 22)) {
    nt$capc[i] <- "15-22"
  } else if(between(nt$C_app[i], 32, 40)) {
    nt$capc[i] <- "32-40"
  } else {
    nt$capc[i] <- ">40"
  }
}
nt$capc <- factor(nt$capc, levels = c("1", "2", "3-6", "7-10", "15-22", "32-40", ">40"))

nt$capm <- NA
for(i in 1:nrow(nt)) {
  if(nt$C_app_ma[i] == 0) {
    nt$capm[i] <- "1 application total"
  } else if(between(nt$C_app_ma[i], 0.001, 1)) {
    nt$capm[i] <- "<1"
  } else if(between(nt$C_app_ma[i], 1.001, 2)) {
    nt$capm[i] <- "1-2"
  } else if(between(nt$C_app_ma[i], 2.001, 4)) {
    nt$capm[i] <- "2-4"
  } else {
    nt$capm[i] <- "4-11"
  }
}
nt$capm <- factor(nt$capm, levels = c("1 application total", "<1", "1-2", "2-4", "4-11"))

nt$capt <- NA
for(i in 1:nrow(nt)) {
  if(nt$C_app_tm[i] == 0) {
    nt$capt[i] <- "1 application total"
  } else if(between(nt$C_app_tm[i], 0.001, 6)) {
    nt$capt[i] <- "1-6"
  } else if(between(nt$C_app_tm[i], 7, 12)) {
    nt$capt[i] <- "7-12"
  } else if(between(nt$C_app_tm[i], 13, 24)) {
    nt$capt[i] <- "13-24"
  } else if(between(nt$C_app_tm[i], 25, 36)) {
    nt$capt[i] <- "25-36"
  } else if(between(nt$C_app_tm[i], 37, 48)) {
    nt$capt[i] <- "37-48"
  } else {
    nt$capt[i] <- ">48"
  }
}
nt$capt <- factor(nt$capt, levels = c("1 application total", "1-6", "7-12", "13-24", 
                                      "25-36", "37-48", ">48"))

nt$plotc <- NA
for(i in 1:nrow(nt)) {
  if(is.na(nt$plot[i])) {
    nt$plotc[i] <- "idk"
  } else if(nt$plot[i] < 1) {
    nt$plotc[i] <- "<1"
  } else if(nt$plot[i] == 1) {
    nt$plotc[i] <- "1"
  } else if(between(nt$plot[i], 1.3, 3.75)) {
    nt$plotc[i] <- "1.3-3.75"
  } else if(nt$plot[i] == 4) {
    nt$plotc[i] <- "4"
  } else if(between(nt$plot[i], 6, 9)) {
    nt$plotc[i] <- "6-9"
  } else if(nt$plot[i] == 12) {
    nt$plotc[i] <- "12"
  } else if(between(nt$plot[i], 12.5, 16)) {
    nt$plotc[i] <- "12.5-16"
  } else if(nt$plot[i] == 25) {
    nt$plotc[i] <- "25"
  } else if(between(nt$plot[i], 28, 50)) {
    nt$plotc[i] <- "28-50"
  } else if(nt$plot[i] == 75) {
    nt$plotc[i] <- "75"
  } else {
    nt$plotc[i] <- ">100"
  }
}
nt$plotc <- factor(nt$plotc, 
                   levels = c("<1", "1", "1.3-3.75", "4", "6-9", "12", "12.5-16", 
                              "25", "28-50", "75", ">100"))

nt <- nt %>% 
  mutate(seedn = rep(NA, nrow(nt)))
for(i in 1:nrow(nt)) {
  if(is.na(nt$seeding_native[i])) {
    nt$seedn[i] <- "native seeded"
  } else if(nt$seeding_native[i] == 0) {
    nt$seedn[i] <- "native not seeded"
  } else {
    nt$seedn[i] <- "native seeded"
  }
}


write.csv(nt,
          file = "data/cleaned/native-cleaned.csv",
          row.names = FALSE)
