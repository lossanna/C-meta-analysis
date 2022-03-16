library(tidyverse)
library(readxl)
library(metafor)
library(parallel)

# Load data ---------------------------------------------------------------

raw <- read_xlsx("C-addition-studies.xlsx", sheet = "screen 3_data (1 res)")
raw$plant_apgfs <- paste(raw$plant_anper, raw$plant_gfs, sep = " ")
raw$C_app_tm <- raw$duration_first - raw$duration_last
raw$C_app_ma <- raw$C_app_tm / raw$C_app

# Data wrangling
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

nt$dfc <- c()
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

nt$dlc <- c()
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

nt$cratc <- c()
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

nt$capc <- c()
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

nt$capm <- c()
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

nt$capt <- c()
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

nt$plotc <- c()
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


# No moderators -----------------------------------------------------------

nt.mv <- rma.mv(yi = yi,
                V = vi,
                random = ~ 1 | exp_ID / obs_ID,
                data = nt)

# Detecting outliers (Habeck)
rstn <- rstandard(nt.mv)
hat <- hatvalues(nt.mv) / mean(hatvalues(nt.mv))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "no moderators")
abline(h = -3)
abline(h = 3)
abline(v = 2)

# Detecting outliers (studentized deleted residuals)
rstd <- rstudent(nt.mv, reestimate = FALSE, progbar = TRUE) # ~ 26 min (Yoga)
x <- subset(rstd, rstd$resid > 1.96)
y <- subset(rstd, rstd$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv$k / 10
dfb <- dfbetas.rma.mv(nt.mv, reestimate = FALSE, progbar = TRUE) ## 23 min (Yoga)
summary(dfb)

# full analysis with no reestimate not attempted


# Biome -------------------------------------------------------------------

nt.mv.bio <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = nt,
                    mods = ~ factor(biome) - 1)

rstn <- rstandard(nt.mv.bio)
hat <- hatvalues(nt.mv.bio) / mean(hatvalues(nt.mv.bio))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "biome")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.bio <- rstudent(nt.mv.bio, reestimate = FALSE, progbar = TRUE)
x <- subset(rstd.bio, rstd.bio$resid > 1.96)
y <- subset(rstd.bio, rstd.bio$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.bio$k / 10
dfb.bio <- dfbetas.rma.mv(nt.mv.bio, reestimate = FALSE, progbar = TRUE)
summary(dfb.bio)



# Region ------------------------------------------------------------------

nt.mv.reg <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = nt,
                    mods = ~ factor(region) - 1)

rstn <- rstandard(nt.mv.reg)
hat <- hatvalues(nt.mv.reg) / mean(hatvalues(nt.mv.reg))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "region")
abline(h = -3)
abline(h = 3)
abline(v = 2)

rstd.reg <- rstudent(nt.mv.reg, reestimate = FALSE, progbar = TRUE)
x <- subset(rstd.reg, rstd.reg$resid > 1.96)
y <- subset(rstd.reg, rstd.reg$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.reg$k / 10
dfb.reg <- dfbetas.rma.mv(nt.mv.reg, reestimate = FALSE, progbar = TRUE)
summary(dfb.reg)



# Soil suborder -----------------------------------------------------------

nt.mv.soil <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(soil_suborder) - 1)

rstn <- rstandard(nt.mv.soil)
hat <- hatvalues(nt.mv.soil) / mean(hatvalues(nt.mv.soil))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "soil_suborder")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.soil <- rstudent(nt.mv.soil, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.soil, rstd.soil$resid > 1.96)
y <- subset(rstd.soil, rstd.soil$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.soil$k / 10
dfb.soil <- dfbetas.rma.mv(nt.mv.soil, reestimate = FALSE, progbar = TRUE) 
summary(dfb.soil)



# Duration first (categorical) --------------------------------------------

nt.mv.dfc <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = nt,
                    mods = ~ factor(dfc) - 1)

rstn <- rstandard(nt.mv.dfc)
hat <- hatvalues(nt.mv.dfc) / mean(hatvalues(nt.mv.dfc))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "dfc")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.dfc <- rstudent(nt.mv.dfc, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.dfc, rstd.dfc$resid > 1.96)
y <- subset(rstd.dfc, rstd.dfc$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.dfc$k / 10
dfb.dfc <- dfbetas.rma.mv(nt.mv.dfc, reestimate = FALSE, progbar = TRUE)
summary(dfb.dfc)
subset(dfb.dfc, dfb.dfc$factor.dfc.5.6 > 1) # 651
x$slab # not influential
nt[651, c("paper", "obs_ID", "dfc")] # 1615


# Duration last (categorical) ---------------------------------------------

nt.mv.dlc <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = nt,
                    mods = ~ factor(dlc) - 1)

rstn <- rstandard(nt.mv.dlc)
hat <- hatvalues(nt.mv.dlc) / mean(hatvalues(nt.mv.dlc))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "dlc")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.dlc <- rstudent(nt.mv.dlc, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.dlc, rstd.dlc$resid > 1.96)
y <- subset(rstd.dlc, rstd.dlc$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.dlc$k / 10
dfb.dlc <- dfbetas.rma.mv(nt.mv.dlc, reestimate = FALSE, progbar = TRUE)
summary(dfb.dlc)



# C type ------------------------------------------------------------------

nt.mv.ctyp <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(C_type) - 1)

rstn <- rstandard(nt.mv.ctyp)
hat <- hatvalues(nt.mv.ctyp) / mean(hatvalues(nt.mv.ctyp))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "C_type")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.ctyp <- rstudent(nt.mv.ctyp, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.ctyp, rstd.ctyp$resid > 1.96)
y <- subset(rstd.ctyp, rstd.ctyp$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.ctyp$k / 10
dfb.ctyp <- dfbetas.rma.mv(nt.mv.ctyp, reestimate = FALSE, progbar = TRUE) 
summary(dfb.ctyp)
subset(dfb.ctyp, dfb.ctyp$factor.C_type.lignin > 1) # 230
x$slab # not influential
subset(dfb.ctyp, dfb.ctyp$factor.C_type.lignin < -1) # 229
y$slab # not influential
nt[c(230, 229), c("paper", "obs_ID", "C_type")]



# C rate (continuous) -----------------------------------------------------

nt.mv.crat <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ C_rate)

rstn <- rstandard(nt.mv.crat)
hat <- hatvalues(nt.mv.crat) / mean(hatvalues(nt.mv.crat))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "C_rate")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.crat <- rstudent(nt.mv.crat, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.crat, rstd.crat$resid > 1.96)
y <- subset(rstd.crat, rstd.crat$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.crat$k / 10
dfb.crat <- dfbetas.rma.mv(nt.mv.crat, reestimate = FALSE, progbar = TRUE) 
summary(dfb.crat)



# C rate (categorical) ----------------------------------------------------

nt.mv.cratc <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(cratc) - 1)

rstn <- rstandard(nt.mv.cratc)
hat <- hatvalues(nt.mv.cratc) / mean(hatvalues(nt.mv.cratc))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "cratc")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.cratc <- rstudent(nt.mv.cratc, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.cratc, rstd.cratc$resid > 1.96)
y <- subset(rstd.cratc, rstd.cratc$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.cratc$k / 10
dfb.cratc <- dfbetas.rma.mv(nt.mv.cratc, reestimate = FALSE, progbar = TRUE) 
summary(dfb.cratc)
subset(dfb.cratc, dfb.cratc$factor.cratc..5000 > 1) # 247, 253
x$slab # 247
nt[c(247, 253), c("paper", "obs_ID", "cratc")]


# C applications (categorical) --------------------------------------------

nt.mv.capc <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(capc) - 1)

rstn <- rstandard(nt.mv.capc)
hat <- hatvalues(nt.mv.capc) / mean(hatvalues(nt.mv.capc))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "capc")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.capc <- rstudent(nt.mv.capc, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.capc, rstd.capc$resid > 1.96)
y <- subset(rstd.capc, rstd.capc$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.capc$k / 10
dfb.capc <- dfbetas.rma.mv(nt.mv.capc, reestimate = FALSE, progbar = TRUE) 
summary(dfb.capc)


# Months between C applications -------------------------------------------

nt.mv.capm <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(capm) - 1)

rstn <- rstandard(nt.mv.capm)
hat <- hatvalues(nt.mv.capm) / mean(hatvalues(nt.mv.capm))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "capm")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.capm <- rstudent(nt.mv.capm, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.capm, rstd.capm$resid > 1.96)
y <- subset(rstd.capm, rstd.capm$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.capm$k / 10
dfb.capm <- dfbetas.rma.mv(nt.mv.capm, reestimate = FALSE, progbar = TRUE) 
summary(dfb.capm)


# Months applying C -------------------------------------------------------

nt.mv.capt <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(capt) - 1)

rstn <- rstandard(nt.mv.capt)
hat <- hatvalues(nt.mv.capt) / mean(hatvalues(nt.mv.capt))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "capt")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.capt <- rstudent(nt.mv.capt, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.capt, rstd.capt$resid > 1.96)
y <- subset(rstd.capt, rstd.capt$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.capt$k / 10
dfb.capt <- dfbetas.rma.mv(nt.mv.capt, reestimate = FALSE, progbar = TRUE) 
summary(dfb.capt)



# Annual/perennial --------------------------------------------------------

nt.mv.panp <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(plant_anper) - 1)

rstn <- rstandard(nt.mv.panp)
hat <- hatvalues(nt.mv.panp) / mean(hatvalues(nt.mv.panp))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "annual/perennial")
abline(h = -3)
abline(h = 3)
abline(v = 2)



rstd.panp <- rstudent(nt.mv.panp, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.panp, rstd.panp$resid > 1.96)
y <- subset(rstd.panp, rstd.panp$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.panp$k / 10
dfb.panp <- dfbetas.rma.mv(nt.mv.panp, reestimate = FALSE, progbar = TRUE) 
summary(dfb.panp)



# Grass/forb/shrub --------------------------------------------------------


nt.mv.pgfs <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(plant_gfs) - 1)

rstn <- rstandard(nt.mv.pgfs)
hat <- hatvalues(nt.mv.pgfs) / mean(hatvalues(nt.mv.pgfs))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "grass/forb/shrub")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.pgfs <- rstudent(nt.mv.pgfs, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.pgfs, rstd.pgfs$resid > 1.96)
y <- subset(rstd.pgfs, rstd.pgfs$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.pgfs$k / 10
dfb.pgfs <- dfbetas.rma.mv(nt.mv.pgfs, reestimate = FALSE, progbar = TRUE) 
summary(dfb.pgfs)



# Annual/perennial and grass/forb/shrub -----------------------------------

nt.mv.papgfs <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = nt,
                       mods = ~ factor(plant_apgfs) - 1)

rstn <- rstandard(nt.mv.papgfs)
hat <- hatvalues(nt.mv.papgfs) / mean(hatvalues(nt.mv.papgfs))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "annual/perennial and grass/forb/shrub")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.papgfs <- rstudent(nt.mv.papgfs, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.papgfs, rstd.papgfs$resid > 1.96)
y <- subset(rstd.papgfs, rstd.papgfs$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.papgfs$k / 10
dfb.papgfs <- dfbetas.rma.mv(nt.mv.papgfs, reestimate = FALSE, progbar = TRUE) 
summary(dfb.papgfs)
subset(dfb.papgfs, dfb.papgfs$factor.plant_apgfs.annual.unknown > 1) # 331
subset(dfb.papgfs, dfb.papgfs$factor.plant_apgfs.unknown.graminoid > 1) # 335
x$slab # not influential
nt[c(331, 335), c("paper", "obs_ID", "plant_apgfs")]



# Plot size ---------------------------------------------------------------

nt.mv.plotc <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = nt,
                       mods = ~ factor(plotc) - 1)

rstn <- rstandard(nt.mv.plotc)
hat <- hatvalues(nt.mv.plotc) / mean(hatvalues(nt.mv.plotc))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "plotc")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.plotc <- rstudent(nt.mv.plotc, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.plotc, rstd.plotc$resid > 1.96)
y <- subset(rstd.plotc, rstd.plotc$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.plotc$k / 10
dfb.plotc <- dfbetas.rma.mv(nt.mv.plotc, reestimate = FALSE, progbar = TRUE) 
summary(dfb.plotc)



# Seeding of native -------------------------------------------------------

nt.mv.seedn <- rma.mv(yi = yi,
                      V = vi,
                      random = ~ 1 | exp_ID / obs_ID,
                      data = nt,
                      mods = ~ factor(seedn) - 1)

rstn <- rstandard(nt.mv.seedn)
hat <- hatvalues(nt.mv.seedn) / mean(hatvalues(nt.mv.seedn))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "seedn")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.seedn <- rstudent(nt.mv.seedn, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.seedn, rstd.seedn$resid > 1.96)
y <- subset(rstd.seedn, rstd.seedn$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.seedn$k / 10
dfb.seedn <- dfbetas.rma.mv(nt.mv.seedn, reestimate = FALSE, progbar = TRUE) 
summary(dfb.seedn)


# Model selection 14 ------------------------------------------------------

nt.ms14 <- nt[!apply(nt[ , c("region", "dlc", "C_type", "cratc", "capt", "plant_apgfs", "plotc")], 1, anyNA), ]

nt.mv.ms14 <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt.ms14,
                     mods = ~ 1 + dlc + cratc + region + C_type + plant_apgfs)

rstn <- rstandard(nt.mv.ms14)
hat <- hatvalues(nt.mv.ms14) / mean(hatvalues(nt.mv.ms14))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "model selection 14")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.ms14 <- rstudent(nt.mv.ms14, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.ms14, rstd.ms14$resid > 1.96)
y <- subset(rstd.ms14, rstd.ms14$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
nt.mv.ms14$k / 10 
dfb.ms14 <- dfbetas.rma.mv(nt.mv.ms14, reestimate = FALSE, progbar = TRUE) 
summary(dfb.ms14)

subset(dfb.ms14, dfb.ms14$intrcpt > 1) # 351
subset(dfb.ms14, dfb.ms14$dlc3.3.5 > 1) # 351
subset(dfb.ms14, dfb.ms14$cratc.5000 > 1) # 637
subset(dfb.ms14, dfb.ms14$plant_apgfsunknown.graminoid > 1) # 325
subset(dfb.ms14, dfb.ms14$plant_apgfsunknown.unknown > 1) # 637
x$slab # none
subset(dfb.ms14, dfb.ms14$dlc3.3.5 < -1) # 637
subset(dfb.ms14, dfb.ms14$cratc420.500 < -1) # 351, 637
subset(dfb.ms14, dfb.ms14$cratc.5000 < -1) # 351
subset(dfb.ms14, dfb.ms14$C_typelignin < -1) # 351
subset(dfb.ms14, dfb.ms14$C_typelignin.AND.sucrose < -1) # 351
subset(dfb.ms14, dfb.ms14$C_typesawdust < -1) # 351
subset(dfb.ms14, dfb.ms14$C_typesucrose < -1) # 351
subset(dfb.ms14, dfb.ms14$C_typesucrose.AND.sawdust < -1) # 351
subset(dfb.ms14, dfb.ms14$plant_apgfsunknown.unknown < -1) # 351
y$slab # 351, 637
nt[c(351, 637, 325), c("paper", "obs_ID", "dlc", "cratc", "C_type", "plant_apgfs")]



save.image(".RData/nt-outliers.RData")
save.image("RMarkdown/nt-outliers.RData")
