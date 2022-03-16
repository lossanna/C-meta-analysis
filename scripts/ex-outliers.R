library(tidyverse)
library(readxl)
library(metafor)

# Load data ---------------------------------------------------------------

raw <- read_xlsx("C addition studies.xlsx", sheet = "screen 3_data (biocov)")
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
biomass$res <- rep("biomass", nrow(biomass))

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
  rename(SD_cntrl = cover_SD_cntrl) # warning because one Burke point is blank (was <1)
cover$res <- rep("cover", nrow(cover))

all.1res <- rbind(biomass, cover)


# Calculating effect size -------------------------------------------------

ex <- all.1res %>%
  filter(str_detect(plant_category, "exotic"))

ex <- escalc(measure = "SMD",
             n1i = n_trt,
             n2i = n_cntrl,
             m1i = mean_trt,
             m2i = mean_cntrl,
             sd1i = SD_trt,
             sd2i = SD_cntrl,
             data = ex)

ex <- ex %>% 
  filter(!is.na(yi)) # 568 to 488 obs



# Categorical versions ----------------------------------------------------

ex$dfc <- c()
for(i in 1:nrow(ex)) {
  if(ex$duration_first[i] == 3) {
    ex$dfc[i] <- "3"
  } else if(between(ex$duration_first[i], 5, 6)) {
    ex$dfc[i] <- "5-6"
  } else if(between(ex$duration_first[i], 7, 12)) {
    ex$dfc[i] <- "7-12"
  } else if(between(ex$duration_first[i], 12, 18)) {
    ex$dfc[i] <- "12-18"
  } else if(between(ex$duration_first[i], 19, 24)) {
    ex$dfc[i] <- "19-24"
  } else if(between(ex$duration_first[i], 25, 36)) {
    ex$dfc[i] <- "25-36"
  } else if(between(ex$duration_first[i], 37, 50)) {
    ex$dfc[i] <- "37-50"
  } else if(between(ex$duration_first[i], 100, 200)) {
    ex$dfc[i] <- "100-200"
  } else {
    ex$dfc[i] <- ">200"
  }
}
ex$dfc <- factor(ex$df, levels = c("3", "5-6", "7-12", "12-18", "19-24", "25-36", "37-50", "100-200", ">200"))

ex$dlc <- c()
for(i in 1:nrow(ex)) {
  if(between(ex$duration_last[i], 0, 1.5)) {
    ex$dlc[i] <- "0-1.5"
  } else if(ex$duration_last[i] == 2) {
    ex$dlc[i] <- "2"
  } else if(between(ex$duration_last[i], 3, 3.5)) {
    ex$dlc[i] <- "3-3.5"
  } else if(between(ex$duration_last[i], 4, 6)) {
    ex$dlc[i] <- "4-6"
  } else if(between(ex$duration_last[i], 7, 12)) {
    ex$dlc[i] <- "7-12"
  } else if(between(ex$duration_last[i], 13, 18)) {
    ex$dlc[i] <- "13-18"
  } else if(between(ex$duration_last[i], 19, 24)) {
    ex$dlc[i] <- "19-24"
  } else if(between(ex$duration_last[i], 36, 49)) {
    ex$dlc[i] <- "36-49"
  } else {
    ex$dlc[i] <- ">100"
  }
}
ex$dlc <- factor(ex$dl, levels = c("0-1.5", "2", "3-3.5", "4-6", "7-12", "13-18", "19-24", 
                                   "36-49", ">100"))

ex$cratc <- c()
for(i in 1:nrow(ex)) {
  if(is.na(ex$C_rate[i])) {
    ex$cratc[i] <- "idk"
  } else if(between(ex$C_rate[i], 0, 100)) {
    ex$cratc[i] <- "30-100"
  } else if(between(ex$C_rate[i], 101, 160)) {
    ex$cratc[i] <- "133-160"
  } else if(between(ex$C_rate[i], 161, 200)) {
    ex$cratc[i] <- "174-200"
  } else if(between(ex$C_rate[i], 201, 300)) {
    ex$cratc[i] <- "210-300"
  } else if(between(ex$C_rate[i], 301, 400)) {
    ex$cratc[i] <- "330-400"
  } else if(between(ex$C_rate[i], 401, 500)) {
    ex$cratc[i] <- "420-500"
  } else if(between(ex$C_rate[i], 501, 600)) {
    ex$cratc[i] <- "506-600"
  } else if(between(ex$C_rate[i], 630, 700)) {
    ex$cratc[i] <- "620-700"
  } else if(between(ex$C_rate[i], 714, 999)) {
    ex$cratc[i] <- "714-999"
  } else if(between(ex$C_rate[i], 1000, 1330)) {
    ex$cratc[i] <- "1000-1330"
  } else if(between(ex$C_rate[i], 1600, 2000)) {
    ex$cratc[i] <- "1600-2000"
  } else if(between(ex$C_rate[i], 2001, 3000)) {
    ex$cratc[i] <- "2110-3000"
  } else if(between(ex$C_rate[i], 3001, 5000)) {
    ex$cratc[i] <- "3346-5000"
  } else {
    ex$cratc[i] <- ">5000"
  }
}
ex$cratc <- factor(ex$cratc, levels = c("30-100", "133-160", "174-200", "210-300", "330-400", "420-500", 
                                        "506-600", "620-700", "714-999", "1000-1330", "1600-2000",
                                        "2110-3000", "3346-5000", ">5000"))

ex$capc <- c()
for(i in 1:nrow(ex)) {
  if(ex$C_app[i] == 1) {
    ex$capc[i] <- "1"
  } else if(ex$C_app[i] == 2) {
    ex$capc[i] <- "2"
  } else if(between(ex$C_app[i], 3, 6)) {
    ex$capc[i] <- "3-6"
  } else if(between(ex$C_app[i], 7, 10)) {
    ex$capc[i] <- "7-10"
  } else if(between(ex$C_app[i], 15, 22)) {
    ex$capc[i] <- "15-22"
  } else if(between(ex$C_app[i], 32, 40)) {
    ex$capc[i] <- "32-40"
  } else {
    ex$capc[i] <- ">40"
  }
}
ex$capc <- factor(ex$capc, levels = c("1", "2", "3-6", "7-10", "15-22", "32-40", ">40"))

ex$capm <- c()
for(i in 1:nrow(ex)) {
  if(ex$C_app_ma[i] == 0) {
    ex$capm[i] <- "1 application total"
  } else if(between(ex$C_app_ma[i], 0.001, 1)) {
    ex$capm[i] <- "<1"
  } else if(between(ex$C_app_ma[i], 1.001, 2)) {
    ex$capm[i] <- "1-2"
  } else if(between(ex$C_app_ma[i], 2.001, 4)) {
    ex$capm[i] <- "2-4"
  } else {
    ex$capm[i] <- "4-11"
  }
}
ex$capm <- factor(ex$capm, levels = c("1 application total", "<1", "1-2", "2-4", "4-11"))

ex$capt <- c()
for(i in 1:nrow(ex)) {
  if(ex$C_app_tm[i] == 0) {
    ex$capt[i] <- "1 application total"
  } else if(between(ex$C_app_tm[i], 0.001, 6)) {
    ex$capt[i] <- "1-6"
  } else if(between(ex$C_app_tm[i], 7, 12)) {
    ex$capt[i] <- "7-12"
  } else if(between(ex$C_app_tm[i], 13, 24)) {
    ex$capt[i] <- "13-24"
  } else if(between(ex$C_app_tm[i], 25, 36)) {
    ex$capt[i] <- "25-36"
  } else if(between(ex$C_app_tm[i], 37, 48)) {
    ex$capt[i] <- "37-48"
  } else {
    ex$capt[i] <- ">48"
  }
}
ex$capt <- factor(ex$capt, levels = c("1 application total", "1-6", "7-12", "13-24", 
                                      "25-36", "37-48", ">48"))

ex$plotc <- c()
for(i in 1:nrow(ex)) {
  if(is.na(ex$plot[i])) {
    ex$plotc[i] <- "idk"
  } else if(ex$plot[i] < 1) {
    ex$plotc[i] <- "<1"
  } else if(ex$plot[i] == 1) {
    ex$plotc[i] <- "1"
  } else if(between(ex$plot[i], 1.3, 3.75)) {
    ex$plotc[i] <- "1.3-3.75"
  } else if(ex$plot[i] == 4) {
    ex$plotc[i] <- "4"
  } else if(between(ex$plot[i], 6, 9)) {
    ex$plotc[i] <- "6-9"
  } else if(ex$plot[i] == 12) {
    ex$plotc[i] <- "12"
  } else if(between(ex$plot[i], 12.5, 16)) {
    ex$plotc[i] <- "12.5-16"
  } else if(ex$plot[i] == 25) {
    ex$plotc[i] <- "25"
  } else if(between(ex$plot[i], 28, 50)) {
    ex$plotc[i] <- "28-50"
  } else if(ex$plot[i] == 75) {
    ex$plotc[i] <- "75"
  } else {
    ex$plotc[i] <- ">100"
  }
}
ex$plotc <- factor(ex$plotc, 
                   levels = c("<1", "1", "1.3-3.75", "4", "6-9", "12", "12.5-16", 
                              "25", "28-50", "75", ">100"))

ex <- ex %>% 
  mutate(seedn = rep(NA, nrow(ex)))
for(i in 1:nrow(ex)) {
  if(is.na(ex$seeding_native[i])) {
    ex$seedn[i] <- "native seeded"
  } else if(ex$seeding_native[i] == 0) {
    ex$seedn[i] <- "native not seeded"
  } else {
    ex$seedn[i] <- "native seeded"
  }
}


# No moderators -----------------------------------------------------------

ex.mv <- rma.mv(yi = yi,
                V = vi,
                random = ~ 1 | exp_ID / obs_ID,
                data = ex)

# Detecting outliers (Habeck)
rstn <- rstandard(ex.mv)
hat <- hatvalues(ex.mv) / mean(hatvalues(ex.mv))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "no moderators")
abline(h = -3)
abline(h = 3)
abline(v = 2)

text(hat, rstn$resid, labels = ex$obs_ID, cex = 1, pos = 4) # 457, 601
filter(ex, obs_ID == 457)[ , c("paper", "obs_ID")]
filter(ex, obs_ID == 601)[ , c("paper", "obs_ID")]

# Detecting outliers (studentized deleted residuals)
rstd <- rstudent(ex.mv, reestimate = FALSE, progbar = TRUE) # ~ 4 min (ThinkPad)
x <- subset(rstd, rstd$resid > 1.96)
y <- subset(rstd, rstd$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv$k / 10
dfb <- dfbetas.rma.mv(ex.mv, reestimate = FALSE, progbar = TRUE) # 3-4 min (ThinkPad)
summary(dfb)

# reestimate = TRUE not attempted


# Biome -------------------------------------------------------------------

ex.mv.bio <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex,
                    mods = ~ factor(biome) - 1)

rstn <- rstandard(ex.mv.bio)
hat <- hatvalues(ex.mv.bio) / mean(hatvalues(ex.mv.bio))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "biome")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.bio <- rstudent(ex.mv.bio, reestimate = FALSE, progbar = TRUE) # ~ 4 min (ThinkPad)
x <- subset(rstd.bio, rstd.bio$resid > 1.96)
y <- subset(rstd.bio, rstd.bio$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.bio$k / 10
dfb.bio <- dfbetas.rma.mv(ex.mv.bio, reestimate = FALSE, progbar = TRUE)
summary(dfb.bio)

subset(dfb.bio, dfb.bio$factor.biome.fynbos > 1) # 339
x$slab # none
subset(dfb.bio, dfb.bio$factor.biome.fynbos < -1) # 338
y$slab # none
ex[c(339, 338), c("paper", "obs_ID", "biome")]


# Region ------------------------------------------------------------------

ex.mv.reg <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex,
                    mods = ~ factor(region) - 1)

rstn <- rstandard(ex.mv.reg)
hat <- hatvalues(ex.mv.reg) / mean(hatvalues(ex.mv.reg))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "region")
abline(h = -3)
abline(h = 3)
abline(v = 2)

text(hat, rstn$resid, labels = ex$obs_ID, cex = 1, pos = 4) # 601, 457
filter(ex, obs_ID == 601)[ , c("paper", "obs_ID", "region")]
filter(ex, obs_ID == 457)[ , c("paper", "obs_ID", "region")]


rstd.reg <- rstudent(ex.mv.reg, reestimate = FALSE, progbar = TRUE) # ~ 4 min (ThinkPad)
x <- subset(rstd.reg, rstd.reg$resid > 1.96)
y <- subset(rstd.reg, rstd.reg$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.reg$k / 10
dfb.reg <- dfbetas.rma.mv(ex.mv.reg, reestimate = FALSE, progbar = TRUE) # ~ 4 min (ThinkPad)
summary(dfb.reg)

subset(dfb.reg, dfb.reg$factor.region.South.Africa > 1) # 339
x$slab # none
subset(dfb.reg, dfb.reg$factor.region.South.Africa < -1) # 338
y$slab # none
ex[c(339, 338), c("paper", "obs_ID", "region")]


# Soil suborder -----------------------------------------------------------

ex.mv.soil <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex,
                     mods = ~ factor(soil_suborder) - 1)

rstn <- rstandard(ex.mv.soil)
hat <- hatvalues(ex.mv.soil) / mean(hatvalues(ex.mv.soil))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "soil_suborder")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.soil <- rstudent(ex.mv.soil, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.soil, rstd.soil$resid > 1.96)
y <- subset(rstd.soil, rstd.soil$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.soil$k / 10
dfb.soil <- dfbetas.rma.mv(ex.mv.soil, reestimate = FALSE, progbar = TRUE) 
summary(dfb.soil)



# Duration first (continuous) ---------------------------------------------

ex.mv.df <- rma.mv(yi = yi,
                   V = vi,
                   random = ~ 1 | exp_ID / obs_ID,
                   data = ex,
                   mods = ~ duration_first)

rstn <- rstandard(ex.mv.df)
hat <- hatvalues(ex.mv.df) / mean(hatvalues(ex.mv.df))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "duration_first")
abline(h = -3)
abline(h = 3)
abline(v = 2)

rstd.df <- rstudent(ex.mv.df, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.df, rstd.df$resid > 1.96)
y <- subset(rstd.df, rstd.df$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.df$k / 10 
dfb.df <- dfbetas.rma.mv(ex.mv.df, reestimate = FALSE, progbar = TRUE) 
summary(dfb.df)


# Duration first (categorical) --------------------------------------------

ex.mv.dfc <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex,
                    mods = ~ factor(dfc) - 1)

rstn <- rstandard(ex.mv.dfc)
hat <- hatvalues(ex.mv.dfc) / mean(hatvalues(ex.mv.dfc))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "dfc")
abline(h = -3)
abline(h = 3)
abline(v = 2)

text(hat, rstn$resid, labels = ex$obs_ID, cex = 1, pos = 4) # 457
filter(ex, obs_ID == 457)[ , c("paper", "obs_ID", "dfc")]


rstd.dfc <- rstudent(ex.mv.dfc, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.dfc, rstd.dfc$resid > 1.96)
y <- subset(rstd.dfc, rstd.dfc$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]]) #53
ex.mv.dfc$k / 10 # outliers > k/10; 48.8
k.dfc <- x[order(x$resid, decreasing = TRUE), ]
k.dfc[1:5, ]
ex[c(38, 26, 35, 34, 30), c("paper", "obs_ID", "dfc")]
dfb.dfc <- dfbetas.rma.mv(ex.mv.dfc, reestimate = FALSE, progbar = TRUE)
summary(dfb.dfc)




# Duration last (continuous) ----------------------------------------------

ex.mv.dl <- rma.mv(yi = yi,
                   V = vi,
                   random = ~ 1 | exp_ID / obs_ID,
                   data = ex,
                   mods = ~ duration_last)

rstn <- rstandard(ex.mv.dl)
hat <- hatvalues(ex.mv.dl) / mean(hatvalues(ex.mv.dl))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "duration_last")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.dl <- rstudent(ex.mv.dl, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.dl, rstd.dl$resid > 1.96)
y <- subset(rstd.dl, rstd.dl$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.dl$k / 10 
dfb.dl <- dfbetas.rma.mv(ex.mv.dl, reestimate = FALSE, progbar = TRUE) 
summary(dfb.dl)




# Duration last (categorical) ---------------------------------------------

ex.mv.dlc <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex,
                    mods = ~ factor(dlc) - 1)

rstn <- rstandard(ex.mv.dlc)
hat <- hatvalues(ex.mv.dlc) / mean(hatvalues(ex.mv.dlc))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "dlc")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.dlc <- rstudent(ex.mv.dlc, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.dlc, rstd.dlc$resid > 1.96)
y <- subset(rstd.dlc, rstd.dlc$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.dlc$k / 10
dfb.dlc <- dfbetas.rma.mv(ex.mv.dlc, reestimate = FALSE, progbar = TRUE)
summary(dfb.dlc)

subset(dfb.dlc, dfb.dlc$factor.dlc..100 < -1) # 5
y$slab # none
ex[5, c("paper", "obs_ID", "dlc")]


# C type ------------------------------------------------------------------

ex.mv.ctyp <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex,
                     mods = ~ factor(C_type) - 1)

rstn <- rstandard(ex.mv.ctyp)
hat <- hatvalues(ex.mv.ctyp) / mean(hatvalues(ex.mv.ctyp))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "C_type")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.ctyp <- rstudent(ex.mv.ctyp, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.ctyp, rstd.ctyp$resid > 1.96)
y <- subset(rstd.ctyp, rstd.ctyp$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.ctyp$k / 10
dfb.ctyp <- dfbetas.rma.mv(ex.mv.ctyp, reestimate = FALSE, progbar = TRUE) 
summary(dfb.ctyp)



# C rate (continuous) -----------------------------------------------------

ex.mv.crat <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex,
                     mods = ~ C_rate)

rstn <- rstandard(ex.mv.crat)
hat <- hatvalues(ex.mv.crat) / mean(hatvalues(ex.mv.crat))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "C_rate")
abline(h = -3)
abline(h = 3)
abline(v = 2)

text(hat, rstn$resid, labels = ex$obs_ID, cex = 1, pos = 4) # 54, 120, 132
filter(ex, obs_ID == 54)[ , c("paper", "obs_ID", "C_rate")]
filter(ex, obs_ID == 120)[ , c("paper", "obs_ID", "C_rate")]
filter(ex, obs_ID == 132)[ , c("paper", "obs_ID", "C_rate")]


rstd.crat <- rstudent(ex.mv.crat, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.crat, rstd.crat$resid > 1.96)
y <- subset(rstd.crat, rstd.crat$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.crat$k / 10
dfb.crat <- dfbetas.rma.mv(ex.mv.crat, reestimate = FALSE, progbar = TRUE) 
summary(dfb.crat)


# C rate (categorical) ----------------------------------------------------

ex.mv.cratc <- rma.mv(yi = yi,
                      V = vi,
                      random = ~ 1 | exp_ID / obs_ID,
                      data = ex,
                      mods = ~ factor(cratc) - 1)

rstn <- rstandard(ex.mv.cratc)
hat <- hatvalues(ex.mv.cratc) / mean(hatvalues(ex.mv.cratc))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "cractc")
abline(h = -3)
abline(h = 3)
abline(v = 2)

rstd.cratc <- rstudent(ex.mv.cratc, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.cratc, rstd.cratc$resid > 1.96)
y <- subset(rstd.cratc, rstd.cratc$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.cratc$k / 10
dfb.cratc <- dfbetas.rma.mv(ex.mv.cratc, reestimate = FALSE, progbar = TRUE) 
summary(dfb.cratc)
subset(dfb.cratc, dfb.cratc$factor.cratc..5000 > 1) # 331
x$slab # 331
ex[331, c("paper", "obs_ID", "cratc")]



# C applications (continuous) ---------------------------------------------

ex.mv.cap <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex,
                    mods = ~ C_app)

rstn <- rstandard(ex.mv.cap)
hat <- hatvalues(ex.mv.cap) / mean(hatvalues(ex.mv.cap))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "C_app")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.cap <- rstudent(ex.mv.cap, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.cap, rstd.cap$resid > 1.96)
y <- subset(rstd.cap, rstd.cap$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.cap$k / 10
dfb.cap <- dfbetas.rma.mv(ex.mv.cap, reestimate = FALSE, progbar = TRUE) 
summary(dfb.cap)




# C applications (categorical) --------------------------------------------

ex.mv.capc <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex,
                     mods = ~ factor(capc) - 1)

rstn <- rstandard(ex.mv.capc)
hat <- hatvalues(ex.mv.capc) / mean(hatvalues(ex.mv.capc))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "capc")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.capc <- rstudent(ex.mv.capc, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.capc, rstd.capc$resid > 1.96)
y <- subset(rstd.capc, rstd.capc$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.capc$k / 10
dfb.capc <- dfbetas.rma.mv(ex.mv.capc, reestimate = FALSE, progbar = TRUE) 
summary(dfb.capc)
subset(dfb.capc, dfb.capc$factor.capc.15.22 > 1) # 322
x$slab # none
ex[322, c("paper", "obs_ID", "capc")]


# Months between C applications -------------------------------------------

ex.mv.capm <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex,
                     mods = ~ factor(capm) - 1)

rstn <- rstandard(ex.mv.capm)
hat <- hatvalues(ex.mv.capm) / mean(hatvalues(ex.mv.capm))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "capm")
abline(h = -3)
abline(h = 3)
abline(v = 2)

text(hat, rstn$resid, labels = ex$obs_ID, cex = 1, pos = 4) # 257, 457, 601
filter(ex, obs_ID == 257)[ , c("paper", "obs_ID", "capm")]
filter(ex, obs_ID == 457)[ , c("paper", "obs_ID", "capm")]
filter(ex, obs_ID == 601)[ , c("paper", "obs_ID", "capm")]

rstd.capm <- rstudent(ex.mv.capm, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.capm, rstd.capm$resid > 1.96)
y <- subset(rstd.capm, rstd.capm$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.capm$k / 10
dfb.capm <- dfbetas.rma.mv(ex.mv.capm, reestimate = FALSE, progbar = TRUE) 
summary(dfb.capm)


# Months applying C -------------------------------------------------------

ex.mv.capt <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex,
                     mods = ~ factor(capt) - 1)

rstn <- rstandard(ex.mv.capt)
hat <- hatvalues(ex.mv.capt) / mean(hatvalues(ex.mv.capt))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "capt")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.capt <- rstudent(ex.mv.capt, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.capt, rstd.capt$resid > 1.96)
y <- subset(rstd.capt, rstd.capt$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.capt$k / 10
dfb.capt <- dfbetas.rma.mv(ex.mv.capt, reestimate = FALSE, progbar = TRUE) 
summary(dfb.capt)



# Annual/perennial --------------------------------------------------------

ex.mv.panp <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex,
                     mods = ~ factor(plant_anper) - 1)

rstn <- rstandard(ex.mv.panp)
hat <- hatvalues(ex.mv.panp) / mean(hatvalues(ex.mv.panp))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "annual/perennial")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.panp <- rstudent(ex.mv.panp, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.panp, rstd.panp$resid > 1.96)
y <- subset(rstd.panp, rstd.panp$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.panp$k / 10
dfb.panp <- dfbetas.rma.mv(ex.mv.panp, reestimate = FALSE, progbar = TRUE) 
summary(dfb.panp)



# Grass/forb/shrub --------------------------------------------------------


ex.mv.pgfs <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex,
                     mods = ~ factor(plant_gfs) - 1)

rstn <- rstandard(ex.mv.pgfs)
hat <- hatvalues(ex.mv.pgfs) / mean(hatvalues(ex.mv.pgfs))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "grass/forb/shrub")
abline(h = -3)
abline(h = 3)
abline(v = 2)

text(hat, rstn$resid, labels = ex$obs_ID, cex = 1, pos = 4) # 257, 258
filter(ex, obs_ID == 257)[ , c("paper", "obs_ID", "plant_gfs")]
filter(ex, obs_ID == 258)[ , c("paper", "obs_ID", "plant_gfs")]


rstd.pgfs <- rstudent(ex.mv.pgfs, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.pgfs, rstd.pgfs$resid > 1.96)
y <- subset(rstd.pgfs, rstd.pgfs$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.pgfs$k / 10
dfb.pgfs <- dfbetas.rma.mv(ex.mv.pgfs, reestimate = FALSE, progbar = TRUE) 
summary(dfb.pgfs)



# Annual/perennial and grass/forb/shrub -----------------------------------

ex.mv.papgfs <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = ex,
                       mods = ~ factor(plant_apgfs) - 1)

rstn <- rstandard(ex.mv.papgfs)
hat <- hatvalues(ex.mv.papgfs) / mean(hatvalues(ex.mv.papgfs))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "annual/perennial and grass/forb/shrub")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.papgfs <- rstudent(ex.mv.papgfs, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.papgfs, rstd.papgfs$resid > 1.96)
y <- subset(rstd.papgfs, rstd.papgfs$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.papgfs$k / 10
dfb.papgfs <- dfbetas.rma.mv(ex.mv.papgfs, reestimate = FALSE, progbar = TRUE) 
summary(dfb.papgfs)


# Plot size (categorical) -------------------------------------------------

ex.mv.plotc <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = ex,
                       mods = ~ factor(plotc) - 1)

rstn <- rstandard(ex.mv.plotc)
hat <- hatvalues(ex.mv.plotc) / mean(hatvalues(ex.mv.plotc))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "plot")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.plotc <- rstudent(ex.mv.plotc, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.plotc, rstd.plotc$resid > 1.96)
y <- subset(rstd.plotc, rstd.plotc$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.plotc$k / 10
dfb.plotc <- dfbetas.rma.mv(ex.mv.plotc, reestimate = FALSE, progbar = TRUE) 
summary(dfb.plotc)



# Seeding of native -------------------------------------------------------

ex.mv.seedn <- rma.mv(yi = yi,
                      V = vi,
                      random = ~ 1 | exp_ID / obs_ID,
                      data = ex,
                      mods = ~ factor(seedn) - 1)

rstn <- rstandard(ex.mv.seedn)
hat <- hatvalues(ex.mv.seedn) / mean(hatvalues(ex.mv.seedn))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "plot")
abline(h = -3)
abline(h = 3)
abline(v = 2)

text(hat, rstn$resid, labels = ex$obs_ID, cex = 1, pos = 4) # 457
filter(ex, obs_ID == 457)[ , c("paper", "obs_ID", "seedn")]


rstd.seedn <- rstudent(ex.mv.seedn, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.seedn, rstd.seedn$resid > 1.96)
y <- subset(rstd.seedn, rstd.seedn$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.seedn$k / 10
dfb.seedn <- dfbetas.rma.mv(ex.mv.seedn, reestimate = FALSE, progbar = TRUE) 
summary(dfb.seedn)



# Model selection 14 -------------------------------------------------------

ex.ms14 <- ex[!apply(ex[ , c("region", "dlc", "C_type", "cratc", "capt", "plant_apgfs", "plotc")], 1, anyNA), ]

ex.mv.ms14 <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex.ms14,
                    mods = ~ 1 + dlc + plotc + region + plant_apgfs)

rstn <- rstandard(ex.mv.ms14)
hat <- hatvalues(ex.mv.ms14) / mean(hatvalues(ex.mv.ms14))
plot(hat, rstn$resid,
     xlab = "hat/mean hat",
     ylab = "standardised residuals",
     main = "model selection 14")
abline(h = -3)
abline(h = 3)
abline(v = 2)


rstd.ms14 <- rstudent(ex.mv.ms14, reestimate = FALSE, progbar = TRUE) 
x <- subset(rstd.ms14, rstd.ms14$resid > 1.96)
y <- subset(rstd.ms14, rstd.ms14$resid < -1.96)
length(x[["resid"]]) + length(y[["resid"]])
ex.mv.ms14$k / 10 
dfb.ms14 <- dfbetas.rma.mv(ex.mv.ms14, reestimate = FALSE, progbar = TRUE) 
summary(dfb.ms14)



save.image("1021 ex outliers.RData")
