library(tidyverse)
library(readxl)
library(metafor)
library(glmulti)

# Load data ---------------------------------------------------------------

raw <- read_xlsx("data/C-addition-studies.xlsx", sheet = "screen 3_data (biocov)")
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

ex$dfc <- NA
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

ex$dlc <- NA
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

ex$cratc <- NA
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

ex$capc <- NA
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

ex$capm <- NA
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

ex$capt <- NA
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

ex$plotc <- NA
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


# Model selection setup ---------------------------------------------------

rma.glmulti <- function(formula, data, V, random,...) {
  do.call("rma.mv", list(as.formula(paste(deparse(formula))), V = as.name(V), random = as.name(random), data = data,  method = "ML", ...))
}
rand <- list(~ 1 | obs_ID, ~ 1 | exp_ID)


# Multimodel inference
eval(metafor:::.glmulti)



# region, dlc, C_type, cratc, capt, plant_apgfs, plotc --------------------

ex.ms14 <- ex[!apply(ex[ , c("region", "dlc", "C_type", "cratc", "capt", "plant_apgfs", "plotc")], 1, anyNA), ]

res14 <- glmulti(yi ~ region + dlc + C_type + cratc + capt + plant_apgfs + plotc, 
                  V = "vi", data = ex.ms14, level = 1, fitfunction = rma.glmulti, 
                  random = "rand", crit = "aicc")

print(res14)
top <- weightable(res14)
top <- top[top$aicc <= min(top$aicc) + 2, ]
top
summary(res14@objects[[1]])
plot(res14, type = "s")

forest(x = res14@objects[[1]]$b,
       ci.lb = res14@objects[[1]]$ci.lb,
       ci.ub = res14@objects[[1]]$ci.ub,
       slab = rownames(res14@objects[[1]]$b),
       alim = c(-5, 5),
       header = c("Top model 1", "Estimate [95% CI]"),
       cex = 0.75)

# No intercept
ex.mv.ms14.ni <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex.ms14,
                     mods = ~ dlc + plotc + region + plant_apgfs - 1)
summary(ex.mv.ms14.ni)

forest(x = ex.mv.ms14.ni$b,
       ci.lb = ex.mv.ms14.ni$ci.lb,
       ci.ub = ex.mv.ms14.ni$ci.ub,
       slab = rownames(ex.mv.ms14.ni$b),
       alim = c(-5, 5),
       header = c("Top model 1, no intercept", "Estimate [95% CI]"),
       cex = 0.75)

# Outliers removed
  # no outliers

save(res14, file = ".RData/ex-mod14.RData")

# Multimodel inference
mmi <- as.data.frame(coef(res14))
mmi <- data.frame(Estimate = mmi$Est, SE = sqrt(mmi$Uncond), Importance = mmi$Importance, row.names = row.names(mmi))
mmi$z <- mmi$Estimate / mmi$SE
mmi$p <- 2 * pnorm(abs(mmi$z), lower.tail = FALSE)
names(mmi) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
mmi$ci.lb <- mmi[[1]] - qnorm(0.975) * mmi[[2]]
mmi$ci.ub <- mmi[[1]] + qnorm(0.975) * mmi[[2]]
mmi <- mmi[order(mmi$Importance, decreasing = TRUE), c(1, 2, 4:7, 3)]
mmi <- round(mmi, 4)
mmi$sig <- rep(NA, nrow(mmi))
for(i in 1:nrow(mmi)) {
  if(between(mmi$`Pr(>|z|)`[i], 0.01, 0.05)) {
    mmi$sig[i] <- "*"
  } else if(between(mmi$`Pr(>|z|)`[i], 0.001, 0.01)) {
    mmi$sig[i] <- "**"
  } else if(mmi$`Pr(>|z|)`[i] < 0.001) {
    mmi$sig[i] <- "***"
  } else if(between(mmi$`Pr(>|z|)`[i], 0.05, 0.1)) {
    mmi$sig[i] <- "."
  } else if(mmi$`Pr(>|z|)`[i] > 0.1) {
    mmi$sig[i] <- ""
  }
}
mmi


## Moderator importance > 0.8
forest(x = mmi$Estimate[1:33],
       ci.lb = mmi$ci.lb[1:33],
       ci.ub = mmi$ci.ub[1:33],
       slab = rownames(mmi[1:33, ]),
       cex = 0.7,
       header = c("Multimodel inference", "Estimate [95% CI]"))





# region, dlc, C_type, cratc, plotc, capt, plant_anper, plant_gfs ---------

ex.ms15 <- ex[!apply(ex[ , c("region", "dlc", "C_type", "cratc", "capt", "plant_anper", "plant_gfs", "plotc")], 1, anyNA), ]

res15 <- glmulti(yi ~ region + dlc + C_type + cratc + capt + plant_anper + plant_gfs + plotc, 
                 V = "vi", data = ex.ms15, level = 1, fitfunction = rma.glmulti, 
                 random = "rand", crit = "aicc")


print(res15)
top <- weightable(res15)
top <- top[top$aicc <= min(top$aicc) + 2, ]
top
summary(res15@objects[[1]])
plot(res15, type = "s")

forest(x = res15@objects[[1]]$b,
       ci.lb = res15@objects[[1]]$ci.lb,
       ci.ub = res15@objects[[1]]$ci.ub,
       slab = rownames(res15@objects[[1]]$b),
       alim = c(-5, 5),
       header = c("Top model 1", "Estimate [95% CI]"),
       cex = 0.75)

forest(x = res15@objects[[2]]$b,
       ci.lb = res15@objects[[2]]$ci.lb,
       ci.ub = res15@objects[[2]]$ci.ub,
       slab = rownames(res15@objects[[2]]$b),
       alim = c(-5, 5),
       header = c("Top model 2", "Estimate [95% CI]"),
       cex = 0.75)


save.image("RData-RMarkdown/ex-modsel.RData")
