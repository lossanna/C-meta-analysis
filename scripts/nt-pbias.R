library(tidyverse)
library(readxl)
library(metafor)

# Load data ---------------------------------------------------------------

raw <- read_xlsx("data/C-addition-studies.xlsx", sheet = "screen 3_data (1 res)")
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


# Outliers (fewer removed) ------------------------------------------------

  # none


# Precision (inverse SE) --------------------------------------------------

nt$n_avg <- rowMeans(nt[ , c("n_cntrl", "n_trt")])
nt$SE_inv <- sqrt(nt$n_avg) / sqrt(nt$vi)


# No moderators (summary effect) ------------------------------------------


nt.mv.pb <- rma.mv(yi = yi,
                   V = vi,
                   random = ~ 1 | exp_ID / obs_ID,
                   data = nt,
                   mods = ~ SE_inv,
                   level = 90)
summary(nt.mv.pb)
nt.mv.pb[["b"]][1]
nt.mv.pb[["pval"]][1]

funnel(nt.mv.pb, yaxis = "seinv", pch = 1, back = "white")




# Biome -------------------------------------------------------------------

nt.mv.bio.pb <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = nt,
                       mods = ~ SE_inv + factor(biome),
                       level = 90)
nt.mv.bio.pb[["b"]][1]
nt.mv.bio.pb[["pval"]][1]

funnel(nt.mv.bio.pb, yaxis = "seinv", pch = 1, back = "white")




# Region ------------------------------------------------------------------

nt.mv.reg.pb <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = nt,
                       mods = ~ SE_inv + factor(region),
                       level = 90)
nt.mv.reg.pb[["b"]][1]
nt.mv.reg.pb[["pval"]][1]

funnel(nt.mv.reg.pb, yaxis = "seinv", pch = 1, back = "white")



# Soil suborder -----------------------------------------------------------

nt.mv.soil.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + factor(soil_suborder),
                        level = 90)
nt.mv.soil.pb[["b"]][1]
nt.mv.soil.pb[["pval"]][1]

funnel(nt.mv.soil.pb, yaxis = "seinv", pch = 1, back = "white")


# Duration_first (categorical) --------------------------------------------

nt.mv.dfc.pb <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = nt,
                       mods = ~ SE_inv + factor(dfc),
                       level = 90)
nt.mv.dfc.pb[["b"]][1]
nt.mv.dfc.pb[["pval"]][1]

funnel(nt.mv.dfc.pb, yaxis = "seinv", pch = 1, back = "white")


# Duration_last (categorical) ---------------------------------------------

nt.mv.dlc.pb <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = nt,
                       mods = ~ SE_inv + factor(dlc),
                       level = 90)
nt.mv.dlc.pb[["b"]][1]
nt.mv.dlc.pb[["pval"]][1]

funnel(nt.mv.dlc.pb, yaxis = "seinv", pch = 1, back = "white")





# C type ------------------------------------------------------------------

nt.mv.ctyp.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + factor(C_type),
                        level = 90)
nt.mv.ctyp.pb[["b"]][1]
nt.mv.ctyp.pb[["pval"]][1]

funnel(nt.mv.ctyp.pb, yaxis = "seinv", pch = 1, back = "white")


# C rate ------------------------------------------------------------------

nt.mv.crat.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + C_rate,
                        level = 90)
nt.mv.crat.pb[["b"]][1]
nt.mv.crat.pb[["pval"]][1]

funnel(nt.mv.crat.pb, yaxis = "seinv", pch = 1, back = "white")


# C applications total (categorical) --------------------------------------

nt.mv.capc.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + factor(capc),
                        level = 90)
nt.mv.capc.pb[["b"]][1]
nt.mv.capc.pb[["pval"]][1]

funnel(nt.mv.capc.pb, yaxis = "seinv", pch = 1, back = "white")


# Months between C applications -------------------------------------------

nt.mv.capm.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + factor(capm),
                        level = 90)
nt.mv.capm.pb[["b"]][1]
nt.mv.capm.pb[["pval"]][1]

funnel(nt.mv.capm.pb, yaxis = "seinv", pch = 1, back = "white")


# Months applying C -------------------------------------------------------

nt.mv.capt.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + factor(capt),
                        level = 90)
nt.mv.capt.pb[["b"]][1]
nt.mv.capt.pb[["pval"]][1]

funnel(nt.mv.capt.pb, yaxis = "seinv", pch = 1, back = "white")


# Annual/perennial --------------------------------------------------------

nt.mv.panp.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + factor(plant_anper),
                        level = 90)
nt.mv.panp.pb[["b"]][1]
nt.mv.panp.pb[["pval"]][1]

funnel(nt.mv.panp.pb, yaxis = "seinv", pch = 1, back = "white")


# Grass/forb/shrub --------------------------------------------------------

nt.mv.pgfs.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + factor(plant_gfs),
                        level = 90)
nt.mv.pgfs.pb[["b"]][1]
nt.mv.pgfs.pb[["pval"]][1]

funnel(nt.mv.pgfs.pb, yaxis = "seinv", pch = 1, back = "white")


# Annual/perennial and grass/forb/shrub -----------------------------------

nt.mv.papgfs.pb <- rma.mv(yi = yi,
                          V = vi,
                          random = ~ 1 | exp_ID / obs_ID,
                          data = nt,
                          mods = ~ SE_inv + factor(plant_apgfs),
                          level = 90)
nt.mv.papgfs.pb[["b"]][1]
nt.mv.papgfs.pb[["pval"]][1]

funnel(nt.mv.papgfs.pb, yaxis = "seinv", pch = 1, back = "white")


# Model selection 9 (top model) -------------------------------------------

# Did not converge



# Combine -----------------------------------------------------------------

intcp.sym.nt <- data.frame(Model = c("No moderators",
                                  "Biome (grassland type)",
                                  "Region",
                                  "Soil suborder",
                                  "duration_first (categorical)", 
                                  "duration_last (categorical)", 
                                  "C type",
                                  "C rate",
                                  "C applications (categorical)",
                                  "Months between C applications",
                                  "Months applying C",
                                  "Annual/perennial",
                                  "Grass/forb/shrub",
                                  "Annual/perennial and grass/forb/shrub",
                                  "Model selection 9"),
                        Intercept = c(nt.mv.pb[["b"]][1],
                                      nt.mv.bio.pb[["b"]][1],
                                      nt.mv.reg.pb[["b"]][1],
                                      nt.mv.soil.pb[["b"]][1],
                                      nt.mv.dfc.pb[["b"]][1],
                                      nt.mv.dlc.pb[["b"]][1],
                                      nt.mv.ctyp.pb[["b"]][1],
                                      nt.mv.crat.pb[["b"]][1],
                                      nt.mv.capc.pb[["b"]][1],
                                      nt.mv.capm.pb[["b"]][1],
                                      nt.mv.capt.pb[["b"]][1],
                                      nt.mv.panp.pb[["b"]][1],
                                      nt.mv.pgfs.pb[["b"]][1],
                                      nt.mv.papgfs.pb[["b"]][1],
                                      NA),
                        Intercept_pval = c(nt.mv.pb[["pval"]][1],
                                           nt.mv.bio.pb[["pval"]][1],
                                           nt.mv.reg.pb[["pval"]][1],
                                           nt.mv.soil.pb[["pval"]][1],
                                           nt.mv.dfc.pb[["pval"]][1],
                                           nt.mv.dlc.pb[["pval"]][1],
                                           nt.mv.ctyp.pb[["pval"]][1],
                                           nt.mv.crat.pb[["pval"]][1],
                                           nt.mv.capc.pb[["pval"]][1],
                                           nt.mv.capm.pb[["pval"]][1],
                                           nt.mv.capt.pb[["pval"]][1],
                                           nt.mv.panp.pb[["pval"]][1],
                                           nt.mv.pgfs.pb[["pval"]][1],
                                           nt.mv.papgfs.pb[["pval"]][1],
                                           NA),
                        Symmetry = rep(NA, 15))

for(i in 1:nrow(intcp.sym.nt)) {
  if(intcp.sym.nt$Intercept_pval[i] > 0.1) {
    intcp.sym.nt$Symmetry[i] <- "symmetric"
  } else {
    intcp.sym.nt$Symmetry[i] <- "bias"
  }
}


save.image("RData-RMarkdown/nt-pbias.RData")

