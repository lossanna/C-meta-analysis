library(tidyverse)
library(readxl)
library(metafor)

# Load data ---------------------------------------------------------------

raw <- read_xlsx("data/cleaned/C-addition-studies.xlsx", sheet = "screen 3_data (1 res)")
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
density$res <- rep("density", nrow(density))

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



# Outliers (fewer removed) ------------------------------------------------

nt.out.cratc <- c(440)


# Functions ---------------------------------------------------------------

single.mod.cat <- function(dat, var, dat.mv, dat.mv.var, h) {
  print(dat.mv.var)
  print(noquote(paste("QM/QT =", dat.mv.var[["QM"]] / sum(dat.mv.var[["QE"]], dat.mv.var[["QM"]]))))
  
  n.forest <- count(dat, !!sym(var))
  colnames(n.forest) <- c("moderator", "n")
  n.forest$end <- rep(")", dim(n.forest)[1])
  n.forest$n <- paste(n.forest$n, n.forest$end, sep = "")
  n.forest$start <- rep("(n = ", dim(n.forest)[1])
  n.forest$n <- paste(n.forest$start, n.forest$n)
  n.forest$var_col <- paste(n.forest$moderator, n.forest$n)
  n.forest <- n.forest %>% 
    filter(!is.na(moderator))
  overall <- paste("Overall (n = ", dat.mv$k, ")", sep = "")
  
  forest(x = c(dat.mv[[1]], dat.mv.var[[1]]),
         ci.lb = c(dat.mv[[6]], dat.mv.var[[6]]),
         ci.ub = c(dat.mv[[7]], dat.mv.var[[7]]),
         slab = c(overall, n.forest$var_col),
         header = h,
         cex = 0.7)
}


single.mod.cont <- function(dat_var, dat.mv.var, dat_vi, dat_yi, xlab, main) {
  print(dat.mv.var)
  print(noquote(paste("QM/QT =", dat.mv.var[["QM"]] / sum(dat.mv.var[["QE"]], dat.mv.var[["QM"]]))))
  
  x <- 1.1 * range(dat_var)[2]
  preds <- predict(dat.mv.var, newmods = c(0:x))
  size <- 1 / sqrt(dat_vi)
  size <- size / max(size) * x / 100
  
  plot(NA, NA, xlim = c(0, x), ylim = c(-10, 8),
       xlab = xlab,
       ylab = "Hedges' d",
       main = main)
  symbols(dat_var, dat_yi, circles = size, inches = FALSE, add = TRUE, bg = "black")
  lines(0:x, preds$pred)
  lines(0:x, preds$ci.lb, lty = "dashed")
  lines(0:x, preds$ci.ub, lty = "dashed")
}



outliers <- function(dat, out) {
  dat %>% 
    filter(!obs_ID %in% out)
}



# No moderators -----------------------------------------------------------

nt.mv <- rma.mv(yi = yi,
                V = vi,
                random = ~ 1 | exp_ID / obs_ID,
                data = nt)
print(nt.mv)
predict(nt.mv)
nt.mv[["QM"]] / sum(nt.mv[["QE"]], nt.mv[["QM"]])

# I^2
W <- diag(1 / nt$vi)
X <- model.matrix(nt.mv)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(nt.mv$sigma2) / (sum(nt.mv$sigma2) + (nt.mv$k-nt.mv$p)/sum(diag(P)))

100 * nt.mv$sigma2 / (sum(nt.mv$sigma2) + (nt.mv$k-nt.mv$p)/sum(diag(P)))


# Outliers removed
  # no outliers (more or fewer)


# Response variable -------------------------------------------------------

nt.mv.res <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = nt,
                    mods = ~ factor(res) - 1)

h <- c("Response variable (all)", "Estimate [95% CI]")
single.mod.cat(nt, "res", nt.mv, nt.mv.res, h)

# Nothing is significant
# Response variables are all responding in the same way
count(nt, res)
filter(nt, res == "density")[ , 1]



# Biome -------------------------------------------------------------------

any(is.na(nt$biome)) # if FALSE, use nt.mv for no-moderator model
  # but still requires new model for outliers removed and levels n > 3

nt.mv.bio <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = nt,
                    mods = ~ factor(biome) - 1)

h <- c("Biome (all)", "Estimate [95% CI]")
single.mod.cat(nt, "biome", nt.mv, nt.mv.bio, h)


# Outliers removed (fewer)
  # none


biome.paper <- nt %>% 
  select(paper, biome)
biome.paper <- unique(biome.paper)
count(biome.paper, biome)

biome.region <- nt %>% 
  select(region, biome)
biome.region <- unique(biome.region)
count(biome.region, biome)

biome.soil <- nt %>% 
  select(soil_suborder, biome)
biome.soil <- unique(biome.soil)
count(biome.soil, biome)
count(count(biome.soil, biome), n)



# Region ------------------------------------------------------------------

any(is.na(nt$region))

nt.mv.reg <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = nt,
                    mods = ~ factor(region) - 1)

h <- c("Region (all)", "Estimate [95% CI]")
single.mod.cat(nt, "region", nt.mv, nt.mv.reg, h)


# Outliers removed (ewer)
  # none

region.paper <- nt %>% 
  select(paper, region)
region.paper <- unique(region.paper)
count(region.paper, region)

region.biome <- nt %>% 
  select(biome, region)
region.biome <- unique(region.biome)
count(region.biome, region)

region.soil <- nt %>% 
  select(soil_suborder, region)
region.soil <- unique(region.soil)
count(region.soil, region)
count(count(region.soil, region), n)



# Soil suborder -----------------------------------------------------------

any(is.na(nt$soil_suborder)) # if TRUE, run no-moderators on subset with no missing data for that moderator

nt.soil.na <- nt %>% 
  filter(!is.na(soil_suborder))

nt.mv.soil <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt.soil.na,
                     mods = ~ factor(soil_suborder) - 1)

nt.mv.soil.na <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt.soil.na)

h <- c("Soil suborder (all)", "Estimate [95% CI]")
single.mod.cat(nt, "soil_suborder", nt.mv.soil.na, nt.mv.soil, h)


soil.paper <- nt %>% 
  select(paper, soil_suborder)
soil.paper <- unique(soil.paper)
count(soil.paper, soil_suborder)
count(count(soil.paper, soil_suborder), n)

soil.biome <- nt %>% 
  select(biome, soil_suborder)
soil.biome <- unique(soil.biome)
count(soil.biome, soil_suborder)
count(count(soil.biome, soil_suborder), n)

soil.region <- nt %>% 
  select(region, soil_suborder)
soil.region <- unique(soil.region)
count(soil.region, soil_suborder)

# Outliers removed (fewer)
  # none


# Duration since first C application (categorical) ------------------------

count(nt, duration_first)
nt$dfc <- NA
for(i in 1:nrow(nt)) {
  if(nt$duration_first[i] == 3) {
    nt$dfc[i] <- "3"
  } else if(between(nt$duration_first[i], 5, 6)) {
    nt$dfc[i] <- "5-6"
  } else if(between(nt$duration_first[i], 7, 12)) {
    nt$dfc[i] <- "7-12"
  } else if(between(nt$duration_first[i], 13, 18)) {
    nt$dfc[i] <- "13-18"
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
nt$dfc <- factor(nt$df, levels = c("3", "5-6", "7-12", "13-18", "19-24", "25-36", "37-50", "100-200", ">200"))
count(nt, dfc)

any(is.na(nt$dfc))

nt.mv.dfc <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = nt,
                    mods = ~ factor(dfc) - 1)

h <- c("Duration (all)", "Estimate [95% CI]")
single.mod.cat(nt, "dfc", nt.mv, nt.mv.dfc, h)


# Outliers removed (fewer)
  # none



# Duration since last C app (categorical) ---------------------------------

count(nt, duration_last)
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
count(nt, dlc)

any(is.na(nt$dlc))

nt.mv.dlc <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = nt,
                    mods = ~ factor(dlc) - 1)

h <- c("Duration (all)", "Estimate [95% CI]")
single.mod.cat(nt, "dlc", nt.mv, nt.mv.dlc, h)

# Outliers removed (fewer)
  # none




# C type ------------------------------------------------------------------

any(is.na(nt$C_type))

nt.mv.ctyp <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(C_type) - 1)

h <- c("C type (all)", "Estimate [95% CI]")
single.mod.cat(nt, "C_type", nt.mv, nt.mv.ctyp, h)

# Outliers removed (fewer)
  # none


# C rate (continous) ------------------------------------------------------

any(is.na(nt$C_rate))

nt.crat.na <- nt %>% 
  filter(!is.na(C_rate))

nt.mv.crat <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt.crat.na,
                     mods = ~ C_rate)

single.mod.cont(nt.crat.na$C_rate, nt.mv.crat, nt.crat.na$vi, nt.crat.na$yi, 
                "C rate (g C/year)", "C rate (all)")

# Outliers removed (fewer)
  # none


# C rate (categorical) ----------------------------------------------------

any(is.na(nt$C_rate))

count(nt, C_rate)
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
count(nt, cratc)

any(is.na(nt$cratc))

nt.cratc.na <- nt %>% 
  filter(!is.na(cratc))

nt.mv.cratc <- rma.mv(yi = yi,
                      V = vi,
                      random = ~ 1 | exp_ID / obs_ID,
                      data = nt.cratc.na,
                      mods = ~ factor(cratc) - 1)

nt.mv.cratc.na <- rma.mv(yi = yi,
                         V = vi,
                         random = ~ 1 | exp_ID / obs_ID,
                         data = nt.cratc.na)

h <- c("C rate (all)", "Estimate [95% CI]")
single.mod.cat(nt.cratc.na, "cratc", nt.mv.cratc.na, nt.mv.cratc, h)

# Outliers removed
nt.cratc.o <- outliers(nt.cratc.na, nt.out.cratc)

nt.mv.cratc.o <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt.cratc.o,
                        mods = ~ factor(cratc) - 1)

nt.mv.cratc.na.o <- rma.mv(yi = yi,
                           V = vi,
                           random = ~ 1 | exp_ID / obs_ID,
                           data = nt.cratc.o)

h <- c("C rate (out rm)", "Estimate [95% CI]")
single.mod.cat(nt.cratc.o, "cratc", nt.mv.cratc.na.o, nt.mv.cratc.o, h)




# C applications total (continuous) ---------------------------------------

any(is.na(nt$C_app))

nt.mv.cap <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = nt,
                    mods = ~ C_app)

single.mod.cont(nt$C_app, nt.mv.cap, nt$vi, nt$yi, 
                "Total C applications", "C app (all)")

# Outliers removed (fewer)
  # none



# C applications total (categorical) --------------------------------------

count(nt, C_app)
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
count(nt, capc)


any(is.na(nt$capc))

nt.mv.capc <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(capc) - 1)

h <- c("C applications (all)", "Estimate [95% CI]")
single.mod.cat(nt, "capc", nt.mv, nt.mv.capc, h)


# Outliers removed (fewer)
  # none




# Months between C applications  ------------------------------------------

# As continuous does not work because 0 = 1 application total and is not on this scale

# As categorical
count(nt, C_app_ma)
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
count(nt, capm)

any(is.na(nt$capm))

nt.mv.capm <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(capm) - 1)

h <- c("Months between C applications (all)", "Estimate [95% CI]")
single.mod.cat(nt, "capm", nt.mv, nt.mv.capm, h)

# Outliers removed (fewer)
  # none


cap.capm <- nt %>% 
  filter(C_app > 10)
cap.capm[ , c("paper", "C_app", "capm")]

cap.cm <- nt
cap.cm$capcm <- paste(nt$C_app, nt$capm, sep = ", ")
count(cap.cm, capcm)

cap.ct <- nt
cap.ct$capct <- paste(nt$C_app, nt$capt, sep = ", ")
count(cap.ct, capct)

cap.capm <- nt %>% 
  filter(C_app > 1)
cap.capm[ , c("paper", "C_app", "C_app_tm", "capm")]



# Months applying C -------------------------------------------------------

# As continuous does not work because 0 = 1 application total and is not on this scale

# As categorical
count(nt, C_app_tm)
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
count(nt, capt)

any(is.na(nt$capt))

nt.mv.capt <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(capt) - 1)

h <- c("Months applying C (all)", "Estimate [95% CI]")
single.mod.cat(nt, "capt", nt.mv, nt.mv.capt, h)

# Outliers removed (fewer)
  # none



# Annual/perennial --------------------------------------------------------

any(is.na(nt$plant_anper))

nt.mv.panp <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(plant_anper) - 1)

h <- c("Annual/perennial (all)", "Estimate [95% CI]")
single.mod.cat(nt, "plant_anper", nt.mv, nt.mv.panp, h)


# Outliers removed (fewer)
  # none




# Grass/forb/shrub --------------------------------------------------------

any(is.na(nt$plant_gfs))

nt.mv.pgfs <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(plant_gfs) - 1)

h <- c("Grass/forb/shrub (all)", "Estimate [95% CI]")
single.mod.cat(nt, "plant_gfs", nt.mv, nt.mv.pgfs, h)


# Outliers removed (fewer)
  # none




# Annual/perennial and grass/forb/shrub -----------------------------------

any(is.na(nt$plant_apgfs))

nt.mv.papgfs <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = nt,
                       mods = ~ factor(plant_apgfs) - 1)

h <- c("Annual/perennial and grass/forb/shrub (all)", "Estimate [95% CI]")
single.mod.cat(nt, "plant_apgfs", nt.mv, nt.mv.papgfs, h)


# Outliers removed (fewer)
  # none



# Plot size ---------------------------------------------------------------

any(is.na(nt$plot))

count(nt, plot)
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
count(nt, plotc)

any(is.na(nt$plotc))

nt.plotc.na <- nt %>% 
  filter(!is.na(plotc))

nt.mv.plotc <- rma.mv(yi = yi,
                      V = vi,
                      random = ~ 1 | exp_ID / obs_ID,
                      data = nt.plotc.na,
                      mods = ~ factor(plotc) - 1)

nt.mv.plotc.na <- rma.mv(yi = yi,
                         V = vi,
                         random = ~ 1 | exp_ID / obs_ID,
                         data = nt.plotc.na)

h <- c("Plot size (all)", "Estimate [95% CI]")
single.mod.cat(nt.plotc.na, "plotc", nt.mv.plotc.na, nt.mv.plotc, h)


# Seeding of native -------------------------------------------------------

count(nt, seeding_native)
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

any(is.na(nt$seedn))

nt.mv.seedn <- rma.mv(yi = yi,
                      V = vi,
                      random = ~ 1 | exp_ID / obs_ID,
                      data = nt,
                      mods = ~ factor(seedn) - 1)


h <- c("Seeding of native (all)", "Estimate [95% CI]")
single.mod.cat(nt, "seedn", nt.mv, nt.mv.seedn, h)




###########################################################################
# Discontinued moderators
###########################################################################



# Climate -----------------------------------------------------------------

any(is.na(nt$climate))

nt.clim.na <- nt %>% 
  filter(!is.na(climate))

nt.mv.clim <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt.clim.na,
                     mods = ~ factor(climate) - 1)

nt.mv.clim.na <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt.clim.na)

h <- c("Climate (all)", "Estimate [95% CI]")
single.mod.cat(nt, "climate", nt.mv.clim.na, nt.mv.clim, h)


# Duration since first C application (continuous) -------------------------

any(is.na(nt$duration_first))

nt.mv.df <- rma.mv(yi = yi,
                   V = vi,
                   random = ~ 1 | exp_ID / obs_ID,
                   data = nt,
                   mods = ~ duration_first)

single.mod.cont(nt$duration_first, nt.mv.df, nt$vi, nt$yi, 
                "Months since first C application", "Duration (all)")


# Duration since last C application (continuous) --------------------------

any(is.na(nt$duration_last))

nt.mv.dl <- rma.mv(yi = yi,
                   V = vi,
                   random = ~ 1 | exp_ID / obs_ID,
                   data = nt,
                   mods = ~ duration_last)

single.mod.cont(nt$duration_last, nt.mv.dl, nt$vi, nt$yi, 
                "Months since last C application", "Duration (all)")


# C total -----------------------------------------------------------------

any(is.na(nt$C_total))

nt.ctot.na <- nt %>% 
  filter(!is.na(C_total))

nt.mv.ctot <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt.ctot.na,
                     mods = ~ C_total)

single.mod.cont(nt.ctot.na$C_total, nt.mv.ctot, nt.ctot.na$vi, nt.ctot.na$yi, 
                "C total (g C/year)", "C total (all)")



# Plant category ----------------------------------------------------------

any(is.na(nt$plant_category))

nt.mv.pcat <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(plant_category) - 1)


h <- c("Plant lifeform (all)", "Estimate [95% CI]")
single.mod.cat(nt, "plant_category", nt.mv, nt.mv.pcat, h)



# Plant measurement -------------------------------------------------------

any(is.na(nt$plant_measurement))

nt.mv.pmes <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(plant_measurement) - 1)


h <- c("Plant measurement (all)", "Estimate [95% CI]")
single.mod.cat(nt, "plant_measurement", nt.mv, nt.mv.pmes, h)


# Presence of native and native -------------------------------------------

any(is.na(nt$presence))

nt.mv.pres <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(presence) - 1)


h <- c("Presence of native (all)", "Estimate [95% CI]")
single.mod.cat(nt, "presence", nt.mv, nt.mv.pres, h)



# Prior vegetation --------------------------------------------------------

any(is.na(nt$prior_veg))

nt.mv.prveg <- rma.mv(yi = yi,
                      V = vi,
                      random = ~ 1 | exp_ID / obs_ID,
                      data = nt,
                      mods = ~ factor(prior_veg) - 1)


h <- c("Prior vegetation (all)", "Estimate [95% CI]")
single.mod.cat(nt, "prior_veg", nt.mv, nt.mv.prveg, h)



# Removal extent of prior vegetation --------------------------------------

any(is.na(nt$removal_extent))

nt.mv.pvrm <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt,
                     mods = ~ factor(removal_extent) - 1)


h <- c("Extent of prior veg removal (all)", "Estimate [95% CI]")
single.mod.cat(nt, "removal_extent", nt.mv, nt.mv.pvrm, h)


# Seeding of exotic -------------------------------------------------------

exotic.seeded <- nt %>% 
  filter(seeding_exotic != 0)
exotic.seeded$seeding_exotic_cat <- rep("exotic seeded", dim(exotic.seeded)[1])
exotic.unseed.obs <- setdiff(nt$obs_ID, exotic.seeded$obs_ID)
exotic.unseed <- nt %>% 
  filter(obs_ID %in% exotic.unseed.obs)
exotic.unseed$seeding_exotic_cat <- rep("exotic not seeded", dim(exotic.unseed)[1])
nt.seede <- rbind(exotic.seeded, exotic.unseed)

any(is.na(nt.seede$seeding_exotic_cat))

nt.mv.seede <- rma.mv(yi = yi,
                      V = vi,
                      random = ~ 1 | exp_ID / obs_ID,
                      data = nt.seede,
                      mods = ~ factor(seeding_exotic_cat) - 1)


h <- c("Seeding of exotic (all)", "Estimate [95% CI]")
single.mod.cat(nt.seede, "seeding_exotic_cat", nt.mv, nt.mv.seede, h)



save.image("RData/nt-1mod.RData")
