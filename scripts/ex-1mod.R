library(tidyverse)
library(readxl)
library(metafor)

# Load data ---------------------------------------------------------------

raw <- read_xlsx("data/cleaned/C-addition-studies.xlsx", sheet = "screen 3_data (biocov)")
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

# Outliers ----------------------------------------------------------------

out <- c(457, 601)
out.reg <- c(601, 457)
out.dfc <- c(457)
out.crat <- c(54, 120, 132)
out.cratc <- c(437)
out.capm <- c(257, 247, 601)
out.pgfs <- c(257, 258)
out.seedn <- c(457)


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


# No moderators -----------------------------------------------------------

ex.mv <- rma.mv(yi = yi,
                V = vi,
                random = ~ 1 | exp_ID / obs_ID,
                data = ex)
print(ex.mv)
predict(ex.mv)
ex.mv[["QM"]] / sum(ex.mv[["QE"]], ex.mv[["QM"]])

# I^2
W <- diag(1 / ex$vi)
X <- model.matrix(ex.mv)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(ex.mv$sigma2) / (sum(ex.mv$sigma2) + (ex.mv$k-ex.mv$p)/sum(diag(P)))

100 * ex.mv$sigma2 / (sum(ex.mv$sigma2) + (ex.mv$k-ex.mv$p)/sum(diag(P)))


# Outliers removed
out <- c(601, 457)
ex.o <- outliers(ex, out)
ex.mv.o <- rma.mv(yi = yi,
                  V = vi,
                  random = ~ 1 | exp_ID / obs_ID,
                  data = ex.o)

print(ex.mv.o)
predict(ex.mv.o)
ex.mv.o[["QM"]] / sum(ex.mv.o[["QE"]], ex.mv.o[["QM"]])


# I^2, outliers removed
W <- diag(1 / ex.o$vi)
X <- model.matrix(ex.mv.o)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(ex.mv.o$sigma2) / (sum(ex.mv.o$sigma2) + (ex.mv.o$k-ex.mv.o$p)/sum(diag(P)))

100 * ex.mv.o$sigma2 / (sum(ex.mv.o$sigma2) + (ex.mv.o$k-ex.mv.o$p)/sum(diag(P)))


# By paper
ex.mv.paper <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex,
                    mods = ~ factor(paper) - 1)

h <- c("Paper (all)", "Estimate [95% CI]")
single.mod.cat(ex, "paper", ex.mv, ex.mv.paper, h)



# Biome -------------------------------------------------------------------

any(is.na(ex$biome)) # if TRUE, run no-moderators on subset with no missing data for that moderator

ex.bio.na <- ex %>% 
  filter(!is.na(biome))

ex.mv.bio <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex.bio.na,
                    mods = ~ factor(biome) - 1)

ex.mv.bio.na <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = ex.bio.na)

h <- c("Biome (all)", "Estimate [95% CI]")
single.mod.cat(ex, "biome", ex.mv.bio.na, ex.mv.bio, h)


# Outliers removed
  # no outliers

biome.paper <- ex %>% 
  select(paper, biome)
biome.paper <- unique(biome.paper)
count(biome.paper, biome)

biome.region <- ex %>% 
  select(region, biome)
biome.region <- unique(biome.region)
count(biome.region, biome)
count(count(biome.region, biome), n)

biome.soil <- ex %>% 
  select(soil_suborder, biome)
biome.soil <- unique(biome.soil)
count(biome.soil, biome)
count(count(biome.soil, biome), n)



# Region ------------------------------------------------------------------

any(is.na(ex$region)) # if FALSE, use ex.mv for no-moderator model
# but still requires new model for outliers removed and levels n > 3

ex.mv.reg <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex,
                    mods = ~ factor(region) - 1)

h <- c("Region (all)", "Estimate [95% CI]")
single.mod.cat(ex, "region", ex.mv, ex.mv.reg, h)


# Outliers removed
ex.reg.o <- outliers(ex, out.reg)
ex.mv.reg.o <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = ex.reg.o,
                       mods = ~ factor(region) - 1)

ex.mv.reg.na.o <- rma.mv(yi = yi,
                          V = vi,
                          random = ~ 1 | exp_ID / obs_ID,
                          data = ex.reg.o)

h <- c("Region (out rm)", "Estimate [95% CI]")
single.mod.cat(ex.reg.o, "region", ex.mv.reg.na.o, ex.mv.reg.o, h)


region.paper <- ex %>% 
  select(paper, region)
region.paper <- unique(region.paper)
count(region.paper, region)
filter(region.paper, region == "Midwest, USA")

# Overlap between biome, region, and soil suborder
region.biome <- ex %>% 
  select(biome, region)
region.biome <- unique(region.biome)
count(region.biome, region)
count(count(region.biome, region), n)

region.soil <- ex %>% 
  select(soil_suborder, region)
region.soil <- unique(region.soil)
count(region.soil, region)
count(count(region.soil, region), n)

# Significant regions
reg.sig <- c("Pacific Northwest, USA", "southeast Australia",
             "southwest USA")
reg.sig <- ex %>% 
  filter(region %in% reg.sig) 

unique(reg.sig[ , c("paper", "region")]) # 12 papers

unique(reg.sig[ , c("region", "biome")])

unique(reg.sig[ , c("region", "soil_suborder")])



# Soil suborder -----------------------------------------------------------

any(is.na(ex$soil_suborder))

ex.soil.na <- ex %>% 
  filter(!is.na(soil_suborder))

ex.mv.soil <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex.soil.na,
                     mods = ~ factor(soil_suborder) - 1)

ex.mv.soil.na <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = ex.soil.na)

h <- c("Soil suborder (all)", "Estimate [95% CI]")
single.mod.cat(ex, "soil_suborder", ex.mv.soil.na, ex.mv.soil, h)

# Outliers removed
  # no outliers


soil.paper <- ex %>% 
  select(paper, soil_suborder)
soil.paper <- unique(soil.paper)
count(soil.paper, soil_suborder)
count(count(soil.paper, soil_suborder), n)

soil.biome <- ex %>% 
  select(biome, soil_suborder)
soil.biome <- unique(soil.biome)
count(soil.biome, soil_suborder)
count(count(soil.biome, soil_suborder), n)

soil.region <- ex %>% 
  select(region, soil_suborder)
soil.region <- unique(soil.region)
count(soil.region, soil_suborder)
count(count(soil.region, soil_suborder), n)


# Duration since first C application (continuous) -------------------------

any(is.na(ex$duration_first))

ex.mv.df <- rma.mv(yi = yi,
                   V = vi,
                   random = ~ 1 | exp_ID / obs_ID,
                   data = ex,
                   mods = ~ duration_first)

single.mod.cont(ex$duration_first, ex.mv.df, ex$vi, ex$yi, 
                "Months since first C application", "Duration (all)")



# Duration since first C application (categorical) ------------------------

count(ex, duration_first)
ex <- ex %>% 
  mutate(dfc = rep(NA, nrow(ex)))
for(i in 1:nrow(ex)) {
  if(ex$duration_first[i] == 3) {
    ex$dfc[i] <- "3"
  } else if(between(ex$duration_first[i], 5, 6)) {
    ex$dfc[i] <- "5-6"
  } else if(between(ex$duration_first[i], 7, 12)) {
    ex$dfc[i] <- "7-12"
  } else if(between(ex$duration_first[i], 13, 18)) {
    ex$dfc[i] <- "13-18"
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
ex$dfc <- factor(ex$dfc, levels = c("3", "5-6", "7-12", "13-18", "19-24", "25-36", "37-50", "100-200", ">200"))
count(ex, dfc)

any(is.na(ex$dfc))

ex.mv.dfc <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex,
                    mods = ~ factor(dfc) - 1)

h <- c("Duration (all)", "Estimate [95% CI]")
single.mod.cat(ex, "dfc", ex.mv, ex.mv.dfc, h)


# Outliers removed
ex.dfc.o <- outliers(ex, out.dfc)
ex.mv.dfc.o <- rma.mv(yi = yi,
                      V = vi,
                      random = ~ 1 | exp_ID / obs_ID,
                      data = ex.dfc.o,
                      mods = ~ factor(dfc) - 1)

ex.mv.dfc.na.o <- rma.mv(yi = yi,
                         V = vi,
                         random = ~ 1 | exp_ID / obs_ID,
                         data = ex.dfc.o)

h <- c("Duration (out rm)", "Estimate [95% CI]")
single.mod.cat(ex.dfc.o, "dfc", ex.mv.dfc.na.o, ex.mv.dfc.o, h)


# Duration since last C application (continuous) --------------------------

any(is.na(ex$duration_last))

ex.mv.dl <- rma.mv(yi = yi,
                   V = vi,
                   random = ~ 1 | exp_ID / obs_ID,
                   data = ex,
                   mods = ~ duration_last)

single.mod.cont(ex$duration_last, ex.mv.dl, ex$vi, ex$yi, 
                "Months since last C application", "Duration (all)")




# Duration since last C app (categorical) ---------------------------------

count(ex, duration_last)
ex <- ex %>% 
  mutate(dlc = rep(NA, nrow(ex)))
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
ex$dlc <- factor(ex$dlc, levels = c("0-1.5", "2", "3-3.5", "4-6", "7-12", "13-18", "19-24", 
                                   "36-49", ">100"))
count(ex, dlc)

any(is.na(ex$dlc))

ex.mv.dlc <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex,
                    mods = ~ factor(dlc) - 1)

h <- c("Duration (all)", "Estimate [95% CI]")
single.mod.cat(ex, "dlc", ex.mv, ex.mv.dlc, h)

# Outliers removed
   # no outliers



# C type ------------------------------------------------------------------

any(is.na(ex$C_type))

ex.mv.ctyp <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex,
                     mods = ~ factor(C_type) - 1)

h <- c("C type (all)", "Estimate [95% CI]")
single.mod.cat(ex, "C_type", ex.mv, ex.mv.ctyp, h)


# Outliers removed
 # no outliers



# C rate (continous) ------------------------------------------------------

any(is.na(ex$C_rate))

ex.crat.na <- ex %>% 
  filter(!is.na(C_rate))

ex.mv.crat <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex.crat.na,
                     mods = ~ C_rate)

single.mod.cont(ex.crat.na$C_rate, ex.mv.crat, ex.crat.na$vi, ex.crat.na$yi, 
                "C rate (g C/year)", "C rate (all)")

# Outliers removed
ex.crat.o <- outliers(ex.crat.na, out.crat)
ex.mv.crat.o <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = ex.crat.o,
                       mods = ~ C_rate)

single.mod.cont(ex.crat.o$C_rate, ex.mv.crat.o, ex.crat.o$vi, ex.crat.o$yi, 
                "C rate (g C/year)", "C rate (out rm)")



# C rate (categorical) ----------------------------------------------------

any(is.na(ex$C_rate))

count(ex, C_rate)
ex <- ex %>% 
  mutate(cratc = rep(NA, nrow(ex)))
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
count(ex, cratc)

any(is.na(ex$cratc))

ex.cratc.na <- ex %>% 
  filter(!is.na(cratc))

ex.mv.cratc <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex.cratc.na,
                     mods = ~ factor(cratc) - 1)

ex.mv.cratc.na <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = ex.cratc.na)

h <- c("C rate (all)", "Estimate [95% CI]")
single.mod.cat(ex.cratc.na, "cratc", ex.mv.cratc.na, ex.mv.cratc, h)


# Outliers removed
ex.cratc.o <- outliers(ex.cratc.na, out.cratc)

ex.mv.cratc.o <- rma.mv(yi = yi,
                      V = vi,
                      random = ~ 1 | exp_ID / obs_ID,
                      data = ex.cratc.o,
                      mods = ~ factor(cratc) - 1)

ex.mv.cratc.na.o <- rma.mv(yi = yi,
                         V = vi,
                         random = ~ 1 | exp_ID / obs_ID,
                         data = ex.cratc.o)

h <- c("C rate (out rm)", "Estimate [95% CI]")
single.mod.cat(ex.cratc.o, "cratc", ex.mv.cratc.na.o, ex.mv.cratc.o, h)




# C applications total (continuous) ---------------------------------------

any(is.na(ex$C_app))

ex.mv.cap <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex,
                    mods = ~ C_app)

single.mod.cont(ex$C_app, ex.mv.cap, ex$vi, ex$yi, 
                "Total C applications", "C app (all)")

# Outliers removed
  # no outliers


# C applications total (categorical) --------------------------------------

count(ex, C_app)
ex <- ex %>% 
  mutate(capc = rep(NA, nrow(ex)))
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
count(ex, capc)


any(is.na(ex$capc))

ex.mv.capc <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex,
                     mods = ~ factor(capc) - 1)

h <- c("C applications (all)", "Estimate [95% CI]")
single.mod.cat(ex, "capc", ex.mv, ex.mv.capc, h)


# Outliers removed
  # no outliers



# Months between C applications  ------------------------------------------

# As continuous does not work because 0 = 1 application total and is not on this scale

# As categorical
count(ex, C_app_ma)
ex <- ex %>% 
  mutate(capm = rep(NA, nrow(ex)))
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
count(ex, capm)

any(is.na(ex$capm))

ex.mv.capm <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex,
                     mods = ~ factor(capm) - 1)

h <- c("Months between C applications (all)", "Estimate [95% CI]")
single.mod.cat(ex, "capm", ex.mv, ex.mv.capm, h)

# Outliers removed
ex.capm.o <- outliers(ex, out.capm)

ex.mv.capm.o <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = ex.capm.o,
                        mods = ~ factor(capm) - 1)

ex.mv.capm.na.o <- rma.mv(yi = yi,
                           V = vi,
                           random = ~ 1 | exp_ID / obs_ID,
                           data = ex.capm.o)

h <- c("Months between C applications (out rm)", "Estimate [95% CI]")
single.mod.cat(ex.capm.o, "capm", ex.mv.capm.na.o, ex.mv.capm.o, h)

cap.capm <- ex %>% 
  filter(C_app > 10)
cap.capm[ , c("paper", "C_app", "capm")]

cap.cm <- ex
cap.cm$capcm <- paste(ex$C_app, ex$capm, sep = ", ")
count(cap.cm, capcm)

cap.ct <- ex
cap.ct$capct <- paste(ex$C_app, ex$capt, sep = ", ")
count(cap.ct, capct)

cap.capm <- ex %>% 
  filter(C_app > 1)
cap.capm[ , c("paper", "C_app", "C_app_tm", "capm")]




# Months applying C -------------------------------------------------------

# As continuous does not work because 0 = 1 application total and is not on this scale

# As categorical
count(ex, C_app_tm)
ex <- ex %>% 
  mutate(capt = rep(NA, nrow(ex)))
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
count(ex, capt)

any(is.na(ex$capt))

ex.mv.capt <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex,
                     mods = ~ factor(capt) - 1)

h <- c("Months applying C (all)", "Estimate [95% CI]")
single.mod.cat(ex, "capt", ex.mv, ex.mv.capt, h)

# Outliers removed
  # no outliers


reapp <- ex %>% 
  filter(capt != "1 application total")
ggplot(reapp, aes(x = capt, y = C_app)) +
  geom_boxplot() +
  geom_jitter() +
  theme_bw(base_size = 14) +
  xlab("Months applying C") +
  ylab("Total number of C applications")


# Relating C rate, C app, and capt (unique combinations)
c.stats <- unique(ex[ , c("paper", "C_rate", "C_total", "C_app", "capt")])
c.stats$C_rate <- round(c.stats$C_rate, 0)

c.app.stats <- c.stats %>% 
  filter(capt != "1 application total")

ggplot(c.app.stats, aes(x = capt, y = C_app)) +
  geom_boxplot() +
  geom_jitter() +
  theme_bw(base_size = 14) +
  xlab("Months applying C") +
  ylab("Total number of C applications")

ggplot(c.app.stats, aes(x = capt, y = C_total)) +
  geom_boxplot() +
  geom_jitter() +
  theme_bw(base_size = 14) +
  xlab("Months applying C") +
  ylab("Total C (g/m2)")





# Annual/perennial --------------------------------------------------------

any(is.na(ex$plant_anper))

ex.mv.panp <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex,
                     mods = ~ factor(plant_anper) - 1)

h <- c("Annual/perennial (all)", "Estimate [95% CI]")
single.mod.cat(ex, "plant_anper", ex.mv, ex.mv.panp, h)


# Outliers removed
  # no outliers



# Grass/forb/shrub --------------------------------------------------------

any(is.na(ex$plant_gfs))

ex.mv.pgfs <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex,
                     mods = ~ factor(plant_gfs) - 1)

h <- c("Grass/forb/shrub (all)", "Estimate [95% CI]")
single.mod.cat(ex, "plant_gfs", ex.mv, ex.mv.pgfs, h)


# Outliers removed
ex.pgfs.o <- outliers(ex, out.pgfs)
ex.mv.pgfs.o <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = ex.pgfs.o,
                       mods = ~ factor(plant_gfs) - 1)

ex.mv.pgfs.na.o <- rma.mv(yi = yi,
                          V = vi,
                          random = ~ 1 | exp_ID / obs_ID,
                          data = ex.pgfs.o)

h <- c("Grass/forb/shrub (out rm)", "Estimate [95% CI]")
single.mod.cat(ex.pgfs.o, "plant_gfs", ex.mv.pgfs.na.o, ex.mv.pgfs.o, h)



# Annual/perennial and grass/forb/shrub -----------------------------------

any(is.na(ex$plant_apgfs))

ex.mv.papgfs <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = ex,
                       mods = ~ factor(plant_apgfs) - 1)

h <- c("Annual/perennial and grass/forb/shrub (all)", "Estimate [95% CI]")
single.mod.cat(ex, "plant_apgfs", ex.mv, ex.mv.papgfs, h)


# Outliers removed
  # none



# Plot size ---------------------------------------------------------------

any(is.na(ex$plot))

count(ex, plot)
ex <- ex %>% 
  mutate(plotc = rep(NA, nrow(ex)))
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
count(ex, plotc)

any(is.na(ex$plotc))

ex.plotc.na <- ex %>% 
  filter(!is.na(plotc))

ex.mv.plotc <- rma.mv(yi = yi,
                      V = vi,
                      random = ~ 1 | exp_ID / obs_ID,
                      data = ex.plotc.na,
                      mods = ~ factor(plotc) - 1)

ex.mv.plotc.na <- rma.mv(yi = yi,
                         V = vi,
                         random = ~ 1 | exp_ID / obs_ID,
                         data = ex.plotc.na)

h <- c("Plot size (all)", "Estimate [95% CI]")
single.mod.cat(ex.plotc.na, "plotc", ex.mv.plotc.na, ex.mv.plotc, h)
# Favors small plots a bit


# Seeding of native -------------------------------------------------------

count(ex, seeding_native)
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

any(is.na(ex$seedn))

ex.mv.seedn <- rma.mv(yi = yi,
                      V = vi,
                      random = ~ 1 | exp_ID / obs_ID,
                      data = ex,
                      mods = ~ factor(seedn) - 1)


h <- c("Seeding of native (all)", "Estimate [95% CI]")
single.mod.cat(ex, "seedn", ex.mv, ex.mv.seedn, h)

# Outliers removed
ex.seedn.o <- outliers(ex, out.seedn)
ex.mv.seedn.o <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = ex.seedn.o,
                       mods = ~ factor(seedn) - 1)

ex.mv.seedn.na.o <- rma.mv(yi = yi,
                          V = vi,
                          random = ~ 1 | exp_ID / obs_ID,
                          data = ex.seedn.o)

h <- c("Seeding of native (out rm)", "Estimate [95% CI]")
single.mod.cat(ex.seedn.o, "seedn", ex.mv.seedn.na.o, ex.mv.seedn.o, h)


save.image("RData/ex-1mod.RData")
