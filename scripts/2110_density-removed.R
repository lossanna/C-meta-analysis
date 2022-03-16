library(tidyverse)
library(readxl)
library(metafor)

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


###########################################################################
# 0621 data
###########################################################################

# Load data (0621) --------------------------------------------------------

raw <- read_xlsx("C addition studies.xlsx", sheet = "screen 3_data (1 res)")
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



# Exotic (0621) -----------------------------------------------------------

# Calculating effect size -------------------------------------------------

ex.0621 <- all.1res %>%
  filter(str_detect(plant_category, "exotic"))

ex.0621 <- escalc(measure = "SMD",
             n1i = n_trt,
             n2i = n_cntrl,
             m1i = mean_trt,
             m2i = mean_cntrl,
             sd1i = SD_trt,
             sd2i = SD_cntrl,
             data = ex.0621)

ex.0621 <- ex.0621 %>% 
  filter(!is.na(yi)) # 607 to 519 obs


# No moderators -----------------------------------------------------------

ex.0621.mv <- rma.mv(yi = yi,
                V = vi,
                random = ~ 1 | exp_ID / obs_ID,
                data = ex.0621)
print(ex.0621.mv)
predict(ex.0621.mv)
ex.0621.mv[["QM"]] / sum(ex.0621.mv[["QE"]], ex.0621.mv[["QM"]])

# I^2
W <- diag(1 / ex.0621$vi)
X <- model.matrix(ex.0621.mv)
P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
100 * sum(ex.0621.mv$sigma2) / (sum(ex.0621.mv$sigma2) + (ex.0621.mv$k-ex.0621.mv$p)/sum(diag(P)))

100 * ex.0621.mv$sigma2 / (sum(ex.0621.mv$sigma2) + (ex.0621.mv$k-ex.0621.mv$p)/sum(diag(P)))

# Response variable -------------------------------------------------------

ex.0621.mv.res <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex.0621,
                    mods = ~ factor(res) - 1)

h <- c("0621 exotic by response variable (all)", "Estimate [95% CI]")
single.mod.cat(ex.0621, "res", ex.0621.mv, ex.0621.mv.res, h)

# Biomass & cover are significant; density is not
# Response variables are not responding in the same way
# But there are only 34 density points



# Native (0621) -----------------------------------------------------------

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



# Response variable -------------------------------------------------------

nt.mv.res <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = nt,
                    mods = ~ factor(res) - 1)

h <- c("0621 native by response variable (all)", "Estimate [95% CI]")
single.mod.cat(nt, "res", nt.mv, nt.mv.res, h)

# Nothing is significant
# Response variables are all responding in the same way



###########################################################################
# Exotic with density data
###########################################################################

# Load data (1021), overwrite objects -------------------------------------

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

all.1res <- rbind(biomass, cover, density)




###########################################################################
# Exotic without any density data
###########################################################################

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


# Response variable -------------------------------------------------------

ex.mv.res <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex,
                    mods = ~ factor(res) - 1)

h <- c("1021 exotic by response variable (all)", "Estimate [95% CI]")
single.mod.cat(ex, "res", ex.mv, ex.mv.res, h)

# Biomass & cover are significant



save.image("1021 density removed.RData")
