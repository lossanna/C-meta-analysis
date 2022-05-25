# Figures for Ossanna & Gornish (2022) "Efficacy of labile carbon addition to reduce exotic invasive plants: A review and meta-analysis"
# Lia Ossanna
# lossanna@email.arizona.edu
# 2022-05-24

library(readxl)
library(tidyverse)
library(metafor)
library(orchaRd)
library(ggpubr)

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

outliers <- function(dat, out) {
  dat %>% 
    filter(!obs_ID %in% out)
}


# Calculating effect size -------------------------------------------------

# Exotic
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

# Native
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


# Outliers ----------------------------------------------------------------

# Exotic
out <- c(457, 601)
out.cratc <- c(437)

# Native
nt.out.cratc <- c(440)



# Figure 1 (summary, CI, PI) ----------------------------------------------

# Figure 1 is an orchard plot to display the results of the exotic and native summary models

# Exotic summary model
out <- c(601, 457)
ex.o <- outliers(ex, out)
ex.mv.o <- rma.mv(yi = yi,
                  V = vi,
                  random = ~ 1 | exp_ID / obs_ID,
                  data = ex.o)

# Native summary model
nt.mv <- rma.mv(yi = yi,
                V = vi,
                random = ~ 1 | exp_ID / obs_ID,
                data = nt)

# Table of estimate, CI, and PI
f1.df <- data.frame(model = c("Exotic", "Native"),
                    ci.lb = c(ex.mv.o[["ci.lb"]], nt.mv[["ci.lb"]]),
                    ci.ub = c(ex.mv.o[["ci.ub"]], nt.mv[["ci.ub"]]),
                    pi.lb = c(predict(ex.mv.o)$pi.lb, predict(nt.mv)$pi.lb),
                    pi.ub = c(predict(ex.mv.o)$pi.ub, predict(nt.mv)$pi.ub),
                    estimate = c(ex.mv.o[["b"]], nt.mv[["b"]]))

# Orchard plots
f1.orc.ex <- orchard_plot(ex.mv.o, mod = "Int", xlab = "Effect size (Hedges' g)", transfm = "none") +
  ggtitle("Exotic plant summary response") +
  annotate(geom = "text", x = -6, y = 1.4,
           label = paste0("95% CI: ", round(f1.df[1, 2], 3), ", ", round(f1.df[1, 3], 3), "\n",
                          "95% PI: ", round(f1.df[1, 4], 3), ", ", round(f1.df[1, 5], 3)))
f1.orc.ex

f1.orc.nt <- orchard_plot(nt.mv, mod = "Int", xlab = "Effect size (Hedges' g)", transfm = "none") +
  ggtitle("Native plant summary response") +
  annotate(geom = "text", x = -7, y = 1.4,
           label = paste0("95% CI: ", round(f1.df[2, 2], 3), ", ", round(f1.df[2, 3], 3), "\n",
                          "95% PI: ", round(f1.df[2, 4], 3), ", ", round(f1.df[2, 5], 3)))
f1.orc.nt

# Write separate TIFFs
tiff("output_figs/Orchard_ex.tiff", width = 6, height = 4, units = "in", res = 300)
f1.orc.ex
dev.off()

tiff("output_figs/Orchard_nt.tiff", width = 6, height = 4, units = "in", res = 300)
f1.orc.nt
dev.off()

# Write Figure_1.pdf
pdf("output_figs/Figure_1.pdf", width = 6)
ggarrange(f1.orc.ex, f1.orc.nt,
          ncol = 1, nrow = 2,
          labels = c("(A)", "(B)"))
dev.off()

# Write Figure_1.tiff
tiff("output_figs/Figure_1.tiff", width = 6, height = 7, units = "in", res = 300)
ggarrange(f1.orc.ex, f1.orc.nt,
          ncol = 1, nrow = 2,
          labels = c("(A)", "(B)"))
dev.off()



# Figure 2 (anpergfs exotic) ----------------------------------------------

# Figure 2 is a forest plot of exotic plant response by plant lifeform (annual/perennial and grass/forb/shrub)

# Exotic anpergfs model
ex.mv.papgfs <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = ex,
                       mods = ~ factor(plant_apgfs) - 1)

# Table of estimate, CI, and PI
n.forest.ex.papgfs <- count(ex, !!sym("plant_apgfs"))
n.forest.ex.papgfs$x <- ex.mv.papgfs[[1]]
n.forest.ex.papgfs$ci.lb <- ex.mv.papgfs[[6]]
n.forest.ex.papgfs$ci.ub <- ex.mv.papgfs[[7]]
n.forest.ex.papgfs <- n.forest.ex.papgfs %>% 
  filter(plant_apgfs %in% c("annual forb", "annual graminoid", 
                            "perennial forb", "perennial graminoid"))

# Write to PDF
pdf("output_figs/Figure_2.pdf", height = 4)
forest(x = n.forest.ex.papgfs$x,
       ci.lb = n.forest.ex.papgfs$ci.lb,
       ci.ub = n.forest.ex.papgfs$ci.ub,
       slab = n.forest.ex.papgfs$plant_apgfs,
       header = "Lifeform",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.ex.papgfs$n,
       ilab.xpos = -1.3,
       cex = 0.75, 
       top = 2)

par(cex = 0.75, font = 4)
text(x = -1.3, y = 6, labels = "k")
par(cex = 0.75, font = 2)
text(x = 0, y = 6, labels = "Exotic plant response")
dev.off()

# Write to TIFF
tiff("output_figs/Figure_2.tiff", width = 6, height = 4, units = "in", res = 300)
forest(x = n.forest.ex.papgfs$x,
       ci.lb = n.forest.ex.papgfs$ci.lb,
       ci.ub = n.forest.ex.papgfs$ci.ub,
       slab = n.forest.ex.papgfs$plant_apgfs,
       header = "Lifeform",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.ex.papgfs$n,
       ilab.xpos = -1.3,
       cex = 0.75,
       top = 2)

par(cex = 0.75, font = 4)
text(x = -1.3, y = 6, labels = "k")
par(cex = 0.75, font = 2)
text(x = 0, y = 6, labels = "Exotic plant response")
dev.off()



# Figure 3 (dlc exotic) ---------------------------------------------------

# Figure 3 is a forest plot of exotic plant response by study duration (months from first C app to harvest)

# Create categorical variable
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
ex$dlc <- factor(ex$dlc, levels = c("0-1.5", "2", "3-3.5", "4-6", "7-12", "13-18", "19-24", 
                                    "36-49", ">100"))

# Exotic dlc model
ex.mv.dlc <- rma.mv(yi = yi,
                    V = vi,
                    random = ~ 1 | exp_ID / obs_ID,
                    data = ex,
                    mods = ~ factor(dlc) - 1)

# Table of estimate, CI, and PI
n.forest.ex.dlc <- count(ex, !!sym("dlc"))
n.forest.ex.dlc$x <- ex.mv.dlc[[1]]
n.forest.ex.dlc$ci.lb <- ex.mv.dlc[[6]]
n.forest.ex.dlc$ci.ub <- ex.mv.dlc[[7]]

# Write to PDF
pdf("output_figs/Figure_3.pdf", height = 5)
forest(x = n.forest.ex.dlc$x,
       ci.lb = n.forest.ex.dlc$ci.lb,
       ci.ub = n.forest.ex.dlc$ci.ub,
       slab = n.forest.ex.dlc$dlc,
       header = "Study duration (months)",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.ex.dlc$n,
       ilab.xpos = -2.4,
       cex = 0.75,
       top = 2)

par(cex = 0.75, font = 4)
text(x = -2.4, y = 11, labels = "k")
par(cex = 0.75, font = 2)
text(x = 0, y = 11, labels = "Exotic plant response")
dev.off()

# Write to TIFF
tiff("output_figs/Figure_3.tiff", width = 6, height = 4, units = "in", res = 300)
forest(x = n.forest.ex.dlc$x,
       ci.lb = n.forest.ex.dlc$ci.lb,
       ci.ub = n.forest.ex.dlc$ci.ub,
       slab = n.forest.ex.dlc$dlc,
       header = "Study duration (months)",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.ex.dlc$n,
       ilab.xpos = -2.4,
       cex = 0.75,
       top = 2)

par(cex = 0.75, font = 4)
text(x = -2.4, y = 11, labels = "k")
par(cex = 0.75, font = 2)
text(x = 0, y = 11, labels = "Exotic plant response")
dev.off()



# Figure 4 (cratc exotic) -------------------------------------------------

# Figure 4 is a forest plot of exotic plant response by C application rate

# Create categorical variable
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

# Exotic C rate models
ex.cratc.na <- ex %>% 
  filter(!is.na(cratc)) # some C rates are missing or couldn't be converted to g C/m2
ex.cratc.o <- outliers(ex.cratc.na, out.cratc) # remove outliers

ex.mv.cratc.o <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = ex.cratc.o,
                        mods = ~ factor(cratc) - 1) # summary model using subset of observations with C rate

ex.mv.cratc.na.o <- rma.mv(yi = yi,
                           V = vi,
                           random = ~ 1 | exp_ID / obs_ID,
                           data = ex.cratc.o) # model with C rate as a moderator

# Table of estimate, CI, and PI
n.forest.ex.cratc <- count(ex.cratc.o, !!sym("cratc"))
n.forest.ex.cratc$x <- ex.mv.cratc.o[[1]]
n.forest.ex.cratc$ci.lb <- ex.mv.cratc.o[[6]]
n.forest.ex.cratc$ci.ub <- ex.mv.cratc.o[[7]]

# Write to PDF
pdf("output_figs/Figure_4.pdf", height = 5)
forest(x = n.forest.ex.cratc$x,
       ci.lb = n.forest.ex.cratc$ci.lb,
       ci.ub = n.forest.ex.cratc$ci.ub,
       slab = n.forest.ex.cratc$cratc,
       annotate = FALSE,
       header = "C rate",
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.ex.cratc$n,
       ilab.xpos = -5,
       cex = 0.75,
       top = 2)

par(cex = 0.65, font = 1)
text(x = -6.55, y = 16.1, labels = bquote(paste("(g C ", m^-2, " ", y^-1, ")")),)
par(cex = 0.75, font = 4)
text(x = -5, y = 16.1, labels = "k")
par(cex = 0.75, font = 2)
text(x = 0, y = 16.1, labels = "Exotic plant response")
dev.off()

# Write to TIFF
tiff("output_figs/Figure_4.tiff", width = 6, height = 4, units = "in", res = 300)
forest(x = n.forest.ex.cratc$x,
       ci.lb = n.forest.ex.cratc$ci.lb,
       ci.ub = n.forest.ex.cratc$ci.ub,
       slab = n.forest.ex.cratc$cratc,
       annotate = FALSE,
       header = "C rate",
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.ex.cratc$n,
       ilab.xpos = -4.7,
       cex = 0.75,
       top = 2)

  # x & y are different than pdf
par(cex = 0.65, font = 1)
text(x = -6.35, y = 16.1, labels = bquote(paste("(g C ", m^-2, " ", y^-1, ")")),)
par(cex = 0.75, font = 4)
text(x = -4.7, y = 16.1, labels = "k")
par(cex = 0.75, font = 2)
text(x = 0, y = 16.1, labels = "Exotic plant response")
dev.off()



# Figure 5 (cratc native) -------------------------------------------------

# Figure 5 is a forest plot of native plant response by C rate

# Create categorical variable
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

# Native C rate models
nt.cratc.na <- nt %>% 
  filter(!is.na(cratc)) # some C rates are missing or couldn't be converted to g C/m2
nt.cratc.o <- outliers(nt.cratc.na, nt.out.cratc) # remove outliers

ex.mv.cratc.o <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = ex.cratc.o,
                        mods = ~ factor(cratc) - 1) # summary model using subset of observations with C rate

nt.mv.cratc.o <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt.cratc.o,
                        mods = ~ factor(cratc) - 1) # model with C rate as a moderator

# Table of estimate, CI, and PI
n.forest.nt.cratc <- count(nt.cratc.o, !!sym("cratc"))
n.forest.nt.cratc$x <- nt.mv.cratc.o[[1]]
n.forest.nt.cratc$ci.lb <- nt.mv.cratc.o[[6]]
n.forest.nt.cratc$ci.ub <- nt.mv.cratc.o[[7]]

pdf("output_figs/Figure_5.pdf", height = 5)
forest(x = n.forest.nt.cratc$x,
       ci.lb = n.forest.nt.cratc$ci.lb,
       ci.ub = n.forest.nt.cratc$ci.ub,
       slab = n.forest.nt.cratc$cratc,
       header = "C rate",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.nt.cratc$n,
       ilab.xpos = -2.5,
       cex = 0.75,
       top = 2)

par(cex = 0.65, font = 1)
text(x = -3.58, y = 16.1, labels = bquote(paste("(g C ", m^-2, " ", y^-1, ")")),)
par(cex = 0.75, font = 4)
text(x = -2.5, y = 16.1, labels = "k")
par(cex = 0.75, font = 2)
text(x = 0, y = 16.1, labels = "Native plant response")
dev.off()

tiff("output_figs/Figure_5.tiff", width = 6, height = 4, units = "in", res = 300)
forest(x = n.forest.nt.cratc$x,
       ci.lb = n.forest.nt.cratc$ci.lb,
       ci.ub = n.forest.nt.cratc$ci.ub,
       slab = n.forest.nt.cratc$cratc,
       header = "C rate",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.nt.cratc$n,
       ilab.xpos = -2.3,
       cex = 0.75,
       top = 2)

# x & y are different than pdf
par(cex = 0.65, font = 1)
text(x = -3.43, y = 16.1, labels = bquote(paste("(g C ", m^-2, " ", y^-1, ")")),)
par(cex = 0.75, font = 4)
text(x = -2.3, y = 16.1, labels = "k")
par(cex = 0.75, font = 2)
text(x = 0, y = 16.1, labels = "Native plant response")
dev.off()
