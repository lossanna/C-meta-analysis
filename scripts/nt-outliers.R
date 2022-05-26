library(tidyverse)
library(readxl)
library(metafor)
library(parallel)

# Load data ---------------------------------------------------------------

nt <- read.csv("data/cleaned/native-cleaned.csv")


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



save.image("RData/nt-outliers.RData")
