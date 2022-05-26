library(tidyverse)
library(readxl)
library(metafor)

# Load data ---------------------------------------------------------------

ex <- read.csv("data/cleaned/exotic-cleaned.csv")


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



save.image("RData/ex-outliers.RData")
