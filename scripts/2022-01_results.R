library(tidyverse)
library(metafor)
library(glmulti)

# Load data ---------------------------------------------------------------

load("RData/ex-1mod.RData")
load("RData/nt-1mod.RData")
load("RData/ex-mod14.RData")
load("RData/nt-mod14o.RData")


# Functions ---------------------------------------------------------------

forest.cat <- function(dat, var, dat.mv, dat.mv.var, h) {
  n.forest <- count(dat, !!sym(var))
  colnames(n.forest) <- c("moderator", "n")
  n.forest$end <- rep(")", dim(n.forest)[1])
  n.forest$n <- paste(n.forest$n, n.forest$end, sep = "")
  n.forest$start <- rep(" (k =", dim(n.forest)[1])
  n.forest$n <- paste(n.forest$start, n.forest$n)
  n.forest$var_col <- paste(n.forest$moderator, n.forest$n)
  n.forest <- n.forest %>% 
    filter(!is.na(moderator))
  overall <- paste("Overall  (k = ", dat.mv$k, ")", sep = "")
  
  forest(x = c(dat.mv[[1]], dat.mv.var[[1]]),
         ci.lb = c(dat.mv[[6]], dat.mv.var[[6]]),
         ci.ub = c(dat.mv[[7]], dat.mv.var[[7]]),
         slab = c(overall, n.forest$var_col),
         header = h,
         top = 2)
}

nointercept <- function(dat.mv.var) {
  data.frame(QE = dat.mv.var$QE,
             QE_df = dat.mv.var$k - 1,
             QE_p = dat.mv.var$QEp,
             QM = dat.mv.var$QM,
             QM_df = dat.mv.var$p,
             QM_p = dat.mv.var$QMp,
             QM_QT = dat.mv.var[["QM"]] / sum(dat.mv.var[["QE"]], dat.mv.var[["QM"]]))
}



# Table of stats ----------------------------------------------------------

t01 <- nointercept(ex.mv.o)
t02 <- nointercept(nt.mv)
t03 <- nointercept(ex.mv.reg.o)
t04 <- nointercept(nt.mv.reg)
t05 <- nointercept(ex.mv.dfc.o)
t06 <- nointercept(nt.mv.dfc)
t07 <- nointercept(ex.mv.dlc)
t08 <- nointercept(nt.mv.dlc)
t09 <- nointercept(ex.mv.ctyp)
t10 <- nointercept(nt.mv.ctyp)
t11 <- nointercept(ex.mv.cratc.o)
t12 <- nointercept(nt.mv.cratc.o)
t13 <- nointercept(ex.mv.capc)
t14 <- nointercept(nt.mv.capc)
t15 <- nointercept(ex.mv.capt)
t16 <- nointercept(nt.mv.capt)
t17 <- nointercept(ex.mv.panp)
t18 <- nointercept(nt.mv.panp)
t19 <- nointercept(ex.mv.pgfs)
t20 <- nointercept(nt.mv.pgfs)
t21 <- nointercept(ex.mv.papgfs)
t22 <- nointercept(nt.mv.papgfs)
t23 <- nointercept(ex.mv.plotc)
t24 <- nointercept(nt.mv.plotc)
t25 <- nointercept(ex.mv.seedn.o)
t26 <- nointercept(nt.mv.seedn)

hetero <- rbind(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15,
                t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26)
hetero <- signif(hetero, digits = 4)
hetero$Model <- c(rep(c("Non-native", "Native"), 13))
hetero <- hetero[ , c(8, 1:7)]



# Model selection ---------------------------------------------------------

# Exotic
top <- weightable(res14)
top.ex <- top[top$aicc <= min(top$aicc) + 2, ]
rm(res14)

ex.ms14 <- ex[!apply(ex[ , c("region", "dlc", "C_type", "cratc", "capt", "plant_apgfs", "plotc")], 1, anyNA), ]
ex.mv.ms14 <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex.ms14,
                     mods = ~ 1 + dlc + plotc + region + plant_apgfs)

# Native
top.nt.o <- weightable(res14.o.nt)
top.nt.o <- top.nt.o[top.nt.o$aicc <= min(top.nt.o$aicc) + 2, ]
top.nt.o
summary(res14.o.nt@objects[[1]])
plot(res14.o.nt, type = "s")
rm(res14.o.nt)

nt.ms14 <- nt[!apply(nt[ , c("region", "dlc", "C_type", "cratc", "capt", "plant_apgfs", "plotc")], 1, anyNA), ]
nt.ms14.o <- nt.ms14 %>% 
  filter(obs_ID != 1697) %>% 
  filter(obs_ID != 1002)

nt.mv.ms14.o <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = nt.ms14.o,
                       mods = ~ 1 + dlc + cratc + region + C_type + plant_apgfs)




save(ex.mv.o, nt.mv, ex.mv.papgfs, ex.mv.dlc, ex.mv.cratc.o, nt.mv.cratc.o,
     ex, nt, ex.cratc.o, nt.cratc.o,
     file = ".RData/Single-mod-models-for-figures.RData")


save.image("RData/2022-01_results.RData")

