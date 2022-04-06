library(tidyverse)
library(metafor)
library(glmulti)


# Load data ---------------------------------------------------------------

load("RData-RMarkdown/ex-1mod.RData")
load("RData-RMarkdown/nt-1mod.RData")
load("RData-RMarkdown/ex-mod14.RData")
load("RData-RMarkdown/nt-mod14o.RData")


# Functions ---------------------------------------------------------------

forest.cat <- function(dat, var, dat.mv, dat.mv.var, h) {
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
t03 <- nointercept(ex.mv.bio)
t04 <- nointercept(nt.mv.bio)
t05 <- nointercept(ex.mv.reg.o)
t06 <- nointercept(nt.mv.reg)
t07 <- nointercept(ex.mv.soil)
t08 <- nointercept(nt.mv.soil)
t09 <- nointercept(ex.mv.dfc.o)
t10 <- nointercept(nt.mv.dfc)
t11 <- nointercept(ex.mv.dlc)
t12 <- nointercept(nt.mv.dlc)
t13 <- nointercept(ex.mv.ctyp)
t14 <- nointercept(nt.mv.ctyp)
t15 <- nointercept(ex.mv.cratc.o)
t16 <- nointercept(nt.mv.cratc.o)
t17 <- nointercept(ex.mv.capt)
t18 <- nointercept(nt.mv.capt)
t19 <- nointercept(ex.mv.panp)
t20 <- nointercept(nt.mv.panp)
t21 <- nointercept(ex.mv.pgfs)
t22 <- nointercept(nt.mv.pgfs)
t23 <- nointercept(ex.mv.papgfs)
t24 <- nointercept(nt.mv.papgfs)
t25 <- nointercept(ex.mv.plotc)
t26 <- nointercept(nt.mv.plotc)
t27 <- nointercept(ex.mv.seedn.o)
t28 <- nointercept(nt.mv.seedn)

hetero <- rbind(t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13, t14, t15,
                t16, t17, t18, t19, t20, t21, t22, t23, t24, t25, t26, t27, t28)

hetero <- signif(hetero, digits = 4)

hetero$Model <- c(rep(c("Exotic", "Native"), 14))

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

forest(x = ex.mv.ms14$b,
       ci.lb = ex.mv.ms14$ci.lb,
       ci.ub = ex.mv.ms14$ci.ub,
       slab = rownames(ex.mv.ms14$b),
       alim = c(-5, 5),
       header = c("Top exotic model 1", "Estimate [95% CI]"),
       cex = 0.75)


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

forest(x = nt.mv.ms14.o$b,
       ci.lb = nt.mv.ms14.o$ci.lb,
       ci.ub = nt.mv.ms14.o$ci.ub,
       slab = rownames(nt.mv.ms14.o$b),
       alim = c(-5, 5),
       header = c("Top native model 1", "Estimate [95% CI]"),
       cex = 0.75)


save.image("RData-RMarkdown/2111_results.RData")
