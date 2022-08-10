library(tidyverse)
library(metafor)
library(orchaRd)
library(ggpubr)

# Load data ---------------------------------------------------------------

load("RData/2022-01_results.RData")


# Study duration (dlc native) ---------------------------------------------

n.forest.nt.dlc <- count(nt, !!sym("dlc"))
n.forest.nt.dlc$x <- nt.mv.dlc[[1]]
n.forest.nt.dlc$ci.lb <- nt.mv.dlc[[6]]
n.forest.nt.dlc$ci.ub <- nt.mv.dlc[[7]]

tiff("output_figs/ESA_2022/dlc-native.tiff", width = 6, height = 4, units = "in", res = 300)
forest(x = n.forest.nt.dlc$x,
       ci.lb = n.forest.nt.dlc$ci.lb,
       ci.ub = n.forest.nt.dlc$ci.ub,
       slab = n.forest.nt.dlc$dlc,
       header = "Study duration (months)",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.nt.dlc$n,
       ilab.xpos = -1.3,
       cex = 0.75,
       top = 2)

par(cex = 0.75, font = 4)
text(x = -1.3, y = 11, labels = "k")
par(cex = 0.75, font = 2)
text(x = 0, y = 11, labels = "Native plant response")
dev.off()


# C type ------------------------------------------------------------------

# Exotic, all
n.forest.ex.ctyp <- count(ex, !!sym("C_type"))
n.forest.ex.ctyp$x <- ex.mv.ctyp[[1]]
n.forest.ex.ctyp$ci.lb <- ex.mv.ctyp[[6]]
n.forest.ex.ctyp$ci.ub <- ex.mv.ctyp[[7]]
rownames(n.forest.ex.ctyp) <- 1:nrow(n.forest.ex.ctyp)
n.forest.ex.ctyp <- n.forest.ex.ctyp %>% 
  arrange(desc(n))

tiff("output_figs/ESA_2022/ctyp-exotic.tiff", width = 6, height = 4, units = "in", res = 300)
forest(x = n.forest.ex.ctyp$x,
       ci.lb = n.forest.ex.ctyp$ci.lb,
       ci.ub = n.forest.ex.ctyp$ci.ub,
       slab = n.forest.ex.ctyp$C_type,
       header = "Carbon type",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.ex.ctyp$n,
       ilab.xpos = -5,
       cex = 0.75,
       top = 2)

par(cex = 0.75, font = 4)
text(x = -5, y = 14, labels = "k")
par(cex = 0.75, font = 2)
text(x = 0, y = 14, labels = "Exotic weed response")
dev.off()

# Exotic, sucrose & sawdust
n.forest.ex.sucsaw <- n.forest.ex.ctyp[1:4, ]

tiff("output_figs/ESA_2022/sucsaw-exotic.tiff", width = 6, height = 4, units = "in", res = 300)
forest(x = n.forest.ex.sucsaw$x,
       ci.lb = n.forest.ex.sucsaw$ci.lb,
       ci.ub = n.forest.ex.sucsaw$ci.ub,
       slab = n.forest.ex.sucsaw$C_type,
       header = "Carbon type",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.ex.sucsaw$n,
       ilab.xpos = -1.6,
       cex = 0.75,
       top = 2)

par(cex = 0.75, font = 4)
text(x = -1.6, y = 6, labels = "k")
par(cex = 0.75, font = 2)
text(x = 0, y = 6, labels = "Exotic weed response")
dev.off()


# Native, all
n.forest.nt.ctyp <- count(nt, !!sym("C_type"))
n.forest.nt.ctyp$x <- nt.mv.ctyp[[1]]
n.forest.nt.ctyp$ci.lb <- nt.mv.ctyp[[6]]
n.forest.nt.ctyp$ci.ub <- nt.mv.ctyp[[7]]
rownames(n.forest.nt.ctyp) <- 1:nrow(n.forest.nt.ctyp)
n.forest.nt.ctyp <- n.forest.nt.ctyp %>% 
  arrange(desc(n))

tiff("output_figs/ESA_2022/ctyp-native.tiff", width = 6, height = 4, units = "in", res = 300)
forest(x = n.forest.nt.ctyp$x,
       ci.lb = n.forest.nt.ctyp$ci.lb,
       ci.ub = n.forest.nt.ctyp$ci.ub,
       slab = n.forest.nt.ctyp$C_type,
       header = "Carbon type",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.nt.ctyp$n,
       ilab.xpos = -7,
       cex = 0.75,
       top = 2)

par(cex = 0.75, font = 4)
text(x = -7, y = 13, labels = "k")
par(cex = 0.75, font = 2)
text(x = 0, y = 13, labels = "Native plant response")
dev.off()


# Native, sucrose & sawdust
n.forest.nt.sucsaw <- n.forest.nt.ctyp[1:4, ]

tiff("output_figs/ESA_2022/sucsaw-native.tiff", width = 6, height = 4, units = "in", res = 300)
forest(x = n.forest.nt.sucsaw$x,
       ci.lb = n.forest.nt.sucsaw$ci.lb,
       ci.ub = n.forest.nt.sucsaw$ci.ub,
       slab = n.forest.nt.sucsaw$C_type,
       header = "Carbon type",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.nt.sucsaw$n,
       ilab.xpos = -0.6,
       cex = 0.75,
       top = 2)

par(cex = 0.75, font = 4)
text(x = -0.6, y = 6, labels = "k")
par(cex = 0.75, font = 2)
text(x = 0.5, y = 6, labels = "Native plant response")
dev.off()



# Lifeform (apgfs), native ------------------------------------------------

n.forest.nt.papgfs <- count(nt, !!sym("plant_apgfs"))
n.forest.nt.papgfs$x <- nt.mv.papgfs[[1]]
n.forest.nt.papgfs$ci.lb <- nt.mv.papgfs[[6]]
n.forest.nt.papgfs$ci.ub <- nt.mv.papgfs[[7]]
n.forest.nt.papgfs <- n.forest.nt.papgfs %>% 
  filter(plant_apgfs %in% c("annual forb", "annual graminoid", 
                            "perennial forb", "perennial graminoid"))

tiff("output_figs/ESA_2022/apgfs-native.tiff", width = 6, height = 4, units = "in", res = 300)
forest(x = n.forest.nt.papgfs$x,
       ci.lb = n.forest.nt.papgfs$ci.lb,
       ci.ub = n.forest.nt.papgfs$ci.ub,
       slab = n.forest.nt.papgfs$plant_apgfs,
       header = "Lifeform",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.nt.papgfs$n,
       ilab.xpos = -1.7,
       cex = 0.75,
       top = 2)

par(cex = 0.75, font = 4)
text(x = -1.7, y = 6, labels = "k")
par(cex = 0.75, font = 2)
text(x = 0, y = 6, labels = "Native plant response")
dev.off()
