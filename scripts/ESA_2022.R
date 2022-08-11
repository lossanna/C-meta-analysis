library(tidyverse)
library(metafor)
library(orchaRd)
library(ggpubr)

# Load data ---------------------------------------------------------------

load("RData/2022-01_results.RData")


# Orchard plots -----------------------------------------------------------

# Table
f1.df <- data.frame(model = c("Exotic", "Native"),
                    ci.lb = c(ex.mv.o[["ci.lb"]], nt.mv[["ci.lb"]]),
                    ci.ub = c(ex.mv.o[["ci.ub"]], nt.mv[["ci.ub"]]),
                    pi.lb = c(predict(ex.mv.o)$pi.lb, predict(nt.mv)$pi.lb),
                    pi.ub = c(predict(ex.mv.o)$pi.ub, predict(nt.mv)$pi.ub),
                    estimate = c(ex.mv.o[["b"]], nt.mv[["b"]]))

# Orchard plots
f1.orc.ex <- orchard_plot(ex.mv.o, mod = "Int", xlab = "Effect size (Hedges' g)", transfm = "none") +
  ggtitle("Exotic") +
  annotate(geom = "text", x = -10, y = 1.4,
           label = paste0("CI: ", round(f1.df[1, 2], 2), ", ", round(f1.df[1, 3], 2), "***", "\n",
                          "PI: ", round(f1.df[1, 4], 2), ", ", round(f1.df[1, 5], 2)),
           hjust = 0,
           size = 6) +
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(size = 20)) 
f1.orc.ex

f1.orc.nt <- orchard_plot(nt.mv, mod = "Int", xlab = "Effect size (Hedges' g)", transfm = "none") +
  ggtitle("Native") +
  annotate(geom = "text", x = -10, y = 1.4,
           label = paste0("CI: ", round(f1.df[2, 2], 2), ", ", round(f1.df[2, 3], 2), "\n",
                          "PI: ", round(f1.df[2, 4], 2), ", ", round(f1.df[2, 5], 2)),
           hjust = 0,
           size = 6) +
  theme(text = element_text(size = 15)) +
  theme(plot.title = element_text(size = 20)) 
f1.orc.nt

tiff("output_figs/ESA_2022/orchard.tiff", width = 11, height = 4, units = "in", res = 300)
ggarrange(f1.orc.ex, f1.orc.nt,
          ncol = 2, nrow = 1)
dev.off()


# C rate ------------------------------------------------------------------

# Exotic
n.forest.ex.cratc <- count(ex.cratc.o, !!sym("cratc"))
n.forest.ex.cratc$x <- ex.mv.cratc.o[[1]]
n.forest.ex.cratc$ci.lb <- ex.mv.cratc.o[[6]]
n.forest.ex.cratc$ci.ub <- ex.mv.cratc.o[[7]]

tiff("output_figs/ESA_2022/crat-exotic.tiff", width = 6, height = 5, units = "in", res = 300)
forest(x = n.forest.ex.cratc$x,
       ci.lb = n.forest.ex.cratc$ci.lb,
       ci.ub = n.forest.ex.cratc$ci.ub,
       slab = n.forest.ex.cratc$cratc,
       annotate = FALSE,
       header = "C rate (g/m2)",
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.ex.cratc$n,
       ilab.xpos = -4.7,
       cex = 1,
       top = 2)

par(cex = 1.1, font = 4)
text(x = -4.7, y = 16.1, labels = "k")
par(cex = 1.1, font = 2)
text(x = 0, y = 16.1, labels = "Exotic")
dev.off()


# Native
n.forest.nt.cratc <- count(nt.cratc.o, !!sym("cratc"))
n.forest.nt.cratc$x <- nt.mv.cratc.o[[1]]
n.forest.nt.cratc$ci.lb <- nt.mv.cratc.o[[6]]
n.forest.nt.cratc$ci.ub <- nt.mv.cratc.o[[7]]

tiff("output_figs/ESA_2022/crat-native.tiff", width = 6, height = 5, units = "in", res = 300)
forest(x = n.forest.nt.cratc$x,
       ci.lb = n.forest.nt.cratc$ci.lb,
       ci.ub = n.forest.nt.cratc$ci.ub,
       slab = n.forest.nt.cratc$cratc,
       header = "C rate (g/m2)",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.nt.cratc$n,
       ilab.xpos = -1.6,
       cex = 1,
       top = 2)

par(cex = 1.1, font = 4)
text(x = -1.6, y = 16.1, labels = "k")
par(cex = 1.1, font = 2)
text(x = 0, y = 16.1, labels = "Native")
dev.off()



# Duration since last (dlc) -----------------------------------------------

# Exotic
n.forest.ex.dlc <- count(ex, !!sym("dlc"))
n.forest.ex.dlc$x <- ex.mv.dlc[[1]]
n.forest.ex.dlc$ci.lb <- ex.mv.dlc[[6]]
n.forest.ex.dlc$ci.ub <- ex.mv.dlc[[7]]

tiff("output_figs/ESA_2022/dlc-exotic.tiff", width = 6, height = 4.5, units = "in", res = 300)
forest(x = n.forest.ex.dlc$x,
       ci.lb = n.forest.ex.dlc$ci.lb,
       ci.ub = n.forest.ex.dlc$ci.ub,
       slab = n.forest.ex.dlc$dlc,
       header = "Months",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.ex.dlc$n,
       ilab.xpos = -2.4,
       cex = 1,
       top = 2)

par(cex = 1.1, font = 4)
text(x = -2.4, y = 11, labels = "k")
par(cex = 1.1, font = 2)
text(x = 0, y = 11, labels = "Exotic")
dev.off()


# Native
n.forest.nt.dlc <- count(nt, !!sym("dlc"))
n.forest.nt.dlc$x <- nt.mv.dlc[[1]]
n.forest.nt.dlc$ci.lb <- nt.mv.dlc[[6]]
n.forest.nt.dlc$ci.ub <- nt.mv.dlc[[7]]

tiff("output_figs/ESA_2022/dlc-native.tiff", width = 6, height = 4.5, units = "in", res = 300)
forest(x = n.forest.nt.dlc$x,
       ci.lb = n.forest.nt.dlc$ci.lb,
       ci.ub = n.forest.nt.dlc$ci.ub,
       slab = n.forest.nt.dlc$dlc,
       header = "Months",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.nt.dlc$n,
       ilab.xpos = -1.3,
       cex = 1,
       top = 2)

par(cex = 1.1, font = 4)
text(x = -1.3, y = 11, labels = "k")
par(cex = 1.1, font = 2)
text(x = 0, y = 11, labels = "Native")
dev.off()


# C type ------------------------------------------------------------------

# Exotic
n.forest.ex.ctyp <- count(ex, !!sym("C_type"))
n.forest.ex.ctyp$x <- ex.mv.ctyp[[1]]
n.forest.ex.ctyp$ci.lb <- ex.mv.ctyp[[6]]
n.forest.ex.ctyp$ci.ub <- ex.mv.ctyp[[7]]
rownames(n.forest.ex.ctyp) <- 1:nrow(n.forest.ex.ctyp)
n.forest.ex.ctyp <- n.forest.ex.ctyp %>% 
  arrange(desc(n))
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
       ilab.xpos = -1.5,
       cex = 1,
       top = 2)

par(cex = 1.1, font = 4)
text(x = -1.5, y = 6, labels = "k")
par(cex = 1.1, font = 2)
text(x = 0, y = 6, labels = "Exotic")
dev.off()


# Native
n.forest.nt.ctyp <- count(nt, !!sym("C_type"))
n.forest.nt.ctyp$x <- nt.mv.ctyp[[1]]
n.forest.nt.ctyp$ci.lb <- nt.mv.ctyp[[6]]
n.forest.nt.ctyp$ci.ub <- nt.mv.ctyp[[7]]
rownames(n.forest.nt.ctyp) <- 1:nrow(n.forest.nt.ctyp)
n.forest.nt.ctyp <- n.forest.nt.ctyp %>% 
  arrange(desc(n))
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
       cex = 1,
       top = 2)

par(cex = 1.1, font = 4)
text(x = -0.6, y = 6, labels = "k")
par(cex = 1.1, font = 2)
text(x = 0.5, y = 6, labels = "Native")
dev.off()




# Plant lifeform (apgfs) --------------------------------------------------

# Exotic
n.forest.ex.papgfs <- count(ex, !!sym("plant_apgfs"))
n.forest.ex.papgfs$x <- ex.mv.papgfs[[1]]
n.forest.ex.papgfs$ci.lb <- ex.mv.papgfs[[6]]
n.forest.ex.papgfs$ci.ub <- ex.mv.papgfs[[7]]
n.forest.ex.papgfs <- n.forest.ex.papgfs %>% 
  filter(plant_apgfs %in% c("annual forb", "annual graminoid", 
                            "perennial forb", "perennial graminoid"))

tiff("output_figs/ESA_2022/apgfs-exotic.tiff", width = 6, height = 4, units = "in", res = 300)
forest(x = n.forest.ex.papgfs$x,
       ci.lb = n.forest.ex.papgfs$ci.lb,
       ci.ub = n.forest.ex.papgfs$ci.ub,
       slab = n.forest.ex.papgfs$plant_apgfs,
       header = "Lifeform",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.ex.papgfs$n,
       ilab.xpos = -1.3,
       cex = 1,
       top = 2)

par(cex = 1.1, font = 4)
text(x = -1.3, y = 6, labels = "k")
par(cex = 1.1, font = 2)
text(x = 0, y = 6, labels = "Exotic")
dev.off()


# Native
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
       ilab.xpos = -1.4,
       cex = 1,
       top = 2)

par(cex = 1.1, font = 4)
text(x = -1.4, y = 6, labels = "k")
par(cex = 1.1, font = 2)
text(x = 0, y = 6, labels = "Native")
dev.off()


# Seeding of native -------------------------------------------------------

# Exotic
n.forest.ex.seedn <- count(ex, !!sym("seedn"))
n.forest.ex.seedn$x <- ex.mv.seedn[[1]]
n.forest.ex.seedn$ci.lb <- ex.mv.seedn[[6]]
n.forest.ex.seedn$ci.ub <- ex.mv.seedn[[7]]

tiff("output_figs/ESA_2022/seedn-exotic.tiff", width = 6, height = 4, units = "in", res = 300)
forest(x = n.forest.ex.seedn$x,
       ci.lb = n.forest.ex.seedn$ci.lb,
       ci.ub = n.forest.ex.seedn$ci.ub,
       slab = n.forest.ex.seedn$seedn,
       header = "Seeding",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.ex.seedn$n,
       ilab.xpos = -1.1,
       cex = 1,
       top = 2)

par(cex = 1.1, font = 4)
text(x = -1.1, y = 4, labels = "k")
par(cex = 1.1, font = 2)
text(x = -0.6, y = 4, labels = "Exotic")
dev.off()


# Native
n.forest.nt.seedn <- count(nt, !!sym("seedn"))
n.forest.nt.seedn$x <- nt.mv.seedn[[1]]
n.forest.nt.seedn$ci.lb <- nt.mv.seedn[[6]]
n.forest.nt.seedn$ci.ub <- nt.mv.seedn[[7]]

tiff("output_figs/ESA_2022/seedn-native.tiff", width = 6, height = 4, units = "in", res = 300)
forest(x = n.forest.nt.seedn$x,
       ci.lb = n.forest.nt.seedn$ci.lb,
       ci.ub = n.forest.nt.seedn$ci.ub,
       slab = n.forest.nt.seedn$seedn,
       header = "Seeding",
       annotate = FALSE,
       xlab = substitute(paste("Effect size (Hedges' ", italic("g"), ")")),
       ilab = n.forest.nt.seedn$n,
       ilab.xpos = -0.7,
       cex = 1,
       top = 2)

par(cex = 1.1, font = 4)
text(x = -0.7, y = 4, labels = "k")
par(cex = 1.1, font = 2)
text(x = -0.2, y = 4, labels = "Native")
dev.off()

