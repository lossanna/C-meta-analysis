library(tidyverse)
library(metafor)
library(orchaRd)
library(ggpubr)

# Load data ---------------------------------------------------------------

load(".RData/Single-mod-models-for-figures.RData")


# Figure 1 (summary, CI, PI) ----------------------------------------------

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
  annotate(geom = "text", x = -6, y = 1.4,
           label = paste0("95% CI: ", round(f1.df[1, 2], 3), ", ", round(f1.df[1, 3], 3), "\n",
                          "95% PI: ", round(f1.df[1, 4], 3), ", ", round(f1.df[1, 5], 3)))
f1.orc.ex

f1.orc.nt <- orchard_plot(nt.mv, mod = "Int", xlab = "Effect size (Hedges' g)", transfm = "none") +
  ggtitle("Native") +
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

n.forest.ex.papgfs <- count(ex, !!sym("plant_apgfs"))
n.forest.ex.papgfs$x <- ex.mv.papgfs[[1]]
n.forest.ex.papgfs$ci.lb <- ex.mv.papgfs[[6]]
n.forest.ex.papgfs$ci.ub <- ex.mv.papgfs[[7]]
n.forest.ex.papgfs <- n.forest.ex.papgfs %>% 
  filter(plant_apgfs %in% c("annual forb", "annual graminoid", 
                            "perennial forb", "perennial graminoid"))

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
text(x = -1.3, y = 6, labels = "n")
par(cex = 0.75, font = 2)
text(x = 0, y = 6, labels = "Exotic plant response")
dev.off()

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

n.forest.ex.dlc <- count(ex, !!sym("dlc"))
n.forest.ex.dlc$x <- ex.mv.dlc[[1]]
n.forest.ex.dlc$ci.lb <- ex.mv.dlc[[6]]
n.forest.ex.dlc$ci.ub <- ex.mv.dlc[[7]]

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

n.forest.ex.cratc <- count(ex.cratc.o, !!sym("cratc"))
n.forest.ex.cratc$x <- ex.mv.cratc.o[[1]]
n.forest.ex.cratc$ci.lb <- ex.mv.cratc.o[[6]]
n.forest.ex.cratc$ci.ub <- ex.mv.cratc.o[[7]]

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


save.image("Main-figures.RData")
