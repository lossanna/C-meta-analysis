library(tidyverse)
library(readxl)
library(metafor)

# Load data ---------------------------------------------------------------

nt <- read.csv("data/cleaned/native-cleaned.csv")


# Outliers (fewer removed) ------------------------------------------------

  # none


# Precision (inverse SE) --------------------------------------------------

nt$n_avg <- rowMeans(nt[ , c("n_cntrl", "n_trt")])
nt$SE_inv <- sqrt(nt$n_avg) / sqrt(nt$vi)


# No moderators (summary effect) ------------------------------------------


nt.mv.pb <- rma.mv(yi = yi,
                   V = vi,
                   random = ~ 1 | exp_ID / obs_ID,
                   data = nt,
                   mods = ~ SE_inv,
                   level = 90)
summary(nt.mv.pb)
nt.mv.pb[["b"]][1]
nt.mv.pb[["pval"]][1]

funnel(nt.mv.pb, yaxis = "seinv", pch = 1, back = "white")




# Biome -------------------------------------------------------------------

nt.mv.bio.pb <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = nt,
                       mods = ~ SE_inv + factor(biome),
                       level = 90)
nt.mv.bio.pb[["b"]][1]
nt.mv.bio.pb[["pval"]][1]

funnel(nt.mv.bio.pb, yaxis = "seinv", pch = 1, back = "white")




# Region ------------------------------------------------------------------

nt.mv.reg.pb <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = nt,
                       mods = ~ SE_inv + factor(region),
                       level = 90)
nt.mv.reg.pb[["b"]][1]
nt.mv.reg.pb[["pval"]][1]

funnel(nt.mv.reg.pb, yaxis = "seinv", pch = 1, back = "white")



# Soil suborder -----------------------------------------------------------

nt.mv.soil.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + factor(soil_suborder),
                        level = 90)
nt.mv.soil.pb[["b"]][1]
nt.mv.soil.pb[["pval"]][1]

funnel(nt.mv.soil.pb, yaxis = "seinv", pch = 1, back = "white")


# Duration_first (categorical) --------------------------------------------

nt.mv.dfc.pb <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = nt,
                       mods = ~ SE_inv + factor(dfc),
                       level = 90)
nt.mv.dfc.pb[["b"]][1]
nt.mv.dfc.pb[["pval"]][1]

funnel(nt.mv.dfc.pb, yaxis = "seinv", pch = 1, back = "white")


# Duration_last (categorical) ---------------------------------------------

nt.mv.dlc.pb <- rma.mv(yi = yi,
                       V = vi,
                       random = ~ 1 | exp_ID / obs_ID,
                       data = nt,
                       mods = ~ SE_inv + factor(dlc),
                       level = 90)
nt.mv.dlc.pb[["b"]][1]
nt.mv.dlc.pb[["pval"]][1]

funnel(nt.mv.dlc.pb, yaxis = "seinv", pch = 1, back = "white")





# C type ------------------------------------------------------------------

nt.mv.ctyp.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + factor(C_type),
                        level = 90)
nt.mv.ctyp.pb[["b"]][1]
nt.mv.ctyp.pb[["pval"]][1]

funnel(nt.mv.ctyp.pb, yaxis = "seinv", pch = 1, back = "white")


# C rate ------------------------------------------------------------------

nt.mv.crat.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + C_rate,
                        level = 90)
nt.mv.crat.pb[["b"]][1]
nt.mv.crat.pb[["pval"]][1]

funnel(nt.mv.crat.pb, yaxis = "seinv", pch = 1, back = "white")


# C applications total (categorical) --------------------------------------

nt.mv.capc.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + factor(capc),
                        level = 90)
nt.mv.capc.pb[["b"]][1]
nt.mv.capc.pb[["pval"]][1]

funnel(nt.mv.capc.pb, yaxis = "seinv", pch = 1, back = "white")


# Months between C applications -------------------------------------------

nt.mv.capm.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + factor(capm),
                        level = 90)
nt.mv.capm.pb[["b"]][1]
nt.mv.capm.pb[["pval"]][1]

funnel(nt.mv.capm.pb, yaxis = "seinv", pch = 1, back = "white")


# Months applying C -------------------------------------------------------

nt.mv.capt.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + factor(capt),
                        level = 90)
nt.mv.capt.pb[["b"]][1]
nt.mv.capt.pb[["pval"]][1]

funnel(nt.mv.capt.pb, yaxis = "seinv", pch = 1, back = "white")


# Annual/perennial --------------------------------------------------------

nt.mv.panp.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + factor(plant_anper),
                        level = 90)
nt.mv.panp.pb[["b"]][1]
nt.mv.panp.pb[["pval"]][1]

funnel(nt.mv.panp.pb, yaxis = "seinv", pch = 1, back = "white")


# Grass/forb/shrub --------------------------------------------------------

nt.mv.pgfs.pb <- rma.mv(yi = yi,
                        V = vi,
                        random = ~ 1 | exp_ID / obs_ID,
                        data = nt,
                        mods = ~ SE_inv + factor(plant_gfs),
                        level = 90)
nt.mv.pgfs.pb[["b"]][1]
nt.mv.pgfs.pb[["pval"]][1]

funnel(nt.mv.pgfs.pb, yaxis = "seinv", pch = 1, back = "white")


# Annual/perennial and grass/forb/shrub -----------------------------------

nt.mv.papgfs.pb <- rma.mv(yi = yi,
                          V = vi,
                          random = ~ 1 | exp_ID / obs_ID,
                          data = nt,
                          mods = ~ SE_inv + factor(plant_apgfs),
                          level = 90)
nt.mv.papgfs.pb[["b"]][1]
nt.mv.papgfs.pb[["pval"]][1]

funnel(nt.mv.papgfs.pb, yaxis = "seinv", pch = 1, back = "white")


# Model selection 9 (top model) -------------------------------------------

# Did not converge



# Combine -----------------------------------------------------------------

intcp.sym.nt <- data.frame(Model = c("No moderators",
                                  "Biome (grassland type)",
                                  "Region",
                                  "Soil suborder",
                                  "duration_first (categorical)", 
                                  "duration_last (categorical)", 
                                  "C type",
                                  "C rate",
                                  "C applications (categorical)",
                                  "Months between C applications",
                                  "Months applying C",
                                  "Annual/perennial",
                                  "Grass/forb/shrub",
                                  "Annual/perennial and grass/forb/shrub",
                                  "Model selection 9"),
                        Intercept = c(nt.mv.pb[["b"]][1],
                                      nt.mv.bio.pb[["b"]][1],
                                      nt.mv.reg.pb[["b"]][1],
                                      nt.mv.soil.pb[["b"]][1],
                                      nt.mv.dfc.pb[["b"]][1],
                                      nt.mv.dlc.pb[["b"]][1],
                                      nt.mv.ctyp.pb[["b"]][1],
                                      nt.mv.crat.pb[["b"]][1],
                                      nt.mv.capc.pb[["b"]][1],
                                      nt.mv.capm.pb[["b"]][1],
                                      nt.mv.capt.pb[["b"]][1],
                                      nt.mv.panp.pb[["b"]][1],
                                      nt.mv.pgfs.pb[["b"]][1],
                                      nt.mv.papgfs.pb[["b"]][1],
                                      NA),
                        Intercept_pval = c(nt.mv.pb[["pval"]][1],
                                           nt.mv.bio.pb[["pval"]][1],
                                           nt.mv.reg.pb[["pval"]][1],
                                           nt.mv.soil.pb[["pval"]][1],
                                           nt.mv.dfc.pb[["pval"]][1],
                                           nt.mv.dlc.pb[["pval"]][1],
                                           nt.mv.ctyp.pb[["pval"]][1],
                                           nt.mv.crat.pb[["pval"]][1],
                                           nt.mv.capc.pb[["pval"]][1],
                                           nt.mv.capm.pb[["pval"]][1],
                                           nt.mv.capt.pb[["pval"]][1],
                                           nt.mv.panp.pb[["pval"]][1],
                                           nt.mv.pgfs.pb[["pval"]][1],
                                           nt.mv.papgfs.pb[["pval"]][1],
                                           NA),
                        Symmetry = rep(NA, 15))

for(i in 1:nrow(intcp.sym.nt)) {
  if(intcp.sym.nt$Intercept_pval[i] > 0.1) {
    intcp.sym.nt$Symmetry[i] <- "symmetric"
  } else {
    intcp.sym.nt$Symmetry[i] <- "bias"
  }
}


save.image("RData/nt-pbias.RData")

