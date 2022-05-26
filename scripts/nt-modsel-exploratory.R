library(tidyverse)
library(readxl)
library(metafor)
library(glmulti)

# Load data ---------------------------------------------------------------

nt <- read.csv("data/cleaned/native-cleaned.csv")


# Model selection setup ---------------------------------------------------

rma.glmulti <- function(formula, data, V, random,...) {
  do.call("rma.mv", list(as.formula(paste(deparse(formula))), V = as.name(V), random = as.name(random), data = data,  method = "ML", ...))
}
rand <- list(~ 1 | obs_ID, ~ 1 | exp_ID)


# Multimodel inference
eval(metafor:::.glmulti)





# region, dlc, C_type, C_rate, capt, plant_apgfs --------------------------

nt.ms9 <- nt[!apply(nt[ , c("region", "dlc", "C_type", "C_rate", "capt", "plant_apgfs")], 1, anyNA), ]

res9 <- glmulti(yi ~ region + dlc + C_type + C_rate + capt + plant_apgfs, 
                V = "vi", data = nt.ms9, level = 1, fitfunction = rma.glmulti, random = "rand",
                crit = "aicc") # did not achieve convergence




# region, soil_suborder, dlc, C_type, C_rate, capt, plant_apgfs -----------

nt.ms12 <- nt[!apply(nt[ , c("region", "soil_suborder", "dlc", "C_type", "C_rate", "capt", "plant_apgfs")], 1, anyNA), ]

res12 <- glmulti(yi ~ region + soil_suborder + dlc + C_type + C_rate + capt + plant_apgfs, 
                 V = "vi", data = nt.ms12, level = 1, fitfunction = rma.glmulti, random = "rand",
                 crit = "aicc", confsetsize = 128) # did not achieve convergence



# biome, C_type, C_rate, plant_apgfs --------------------------------------

nt.ms13 <- nt[!apply(nt[ , c("biome", "C_type", "C_rate")], 1, anyNA), ]

res13 <- glmulti(yi ~ biome + C_type + C_rate + plant_apgfs, 
                V = "vi", data = nt.ms13, level = 1, fitfunction = rma.glmulti, random = "rand",
                crit = "aicc")
print(res13)
plot(res13)
top <- weightable(res13)
top <- top[top$aicc <= min(top$aicc) + 2, ]
top
summary(res13@objects[[1]])
plot(res13, type = "s")


# region, dlc, C_type, cratc, capt, plant_apgfs, plotc --------------------

nt.ms14 <- nt[!apply(nt[ , c("region", "dlc", "C_type", "cratc", "capt", "plant_apgfs", "plotc")], 1, anyNA), ]

res14.nt <- glmulti(yi ~ region + dlc + C_type + cratc + capt + plant_apgfs + plotc, 
                 V = "vi", data = nt.ms14, level = 1, fitfunction = rma.glmulti, 
                 random = "rand", crit = "aicc")

print(res14.nt)
top <- weightable(res14.nt)
top <- top[top$aicc <= min(top$aicc) + 2, ]
top
summary(res14@objects[[1]])
plot(res14, type = "s")


# Outliers removed
nt.ms14.o <- nt.ms14 %>% 
  filter(obs_ID != 1697) %>% 
  filter(obs_ID != 1002)

res14.o.nt <- glmulti(yi ~ region + dlc + C_type + cratc + capt + plant_apgfs + plotc, 
                    V = "vi", data = nt.ms14.o, level = 1, fitfunction = rma.glmulti, 
                    random = "rand", crit = "aicc")

print(res14.o.nt)
top.nt.o <- weightable(res14.o.nt)
top.nt.o <- top.nt.o[top.nt.o$aicc <= min(top.nt.o$aicc) + 2, ]
top.nt.o
summary(res14.o@objects[[1]])
plot(res14.o, type = "s")

save(res14.o.nt, file = "RData/nt-mod14o.RData")



save.image("RData/nt-modsel-exploratory.RData")
