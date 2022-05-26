library(tidyverse)
library(readxl)
library(metafor)
library(glmulti)

# Load data ---------------------------------------------------------------

ex <- read.csv("data/cleaned/exotic-cleaned.csv")


# Model selection setup ---------------------------------------------------

rma.glmulti <- function(formula, data, V, random,...) {
  do.call("rma.mv", list(as.formula(paste(deparse(formula))), V = as.name(V), random = as.name(random), data = data,  method = "ML", ...))
}
rand <- list(~ 1 | obs_ID, ~ 1 | exp_ID)


# Multimodel inference
eval(metafor:::.glmulti)



# region, dlc, C_type, cratc, capt, plant_apgfs, plotc --------------------

ex.ms14 <- ex[!apply(ex[ , c("region", "dlc", "C_type", "cratc", "capt", "plant_apgfs", "plotc")], 1, anyNA), ]

res14 <- glmulti(yi ~ region + dlc + C_type + cratc + capt + plant_apgfs + plotc, 
                  V = "vi", data = ex.ms14, level = 1, fitfunction = rma.glmulti, 
                  random = "rand", crit = "aicc")

print(res14)
top <- weightable(res14)
top <- top[top$aicc <= min(top$aicc) + 2, ]
top
summary(res14@objects[[1]])
plot(res14, type = "s")

forest(x = res14@objects[[1]]$b,
       ci.lb = res14@objects[[1]]$ci.lb,
       ci.ub = res14@objects[[1]]$ci.ub,
       slab = rownames(res14@objects[[1]]$b),
       alim = c(-5, 5),
       header = c("Top model 1", "Estimate [95% CI]"),
       cex = 0.75)

# No intercept
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

# Outliers removed
  # no outliers

save(res14, file = ".RData/ex-mod14.RData")

# Multimodel inference
mmi <- as.data.frame(coef(res14))
mmi <- data.frame(Estimate = mmi$Est, SE = sqrt(mmi$Uncond), Importance = mmi$Importance, row.names = row.names(mmi))
mmi$z <- mmi$Estimate / mmi$SE
mmi$p <- 2 * pnorm(abs(mmi$z), lower.tail = FALSE)
names(mmi) <- c("Estimate", "Std. Error", "Importance", "z value", "Pr(>|z|)")
mmi$ci.lb <- mmi[[1]] - qnorm(0.975) * mmi[[2]]
mmi$ci.ub <- mmi[[1]] + qnorm(0.975) * mmi[[2]]
mmi <- mmi[order(mmi$Importance, decreasing = TRUE), c(1, 2, 4:7, 3)]
mmi <- round(mmi, 4)
mmi$sig <- rep(NA, nrow(mmi))
for(i in 1:nrow(mmi)) {
  if(between(mmi$`Pr(>|z|)`[i], 0.01, 0.05)) {
    mmi$sig[i] <- "*"
  } else if(between(mmi$`Pr(>|z|)`[i], 0.001, 0.01)) {
    mmi$sig[i] <- "**"
  } else if(mmi$`Pr(>|z|)`[i] < 0.001) {
    mmi$sig[i] <- "***"
  } else if(between(mmi$`Pr(>|z|)`[i], 0.05, 0.1)) {
    mmi$sig[i] <- "."
  } else if(mmi$`Pr(>|z|)`[i] > 0.1) {
    mmi$sig[i] <- ""
  }
}
mmi


## Moderator importance > 0.8
forest(x = mmi$Estimate[1:33],
       ci.lb = mmi$ci.lb[1:33],
       ci.ub = mmi$ci.ub[1:33],
       slab = rownames(mmi[1:33, ]),
       cex = 0.7,
       header = c("Multimodel inference", "Estimate [95% CI]"))





# region, dlc, C_type, cratc, plotc, capt, plant_anper, plant_gfs ---------

ex.ms15 <- ex[!apply(ex[ , c("region", "dlc", "C_type", "cratc", "capt", "plant_anper", "plant_gfs", "plotc")], 1, anyNA), ]

res15 <- glmulti(yi ~ region + dlc + C_type + cratc + capt + plant_anper + plant_gfs + plotc, 
                 V = "vi", data = ex.ms15, level = 1, fitfunction = rma.glmulti, 
                 random = "rand", crit = "aicc")


print(res15)
top <- weightable(res15)
top <- top[top$aicc <= min(top$aicc) + 2, ]
top
summary(res15@objects[[1]])
plot(res15, type = "s")

forest(x = res15@objects[[1]]$b,
       ci.lb = res15@objects[[1]]$ci.lb,
       ci.ub = res15@objects[[1]]$ci.ub,
       slab = rownames(res15@objects[[1]]$b),
       alim = c(-5, 5),
       header = c("Top model 1", "Estimate [95% CI]"),
       cex = 0.75)

forest(x = res15@objects[[2]]$b,
       ci.lb = res15@objects[[2]]$ci.lb,
       ci.ub = res15@objects[[2]]$ci.ub,
       slab = rownames(res15@objects[[2]]$b),
       alim = c(-5, 5),
       header = c("Top model 2", "Estimate [95% CI]"),
       cex = 0.75)


save.image("RData/ex-modsel.RData")
