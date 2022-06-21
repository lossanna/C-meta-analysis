library(tidyverse)
library(metafor)

# Load data ---------------------------------------------------------------

load("RData/Main-figures.RData")


# R1 comment 6 ------------------------------------------------------------

# Looking at study overlap between native and exotic datasets
exp.diff.ex <- setdiff(ex$exp_ID, nt$exp_ID) # 9 experiments in ex but not nt
length(setdiff(ex$paper_ID, nt$paper_ID)) # 9 papers in ex but not nt
exp.diff.nt <- setdiff(nt$exp_ID, ex$exp_ID) # 12 experiments in nt but not ex

ex.diff <- ex %>% 
  filter(!exp_ID %in% exp.diff.ex)
nt.diff <- nt %>%
  filter(!exp_ID %in% exp.diff.nt)

# Summary models
ex.diff.mv <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = ex.diff) # no difference in significance
print(ex.diff.mv)
predict(ex.diff.mv)

nt.diff.mv <- rma.mv(yi = yi,
                     V = vi,
                     random = ~ 1 | exp_ID / obs_ID,
                     data = nt.diff)
print(nt.diff.mv)
predict(nt.diff.mv)

# Compare
pred.ex.diff <- predict(ex.diff.mv)
pred.ex <- predict(ex.mv.o)

pred.nt.diff <- predict(nt.diff.mv)
pred.nt <- predict(nt.mv)


r1c6.table <- data.frame(model = c("ex removed", "ex original", "nt removed", "nt original"),
                         g = c(pred.ex.diff[[1]], pred.ex[[1]], pred.nt.diff[[1]], pred.nt[[1]]),
                         ci.lb = c(pred.ex.diff[[3]], pred.ex[[3]], pred.nt.diff[[3]], pred.nt[[3]]),
                         ci.ub = c(pred.ex.diff[[4]], pred.ex[[4]], pred.nt.diff[[4]], pred.nt[[4]]),
                         pi.lb = c(pred.ex.diff[[5]], pred.ex[[5]], pred.nt.diff[[5]], pred.nt[[5]]),
                         pi.ub = c(pred.ex.diff[[6]], pred.ex[[6]], pred.nt.diff[[6]], pred.nt[[6]]))
r1c6.table <- r1c6.table %>% 
  mutate_if(is.numeric, round, 3)


# Save --------------------------------------------------------------------

save.image("RData/reviewer-comments.RData")
