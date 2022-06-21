library(tidyverse)
library(metafor)

# Load data ---------------------------------------------------------------

load("RData/Main-figures.RData")



# R1 comment 6 ------------------------------------------------------------

# Looking at study overlap between native and exotic datasets
exp.diff.ex <- setdiff(ex$exp_ID, nt$exp_ID) # 9 experiments
length(setdiff(ex$paper_ID, nt$paper_ID)) # 9 papers, but that includes Iannone 2008 (paper_ID 24), which is not
exp.diff.nt <- setdiff(nt$exp_ID, ex$exp_ID) # 12 experiments

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
predict(ex.mv.o)
predict(ex.diff.mv)

predict(nt.mv)
predict(nt.diff.mv)
