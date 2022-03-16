library(tidyverse)

# Load data ---------------------------------------------------------------

load("1021 ex 1mod.RData")
load("0821 nt 1mod.RData")


# Count of all papers used in meta-analysis -------------------------------

ex.o.paper <- ex.o %>% 
  select(paper, paper_ID) %>% 
  distinct()

nt.paper <- nt %>% 
  select(paper, paper_ID) %>% 
  distinct()

all.paper <- rbind(ex.o.paper, nt.paper)
all.paper <- arrange(all.paper) 

unique(all.paper$paper)
length(unique(all.paper$paper)) # Iannone_2008 is 2 different papers
length(unique(all.paper$paper_ID))


length(unique(ex.o$paper_ID))
length(unique(nt$paper_ID))

# Count of all experiments used in meta-analysis --------------------------

length(unique(ex.o$exp_ID))
length(unique(nt$exp_ID))

all.exp <- c(ex.o$exp_ID, nt$exp_ID)
length(unique(all.exp))
