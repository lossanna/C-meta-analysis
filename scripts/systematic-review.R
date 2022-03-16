library(readxl)
library(tidyverse)

# Load data ---------------------------------------------------------------

review <- read_xlsx("C-addition-studies.xlsx", sheet = "screen_review")

screen1 <- read_xlsx("C-addition-studies.xlsx", sheet = "screen 1_intake")
screen1 <- screen1[ , c(1:2, 48:50)]
colnames(screen1) <- c("authors", "year", "review", "screen2", "screen3")
screen1$authors <- str_split_fixed(screen1$authors, " ", 2)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# Biome -------------------------------------------------------------------

bio <- review %>%
  filter(!is.na(biome)) %>% 
  count(biome)
bio <- bio %>% 
  mutate(perc = n / sum(bio$n) * 100)


# Region ------------------------------------------------------------------

reg <- review %>%
  filter(!is.na(region)) %>% 
  count(region)
reg <- reg %>% 
  mutate(perc = n / sum(reg$n) * 100)

reg.usa <- reg %>% 
  filter(str_detect(region, "USA"))
sum(reg.usa$n)
sum(reg.usa$perc)


# Duration ----------------------------------------------------------------

summary(review$dur, na.rm = TRUE)
hist(review$dur, breaks = 40)

# To better see first part of histogram
dur.majority <- review %>% 
  filter(dur < 60)
hist(dur.majority$dur, breaks = 20)


# C type ------------------------------------------------------------------

# This whole thing is a terrible example of code
# Combine counts of studies that tested multiple C types
ctypa.s <- review %>%
  filter(ctypm == "single") %>% 
  count(ctyp1)

ctypa.m <- review %>%
  filter(ctypm == "multiple") %>% 
  select(ctyp2, ctyp3, ctyp4) %>% 
  pivot_longer(everything(), names_to = "col", values_to = "ctyp1") %>% 
  filter(!is.na(ctyp1)) %>% 
  select(ctyp1) %>% 
  count(ctyp1)

ctypa.int <- intersect(ctypa.s$ctyp1, ctypa.m$ctyp1)
ctypa.add.s <- ctypa.s %>% 
  filter(ctyp1 %in% ctypa.int) %>% 
  select(order(colnames(.)))
ctypa.add.m <- ctypa.m %>% 
  filter(ctyp1 %in% ctypa.int) %>% 
  select(order(colnames(.))) %>% 
  mutate(n2 = ctypa.add.s$n) %>% 
  select(-ctyp1) %>% 
  mutate(n = n + n2) %>% 
  select(-n2)
ctypa.add.s <- ctypa.add.s[ , -2]
ctypa.add.s <- cbind(ctypa.add.s, ctypa.add.m)

ctypa.s <- ctypa.s %>% 
  filter(!ctyp1 %in% ctypa.int)
ctypa <- rbind(ctypa.add.s, ctypa.s)

rm(ctypa.add.m, ctypa.add.s, ctypa.m, ctypa.s, ctypa.int)

# Percentages
ctypa <- ctypa %>% 
  mutate(perc = n / sum(ctypa$n) * 100)

ctypm <- review %>%
  filter(!is.na(ctypm)) %>% 
  count(ctypm)
ctypm.sum <- sum(ctypm["n"])
ctypm <- ctypm %>% 
  mutate(perc = n / ctypm.sum * 100)


# C rate ------------------------------------------------------------------

cratav.na <- review %>% 
  filter(!is.na(cratav))
summary(cratav.na$cratav)
hist(cratav.na$cratav, breaks = 40)

# To better see first part of histogram
cratav.majority <- cratav.na %>% 
  filter(cratav < 1000)
hist(cratav.majority$cratav, breaks = 40)


cratm <- review %>%
  filter(!is.na(cratm)) %>% 
  count(cratm)
cratm.sum <- sum(cratm["n"])
cratm <- cratm %>% 
  mutate(perc = n / cratm.sum * 100)



# Reapplication -----------------------------------------------------------

capr <- review %>%
  filter(!is.na(capr)) %>% 
  count(capr)
capr.sum <- sum(capr["n"])
capr <- capr %>% 
  mutate(perc = n / capr.sum * 100)



# Multiple treatments -----------------------------------------------------

# This is what I did for C type and it's still terrible
# Combine counts of studies that tested multiple treatments
mtrt.s <- review %>%
  filter(mtrtn == 1) %>% 
  count(mtrt1)

mtrt.m <- review %>%
  filter(mtrtn != 1) %>% 
  select(mtrt2, mtrt3) %>% 
  pivot_longer(everything(), names_to = "col", values_to = "mtrt1") %>% 
  filter(!is.na(mtrt1)) %>% 
  select(mtrt1) %>% 
  count(mtrt1)

mtrt.int <- intersect(mtrt.s$mtrt1, mtrt.m$mtrt1)
mtrt.add.s <- mtrt.s %>% 
  filter(mtrt1 %in% mtrt.int) %>% 
  select(order(colnames(.)))
mtrt.add.m <- mtrt.m %>% 
  filter(mtrt1 %in% mtrt.int) %>% 
  select(order(colnames(.))) %>% 
  mutate(n2 = mtrt.add.s$n) %>% 
  select(-mtrt1) %>% 
  mutate(n = n + n2) %>% 
  select(-n2)
mtrt.add.s <- mtrt.add.s[ , -2]
mtrt.add.s <- cbind(mtrt.add.s, mtrt.add.m)

mtrt.s <- mtrt.s %>% 
  filter(!mtrt1 %in% mtrt.int)
mtrt <- rbind(mtrt.add.s, mtrt.s)

rm(mtrt.add.m, mtrt.add.s, mtrt.m, mtrt.s, mtrt.int)

# Percentages
mtrt <- mtrt %>% 
  mutate(perc = n / sum(mtrt$n) * 100)

mtrtn <- review %>%
  filter(!is.na(mtrtn)) %>% 
  count(mtrtn)
mtrtn.sum <- sum(mtrtn["n"])
mtrtn <- mtrtn %>% 
  mutate(perc = n / mtrtn.sum * 100)


# Absolute percentages
mtrt.absolute <- mtrt %>% 
  mutate(perc = n / dim(review)[1] * 100)




# Cost --------------------------------------------------------------------

cost <- review %>%
  filter(!is.na(cost)) %>% 
  count(cost)
cost.sum <- sum(cost["n"])
cost <- cost %>% 
  mutate(perc = n / cost.sum * 100)


# Plot size ---------------------------------------------------------------

plot.na <- review %>% 
  filter(!str_detect(plot, "not")) %>% 
  mutate(plot = round(as.numeric(plot), 2))
summary(plot.na$plot)
hist(plot.na$plot, breaks = 30)

# To better see first part of histogram
plot.majority <- plot.na %>% 
  filter(plot < 30)
hist(plot.majority$plot, breaks = 20)

Mode(plot.na$plot)


# Peer review -------------------------------------------------------------

peer <- review %>%
  count(peer)
peer.sum <- sum(peer["n"])
peer <- peer %>% 
  mutate(perc = n / peer.sum * 100)



# Difference between screen 2 and review ----------------------------------

yscrn2.nrev <- screen1 %>% 
  filter(review == "no", screen2 == "yes")

nscrn2.ynrev <- screen1 %>% 
  filter(review == "yes", screen2 == "no") # 15, incompatible experiments

save.image(".RData/systematic-review.RData")

