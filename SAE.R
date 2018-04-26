rm(list = ls())
set.seed(2017)
library(TeachingSampling)
library(dplyr)
data("BigLucy")

summary(BigLucy$Level)
levels(BigLucy$Zone)

Total <- BigLucy %>%
  group_by(Zone) %>%
  summarise(Income. = sum(Income)) %>%
  arrange(Zone)

N <- BigLucy %>%
  group_by(Zone) %>%
  summarise(N.county = n()) %>%
  arrange(Zone)

Results <- data.frame(N, Total$Income.)

# Level is the stratifying variable
summary(BigLucy$Level)
attach(BigLucy)
# Defines the size of each stratum
N1 <- summary(Level)[[1]]
N2 <- summary(Level)[[2]]
N3 <- summary(Level)[[3]]
N1;N2;N3
Nh <- c(N1,N2,N3)
# Defines the sample size at each stratum

n1 <- round(N1 * 0.05)
n2 <- round(N2 * 0.05)
n3 <- round(N3 * 0.05)
(nh<-c(n1,n2,n3))
# Draws a stratified sample
sam <- S.STSI(Level, Nh, nh)

data.sam <- BigLucy[sam,]