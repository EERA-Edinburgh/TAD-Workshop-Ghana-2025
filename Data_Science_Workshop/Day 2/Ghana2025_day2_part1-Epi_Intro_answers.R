#MScStats2019-sess2-Epiintro Answers

##Exercises
###Ex1. 
library(tidyverse)
library(epiR)
library(epiDisplay)
library(here)


dat <- read.csv(here("data", "cattle_data.csv"))
map(dat, class)
str(dat)

summary(dat)

table(dat$SL, useNA = "always")
table(dat$CalfSex, useNA = "always")

summary(dat$GirthDam)
#install.packages("skimr")
library(skimr)
skim(dat)

###Ex2. 
dat <- dat %>%
  mutate(DCalving = na_if(DCalving, -888),
         GirthDam = na_if(GirthDam, -888),
         TDFarm = na_if(TDFarm, -888))

###Ex3. 
ci.binomial(dat$BrucellaLFA, conf.level = 0.95)
ci.binomial(dat$BrucellaRBG, conf.level = 0.95)

ci(dat$BrucellaLFA, conf.level = 0.95)
ci(dat$BrucellaRBG, conf.level = 0.95)

#### alternatively use binom.exact

table(dat$BrucellaLFA, dat$BrucellaRBG, useNA = "always", dnn=c("LFA","RBG"))

with(dat, table(BrucellaLFA, BrucellaRBG))


###Ex4. 
# epi_inc(dat, BrucellaRBG == 1, DAge2)
# install.packages("remotes")
# remotes::install_github("ianhandel/epidemr")

time_at_risk <- sum(dat$DAge2)
time_at_risk
cases <- sum(dat$BrucellaRBG)
cases
cases / time_at_risk ## cases per year at risk
cases / time_at_risk * 1000 ## cases per 1000 cows per year at risk

ci.poisson(cases, time_at_risk, alpha=0.05)

         
