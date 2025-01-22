## ----results='hide', message=FALSE, warning=FALSE---------------------------------------------------------------------
library(tidyverse)
library(epiDisplay)
library(epiR)
library(epitools)
library(here)
library(survey)
library(skimr)
library(gt)
library(gtsummary)
library(janitor)
library(ggrepel)


## ----results='hide', message=FALSE, warning=FALSE---------------------------------------------------------------------
dat <- read_csv(here("data", "fmd_herd_training.csv"))


## ---------------------------------------------------------------------------------------------------------------------
summary(dat)


## ---------------------------------------------------------------------------------------------------------------------
dim(dat)


## ---------------------------------------------------------------------------------------------------------------------
head(dat)
tail(dat)


## ----results='hide'---------------------------------------------------------------------------------------------------
skim(dat)


## ----results='hide', message=FALSE, warning=FALSE---------------------------------------------------------------------
view(dat)


## ---------------------------------------------------------------------------------------------------------------------
ggplot(data=dat, aes(x=DDlongJitter, y=DDlatJitter) ) +
  geom_point()


## ----results='hide', message=FALSE, warning=FALSE, fig.keep='none'----------------------------------------------------
ggplot(data=dat, aes(x=DDlongJitter, y=DDlatJitter, color = disltyr)) +
  geom_point()


## ---------------------------------------------------------------------------------------------------------------------
dat %>%
  tabyl(Div) %>%
  gt() %>%
  fmt_number(columns = vars(percent), decimals = 3)


## ---------------------------------------------------------------------------------------------------------------------
dat %>%
  tabyl(Div, disltyr) %>%
  gt()


## ---------------------------------------------------------------------------------------------------------------------
dat <- dat %>% 
  dplyr::select(-c('...1')) %>%
  mutate(DDlatJitter = case_when(hcode == 118 & is.na(DDlatJitter) ~ 6.43197, TRUE ~ DDlatJitter),
         DDlongJitter = case_when(hcode == 118 & is.na(DDlongJitter) ~ 12.4228, TRUE ~ DDlongJitter),
         Ccode = case_when(hcode == 118 & is.na(Ccode) ~ "DAM", TRUE ~ Ccode),
         Div = case_when(hcode == 118 & is.na(Div) ~ "DJEREM", TRUE ~ Div),
         cluster = case_when(hcode == 118 & is.na(cluster) ~ 42, TRUE ~ cluster),
         vc.herds = case_when(hcode == 118 & is.na(vc.herds) ~ 203, TRUE ~ vc.herds),
         weight.h = case_when(hcode == 118 & is.na(weight.h) ~ 101.50000, TRUE ~ weight.h))


## ----results='hide', message=FALSE, warning=FALSE, fig.keep='none'----------------------------------------------------
ggplot(data=dat, aes(x=DDlongJitter, y=DDlatJitter)) +
  geom_point(color = "blue", size = 3) +
  geom_label_repel(aes(label = Ccode),
                  box.padding   = 0.35, 
                  point.padding = 0.5,
                  segment.color = 'grey50')


## ----results='hide', message=FALSE, warning=FALSE---------------------------------------------------------------------
dat %>%
  group_by(disltyr) %>%
  tally()



## ----results='hide', message=FALSE, warning=FALSE---------------------------------------------------------------------
ci(dat$disltyr)
ci.binomial(dat$disltyr)


## ---------------------------------------------------------------------------------------------------------------------
tab <- dat %>%
     group_by(disltyr) %>%
     tally()
ci.binomial(as.numeric(tab[2,2]),
            as.numeric(tab[2,2], tab[1,2]))
ci.binomial(87, 147)


## ---------------------------------------------------------------------------------------------------------------------
## think about why the second version might be safer.
# an even simpler way
binom.exact(1, 147)
binom.approx(1, 147)
ci.binomial(1, 147)


## ----results='hide', message=FALSE, warning=FALSE---------------------------------------------------------------------
dat <- read_csv(here("data", "cattle_data.csv"))
dim(dat)
class(dat)
head(dat, 14)
tail(dat, 12)
summary(dat)


## ---------------------------------------------------------------------------------------------------------------------
dat %>%
  group_by(Lepto) %>%
     tally()


## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------------------
time_at_risk <- sum(dat$DAge2)
time_at_risk
cases <- sum(dat$Lepto)
cases
cases / time_at_risk ## cases per year at risk
cases / time_at_risk * 1000 ## cases per 1000 cows per year at risk


## ----message=FALSE, warning=FALSE-------------------------------------------------------------------------------------
ci.poisson(cases, time_at_risk, alpha=0.05)

