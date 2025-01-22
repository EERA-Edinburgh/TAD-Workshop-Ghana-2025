## ----message=FALSE,warning=FALSE-----------------------------------------------------------------------
library(tidyverse)
library(here)
library(epiDisplay)
library(epiR)


## ----echo=TRUE, eval = TRUE----------------------------------------------------------------------------
#Get the data back into RStudio with the code from before and clean it up if needed
dat <- read_csv(here("data", "cattle_data.csv"))


## ----message=FALSE,warning=FALSE-----------------------------------------------------------------------
#Have a look at the data
head(dat)
# Data cleaning (as in previous session)
dat <- dat %>%
      mutate(North = case_when(North == 0.060574 ~ 0.60574,
                           TRUE ~ North),
             East = case_when(East == 3.446714 ~ 34.46714,
                           TRUE ~ East)) %>%
      mutate(DAge = na_if(DAge, -888),
             DCalving = na_if(DCalving, -888),
             TDFarm = na_if(TDFarm, -888),
             CalfSex = case_when(as.factor(CalfSex) == 1 ~ "Male", 
                                as.factor(CalfSex) == 2 ~ "Female"))


## ----message=FALSE, warning=FALSE----------------------------------------------------------------------
cc(dat$CalfSex, dat$Lepto)

tab <- table(dat$Lepto, dat$CalfSex)
# exp_case, exp_noncase, nonexp_case, nonexp_noncase
x <- c(tab[2,2], tab[1,2], tab[2,1], tab[1,1]) 
# you will need to check this order for any given table to make sure it is the comparison you want

epi.2by2(x, method = "cohort.count")


## ----message=FALSE, warning=FALSE----------------------------------------------------------------------
tab <- dat %>%
  group_by(CalfSex) %>%
  summarise(
    Dis = sum(Lepto),
    Time = sum(DAge2)
  )
tab
x <- as.numeric(c(tab[2,2], tab[2,3], tab[1,2], tab[1,3]))
# you will need to check this order for any given table to make sure it is the comparison you want

epi.2by2(x, method="cohort.time")

#Or just put in the numbers manually. 
epi.2by2(c(8, 722, 4, 713), method="cohort.time")


## ------------------------------------------------------------------------------------------------------
epi.2by2(c(18,250,8,236), method="cohort.time")


## ------------------------------------------------------------------------------------------------------
tab <- table(dat$Lepto, dat$CalfSex)
# exp_case, exp_noncase, nonexp_case, nonexp_noncase
x <- c(tab[2,2], tab[1,2], tab[2,1], tab[1,1]) 

epi.2by2(x, method = "case.control")


## ------------------------------------------------------------------------------------------------------
epi.2by2(c(13,2163,5,3349), method="cross.sectional")

