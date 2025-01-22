## ----setup, include=FALSE------------------------------
knitr::opts_chunk$set(message=FALSE, warning=FALSE)


## ------------------------------------------------------
#load packages
library(tidyverse)
library(survey)
library(srvyr)
library(knitr)
library(here)
library(ggrepel)


## ------------------------------------------------------
x <- c(100, 77, 56, 84, 68, 63, 83, 81, 93, 78)
mean(x)
sd(x)
# manually calculate the 95% CI using normal approximation
mean(x) - 1.96*sd(x)/sqrt(length(x))
mean(x) + 1.96*sd(x)/sqrt(length(x))

# manually calculate the 95% CI using t distribution for small sample sizes
t.test(x)
mean(x) - 2.262*sd(x)/sqrt(length(x))
mean(x) + 2.262*sd(x)/sqrt(length(x))


## ------------------------------------------------------
rbinom( 1 , 10, 0.1)
rbinom( 1 , 10, 0.1)
rbinom( 1 , 10, 0.1)/10*100
rbinom( 1 , 10, 0.1)/10*100


## ----warning=FALSE, eval = FALSE-----------------------
# set.seed(35)
# num_pos <- rbinom(100 , 30, 0.1)
# estimate <- num_pos/30*100
# n <- rep(30,100)
# order <- 1:100
# bin <- as.data.frame(cbind(order, num_pos, estimate, n))
# for (i in 1:100){
# bin$lower[i] <- binom.exact(num_pos[i], n[i])$conf.int[1]*100
# bin$upper[i] <- binom.exact(num_pos[i], n[i])$conf.int[2]*100
# }
# threshold <- as.numeric(lower>10)
# 
# ggplot(data=bin, aes(y=estimate, x= as.factor(order))) +
#   geom_point() +
#   geom_errorbar(data = bin, aes(ymin = lower, ymax=upper, colour = as.factor(threshold))) +
#   scale_color_manual(values = c("black", "red")) +
#   ylab("Estimated Prevalence") +
#   theme_bw() +
#   theme(axis.title.y=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   coord_flip() +
#   guides(color = guide_legend(title = "CI that do not\n include true\n prevalence"))


## ------------------------------------------------------
# import data
dat <- read_csv(here("data", "fmd_herd_training.csv"))


## ------------------------------------------------------
dat <- dat %>% 
  dplyr::select(-c('...1')) %>%
  mutate(DDlatJitter = case_when(hcode == 118 & is.na(DDlatJitter) ~ 6.43197, TRUE ~ DDlatJitter),
         DDlongJitter = case_when(hcode == 118 & is.na(DDlongJitter) ~ 12.4228, TRUE ~ DDlongJitter),
         Ccode = case_when(hcode == 118 & is.na(Ccode) ~ "DAM", TRUE ~ Ccode),
         Div = case_when(hcode == 118 & is.na(Div) ~ "DJEREM", TRUE ~ Div),
         cluster = case_when(hcode == 118 & is.na(cluster) ~ 42, TRUE ~ cluster),
         vc.herds = case_when(hcode == 118 & is.na(vc.herds) ~ 203, TRUE ~ vc.herds),
         weight.h = case_when(hcode == 118 & is.na(weight.h) ~ 101.50000, TRUE ~ weight.h))


## ------------------------------------------------------
adm <- sf::read_sf(here("data", "adamawa.shp"))

 ggplot(data = adm) +
  geom_sf() +
  geom_point(data=dat, aes(x=DDlongJitter, y=DDlatJitter), color = "blue", size = 3) +
  geom_label_repel(data=dat, aes(x=DDlongJitter, y=DDlatJitter, label = Ccode),
                  box.padding   = 0.35, 
                  point.padding = 0.5,
                  segment.color = 'grey50') +
   theme_bw()


## ------------------------------------------------------
dat <- dat %>%
  mutate(weight = vc.herds/entries)


## ----eval=FALSE----------------------------------------
# survey_design <- dat %>%
#   as_survey_design(ids = Ccode,
#                    # cluster id
#                    weights = weight.h,
#                    # weights
#                    strata = Div
#                    # strata id
#                    )


## ------------------------------------------------------
survey_design_simple <- dat %>% 
  as_survey_design(ids = 1, # 1 for no cluster ids 
                   weights = NULL, # No weight added
                   strata = NULL # sampling was simple 
                   #(no strata)
                  )


## ------------------------------------------------------
prop_srs <- survey_design_simple %>%
  group_by(disltyr) %>%
  summarize(fmd_prop = survey_prop(vartype = c("ci"))) %>%
  filter(disltyr == 1) %>%
  mutate(type = "SRS")

prop_srs %>% kable()


## ------------------------------------------------------
survey_design_strata <- dat %>%
  as_survey_design(ids = 1, 
                   weights = NULL,
                   strata = Div
                  )


## ------------------------------------------------------
prop_strata <- survey_design_strata %>%
  group_by(disltyr) %>%
  summarize(fmd_prop = survey_prop(vartype = c("ci"))) %>%
  filter(disltyr == 1) %>%
  mutate(type = "STR")

prop_strata %>% kable()


## ------------------------------------------------------
survey_design_cluster <- dat %>%
  as_survey_design(ids = Ccode, 
                   weights = NULL, 
                   strata = NULL 
                  )


## ------------------------------------------------------
prop_clus <- survey_design_cluster %>%
  group_by(disltyr) %>%
  summarize(fmd_prop = survey_prop(vartype = c("ci"))) %>%
  filter(disltyr == 1) %>%
  mutate(type = "CLUS")

prop_clus %>% kable()


## ------------------------------------------------------
survey_design_strata_cluster <- dat %>%
  as_survey_design(ids = Ccode,
                   weights = NULL,
                   strata = Div) 


## ------------------------------------------------------
prop_str_clus <- survey_design_strata_cluster %>%
  group_by(disltyr) %>%
  summarize(fmd_prop = survey_prop(vartype = c("ci"))) %>%
  filter(disltyr == 1) %>%
  mutate(type = "STR_CLUS")

prop_str_clus %>% kable()


## ------------------------------------------------------
survey_design_strata_cluster_weight <- dat %>%
  as_survey_design(ids = Ccode,
                   weights = weight.h,
                   strata = Div) 


## ------------------------------------------------------
prop_str_clus_weight <- survey_design_strata_cluster_weight %>%
  group_by(disltyr) %>%
  summarize(fmd_prop = survey_prop(vartype = c("ci"))) %>%
  filter(disltyr == 1) %>%
  mutate(type = "STR_CLUS_W")

prop_str_clus_weight %>% kable()


## ------------------------------------------------------
df_all <- bind_rows(
  prop_srs,
  prop_strata,
  prop_clus,
  prop_str_clus,
  prop_str_clus_weight
)

df_all %>% kable(digits = 3)


## ----out.width = "200px", fig.align='center'-----------
ggplot(data=df_all, 
       aes(x=type, y=fmd_prop, ymin=fmd_prop_low, ymax=fmd_prop_upp)) +
        geom_pointrange() +
  coord_flip() +
  labs(x = "Adjustment type", y = "FMD proportion and 95% CI") +
  theme_bw(base_size = 18)

