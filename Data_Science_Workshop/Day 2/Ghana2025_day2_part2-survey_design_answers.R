### Exercises on complex design

1) Compare the estimate and CIs for the proportion of herdsmen reporting went on transhumance (trans1yr) assuming we have a SRS compared to the actual design which was a weighted stratified cluster design?
  
  
  ```{r}
survey_design_strata_cluster_weight %>%
  group_by(trans1yr) %>%
  summarize(fmd_prop = survey_prop(vartype = c("ci"))) %>%
  filter(trans1yr == 1)

survey_design_strata_cluster %>%
  group_by(trans1yr) %>%
  summarize(fmd_prop = survey_prop(vartype = c("ci"))) %>%
  filter(trans1yr == 1)
```

2) Set up a design object just with weighting and stratification and compare to the fully adjusted result from Ex1.

```{r}
survey_design_strata_weight <- dat %>%
  as_survey_design(ids = 1,
                   weights = weight.h,
                   strata = Div)

survey_design_strata_weight %>%
  group_by(trans1yr) %>%
  summarize(fmd_prop = survey_prop(vartype = c("ci"))) %>%
  filter(trans1yr == 1)

```

3) Plot the 3 sets of results as we did for FMD above.

```{r}
a <- survey_design_strata_cluster_weight %>%
  group_by(trans1yr) %>%
  summarize(fmd_prop = survey_prop(vartype = c("ci"))) %>%
  filter(trans1yr == 1) %>%
  mutate(type = "STR_CLUS_W")

b <- survey_design_strata_cluster %>%
  group_by(trans1yr) %>%
  summarize(fmd_prop = survey_prop(vartype = c("ci"))) %>%
  filter(trans1yr == 1)%>%
  mutate(type = "STR_CLUS")

c <- survey_design_strata_weight %>%
  group_by(trans1yr) %>%
  summarize(fmd_prop = survey_prop(vartype = c("ci"))) %>%
  filter(trans1yr == 1)%>%
  mutate(type = "STR_W")

df_all <- bind_rows(a, b, c)

ggplot(data=df_all, 
       aes(x=type, y=fmd_prop, ymin=fmd_prop_low, ymax=fmd_prop_upp)) +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Adjustment type", y = "Transhumance in last year proportion and 95% CI") +
  theme_bw(base_size = 18)

```
