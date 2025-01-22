
# Exercises

### Ex 3.1.

You set out and do a study of 90 cattle randomly selected from a herd of 320 and tested for antibodies to EBL.   The farmer is concerned that buying-in animals might be a risk factor for EBL in the herd. A study of the records showed that 19 of the study animals had been bought in, whereas 71 had been bred on the farm with EBL status as follows:
  
  ||**EBL+'ve**|**EBL-'ve**|
  |---|---|---|---|
  |**Bought in**|12|7| 
  |**Home bred**|35|36|
  
  a) what sort of study is this?
  b) estimate the association between the exposure (with 95\% CI)  and being EBL positive using the appropriate estimator showing your working (or code in R)
c) write a brief paragraph explaining the results

```{r, eval=FALSE}
# cross sectional study

epi.2by2(c(12,7,35,36), method = "cross.sectional")

# Prev risk ratio 1.28 (0.84 - 1.94)

# statistically not significant

```

### Ex 3.2.

A telephone questionnaire was used to gather information from owners of 38 cats diagnosed at the Small Animal Hospital with chronic renal failure (CRF) and from 56 cats visiting the clinic for annual vaccination boosters. Data were collected from all owners on how they fed their cats to determine whether diet was a risk factor for chronic renal failure.  The following data were recorded: 
  
  ||**CRF+'ve**|**CRF-'ve**| |
  |---|---|---|
  |**ad lib +'ve**|35|41| 
|**ad lib -'ve**|3|15|
  
  a) what sort of study is this?
  b) estimate the association between the exposure (with 95\% CI)  and having CRF using the appropriate estimator showing your working (or code in R)
c) write a brief paragraph explaining the results

```{r, eval=FALSE}
# case control study

epi.2by2(c(35, 41, 3, 15), method = "case.control")

# OR 4.27 (1.14 - 15.96)

# statistically significant
```