
# Data reduction with CFA

options(digits = 2)

## CFA - Organisational practices from ESENER 2014

load("./Data/Working_data/ESENER14_items.rda") # this is the file for analysis

l_ESENER14_items <- get_labels(ESENER14_items, values = "n") # value labels 

ESENER14_items <- as_factor(ESENER14_items)

### Model 

m_op <- ' organisational_practices  =~ Q300 + Q301 + Q302 ' 

fit_op <- lavaan::cfa(m_op, data = ESENER14_items, 
                      estimator = "WLSM",  mimic = "Mplus",
                      ordered = c("Q300", "Q301", "Q302"))

### CFA model

summary_op <- summary(fit_op, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)
fitm_op <- fitMeasures(fit_op)
standardizedSolution(fit_op, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE)
resid(fit_op, "cor") # covariance of the residuals
fitted(fit_op) # covariance matrix
semPaths(fit_op, "model", "std", intercepts = FALSE) # Plots

rel_op <- semTools::reliability(fit_op) ### reliability 

#### export results to excel

write.xlsx(summary_op$PE,"./Results/org_practices_loadings.xlsx")
write.xlsx(fitm_op,"./Results/org_practices_modelfit.xlsx")
write.xlsx(rel_op,"./Results/org_practices_reliability.xlsx")

rm(list = ls())


#### JDR MODEL AND OUTCOMES

load("./Data/Working_data/EWCS2015_items.rda") # this is the file for analysis

l_EWCS2015_items <- get_labels(EWCS2015_items, values = "n") # value labels 

EWCS2015_items <- as_factor(EWCS2015_items)

### Filtering for those in employment and working at least 20 hours

EWCS2015_items <- EWCS2015_items %>% filter(Q2c == "1" & Q7 == "1") # filtering for employees

EWCS2015_items <- EWCS2015_items %>% filter(as_numeric(Q24) >= "20" & as_numeric(Q24) < "888") # filtering for those working at least 20 hours per week


#### JDR model

##### step 1) CFA JDR full model including all subfactors
##### step 2) removed: skill_r =~ Q53c + Q53e + Q53f + Q61i
##### step 3) removed: quantitative_d =~ Q51 
##### step 4) removed: pace_d =~ Q50a + Q50c + Q50d + Q50e

m_jdr <- 'emotional_demands =~ Q30g + Q30h + Q61o
          quantitative_demands =~ Q49a + Q49b + Q61g
          work_resources =~ Q54a + Q54b + Q54c + Q61f           
          participation_resources =~ Q61c + Q61d + Q61n 
          coleague_resources =~ Q61a + Q70e + Q89d
          supervisor_resources =~ Q61b + Q63a + Q63b + Q63c + Q63d + Q63e + Q63f'

fit_jdr <- lavaan::cfa(m_jdr, data = EWCS2015_items, 
                      estimator = "WLSM", mimic = "Mplus",
                      ordered = c("Q30g", "Q30h", "Q61o", "Q49a", "Q49b", "Q61g", "Q51", "Q61a", "Q70e", "Q89d",
                                  "Q61b", "Q63a", "Q63b", "Q63c", "Q63d", "Q63e", "Q63f", "Q54a", "Q54b", "Q54c", "Q61f", "Q53c",
                                  "Q53e", "Q53f", "Q61i", "Q61c", "Q61d", "Q61n",
                                  "Q50a", "Q50b", "Q50c", "Q50d", "Q50e"))

summary_jdr <- summary(fit_jdr, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)
fitm_jdr <- fitMeasures(fit_jdr)
standardizedSolution(fit_jdr, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE)
resid(fit_jdr, "cor") # covariance of the residuals
fitted(fit_jdr) # implied moments
semPaths(fit_jdr, "model", "std", intercepts = FALSE) # Plots
rel_jdr <- semTools::reliability(fit_jdr)

#### export to excel

write.xlsx(summary_jdr$PE,"./Results/jdrfactors_loadings.xlsx")
write.xlsx(fitm_jdr,"./Results/jdrfactors_modelfit.xlsx")
write.xlsx(rel_jdr,"./Results/jdrfactors_reliability.xlsx")

rm(list = ls())
