
##### Data reduction and merging datasets

# CFA

options(digits = 2)

# load clean datasets

load("./Data/Working_data/ESENER14_items.rda")
load("./Data/Working_data/policy_index.rda") 
load("./Data/Working_data/EWCS2015_items.rda")


## CFA - Organisational practices from ESENER 2014 ################## we do not need this as we are using the items

l_ESENER14_items <- get_labels(ESENER14_items, values = "n") # value labels 

### Model 

m_op <- ' organisational_practices  =~ practice_stress + practice_harassment + practice_violence ' 

fit_op <- lavaan::cfa(m_op, data = ESENER14_items, 
                      estimator = "WLSM",  mimic = "Mplus",
                      ordered = c("practice_stress", "practice_harassment", "practice_violence"))

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

## calculating country level index for each organisational practice in the ESENER

practices_index <- ESENER14_items %>% 
  group_by(country) %>%
  summarise(practice_stress = mean(practice_stress, na.rm = TRUE), 
            practice_harassment = mean(practice_harassment, na.rm = TRUE), 
            practice_violence = mean(practice_violence, na.rm = TRUE))

# linking policy and practices datasets

indexes <- left_join(policy_index, practices_index, by = "country")


#########################################################################################

#### JDR MODEL AND OUTCOMES

## filtering for those in employment who are employees 

EWCS2015_items <- EWCS2015_items %>% filter(Q2c == 1 & Q7 == 1)

l_EWCS2015_items <- get_labels(EWCS2015_items, values = "n") # value labels 

## not used # EWCS2015_items <- EWCS2015_items %>% filter(as_numeric(Q24) >= "20") # filtering for those working at least 20 hours per week


#### JDR model for CFA

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

#### export to excel results of CFA

write.xlsx(summary_jdr$PE,"./Results/jdrfactors_loadings.xlsx")
write.xlsx(fitm_jdr,"./Results/jdrfactors_modelfit.xlsx")
write.xlsx(rel_jdr,"./Results/jdrfactors_reliability.xlsx")


### composite index (mean per person)

EWCS2015_items <- EWCS2015_items %>% 
  mutate(mean_emotional_demands = (Q30g + Q30h + Q61o) / 3,
         mean_quantitative_demands = (Q49a + Q49b + Q61g) / 3,
         mean_work_resources = (Q54a + Q54b + Q54c + Q61f) / 4,
         mean_participation_resources = (Q61c + Q61d + Q61n) / 3,
         mean_coleague_resources = (Q61a + Q70e + Q89d) / 3,
         mean_supervisor_resources = (Q61b + Q63a + Q63b + Q63c + Q63d + Q63e + Q63f) / 7,
         mean_demands = (mean_emotional_demands + mean_quantitative_demands) / 2,
         mean_resources = (mean_work_resources + mean_participation_resources + mean_coleague_resources + mean_supervisor_resources) / 4)

JDR_index <- EWCS2015_items %>% select(Country, nace_rev2_1, isco_08_1, Q2a, Q2b, Q2d, Q24,
                                       Q30g, Q30h, Q61o,
                                       Q49a, Q49b, Q61g, 
                                       Q54a, Q54b, Q54c, Q61f,           
                                       Q61c, Q61d, Q61n, 
                                       Q61a, Q70e, Q89d,
                                       Q61b, Q63a, Q63b, Q63c, Q63d, Q63e, Q63f,
                                       mean_emotional_demands, mean_quantitative_demands, mean_work_resources, mean_participation_resources,
                                       mean_coleague_resources, mean_supervisor_resources, mean_demands, mean_resources,
                                       experience_stress, experience_harassment, experience_violence) 

# linking JDR wit the country and company indexes

JDR_index  <- JDR_index %>% rename(country = Country, sex = Q2a, age = Q2b, occupation = isco_08_1, industry = nace_rev2_1, job_type = Q2d, hours = Q24) 

combined_dataset <- left_join(indexes, JDR_index, by = "country") # this is the dataset for data analysis

save(combined_dataset, file = "./Data/Working_data/combined_dataset.rda") # save database in r format
write.xlsx(combined_dataset,"./Data/Working_data/combined_dataset.xlsx") # save database in excel format

# end data reduction

rm(list = ls()) # to save memory
