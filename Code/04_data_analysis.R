
### load combined dataset

load("./Data/Working_data/combined_dataset.rda")


# Correlation matrix

corr

# Models

combined_dataset_fulltime <- combined_dataset %>% filter(job_type == 2)
  
## Stress outcome

model_stress <- ' 
                  # partial measurement model JDR

                  #          emotional_demands =~ Q30g + Q30h + Q61o
                  #          quantitative_demands =~ Q49a + Q49b + Q61g 
                  #          work_resources =~ Q54a + Q54b + Q54c + Q61f           
                  #          participation_resources =~ Q61c + Q61d + Q61n 
                  #          coleague_resources =~ Q61a + Q70e + Q89d
                  #          supervisor_resources =~ Q61b + Q63a + Q63b + Q63c + Q63d + Q63e + Q63f
                  #          demands =~ emotional_demands + quantitative_demands
                  #          resources =~ work_resources + participation_resources + coleague_resources + supervisor_resources

                   # direct effect - regression
                            
                   # regression
                   
                        # practice on stress
                            
                            experience_stress ~ d1*practice_stress
                   
                        # JDR on outcome
                            experience_stress ~ i2*mean_demands
                            experience_stress ~ i3*mean_resources
                        
                        # organisational practices on JDR
                            mean_demands ~ i4*practice_stress
                            mean_resources ~ i5*practice_stress
                        
                        # Law on organisational practices
                            practice_stress ~ i6*law_stress1
                     
                     # indirect effect
                            ind_demands := i6*i2*i4
                            ind_resources := i6*i3*i5
                        
                     # total effect
                            tot_demands := d1 + (i6*i2*i4)
                            tot_resources := d1 + (i6*i3*i5)  '


stress_path <- sem(model_stress, data = combined_dataset_fulltime, estimator = "MLR", group = NULL, bootstrap = 1000, test = "boot")
summary(stress_path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
standardizedsolution(stress_path)

get_layout(stress_path)

lay_stress <- get_layout("", "","mean_demands", "", 
                  "law_stress1", "practice_stress","", "experience_stress", 
                  "", "" ,"mean_resources", "",
                  rows = 3)

graph_sem(model = stress_path, edges = get_edges(x = model), nodes =  get_nodes(x = model), layout = lay_stress)


##### bias corrected bootstrap
lavaan::parameterEstimates(stress_path, zstat = TRUE, pvalue = TRUE, ci = TRUE, level = 0.95, 
                   boot.ci.type = "bca.simple", standardized = FALSE, remove.system.eq = TRUE, 
                   remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE, rsquare = FALSE, add.attributes = FALSE)




## Harassment outcome

model_harassment <- ' 
                  # partial measurement model JDR

                  #          emotional_demands =~ Q30g + Q30h + Q61o
                  #          quantitative_demands =~ Q49a + Q49b + Q61g 
                  #          work_resources =~ Q54a + Q54b + Q54c + Q61f           
                  #          participation_resources =~ Q61c + Q61d + Q61n 
                  #          coleague_resources =~ Q61a + Q70e + Q89d
                  #          supervisor_resources =~ Q61b + Q63a + Q63b + Q63c + Q63d + Q63e + Q63f
                  #          demands =~ emotional_demands + quantitative_demands
                  #          resources =~ work_resources + participation_resources + coleague_resources + supervisor_resources

                   # direct effect - regression

                        # organisational practices on outcome
                            experience_harassment ~ d1*practice_harassment

                   # mediation - regression
                   
                        # JDR on outcome
                            experience_harassment ~ i1*mean_demands
                            experience_harassment ~ i2*mean_resources
                        
                        # organisational practices on JDR
                            mean_demands ~ i3*practice_harassment
                            mean_resources ~ i4*practice_harassment
                        
                        # Law on organisational practices
                            practice_harassment ~ i5*law_harassment1
                     
                     # indirect effect
                            Ind_demands := i1*i3*i5
                            Ind_resources := i2*i4*i5
                        
                     # total effect
                            tot_demands := d1 + (i1*i3*i5)
                            tot_resources := d1 + (i2*i4*i5)  '


harassment_path <- sem(model_harassment, data = combined_dataset_fulltime, estimator = "MLR", group = NULL, bootstrap = 1000, test = "boot")
summary(harassment_path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
standardizedsolution(harassment_path)

lay_harassment <- get_layout("", "","mean_demands", "", 
                         "law_harassment1", "practice_harassment","", "experience_harassment", 
                         "", "" ,"mean_resources", "",
                         rows = 3)

graph_sem(model = harassment_path, edges = get_edges(x = model), nodes =  get_nodes(x = model), layout = lay_harassment)


##### bias corrected bootstrap
lavaan::parameterEstimates(harassment_path, zstat = TRUE, pvalue = TRUE, ci = TRUE, level = 0.95, 
                           boot.ci.type = "bca.simple", standardized = FALSE, remove.system.eq = TRUE, 
                           remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE, rsquare = FALSE, add.attributes = FALSE)


## Violence outcome

model_violence <- ' 
                  # partial measurement model JDR

                  #          emotional_demands =~ Q30g + Q30h + Q61o
                  #          quantitative_demands =~ Q49a + Q49b + Q61g 
                  #          work_resources =~ Q54a + Q54b + Q54c + Q61f           
                  #          participation_resources =~ Q61c + Q61d + Q61n 
                  #          coleague_resources =~ Q61a + Q70e + Q89d
                  #          supervisor_resources =~ Q61b + Q63a + Q63b + Q63c + Q63d + Q63e + Q63f
                  #          demands =~ emotional_demands + quantitative_demands
                  #          resources =~ work_resources + participation_resources + coleague_resources + supervisor_resources

                   # direct effect - regression

                        # organisational practices on outcome
                            experience_violence ~ d1*practice_violence

                   # mediation - regression
                   
                        # JDR on outcome
                            experience_violence ~ i1*mean_demands
                            experience_violence ~ i2*mean_resources
                        
                        # organisational practices on JDR
                            mean_demands ~ i3*practice_violence
                            mean_resources ~ i4*practice_violence
                        
                        # Law on organisational practices
                            practice_violence ~ i5*law_violence1
                     
                     # indirect effect
                            Ind_demands := i1*i3*i5
                            Ind_resources := i2*i4*i5
                        
                     # total effect
                            tot_demands := d1 + (i1*i3*i5)
                            tot_resources := d1 + (i2*i4*i5)  '


violence_path <- sem(model_violence, data = combined_dataset_fulltime, estimator = "MLR", group = NULL, bootstrap = 1000, test = "boot")
summary(violence_path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
standardizedsolution(violence_path)

lay_violence <- get_layout("", "","mean_demands", "", 
                             "law_violence1", "practice_violence","", "experience_violence", 
                             "", "" ,"mean_resources", "",
                             rows = 3)

graph_sem(model = violence_path, layout = lay_violence)


##### bias corrected bootstrap
lavaan::parameterEstimates(violence_path, zstat = TRUE, pvalue = TRUE, ci = TRUE, level = 0.95, 
                           boot.ci.type = "bca.simple", standardized = FALSE, remove.system.eq = TRUE, 
                           remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE, rsquare = FALSE, add.attributes = FALSE)


