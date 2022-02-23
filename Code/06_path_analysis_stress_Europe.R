

# Load database

load("./Data/Working_data/combined_stress .rda") 

l_combined_stress <- get_labels(combined_stress, values = "n") # value labels

combined_stress %>% nrow()

# Summary 

summary_combined1 <- combined_stress %>% 
  group_by(as_label(law_stress_yes), as_label(size)) %>% 
  summarise(mean_index = mean(na.omit(csize_index))) %>% 
  rename(law = `as_label(law_stress_yes)`, size = `as_label(size)`, ) %>% 
  pivot_wider(names_from = size, values_from = mean_index) %>%
  group_by(law) %>%
  mutate(mean_law = (`10-249` + `250+`)/ 2)

summary_combined2 <- combined_stress %>% 
  filter(!is.na(sex)) %>% 
  group_by(as_label(law_stress_yes), as_label(sex)) %>% 
  summarise(mean_index = mean(na.omit(csize_index))) %>% 
  rename(law = `as_label(law_stress_yes)`, sex = `as_label(sex)`, ) %>% 
  pivot_wider(names_from = sex, values_from = mean_index) %>%
  group_by(law) %>%
  mutate(mean_law = (`Male` + `Female`)/ 2)


# Correlation matrix

options(digits = 3)

stress_corr <- combined_stress %>% select(law_stress_yes, csize_index, experience_stress,
                                         avg_demands, avg_resources) %>% 
  as.matrix(.) %>%
  Hmisc::rcorr(., type = c("pearson"))

knitr::kable(stress_corr$r) %>% kable_styling() # value
knitr::kable(stress_corr$P) %>% kable_styling() # significance


# descriptives

options(digits = 3)

combined_stress %>% summarise(mean = mean(na.omit(c(csize_index))), sd = sd(na.omit(csize_index)))
combined_stress %>% summarise(mean = mean(na.omit(experience_stress)), sd = sd(na.omit(experience_stress)))
combined_stress %>% summarise(mean = mean(na.omit(avg_demands)), sd = sd(na.omit(avg_demands)))
combined_stress %>% summarise(mean = mean(na.omit(avg_resources)), sd = sd(na.omit(avg_resources)))


# outcome data assessments for linearity and normality

## outcome variable assessment 

boxplot(combined_stress$experience_stress) # outliers in the outcome variables
ggplot(combined_stress, aes(x=experience_stress)) + geom_density() # distribution of the the outcome variables
ggplot(combined_stress, aes(sample= scale(experience_stress))) + geom_qq() + geom_abline() # linearity of the the outcome variables

## Skewness 
#### If skewness value:  > +1 or < -1 = highly skewed 
####                    Between +0.5 to -0.5 = moderately skewed
####                    0 = data is symmetric

skewness(na.omit(combined_stress$experience_stress)) # moderately skewed
ggplot(combined_stress, aes(x=experience_stress)) + geom_histogram() # distribution

skewness(na.omit(combined_stress$csize_index)) # moderately skewed
ggplot(combined_stress, aes(x=csize_index)) + geom_histogram() # distribution

skewness(na.omit(combined_stress$avg_demands)) # moderately skewed
ggplot(combined_stress, aes(x=avg_demands)) + geom_histogram() # distribution

skewness(na.omit(combined_stress$avg_resources)) # moderately skewed (negative)
ggplot(combined_stress, aes(x=avg_resources)) + geom_histogram() # distribution

## regression assumptions 

linear_index <- gvlma.form(experience_stress ~ csize_index, combined_stress) # linear assumptions
linear_demands <- gvlma.form(experience_stress ~ avg_demands, combined_stress) # linear assumptions
linear_resources <- gvlma.form(experience_stress ~ avg_resources, combined_stress) # linear assumptions
linear_cdemands <- gvlma.form(avg_demands ~ csize_index, combined_stress) # linear assumptions
linear_cresources <- gvlma.form(avg_resources ~ csize_index, combined_stress) # linear assumptions

plot.gvlma(linear_index)
plot.gvlma(linear_demands)
plot.gvlma(linear_resources)
plot.gvlma(linear_cdemands)
plot.gvlma(linear_cresources)
plot.gvlma(linear_law)



# Models

## Model 1 mediation: using the resource and demand averages

model_stress_1 <- '
                   # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ csize_index

                        # JDR on outcome
                            experience_stress ~ avg_demands
                            experience_stress ~ avg_resources
                        
                        # organisational practices on JDR
                            avg_demands ~ csize_index
                            avg_resources ~ csize_index
                        
                        # Law on organisational practices
                            csize_index ~ law_stress_yes

                       # covariances 
                           avg_demands ~~ avg_resources
                                                  '

stress_path_1 <- sem(model_stress_1, data = combined_stress, estimator = "MLR", 
                  model.type = "sem",
                   group = NULL, 
                   se="robust",
                   test = "bootstrap",
                   std.ov = TRUE,
                   missing = "listwise",
                   bootstrap = 1000) ##### bias corrected bootstrap

summary(stress_path_1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)
standardizedsolution(stress_path_1)


### Model 2: multi group modelling

model_stress_2  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ csize_index

                        # JDR on outcome
                            experience_stress ~ avg_demands
                            experience_stress ~ avg_resources
                        
                        # organisational practices on JDR
                            avg_demands ~ csize_index
                            avg_resources ~ csize_index

                    # covariances 
                            avg_demands ~~ avg_resources '

multigroup <- sem(model_stress_2, data = combined_stress, estimator = "MLR",
                   group = "law_stress_yes",
                   group.label = NULL,
                   se = "robust",
                   test = "bootstrap",
                   bootstrap = 1000) # no law

summary(multigroup, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)
standardizedsolution(multigroup)


## Free vs constrained model - overall

free__model <- sem(model_stress_2, data = combined_stress, estimator = "MLR",
                   group = "law_stress_yes",
                   group.label = NULL,
                   se = "robust",
                   test = "bootstrap",
                   bootstrap = 1000) # bias corrected bootstrap

summary(free__model, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)

constrained__model <- sem(model_stress_2, data = combined_stress, estimator = "MLR",
                          group = "law_stress_yes", 
                          group.label = NULL,
                          se = "robust",
                          test = "bootstrap",
                          bootstrap = 1000, # bias corrected bootstrap
                          group.equal = c("intercepts", "regressions"))

summary(constrained__model, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)

lavTestLRT(free__model, constrained__model) # scaled chi-squared difference test


## Free vs constrained model - detailed

#### index to stress - not significant 

model_index_stress  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ c("b1", "b1") * csize_index
                            
                        # JDR on outcome
                            
                            experience_stress ~ avg_demands
                            experience_stress ~ avg_resources
                        
                        # organisational practices on JDR
                        
                            avg_demands ~ csize_index
                            avg_resources ~ csize_index

                    # covariances 
                    
                            avg_demands ~~ avg_resources '


index_stress__path <- sem(model_index_stress, data = combined_stress, estimator = "MLR",
                           group = "law_stress_yes",
                           group.label = NULL,
                           se = "robust",
                           test = "bootstrap",
                           bootstrap = 1000) # bias corrected bootstrap

summary(index_stress__path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)

lavTestLRT(free__model, index_stress__path) # scaled chi-squared difference test


### demands path

#### index to demands - no significant

model_index_demands  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ csize_index
                            
                        # JDR on outcome
                            
                            experience_stress ~ avg_demands
                            experience_stress ~ avg_resources
                        
                        # organisational practices on JDR
                        
                            avg_demands ~ c("b1", "b1") * csize_index
                            avg_resources ~ csize_index

                    # covariances 
                    
                            avg_demands ~~ avg_resources '
       

index_demands__path <- sem(model_index_demands, data = combined_stress, estimator = "MLR",
                   group = "law_stress_yes",
                   group.label = NULL,
                   se = "robust",
                   test = "bootstrap",
                   bootstrap = 1000) # bias corrected bootstrap

summary(index_demands__path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)

lavTestLRT(free__model, index_demands__path) # scaled chi-squared difference test


#### demands to stress - no significant

model_demands_stress  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ csize_index
                            
                        # JDR on outcome
                            
                            experience_stress ~ c("b1", "b1") * avg_demands
                            experience_stress ~ avg_resources
                        
                        # organisational practices on JDR
                        
                            avg_demands ~ csize_index
                            avg_resources ~ csize_index

                    # covariances 
                    
                            avg_demands ~~ avg_resources '


demands_stress__path <- sem(model_demands_stress, data = combined_stress, estimator = "MLR",
                           group = "law_stress_yes",
                           group.label = NULL,
                           se = "robust",
                           test = "bootstrap",
                           bootstrap = 1000) # bias corrected bootstrap

summary(demands_stress__path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)

lavTestLRT(free__model, demands_stress__path) # scaled chi-squared difference test


#### full demands path - No significant

model_full_demands  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ csize_index
                            
                        # JDR on outcome
                            
                            experience_stress ~ c("b1", "b1") * avg_demands
                            experience_stress ~ avg_resources
                        
                        # organisational practices on JDR
                        
                            avg_demands ~ c("b2", "b2") *csize_index
                            avg_resources ~ csize_index

                    # covariances 
                    
                            avg_demands ~~ avg_resources '


full_demands_path <- sem(model_full_demands, data = combined_stress, estimator = "MLR",
                            group = "law_stress_yes",
                            group.label = NULL,
                            se = "robust",
                            test = "bootstrap",
                            bootstrap = 1000) # bias corrected bootstrap

summary(full_demands_path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)

lavTestLRT(free__model, full_demands_path) # scaled chi-squared difference test


### resources path

#### index to resources - Significant 

model_index_resources  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ csize_index
                            
                        # JDR on outcome
                            
                            experience_stress ~ avg_demands
                            experience_stress ~ avg_resources
                        
                        # organisational practices on JDR
                        
                            avg_demands ~ csize_index
                            avg_resources ~ c("b1", "b1") * csize_index

                    # covariances 
                    
                            avg_demands ~~ avg_resources '


index_resources__path <- sem(model_index_resources, data = combined_stress, estimator = "MLR",
                           group = "law_stress_yes",
                           group.label = NULL,
                           se = "robust",
                           test = "bootstrap",
                           bootstrap = 1000) # bias corrected bootstrap

summary(index_resources__path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)

lavTestLRT(free__model, index_resources__path) # scaled chi-squared difference test



#### resources to stress - No significant 

model_resources_stress  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ csize_index
                            
                        # JDR on outcome
                            
                            experience_stress ~ avg_demands
                            experience_stress ~ c("b1", "b1") * avg_resources
                        
                        # organisational practices on JDR
                        
                            avg_demands ~ csize_index
                            avg_resources ~ csize_index

                    # covariances 
                    
                            avg_demands ~~ avg_resources '


resources_stress__path <- sem(model_resources_stress, data = combined_stress, estimator = "MLR",
                            group = "law_stress_yes",
                            group.label = NULL,
                            se = "robust",
                            test = "bootstrap",
                            bootstrap = 1000) # bias corrected bootstrap

summary(resources_stress__path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)

lavTestLRT(free__model, resources_stress__path) # scaled chi-squared difference test


#### full demands path - Significant 

model_full_resources  <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ csize_index
                            
                        # JDR on outcome
                            
                            experience_stress ~ avg_demands
                            experience_stress ~ c("b1", "b1") * avg_resources
                        
                        # organisational practices on JDR
                        
                            avg_demands ~ csize_index
                            avg_resources ~ c("b2", "b2") * csize_index

                    # covariances 
                    
                            avg_demands ~~ avg_resources '


full_resources_path <- sem(model_full_resources, data = combined_stress, estimator = "MLR",
                          group = "law_stress_yes",
                          group.label = NULL,
                          se = "robust",
                          test = "bootstrap",
                          bootstrap = 1000) # bias corrected bootstrap

summary(full_resources_path, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE)

lavTestLRT(free__model, full_resources_path) # scaled chi-squared difference test




## Conditional mediation analysis - Not reported

model_stress_3 <- '
                    # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ d1*csize_index
                            
                        ## MEDIATION
                        
                        # JDR on outcome
                            experience_stress ~ i2*avg_demands
                            experience_stress ~ i3*avg_resources
                        
                        # organisational practices on JDR
                            avg_demands ~ i4*csize_index
                            avg_resources ~ i5*csize_index

                     # indirect effect
                            ind_demands := i2*i4
                            ind_resources := i3*i5
                        
                     # total effect
                            tot_demands := d1 + (i2*i4)
                            tot_resources := d1 + (i3*i5) 
                            tot_full := d1 + (i2*i4*i3*i5)
                    
                    # covariances 
                            avg_demands ~~ avg_resources '
                            

# without legislation 

stress_path_no <- sem(model_stress_3, data = combined_stress, estimator = "MLR", 
                     group = "law_stress_yes", 
                     group.label = 0,
                     se = "robust",
                     test = "bootstrap",
                     bootstrap = 1000) ##### bias corrected bootstrap for legislation = no

summary(stress_path_no, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE) # no legislation 
standardizedsolution(stress_path_no)
parameterEstimates(stress_path_no, zstat = TRUE, pvalue = TRUE, ci = TRUE, level = 0.95, 
                   boot.ci.type = "bca.simple", standardized = TRUE, remove.system.eq = TRUE, 
                   remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE, rsquare = FALSE, add.attributes = FALSE)


# with legislation 

stress_path_yes <- sem(model_stress_3, data = combined_stress, estimator = "MLR", 
                     group = "law_stress_yes", 
                     group.label = 100,
                     se = "robust",
                     test = "bootstrap",
                     bootstrap = 1000) ##### bias corrected bootstrap for legislation = no

summary(stress_path_yes, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = FALSE) # no legislation 
standardizedsolution(stress_path_yes)
parameterEstimates(stress_path_yes, zstat = TRUE, pvalue = TRUE, ci = TRUE, level = 0.95, 
                   boot.ci.type = "bca.simple", standardized = TRUE, remove.system.eq = TRUE, 
                   remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE, rsquare = FALSE, add.attributes = FALSE)


### delete workspace

rm(list = ls())