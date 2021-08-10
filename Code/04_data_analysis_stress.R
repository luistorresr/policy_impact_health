
load("./Data/Working_data/policy_ready.rda") 
load("./Data/Working_data/practices_ready.rda") 
load("./Data/Working_data/JDR_clean.rda") 


## Selecting only stress relevant variables 

### from policy index (indirect = yes) 

policy_stress <- policy_ready %>% select(country, law_stress_yes) 

rm(policy_ready)

### from practices_ready

practices_stress <- practices_ready %>% select(-c(practice_harassment, practice_violence))

rm(practices_ready)

### from JDR ready

JDR_stress_outcome <- JDR_clean %>% select(-c(experience_harassment, experience_violence))

rm(JDR_clean)


## Calculating country index for stress practices in the ESENER

practices_stress <- practices_stress %>% rename(size = size_esener) # change size name

practices_country <- practices_stress %>% 
  group_by(country) %>%
  summarise(country_index = mean(na.omit(practice_stress))) # per country

practices_industry <- practices_stress  %>% 
  group_by(country, industry) %>%
  summarise(industry_index = mean(na.omit(practice_stress))) # per country and industry

practices_size <- practices_stress  %>% 
  group_by(country, industry, size) %>%
  summarise(csize_index = mean(na.omit(practice_stress)))  # per country, industry and size


## Joining tables with replacement of NA following if industry NA = country; if size NA = industry.

practices_index_temp <- left_join(practices_country, practices_industry, by = "country")

practices_index_temp$industry_index <- ifelse(is.na(practices_index_temp$industry_index), 
                                          practices_index_temp$country_index, practices_index_temp$industry_index) # replace "NA" in industry


practices_index <- left_join(practices_index_temp, practices_size, by = c("country", "industry"))

practices_index$csize_index <- ifelse(is.na(practices_index$csize_index), 
                                          practices_index$industry_index, practices_index$csize_index) # replace "NA" in industry


### linking practices and JDR by country, industry and size

JDR_stress_outcome <- JDR_stress_outcome %>% rename(size = size_ewcs)

comb_JDR_practice <- left_join(JDR_stress_outcome, practices_index, by = c("country", "industry", "size"))


### linking combined practices and JDR dataset with policy table

combined_stress <- left_join(comb_JDR_practice, policy_stress, by = c("country")) # this is the working dataset for the stress outcome analysis

l_combined_stress <- get_labels(combined_stress, values = "n") # value labels

save(combined_stress, file = "./Data/Working_data/combined_stress .rda") # r data


### delete not useful datasets from memory

rm(comb_JDR_practice, JDR_stress_outcome, policy_stress, practices_country, practices_index, practices_index_temp, practices_industry,
   practices_size, practices_stress)


# Correlation matrix

options(digits = 2)

stress_corr <- combined_stress %>% select(law_stress_yes, industry_index, experience_stress,
                                               avg_emot_d, avg_quant_d, avg_pace_d,
                                               avg_jobctrl_r, avg_parti_r, avg_col_r, avg_super_r,
                                               avg_demands, avg_resources,
                                               size, sex, age, hours, experience) %>% 
  as.matrix(.) %>%
  rcorr(., type = c("pearson"))

knitr::kable(stress_corr$r) %>% kable_styling()
knitr::kable(stress_corr$P) %>% kable_styling()

# Models

## Model 1: using the resource and demand averages


model_stress_1 <- '
                   # direct effect
      
                        # direct effect - practice on stress
                            
                            experience_stress ~ d1*csize_index
                            
                        
                        # JDR on outcome
                            experience_stress ~ i2*avg_demands
                            experience_stress ~ i3*avg_resources
                        
                        # organisational practices on JDR
                            avg_demands ~ i4*csize_index
                            avg_resources ~ i5*csize_index
                        
                        # Law on organisational practices
                            csize_index ~ i6*law_stress_yes
                     
                     # indirect effect
                            ind_demands := i2*i4
                            ind_resources := i3*i5
                        
                     # total effect
                            tot_demands := d1 + (i2*i4)
                            tot_resources := d1 + (i3*i5) 
                    
                    # controls 
                            csize_index ~ size
                            experience_stress ~ size + hours + age
                            avg_demands ~ size + hours + experience + age
                            avg_resources ~ size + experience + age

                    # covariances 
                            avg_demands ~~ avg_resources '

stress_path_1 <- sem(model_stress_1, data = combined_stress, estimator = "MLR", 
                   group = NULL, 
                   se = "robust",
                   test = "boot",
                   bootstrap = 1000)

summary(stress_path_1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = TRUE)
standardizedsolution(stress_path_1)

##### bias corrected bootstrap
lavaan::parameterEstimates(stress_path_1, zstat = TRUE, pvalue = TRUE, ci = TRUE, level = 0.95, 
                   boot.ci.type = "bca.simple", standardized = FALSE, remove.system.eq = TRUE, 
                   remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE, rsquare = FALSE, add.attributes = FALSE)



## Model 1: multigroup analysis = sex

combined_stress_no_na_sex <- combined_stress %>% filter(!is.na(sex))

stress_path_2 <- sem(model_stress_1, data = combined_stress_no_na_sex, estimator = "MLR", 
                     group = "sex", group.label = 1,
                     se = "robust",
                     test = "boot",
                     bootstrap = 1000)

summary(stress_path_2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = TRUE)
standardizedsolution(stress_path_2)


