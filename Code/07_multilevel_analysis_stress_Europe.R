
# Load database

load("./Data/Working_data/combined_stress .rda") 

l_combined_stress <- get_labels(combined_stress, values = "n") # value labels


# Multilevel modelling

# Data preparation 

## reduce the number of industries

combined_stress <- combined_stress %>% mutate(industry2 = industry) # for 9 industries

combined_stress$industry2 <- dplyr::recode_factor(combined_stress$industry2, `1` = 1,  `2` = 2, `3` = 3, `4` = 2, `5` = 2, `6` = 4, `7` = 5, `8` = 6, `9` = 5, `10` = 6,
                                                                              `11` = 7, `12` = 7, `13` = 7, `14` = 7, `15` = 8, `16` = 8, `17` = 8, `18` = 9, `19` = 9, `20` = 9, `21` = 9) 


combined_stress$industry2 <- replace_labels(combined_stress$industry2, labels = c("Agriculture, forestry and fishing" = 1,
                                                                                  "Mining, Energy and water" = 2,
                                                                                  "Manufacturing" = 3,
                                                                                  "Construction" = 4,
                                                                                  "Distribution, hotels and restaurants" = 5,
                                                                                  "Transport and communication" = 6,
                                                                                  "Banking and finance" = 7,
                                                                                  "Public admin, education and health" = 8,
                                                                                  "Other services" = 9)) %>% as_numeric()

summary_industries <- combined_stress %>% 
                      group_by(as_label(industry2), as_label(country)) %>% summarise(total = n()) %>%
                      pivot_wider(names_from = `as_label(industry2)`, values_from = total) %>%
                      adorn_totals("col")


combined_stress <- combined_stress %>% mutate(sector = industry2) # only two sectors 

# based on https://ilostat.ilo.org/resources/concepts-and-definitions/classification-economic-activities/

combined_stress$sector <- dplyr::recode_factor(combined_stress$sector, `1` = 1,  `2` = 1, `3` = 1,
                                               `4` = 1, `5` = 2, `6` = 2, `7` = 2, `8` = 2, `9` = 2) 

combined_stress$sector <- replace_labels(combined_stress$sector, 
                                         labels = c("Industry" = 1, # including agriculture
                                                    "Services" = 2)) %>% as_numeric()
summary_sectors <- combined_stress %>% 
                    group_by(as_label(sector), as_label(country)) %>% summarise(total = n()) %>%
                    pivot_wider(names_from = `as_label(sector)`, values_from = total) %>%
                    adorn_totals("col")


## Center means

### practices 

combined_stress <- combined_stress %>% mutate(practice_centered = 
                                                      center(csize_index, type = "CWC", cluster = law_stress_yes, 
                                                             value = NULL, as.na = NULL, check = TRUE)) # center at the law level

combined_stress <- combined_stress %>% mutate(practice_centered_2 = 
                                                center(csize_index, type = "CWC", cluster = country, 
                                                       value = NULL, as.na = NULL, check = TRUE)) # center at country level

combined_stress <- combined_stress %>% mutate(practice_centered_3 = 
                                                center(csize_index, type = "CWC", cluster = sector, 
                                                       value = NULL, as.na = NULL, check = TRUE)) # center at country level

### demands and controls

combined_stress <- combined_stress %>% mutate(demands_centered = 
                                                center(avg_demands, type = "CGM", cluster = NULL, 
                                                       value = NULL, as.na = NULL, check = TRUE)) # grand mean

combined_stress <- combined_stress %>% mutate(demands_centered_2 = 
                                                center(avg_demands, type = "CWC", cluster = law_stress_yes, 
                                                       value = NULL, as.na = NULL, check = TRUE)) # center at the law level


combined_stress <- combined_stress %>% mutate(resources_centered = 
                                                center(avg_resources, type = "CGM", cluster = NULL, 
                                                       value = NULL, as.na = NULL, check = TRUE)) # grand mean

combined_stress <- combined_stress %>% mutate(resources_centered_2 = 
                                                center(avg_resources, type = "CWC", cluster = law_stress_yes, 
                                                       value = NULL, as.na = NULL, check = TRUE)) # center at the law level


### practice and legislation interaction

combined_stress <- combined_stress %>% mutate(pl_interaction = practice_centered * law_stress_yes)

## no sector NAs 

#combined_stress_NA <- combined_stress %>% filter(!is.na(sector)) # delete NAs to avoid ranova error


## Recoding legislation for moderation (1, 0)

combined_stress <- combined_stress %>% mutate(law_stress_yes_2 = law_stress_yes)
combined_stress$law_stress_yes_2 <- dplyr::recode(combined_stress$law_stress_yes_2, `100` = 1, `0` = 0) # recoding legislation


## Multilevel models

#### null models (no explanatory variables)

##### Null model - No predictors, only levels (unconditional means model)

###### Outcome: Stress 

nullmodel_1 <- lmer(scale(experience_stress) ~ 
                      (1 | country),
                    data = combined_stress, REML = FALSE, 
                    control = lmerControl(optimizer ="Nelder_Mead")) # simplest model

summary(nullmodel_1)
ranova(nullmodel_1)
performance::icc(nullmodel_1) # intraclass correlation

###### Outcome: Organisational practices 

nullmodel_2 <- lmer(scale(csize_index) ~ 1 +
                            (1 | country),
                          data = combined_stress, REML = FALSE, 
                          control = lmerControl(optimizer ="Nelder_Mead")) 
summary(nullmodel_2)
ranova(nullmodel_2)
performance::icc(nullmodel_2) # intraclass correlation

##### outcome: Demands

nullmodel_3 <- lmer(scale(avg_demands) ~ 1 +
                            (1 | country),
                          data = combined_stress, REML = FALSE, 
                          control = lmerControl(optimizer ="Nelder_Mead")) 

summary(nullmodel_3)
ranova(nullmodel_3)
performance::icc(nullmodel_3) # intraclass correlation


##### outcome: Resources

nullmodel_4 <- lmer(scale(avg_resources) ~ 1 +
                            (1 | country),
                          data = combined_stress, REML = FALSE, 
                          control = lmerControl(optimizer ="Nelder_Mead")) 
summary(nullmodel_4)
ranova(nullmodel_4)
performance::icc(nullmodel_4) # intraclass correlation


#### Fitting model

##### Experience stress outcome

stress_model_1 <- lmer(scale(experience_stress) ~ 1 +
                         scale(avg_resources) + 
                         scale(avg_demands) +
                         (1 + scale(avg_resources) | avg_demands) +
                         #scale(pl_interaction) +
                         scale(practice_centered):scale(law_stress_yes) +                         
                         (1 | country) +
                         #(0 + scale(pl_interaction) | country),
                         (0 + scale(practice_centered):scale(law_stress_yes) | country),
                       data = combined_stress, REML = F, 
                       control = lmerControl(optimizer ="Nelder_Mead")) # individual level predictors 

options(scipen = 100, digits = 4)
summary(stress_model_1)
ranova(stress_model_1)
performance::icc(stress_model_1) # intra class correlation
hist(resid(stress_model_1)) # normal distribution 

##### Organisational practices outcome

practice_model_1 <- lmer(scale(csize_index) ~  1 +
                          law_stress_yes_2 +
                         (1 | country) +
                         (0 + law_stress_yes_2 | country),
                         data = combined_stress, REML = F, 
                         control = lmerControl(optimizer ="Nelder_Mead")) 

summary(practice_model_1)
ranova(practice_model_1)
performance::icc(practice_model_1) # intraclass correlation


##### Resources outcome

resources_model_1 <- lmer(scale(avg_resources) ~ 1 +
                         scale(practice_centered):scale(law_stress_yes) +                         
                         (1 | country) +
                         (0 + scale(practice_centered):scale(law_stress_yes) | country),
                       data = combined_stress, REML = F, 
                       control = lmerControl(optimizer ="Nelder_Mead")) # individual level predictors 

summary(resources_model_1)
ranova(resources_model_1)
performance::icc(resources_model_1) # intra class correlation
hist(resid(resources_model_1)) # normal distribution 

##### Demands outcome

demands_model_1 <- lmer(scale(avg_demands) ~ 1 +
                            scale(practice_centered):scale(law_stress_yes) +                         
                            (1 | country) +
                            (0 + scale(practice_centered):scale(law_stress_yes) | country),
                          data = combined_stress, REML = F, 
                          control = lmerControl(optimizer ="Nelder_Mead")) # individual level predictors 

summary(demands_model_1)
ranova(demands_model_1)
performance::icc(demands_model_1) # intra class correlation
hist(resid(demands_model_1)) # normal distribution 
