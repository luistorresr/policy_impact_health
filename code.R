# packages

library(semTools)
library(semPlot)
library(lavaan)
library(haven)
library(tidyverse)
library(Hmisc)
library(sjlabelled)
library(psych)

#### Psychosocial safety climate (PSC) 

# is a workplace protective factor that reflects the will of management
# to prevent and respondto stressful conditions. Psychosocial safety climate concerns how management values
# worker psychological health, commits to and supports psychological health protection, and prioritizes the
# psychological health of workers over profit and productivity.
# Psychosocial safety climate theory pro-poses that PSC is the “cause of the causes” of common psychosocial
# risks. In a high PSC context managers will have a range of policies, practices and procedures in place to
# ensure that work conditions are not too demanding for workers, that resources are adequate to manage demands, and that overt psychosocial risks such as bullying, and violence and more subtle forms of aggression like incivility. In addition to having a preventative role, PSC may have a buffering role. Psychosocial safety climate may act as a safety signal to employees indicating when it is safe to utilize personal resources (e.g. coping strategies) and/or organizational resources (e.g., utilize autonomy) to cope with job demands

## Psychosocial safety climate measure (Alpha was .87)

# Using the ESENER data (ESENER,2009a) OSH managers were asked five questions that represented best
# procedures to deal with psychosocial risks, and consultation and participation in the resolution
# of psychosocial risks in the workplace. Questions were:
#      “Does your establishment have a procedure to deal with,
#               (1) work-related stress (MM 250)
#               (2) bullying or harassment (MM 251)
#               (3) work-related violence (MM 252) (reversed to No 0, Yes 1, not an issue system missing)
#               (4) What about the role of employees: Have they been consulted regarding measures to deal with psychosocial risks (MM 266)?
#               (5) Are employees encouraged to participate actively in the implementation and evaluation of the measures?”(MM 267)(recoded to Don’t know¼system missing, No¼0, Yes¼1). 


### European Working Conditions Survey

EWCS2015 <- read_spss("./Data/UKDA-8098-spss/spss/spss19/ewcs6_2015_ukda_1904.sav")

items_EWCS <- c(
  
  "Q2c", # filtering for those in work
  
  "Country", "Q2a", # gender
  "Q2b", # age
              
  ## Job Demands 
  
  # Irregular Working Hours 

  "Q37a", # Normally, how many times a month do you work at night, for at least 2 hours between 10.00 pm and 05.00 am? 
  "Q37b",  # And how many times a month do you work on Sundays? 
  "Q37c", # And how many times a month do you work on Saturdays? 
  "Q37d", #And how many times a month do you work more than 10 hours a day? 
    
  # Work-life Conflict 

  "Q44", # In general, how do your working hours fit in with your family or social commitments outside work? (Reverse score) 
  "Q45a", # Kept worrying about work when you were not working [How often have you…?] 
  "Q45b", # Felt too tired after work to do some of the household jobs which need to be done [How often have you…?] 
  "Q45c", # Found that your job prevented you from giving the time you wanted to your family [How often have you…?] 
  "Q45d", # Found it difficult to concentrate on your job because of your family responsibilities [How often have you…?] 
  "Q45e", # Found that your family responsibilities prevented you from giving the time you should to your job [How often have you…?] 
  "Q46", # Since you started your main paid job, how often have you worked in your free time to meet work demands? 
  
  # Time Pressure 

  "Q49a", # Working at very high speed [And, does your job involve…] 
  "Q49b", # Working to tight deadlines  [And, does your job involve…] 

  # Task Complexity 
  "Q53a", # Meeting precise quality standards [Generally, does your main paid job involve…] 
  "Q53b", # Assessing yourself the quality of your own work [Generally, does your main paid job involve…] 
  "Q53c", # Solving unforeseen problems on your own [Generally, does your main paid job involve…] 
  "Q53d", # Monotonous tasks [Generally, does your main paid job involve…] 
  "Q53e", # Complex tasks [Generally, does your main paid job involve…] 
  "Q53f", # Learning new things [Generally, does your main paid job involve…] 

  # Job Resources 

  # Job control 

  "Q54a", # Your order of tasks [are you able to change] 
  "Q54b", # Your methods of work [Generally, does your main paid job involve…] 
  "Q54c", # Your speed or rate of work [Generally, does your main paid job involve…] 

  # Employee participation 

  "Q61c", # You are consulted before objectives are set for your work? 
  "Q61d", # You are involved in improving the work organisation or work processes of your department or organisation? 
  "Q61e", # You have a say in the choice of your work colleagues? 
  
  # Supervisor support 

  "Q63a", # Your immediate boss… - Respects you as a person 
  "Q63b", # Your immediate boss... - Gives you praise and recognition when you do a good job 
  "Q63c", # Your immediate boss… - Is successful in getting people to work together 
  "Q63d", # Your immediate boss… - Is helpful in getting the job done 
  "Q63e", # Your immediate boss… - Provides useful feedback on your work 
  "Q63f", # Your immediate boss… - Encourages and supports your development 

  # Culture 
  "Q70a", # Employees are appreciated when they have done a good job [Agree with the following statements?] 
  "Q70b", # The management trusts the employees to do their work well [Agree with the following statements?] 
  "Q70c", # Conflicts are resolved in a fair way [Agree with the following statements?] 
  "Q70d", # The work is distributed fairly [Agree with the following statements?] 
  "Q70e", # There is good cooperation between you and your colleagues [Agree with the following statements?] 
  "Q70f", # In general, employees trust management [Agree with the following statements?] 

  # Recognition 

  "Q89a", # Considering all my efforts and achievements in my job, I feel I get paid appropriately [Agree, about your job?] 
  "Q89b", # My job offers good prospects for career advancement [Agree, about your job?] 
  "Q89c", # I receive the recognition I deserve for my work [Agree, about your job?] 
  "Q89d", # I generally get on well with my work colleagues [Agree, about your job?] 
  "Q89e", # The organisation I work for motivates me to give my best job performance [Agree, about your job?] 
    # [not sure if 89d and 89e fit in here, could take them out] 

  # Wellbeing 

  # Sleep 

  "Q79a", # Difficulty falling asleep [Last 12 months, any sleep related problems?] 
  "Q79b", # Waking up repeatedly during the sleep [Last 12 months, any sleep related problems?] 
  "Q79c", # Waking up with a feeling of exhaustion and fatigue [Last 12 months, any sleep related problems?] 

  # Sickness absence 

  "Q82", # Over the past 12 months how many days in total were you absent from work due to sick leave or health-related leave? 
  
  # Presenteeism 

  "Q84b", # How many working days? […did you work when you were sick?] 

  # Work engagement 

  "Q90a", #  - At my work I feel full of energy [Please tell me how often you feel this way...] 
  "Q90b", # I am enthusiastic about my job [Please tell me how often you feel this way...] 
  "Q90c", # Time flies when I am working [Please tell me how often you feel this way...] 

  # Exhaustion 

  "Q90d", # I feel exhausted at the end of the working day [Please tell me how often you feel this way...] 

  # General wellbeing 

  "Q87a", # I have felt cheerful and in good spirits [...which is the closest to how you have been feeling over the last two weeks] 
  "Q87b", # I have felt calm and relaxed [...which is the closest to how you have been feeling over the last two weeks] 
  "Q87c", # I have felt active and vigorous [...which is the closest to how you have been feeling over the last two weeks] 
  "Q87d", # I woke up feeling fresh and rested [...which is the closest to how you have been feeling over the last two weeks] 
  "Q87e", # My daily life has been filled with things that interest me [...which is the closest to how you have been feeling over the last two weeks] 

  # Job satisfaction 

  "Q88" # On the whole, are you very satisfied, satisfied, not very satisfied or not at all satisfied with working conditions in your main paid job? 

)

# select variables from EWCS 2015

EWCS2015_items <- EWCS2015 %>% select(items_EWCS) %>% as_tibble()

EWCS2015_items <- EWCS2015_items %>% filter(Q2c == 1) # filtering for those with employment

# labels 

l_EWCS2015_items <- get_labels(EWCS2015_items, values = "n") # value labels

#### Job demands

## cleaning the variables

EWCS2015_items <- EWCS2015_items %>% 
  set_na(na = c(Q37a= c(88, 99), Q37b = c(88, 99),  Q37c = c(88, 99), Q37d = c(88, 99)), 
         drop.levels = TRUE, as.tag = FALSE) %>% 
  as_tibble(.)

EWCS2015_items$Q37a[EWCS2015_items$Q37a==999] <- 0
EWCS2015_items$Q37b[EWCS2015_items$Q37b==999] <- 0
EWCS2015_items$Q37c[EWCS2015_items$Q37c==999] <- 0
EWCS2015_items$Q37d[EWCS2015_items$Q37d==999] <- 0

EWCS2015_items$Q37a <- remove_labels(EWCS2015_items$Q37a, labels= "Never")
EWCS2015_items$Q37b <- remove_labels(EWCS2015_items$Q37b, labels= "Never")
EWCS2015_items$Q37c <- remove_labels(EWCS2015_items$Q37c, labels= "Never")
EWCS2015_items$Q37d <- remove_labels(EWCS2015_items$Q37d, labels= "Never")

EWCS2015_items <- EWCS2015_items %>% dplyr::mutate(Q44a = Q44)
EWCS2015_items <- EWCS2015_items %>% dplyr::mutate(Q44a = dplyr::recode(Q44a, 
                                                                        `1` = 4,
                                                                        `2` = 3,
                                                                        `3` = 2,
                                                                        `4` = 1))

EWCS2015_items <- EWCS2015_items %>% 
  set_na(na = c(Q49a = c(8, 9), Q49b = c(8, 9)), 
         drop.levels = TRUE, as.tag = FALSE) %>% 
  as_tibble(.)


## CFA job demands

m_jd <- 'iwkh =~ Q37a + Q37b + Q37c + Q37d 
          wlc =~ Q44a + Q45a + Q45b + Q45c + Q45d + Q45e + Q46
          tc =~ Q53a + Q53b + Q53c + Q53e + Q53f 
          tp  =~ Q49a + Q49b ' # Q53d deleted for bad performance'

fit_jd <- lavaan::cfa(m_jd, data = EWCS2015_items, 
                      estimator = "ULSMV",  mimic = "Mplus",
                      ordered = c("Q44a", "Q45a", "Q45b", "Q45c", "Q45d", "Q45e", "Q46",
                                  "Q53a", "Q53b", "Q53c", "Q53e", "Q53f",
                                  "Q49a", "Q49b"))
                      
summary(fit_jd, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = TRUE)
fitMeasures(fit_jd)
standardizedSolution(fit_jd, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE)
resid(fit_jd, "cor") # covariance of the residuals
fitted(fit_jd) # implied moments
semPaths(fit_jd, "model", "std", intercepts = FALSE) # Plots

EWCS2015_items %>% select(Q37a, Q37b, Q37c, Q37d) %>% alpha()
EWCS2015_items %>% select(Q44a, Q45a, Q45b, Q45c, Q45d, Q45e, Q46) %>% alpha()
EWCS2015_items %>% select(Q53a, Q53b, Q53c, Q53e, Q53f) %>% alpha()
EWCS2015_items %>% select(Q49a, Q49b) %>% alpha()


#### Job resources

EWCS2015_items <- EWCS2015_items %>% 
  set_na(na = c(Q61c = 7, Q61d = 7, Q61e = 7,
                Q63a = 7, Q63b = 7, Q63c = 7, Q63d = 7, Q63e = 7, Q63f = 7,
                Q70a = 7, Q70b = 7, Q70c = 7, Q70d = 7, Q70e = 7, Q70f = 7,
                Q89a = 7, Q89b = 7, Q89c = 7, Q89d = 7, Q89e = 7), 
         drop.levels = TRUE, as.tag = FALSE) %>% 
  as_tibble(.)


### CFA job demands 

m_jr <- ' jc =~ Q54a + Q54b + Q54c
          ep =~ Q61c + Q61d + Q61e 
          sp =~ Q63a + Q63b + Q63c + Q63d + Q63e + Q63f 
          cul =~ Q70a + Q70b + Q70c + Q70d + Q70e + Q70f
          rec =~ Q89a + Q89b + Q89c + Q89d + Q89e ' 

fit_jr <- lavaan::cfa(m_jr, data = EWCS2015_items, 
                      estimator = "ULSMV",  mimic = "Mplus",
                      ordered = c("Q54a", "Q54b", "Q54c", 
                                  "Q61c", "Q61d", "Q61e",  
                                  "Q63a", "Q63b", "Q63c", "Q63d", "Q63e", "Q63f",  
                                  "Q70a", "Q70b", "Q70c", "Q70d", "Q70e", "Q70f", 
                                  "Q89a", "Q89b", "Q89c", "Q89d", "Q89e"))

summary(fit_jr, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = TRUE)
fitMeasures(fit_jr)
standardizedSolution(fit_jr, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE)
resid(fit_jr, "cor") # covariance of the residuals
fitted(fit_jr) # implied moments
semPaths(fit_jr, "model", "std", intercepts = FALSE) # Plots
reliability(fit_jr)


# Wellbeing

EWCS2015_items <- EWCS2015_items %>% 
  set_na(na = c(Q79a = c(7, 8, 9),Q79b = c(7, 8, 9), Q79c = c(7, 8, 9),
                Q90a = c(8, 9), Q90b = c(8, 9), Q90c = c(8, 9),
                Q87a = c(8, 9), Q87b = c(8, 9), Q87c = c(8, 9), Q87d = c(8, 9), Q87e = c(8, 9)), 
         drop.levels = TRUE, as.tag = FALSE) %>% 
  as_tibble(.)



m_well <- ' slp =~ Q79a + Q79b + Q79c  
            we =~ Q90a + Q90b + Q90c
            ge =~ Q87a + Q87b + Q87c + Q87d + Q87e ' 

fit_well <- lavaan::cfa(m_well, data = EWCS2015_items, 
                      estimator = "ULSMV",  mimic = "Mplus",
                      ordered = c("Q79a", "Q79b", "Q79c",   
                                  "Q90a", "Q90b", "Q90c",
                                  "Q87a", "Q87b", "Q87c", "Q87d", "Q87e"))

summary(fit_well, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = TRUE)
fitMeasures(fit_well)
standardizedSolution(fit_well, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE)
resid(fit_well, "cor") # covariance of the residuals
fitted(fit_well) # implied moments
semPaths(fit_well, "model", "std", intercepts = FALSE) # Plots
reliability(fit_well)
