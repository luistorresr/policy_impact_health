# is legislation making a difference?

# Packages

library(semTools)
library(semPlot)
library(lavaan)
library(haven)
library(tidyverse)
library(Hmisc)
library(sjlabelled)
library(psych)
library(labelled)


##### ORGANISATIONAL PRACTICES 

#### ESENER 2014

ESENER14 <- read_spss("./Data/UKDA-8690-spss/spss/spss19/esener2_data_q100rev.sav")

l_ESENER14 <- get_labels(ESENER14, values = "n") # value labels
q_ESENER14 <- as.data.frame(label(ESENER14))

# Selecting items

items_ESENER14 <- c(
   # merging variables
  "country",
  
  
   # Organisational practices  
  "Q300", # Does your establishment have an action plan to prevent work-related stress? [If Q104 >19 and <99999]
  "Q301", # Is there a procedure in place to deal with possible cases of bullying or harassment? [If Q104 >19 and <99999]
  "Q302" # And is there a procedure to deal with possible cases of threats, abuse or assaults by clients, patients, pupils or other external persons? [If Q104 >19 and <99999 and Q201_5 = 1]
  )

ESENER14_items <- ESENER14 %>% select(items_ESENER14) %>% as_tibble()


### cleaning the sub-dataset 

ESENER14_items <- ESENER14_items %>% 
  set_na(na = 9, drop.levels = TRUE, as.tag = FALSE) %>% 
  as_tibble(.)

### recoding variables (no to 0)

ESENER14_items$Q300 <- dplyr::recode(ESENER14_items$Q300, `1` = 100, `2` = 0) 
ESENER14_items$Q301 <- dplyr::recode(ESENER14_items$Q301, `1` = 100, `2` = 0) 
ESENER14_items$Q302 <- dplyr::recode(ESENER14_items$Q302, `1` = 100, `2` = 0) 

### CFA Organisational practices

## Model 

m_op <- ' practices  =~ Q300 + Q301 + Q302 ' 

fit_op <- lavaan::cfa(m_op, data = ESENER14_items, 
                      estimator = "ULSM",  mimic = "Mplus",
                      ordered = c("Q300", "Q301", "Q302"))

summary(fit_op, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = TRUE)
fitMeasures(fit_op)
standardizedSolution(fit_op, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE)
resid(fit_op, "cor") # covariance of the residuals
fitted(fit_op) # implied moments
semPaths(fit_op, "model", "std", intercepts = FALSE) # Plots
reliability(fit_op)


#### JDR MODEL AND OUTCOMES

### European Working Conditions Survey 2015

EWCS2015 <- read_spss("./Data/UKDA-8098-spss/spss/spss19/ewcs6_2015_ukda_1904.sav")

l_EWCS2015 <- get_labels(EWCS2015, values = "n") # value labels
q_EWCS2015 <- as.data.frame(label(EWCS2015))


### Filtering for those in employment and working at least 20 hours

EWCS2015 <- EWCS2015 %>% filter(Q2c == 1 & Q7 == 1) # filtering for employees

EWCS2015 <- EWCS2015 %>% filter(Q24 >= 20 & Q24 < 888) # filtering for those working at least 20 hours per week


### items 

items_EWCS <- c(
  
  # Linking variables 
  "Country", # country 
  "nace_rev2_1", # sectors
  "nace_rev2_2", 
  "nace_rev1_1", 
  "nace_rev1_2", 
  
  # Job Demands
  
  # Emotional demands
  
  "Q30g", # Handling angry clients
  "Q30h", # Emotionally disturbing situations 
  "Q61o", # Your job requires that you hide your feelings?

  # Quantitative demands 
  
  "Q49a", # Working at very high speed
  "Q49b", # Working to tight deadlines 
  "Q61g", # You have enough time to get the job done?
  "Q51", # How often do you have to interrupt a task you are doing in order to take on an unforeseen task?

  # Pace determinants 
  "Q50a", 
  "Q50b",
  "Q50c",
  "Q50d",
  "Q50e",

  # Job resources
  
  # Social resources
  
  "Q61a", # Colleague social support index
  "Q70e", 
  "Q89d", 
  "Q61b", # Supervisor social support index
  "Q63a",
  "Q63b",
  "Q63c",
  "Q63d",
  "Q63e",
  "Q63f",
  
  # Work resources
  
  "Q54a", # Job control index 
  "Q54b", 
  "Q54c",
  "Q61f",
  "Q53c", # Skill discretion index
  "Q53e",
  "Q53f",
  "Q61i", 
  "Q61c", # Participation index
  "Q61d",
  "Q61n",

  # outcomes 
  
  "Q61m", # stress  
  "Q81c", # bullying 
  "Q81a" # violence 
  )

# select variables from EWCS 2015

EWCS2015_items <- EWCS2015 %>% select(items_EWCS) %>% as_tibble()

## cleaning the variables

EWCS2015_sub1 <- EWCS2015_items %>% select(-c(Country, nace_rev2_1, nace_rev2_2, nace_rev1_1, nace_rev1_2)) %>%
  set_na(na =c(8, 9), drop.levels = TRUE, as.tag = FALSE) %>% 
  as_tibble(.)

EWCS2015_sub1 <- EWCS2015_sub1 %>% set_na(na =c(Q61o = 7, Q61g = 7, Q61a = 7, Q70e = 7, Q89d = 7, Q61b = 7, Q63a = 7,
                                                    Q63b = 7, Q63c = 7, Q63d = 7, Q63e = 7, Q63f = 7, Q61f = 7, Q61i = 7,
                                                    Q61c = 7, Q61d = 7, Q61n = 7, Q61m = 7,
                                                Q50a = 7 , Q50b = 7, Q50c = 7, Q50d = 7, Q50e = 7), 
                                              drop.levels = TRUE, as.tag = FALSE) %>% as_tibble(.)

EWCS2015_sub2 <- EWCS2015_items %>% select(c(Country, nace_rev2_1, nace_rev2_2, nace_rev1_1, nace_rev1_2)) %>% as_tibble(.)

## working dataset

EWCS2015_items2 <- cbind(EWCS2015_sub2, EWCS2015_sub1) # working dataset


## Recode variables - the higher, the more

### outcomes 

EWCS2015_items2$Q81a <- dplyr::recode(EWCS2015_items2$Q81a, `1` = 100, `2` = 0)  # YES = 1, I have experienced physical violence
EWCS2015_items2$Q81c <- dplyr::recode(EWCS2015_items2$Q81c, `1` = 100, `2` = 0)  # YES = 1, I have experienced bullying/harassment
EWCS2015_items2$Q61m <- dplyr::recode(EWCS2015_items2$Q61m, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) # the higher the more stress

### job  resources

#### work resources 
##### job control index
EWCS2015_items2$Q54a <- dplyr::recode(EWCS2015_items2$Q54a, `1` = 100, `2` = 0) # YES = 1 and NO = 0
EWCS2015_items2$Q54b <- dplyr::recode(EWCS2015_items2$Q54b, `1` = 100, `2` = 0) # YES = 1 and NO = 0
EWCS2015_items2$Q54c <- dplyr::recode(EWCS2015_items2$Q54c, `1` = 100, `2` = 0) # YES = 1 and NO = 0
EWCS2015_items2$Q61f <- dplyr::recode(EWCS2015_items2$Q61f, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)

##### Skill discretion index
EWCS2015_items2$Q53c <- dplyr::recode(EWCS2015_items2$Q53c, `1` = 100, `2` = 0) # YES = 1 and NO = 0
EWCS2015_items2$Q53e <- dplyr::recode(EWCS2015_items2$Q53e, `1` = 100, `2` = 0) # YES = 1 and NO = 0
EWCS2015_items2$Q53f <- dplyr::recode(EWCS2015_items2$Q53f, `1` = 100, `2` = 0) # YES = 1 and NO = 0
EWCS2015_items2$Q61i <- dplyr::recode(EWCS2015_items2$Q61i, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)

##### Participation index
EWCS2015_items2$Q61c <- dplyr::recode(EWCS2015_items2$Q61c, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)
EWCS2015_items2$Q61d <- dplyr::recode(EWCS2015_items2$Q61d, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)
EWCS2015_items2$Q61n <- dplyr::recode(EWCS2015_items2$Q61n, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)

#### social resources
##### Colleague social support index
EWCS2015_items2$Q61a <- dplyr::recode(EWCS2015_items2$Q61a, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)
EWCS2015_items2$Q70e <- dplyr::recode(EWCS2015_items2$Q70e, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)
EWCS2015_items2$Q89d <- dplyr::recode(EWCS2015_items2$Q89d, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)

##### Supervisor social support index
EWCS2015_items2$Q61b <- dplyr::recode(EWCS2015_items2$Q61b, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)
EWCS2015_items2$Q63a <- dplyr::recode(EWCS2015_items2$Q63a, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)
EWCS2015_items2$Q63b <- dplyr::recode(EWCS2015_items2$Q63b, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)
EWCS2015_items2$Q63c <- dplyr::recode(EWCS2015_items2$Q63c, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)
EWCS2015_items2$Q63d <- dplyr::recode(EWCS2015_items2$Q63d, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)
EWCS2015_items2$Q63e <- dplyr::recode(EWCS2015_items2$Q63e, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)
EWCS2015_items2$Q63f <- dplyr::recode(EWCS2015_items2$Q63f, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)

### job demands
#### emotional demands
EWCS2015_items2$Q30g <- dplyr::recode(EWCS2015_items2$Q30g, `1` = 100, `2` = 85, `3` = 70, `4` = 50, `5` = 30, `6` = 15, `7` = 0)
EWCS2015_items2$Q30h <- dplyr::recode(EWCS2015_items2$Q30h, `1` = 100, `2` = 85, `3` = 70, `4` = 50, `5` = 30, `6` = 15, `7` = 0)
EWCS2015_items2$Q61o <- dplyr::recode(EWCS2015_items2$Q61o, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0)

#### Quantitative demands 
EWCS2015_items2$Q49a <- dplyr::recode(EWCS2015_items2$Q49a, `1` = 100, `2` = 85, `3` = 70, `4` = 50, `5` = 30, `6` = 15, `7` = 0)
EWCS2015_items2$Q49b <- dplyr::recode(EWCS2015_items2$Q49b, `1` = 100, `2` = 85, `3` = 70, `4` = 50, `5` = 30, `6` = 15, `7` = 0)
EWCS2015_items2$Q51 <- dplyr::recode(EWCS2015_items2$Q51, `1` = 100, `2` = 70, `3` = 35, `4` = 0)
EWCS2015_items2$Q61g <- dplyr::recode(EWCS2015_items2$Q61g, `1` = 0, `2` = 25, `3` = 50, `4` = 75, `5` = 100)

#### Pace determinats
EWCS2015_items2$Q50a <- dplyr::recode(EWCS2015_items2$Q50a,`1` = 100, `2` = 0)
EWCS2015_items2$Q50b <- dplyr::recode(EWCS2015_items2$Q50b,`1` = 100, `2` = 0)
EWCS2015_items2$Q50c <- dplyr::recode(EWCS2015_items2$Q50c,`1` = 100, `2` = 0)
EWCS2015_items2$Q50d <- dplyr::recode(EWCS2015_items2$Q50d,`1` = 100, `2` = 0)
EWCS2015_items2$Q50e <- dplyr::recode(EWCS2015_items2$Q50e,`1` = 100, `2` = 0)

#### JDR model

##### CFA JDR full model including all factors, subfactors and indexes
##### removed: skill_r =~ Q53c + Q53e + Q53f + Q61i
##### removed: quantitative_d =~ Q51 
##### removed: pace_d =~ Q50a + Q50c + Q50d + Q50e

m_jdr <- 'emotional_d =~ Q30g + Q30h + Q61o
          quantitative_d =~ Q49a + Q49b + Q61g
          job_r =~ Q54a + Q54b + Q54c + Q61f           
          participation_r =~ Q61c + Q61d + Q61n 
          coleague_r =~ Q61a + Q70e + Q89d
          supervisor_r =~ Q61b + Q63a + Q63b + Q63c + Q63d + Q63e + Q63f
          job_demands =~ emotional_d + quantitative_d
          job_resources =~ job_r + participation_r + coleague_r + supervisor_r'

fit_jdr <- lavaan::cfa(m_jdr, data = EWCS2015_items2, 
                      estimator = "WLSM", mimic = "Mplus",
                      ordered = c("Q30g", "Q30h", "Q61o", "Q49a", "Q49b", "Q61g", "Q51", "Q61a", "Q70e", "Q89d",
                                  "Q61b", "Q63a", "Q63b", "Q63c", "Q63d", "Q63e", "Q63f", "Q54a", "Q54b", "Q54c", "Q61f", "Q53c",
                                  "Q53e", "Q53f", "Q61i", "Q61c", "Q61d", "Q61n",
                                  "Q50a", "Q50b", "Q50c", "Q50d", "Q50e"))

summary(fit_jdr, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE, modindices = TRUE)
fitMeasures(fit_jdr)
standardizedSolution(fit_jdr, type = "std.all", se = TRUE, zstat = TRUE, pvalue = TRUE, remove.eq = TRUE, remove.ineq = TRUE, remove.def = FALSE)
resid(fit_jdr, "cor") # covariance of the residuals
fitted(fit_jdr) # implied moments
semPaths(fit_jdr, "model", "std", intercepts = FALSE) # Plots
reliability(fit_jdr)

