
# Cleaning the datasets: ESENER 2015 and EWCS2015

## EWCS2015 - for JDR model 

### loading the original dataset 

EWCS2015_base <- read_spss("./Data/UKDA-8098-spss/spss/spss19/ewcs6_2015_ukda_1904.sav")

l_EWCS2015_base <- get_labels(EWCS2015_base, values = "n") # value labels
q_EWCS2015_base <- as.data.frame(label(EWCS2015_base))

### items 

items_EWCS2015 <- c(
  
  # filtering variables
  
  "Q2a", # gender (dont know 9)
  "isco_08_1", # occupation
  "nace_rev2_1", # industry
  "Q2b", # age as a numeric value (na 888, 999)
  "Q2c", # filter for 1 = in employment (na 88, 99)
  "Q7", # filter for 1 = employee (na 8 and 9)
  "Q2d", # 1 = full time, 2 = part-time (na 8 and 9)
  "Q24", # filter for hours per week if needed (na 888, 999)
  
  # Linking variables 
  
  "Country", # country 

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
  
  "Q61m", # experience stress  
  "Q81c", # experience harassment  
  "Q81a" # experience violence
)

### select variables from EWCS 2015

EWCS2015_items <- EWCS2015_base %>% select(all_of(items_EWCS2015)) %>% as_tibble()

q_EWCS2015_items <- as.data.frame(label(EWCS2015_items))

## cleaning the variables

EWCS2015_sub1 <- EWCS2015_items %>% select(-c(Country, isco_08_1, nace_rev2_1)) %>%
  set_na(na =c(8, 9), drop.levels = TRUE, as.tag = FALSE) %>% 
  as_tibble(.)

EWCS2015_sub1 <- EWCS2015_sub1 %>% set_na(na =c(Q61o = 7, Q61g = 7, Q61a = 7, Q70e = 7, Q89d = 7, Q61b = 7, Q63a = 7,
                                                Q63b = 7, Q63c = 7, Q63d = 7, Q63e = 7, Q63f = 7, Q61f = 7, Q61i = 7,
                                                Q61c = 7, Q61d = 7, Q61n = 7, Q61m = 7,
                                                Q50a = 7 , Q50b = 7, Q50c = 7, Q50d = 7, Q50e = 7,
                                                Q2b = c(888, 999), Q24 = c(888, 999)), 
                                          drop.levels = TRUE, as.tag = FALSE) %>% as_tibble(.)

EWCS2015_sub2 <- EWCS2015_items %>% select(Country, isco_08_1, nace_rev2_1) %>% as_tibble(.)

## working dataset

EWCS2015_items <- cbind(EWCS2015_sub2, EWCS2015_sub1) # working dataset


### Recode variables - the higher, the more

#### outcomes 

EWCS2015_items$Q81a <- dplyr::recode_factor(EWCS2015_items$Q81a, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q81a, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)  # YES = 1, I have experienced physical violence

EWCS2015_items$Q81c <- dplyr::recode_factor(EWCS2015_items$Q81c, `1` = 100, `2` = 0)  %>% 
  replace_labels(EWCS2015_items$Q81c, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1, I have experienced bullying/harassment

EWCS2015_items$Q61m <- dplyr::recode_factor(EWCS2015_items$Q61m, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61m, labels = c("Always" = 100,
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)# the higher the more stress

##### rename the outcomes variables

EWCS2015_items <- EWCS2015_items %>% dplyr::mutate(experience_stress = Q61m,
                                    experience_harassment = Q81c,
                                    experience_violence = Q81a)


#### job  resources

##### work resources 
###### job control index
EWCS2015_items$Q54a <- dplyr::recode_factor(EWCS2015_items$Q54a, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q54a, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q54b <- dplyr::recode_factor(EWCS2015_items$Q54b, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q54b, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q54c <- dplyr::recode_factor(EWCS2015_items$Q54c, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q54c, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q61f <- dplyr::recode_factor(EWCS2015_items$Q61f, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61f, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

###### Skill discretion index
EWCS2015_items$Q53c <- dplyr::recode_factor(EWCS2015_items$Q53c, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q53c, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q53e <- dplyr::recode_factor(EWCS2015_items$Q53e, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q53e, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q53f <- dplyr::recode_factor(EWCS2015_items$Q53f, `1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q53f, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE) # YES = 1 and NO = 0

EWCS2015_items$Q61i <- dplyr::recode_factor(EWCS2015_items$Q61i, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61i, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                  "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

###### Participation index
EWCS2015_items$Q61c <- dplyr::recode_factor(EWCS2015_items$Q61c, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61c, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q61d <- dplyr::recode_factor(EWCS2015_items$Q61d, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61d, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q61n <- dplyr::recode_factor(EWCS2015_items$Q61n, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61n, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

##### social resources
###### Colleague social support index
EWCS2015_items$Q61a <- dplyr::recode_factor(EWCS2015_items$Q61a, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61a, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q70e <- dplyr::recode_factor(EWCS2015_items$Q70e, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q70e, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q89d <- dplyr::recode_factor(EWCS2015_items$Q89d, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q89d, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

###### Supervisor social support index
EWCS2015_items$Q61b <- dplyr::recode_factor(EWCS2015_items$Q61b, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61b, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63a <- dplyr::recode_factor(EWCS2015_items$Q63a, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63a, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63b <- dplyr::recode_factor(EWCS2015_items$Q63b, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63b, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63c <- dplyr::recode_factor(EWCS2015_items$Q63c, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63c, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                  "Tend to disagree" = 25, 
                                                  "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63d <- dplyr::recode_factor(EWCS2015_items$Q63d, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63d, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63e <- dplyr::recode_factor(EWCS2015_items$Q63e, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63e, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q63f <- dplyr::recode_factor(EWCS2015_items$Q63f, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q63f, labels = c("Strongly agree" = 100, 
                                                 "Tend to agree" = 75, 
                                                 "Neither agree nor disagree" = 50, 
                                                 "Tend to disagree" = 25, 
                                                 "Strongly disagree" = 0)) %>% as_numeric(., drop.levels = TRUE)

#### job demands
##### emotional demands
EWCS2015_items$Q30g <- dplyr::recode_factor(EWCS2015_items$Q30g, `1` = 100, `2` = 90, `3` = 75, `4` = 50, `5` = 25, `6` = 10, `7` = 0) %>% 
  replace_labels(EWCS2015_items$Q30g, labels = c("All of the time" = 100, 
                                                 "Almost all of the time" = 90, 
                                                 "Around 3/4 of the time" = 75, 
                                                 "Around half of the time" = 50, 
                                                 "Around 1/4 of the time" = 25, 
                                                 "Almost never" = 10, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q30h <- dplyr::recode_factor(EWCS2015_items$Q30h, `1` = 100, `2` = 90, `3` = 75, `4` = 50, `5` = 25, `6` = 10, `7` = 0) %>% 
  replace_labels(EWCS2015_items$Q30h, labels = c("All of the time" = 100, 
                                                 "Almost all of the time" = 90, 
                                                 "Around 3/4 of the time" = 75, 
                                                 "Around half of the time" = 50, 
                                                 "Around 1/4 of the time" = 25, 
                                                 "Almost never" = 10, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q61o <- dplyr::recode_factor(EWCS2015_items$Q61o, `1` = 100, `2` = 75, `3` = 50, `4` = 25, `5` = 0) %>% 
  replace_labels(EWCS2015_items$Q61o, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

##### Quantitative demands 
EWCS2015_items$Q49a <- dplyr::recode_factor(EWCS2015_items$Q49a, `1` = 100, `2` = 90, `3` = 75, `4` = 50, `5` = 25, `6` = 10, `7` = 0) %>% 
  replace_labels(EWCS2015_items$Q49a, labels = c("All of the time" = 100, 
                                                 "Almost all of the time" = 90, 
                                                 "Around 3/4 of the time" = 75, 
                                                 "Around half of the time" = 50, 
                                                 "Around 1/4 of the time" = 25, 
                                                 "Almost never" = 10, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q49b <- dplyr::recode_factor(EWCS2015_items$Q49b, `1` = 100, `2` = 90, `3` = 75, `4` = 50, `5` = 25, `6` = 10, `7` = 0) %>% 
  replace_labels(EWCS2015_items$Q49b, labels = c("All of the time" = 100, 
                                                 "Almost all of the time" = 90, 
                                                 "Around 3/4 of the time" = 75, 
                                                 "Around half of the time" = 50, 
                                                 "Around 1/4 of the time" = 25, 
                                                 "Almost never" = 10, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)


EWCS2015_items$Q51 <- dplyr::recode_factor(EWCS2015_items$Q51, `1` = 100, `2` = 70, `3` = 30, `4` = 0) %>% 
  replace_labels(EWCS2015_items$Q51, labels = c("Very often" = 100, 
                                                "Fairly often" = 70, 
                                                "Occasionally" = 30, 
                                                "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

EWCS2015_items$Q61g <- dplyr::recode_factor(EWCS2015_items$Q61g, `1` = 0, `2` = 25, `3` = 50, `4` = 75, `5` = 100) %>% 
  replace_labels(EWCS2015_items$Q61g, labels = c("Always" = 100, 
                                                 "Most of the time" = 75, 
                                                 "Sometimes" = 50, 
                                                 "Rarely" = 25, 
                                                 "Never" = 0)) %>% as_numeric(., drop.levels = TRUE)

##### Pace determinats
EWCS2015_items$Q50a <- dplyr::recode_factor(EWCS2015_items$Q50a,`1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q50a, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

EWCS2015_items$Q50b <- dplyr::recode_factor(EWCS2015_items$Q50b,`1` = 100, `2` = 0) %>% 
   replace_labels(EWCS2015_items$Q50b, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

EWCS2015_items$Q50c <- dplyr::recode_factor(EWCS2015_items$Q50c,`1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q50c, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

EWCS2015_items$Q50d <- dplyr::recode_factor(EWCS2015_items$Q50d,`1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q50d, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

EWCS2015_items$Q50e <- dplyr::recode_factor(EWCS2015_items$Q50e,`1` = 100, `2` = 0) %>% 
  replace_labels(EWCS2015_items$Q50e, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)# YES = 1 and NO = 0

### Saving the resulting dataset 

save(EWCS2015_items, file = "./Data/Working_data/EWCS2015_items.rda") # r data


############################################################################################################################

## ESENER 2014 - To estimate organisational practices

### Load the original dataset

ESENER14_base <- read_spss("./Data/UKDA-8690-spss/spss/spss19/esener2_data_q100rev.sav")

l_ESENER14_base <- get_labels(ESENER14_base, values = "n") # value labels
q_ESENER14_base <- as.data.frame(label(ESENER14_base)) # questions

### Selecting items to estimate organisational practices

items_ESENER14 <- c(
  
  # merging variables
  
  "country",
  
  # Organisational practices indicators
  
  "Q300", # Does your establishment have an action plan to prevent work-related stress? [If Q104 >19 and <99999]
  "Q301", # Is there a procedure in place to deal with possible cases of bullying or harassment? [If Q104 >19 and <99999]
  "Q302" # And is there a procedure to deal with possible cases of threats, abuse or assaults by clients, patients, pupils or other external persons? [If Q104 >19 and <99999 and Q201_5 = 1]
)

### selecting the variables from the original dataset

ESENER14_items <- ESENER14_base %>% select(all_of(items_ESENER14)) %>% as_tibble()

### rename the variables

ESENER14_items <- ESENER14_items %>% dplyr::mutate(practice_stress = Q300,
                                                   practice_harassment = Q301,
                                                   practice_violence = Q302)

### selecting only the relevant variables. 

ESENER14_items <- ESENER14_items %>% select(country, practice_stress, practice_harassment, practice_violence)

### extracting variable labels 

l_ESENER14_items <- get_labels(ESENER14_items, values = "n") # value labels
q_ESENER14_items <- as.data.frame(label(ESENER14_items)) # questions

write.xlsx(q_ESENER14_items, file = "./Data/Working_data/q_ESENER14.xlsx") # export questions to excel

### creating no answers as NA's only for items

ESENER14_sub1 <- ESENER14_items %>% select(country)

ESENER14_sub2 <- ESENER14_items %>% select(-c(country)) %>% 
  set_na(na = c(practice_stress = 9, practice_harassment = 9, Q302 = 9), drop.levels = TRUE, as.tag = FALSE) %>% 
  as_tibble(.)

ESENER14_items <- cbind(ESENER14_sub1, ESENER14_sub2)

### recoding variables (yes = 1, no = 0)

ESENER14_items$practice_stress <- dplyr::recode_factor(ESENER14_items$practice_stress, `1` = 100, `2` = 0) %>% 
  replace_labels(ESENER14_items$practice_stress, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)

ESENER14_items$practice_harassment <- dplyr::recode_factor(ESENER14_items$practice_harassment, `1` = 100, `2` = 0) %>% 
  replace_labels(ESENER14_items$practice_harassment, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)

ESENER14_items$practice_violence <- dplyr::recode_factor(ESENER14_items$practice_violence, `1` = 100, `2` = 0) %>% 
  replace_labels(ESENER14_items$practice_violence, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)


# Standardising country variable to link with  EWCS2015

val_labels(ESENER14_base$country)
val_labels(EWCS2015_items$Country)

ESENER14_items$country <- dplyr::recode_factor(ESENER14_items$country, `1` = 35, `2` = 1, `3` = 2, `4` = 3, `5` = 34, `6` = 5, `7` = 6, `8` = 11, 
                                               `9` = 7, `10`= 8, `11`= 12, `12`= 26, `13`= 9, `14`= 10, `15`= 4, `16`= 13, `17`= 14, `18`= 999, 
                                               `19`= 15, `20`= 17, `21`= 18, `22`= 16, `23`= 29, `24`= 30, `25`= 19, `26`= 20, `27`= 33, `28`= 21,
                                               `29`= 22, `30`= 23, `31`= 31, `32`= 27, `33`= 25, `34`= 24, `35`= 32, `36`= 28) 

ESENER14_items$country <- replace_labels(ESENER14_items$country, labels = c( "Albania"=35, "Austria"=1, "Belgium"=2, "Bulgaria"=3, 
                                                                             "Switzerland"=34, "Cyprus"=5, "Czech Republic"=6, "Germany"=11, 
                                                                             "Denmark"=7, "Estonia"=8, "Greece"=12, "Spain"=26, "Finland"=9, 
                                                                             "France"=10, "Croatia"= 4, "Hungary"=13, "Ireland"=14, "Iceland"=999,
                                                                             "Italy"=15, "Lithuania"=17, "Luxembourg"=18, "Latvia"=16, "Montenegro"=29, 
                                                                             "FYROM"=30, "Malta"=19, "Netherlands"=20, "Norway"=33, "Poland"=21, 
                                                                             "Portugal"=22, "Romania"=23, "Serbia"=31, "Sweden"=27, "Slovenia"=25, 
                                                                             "Slovakia"=24, "Turkey"=32, "UK"=28)) %>% as_numeric(., drop.levels = TRUE)


### Saving the resulting dataset for the data reduction step

save(ESENER14_items, file = "./Data/Working_data/ESENER14_items.rda") # r data
write.xlsx(ESENER14_items, file = "./Data/Working_data/ESENER14_items.xlsx") # excel file

# Policy index 

## Base with indirect: No` = 0, `Yes, indirect` = 1, `Yes, direct` = 2

policy_index <- tibble(country = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 
                                   19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36),  # based on ESENER labels
  law_stress0 = c(0, 2, 2, 2, 2, 0, 2, 2, 2, 2, 0,	0, 2,	2, 2, 2, 1, 2, 0, 0, 0, 0, 0, 0, 2, 2, 0, 2, 0, 0, 2, 0, 0, 0, 1),                                                    
  law_harassment0 = c(1, 2, 2, 1, 2, 1, 2, 2, 2, 1, 1, 2, 2, 2, 1, 1, 1, 2, 1, 2, 1, 1, 2, 1, 2, 2, 2, 2, 1, 2, 2, 2, 1, 2, 1), 
  law_violence0 = c(0,	2, 2, 0, 2,	0, 0,	2, 2, 0, 2, 2, 2, 2, 0, 0, 2, 2, 0, 2, 2,	0, 0,	0, 2, 2, 0, 2, 0, 0, 2, 2, 0, 0, 2))
  
policy_index$law_stress0 <- add_labels(policy_index$law_stress0, labels = c(`No` = 0, `Yes, indirect` = 1, `Yes, direct` = 2))
policy_index$law_harassment0 <- add_labels(policy_index$law_harassment0, labels = c(`No` = 0, `Yes, indirect` = 1, `Yes, direct` = 2))
policy_index$law_violence0 <- add_labels(policy_index$law_violence0, labels = c(`No` = 0, `Yes, indirect` = 1, `Yes, direct` = 2))

policy_index$country <- dplyr::recode(policy_index$country, `1` = 35, `2` = 1, `3` = 2, `4` = 3, `5` = 34, `6` = 5, `7` = 6, `8` = 11, 
                                               `9` = 7, `10`= 8, `11`= 12, `12`= 26, `13`= 9, `14`= 10, `15`= 4, `16`= 13, `17`= 14,  
                                               `19`= 15, `20`= 17, `21`= 18, `22`= 16, `23`= 29, `24`= 30, `25`= 19, `26`= 20, `27`= 33, `28`= 21,
                                               `29`= 22, `30`= 23, `31`= 31, `32`= 27, `33`= 25, `34`= 24, `35`= 32, `36`= 28) # aligning to EWCS labels

policy_index$country <- add_labels(policy_index$country, 
                                   labels = c( "Albania"=35, "Austria"=1, "Belgium"=2, "Bulgaria"=3, 
                                               "Switzerland"=34, "Cyprus"=5, "Czech Republic"=6, "Germany"=11, 
                                               "Denmark"=7, "Estonia"=8, "Greece"=12, "Spain"=26, "Finland"=9, 
                                               "France"=10, "Croatia"= 4, "Hungary"=13, "Ireland"=14, "Italy"=15, 
                                               "Lithuania"=17, "Luxembourg"=18, "Latvia"=16, "Montenegro"=29, 
                                               "FYROM"=30, "Malta"=19, "Netherlands"=20, "Norway"=33, "Poland"=21, 
                                               "Portugal"=22, "Romania"=23, "Serbia"=31, "Sweden"=27, "Slovenia"=25, 
                                               "Slovakia"=24, "Turkey"=32, "UK"=28)) %>% as_numeric(.)


## Recoding to match percentages: No` = 0, `Yes, indirect` = 50, `Yes, direct` = 100


policy_index <- policy_index %>% mutate(law_stress1 = recode_factor(law_stress0, `0` = 0, `1` = 50, `2` = 100),
                                        law_harassment1 = recode_factor(law_harassment0, `0` = 0, `1` = 50, `2` = 100),
                                        law_violence1 = recode_factor(law_violence0, `0` = 0, `1` = 50, `2` = 100)) %>% 
  add_labels(law_stress1, labels = c(`No` = 0, `Yes, indirect` = 50, `Yes, direct` = 100)) %>% 
  add_labels(law_harassment1, labels = c(`No` = 0, `Yes, indirect` = 50, `Yes, direct` = 100)) %>% 
  add_labels(law_violence1, labels = c(`No` = 0, `Yes, indirect` = 50, `Yes, direct` = 100)) %>%
  as_numeric(.)

get_labels(policy_index$law_stress1, values = "n") # check

## Recoding indirect as "No": No` = 0, `Yes, indirect` = 0, `Yes, direct` = 100

policy_index <- policy_index %>% mutate(law_stress_no = recode_factor(law_stress0, `0` = 0, `1` = 0, `2` = 100),
                                        law_harassment_no = recode_factor(law_harassment0, `0` = 0, `1` = 0, `2` = 100),
                                        law_violence_no = recode_factor(law_violence0, `0` = 0, `1` = 0, `2` = 100)) %>% 
  add_labels(law_stress_no, labels = c(`No` = 0, `Yes` = 100)) %>% 
  add_labels(law_harassment_no, labels = c(`No` = 0, `Yes` = 100)) %>% 
  add_labels(law_violence_no, labels = c(`No` = 0, `Yes` = 100)) %>%
  as_numeric(.)

get_labels(policy_index$law_stress_no, values = "n") # check


## Recoding indirect as "Yes": No` = 0, `Yes, indirect` = 100, `Yes, direct` = 100

policy_index <- policy_index %>% mutate(law_stress_yes = recode_factor(law_stress0, `0` = 0, `1` = 100, `2` = 100),
                                        law_harassment_yes = recode_factor(law_harassment0, `0` = 0, `1` = 100, `2` = 100),
                                        law_violence_yes = recode_factor(law_violence0, `0` = 0, `1` = 100, `2` = 100)) %>% 
  add_labels(law_stress_yes, labels = c(`No` = 0, `Yes` = 100)) %>% 
  add_labels(law_harassment_yes, labels = c(`No` = 0, `Yes` = 100)) %>% 
  add_labels(law_violence_yes, labels = c(`No` = 0, `Yes` = 100)) %>%
  as_numeric(.)

get_labels(policy_index$law_stress_yes, values = "n") # check

save(policy_index, file = "./Data/Working_data/policy_index.rda") # r data
write.xlsx(policy_index, file = "./Data/Working_data/policy_index.xlsx") # excel file

# End data wrangling 

rm(list = ls()) # delete workspace to save memory
