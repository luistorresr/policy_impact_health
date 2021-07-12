
# Cleaning the datasets: ESENER 2015 and EWCS2015

## ESENER 2014 - To estimate organisational practices

### Load the original dataset

ESENER14_base <- read_spss("./Data/UKDA-8690-spss/spss/spss19/esener2_data_q100rev.sav")

l_ESENER14_base <- get_labels(ESENER14_base, values = "n") # value labels
q_ESENER14_base <- as.data.frame(label(ESENER14_base)) # questions

### Selecting items to estimate organisational practices

items_ESENER14 <- c(
  
  # merging variables
  
  "country",
  "Nace1", # 19 industry sectors identified by letters plus 99 = unknown
  "Nace1_16", # 16 industry sectors identified by letters
  "Sectorgrp", # 7 industry sector groups plus 9 = unknown
  
  # Organisational practices indicators
  
  "Q300", # Does your establishment have an action plan to prevent work-related stress? [If Q104 >19 and <99999]
  "Q301", # Is there a procedure in place to deal with possible cases of bullying or harassment? [If Q104 >19 and <99999]
  "Q302" # And is there a procedure to deal with possible cases of threats, abuse or assaults by clients, patients, pupils or other external persons? [If Q104 >19 and <99999 and Q201_5 = 1]
)

### selecting the variables from the original dataset

ESENER14_items <- ESENER14_base %>% select(all_of(items_ESENER14)) %>% as_tibble()

l_ESENER14_items <- get_labels(ESENER14_items, values = "n") # value labels
q_ESENER14_items <- as.data.frame(label(ESENER14_items)) # questions

write.xlsx(q_ESENER14_items, file = "./Data/Working_data/q_ESENER14.xlsx") # export questions to excel


### creating no answers as NA's only for items

ESENER14_items <- ESENER14_items %>% 
  set_na(na = c(Q300 = 9, Q301 = 9, Q302 = 9), drop.levels = TRUE, as.tag = FALSE) %>% 
  as_tibble(.)

### recoding variables (yes = 1, no = 0)

ESENER14_items$Q300 <- dplyr::recode_factor(ESENER14_items$Q300, `1` = 100, `2` = 0) %>% 
  replace_labels(ESENER14_items$Q300, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)
  
ESENER14_items$Q301 <- dplyr::recode_factor(ESENER14_items$Q301, `1` = 100, `2` = 0) %>% 
  replace_labels(ESENER14_items$Q301, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)

ESENER14_items$Q302 <- dplyr::recode_factor(ESENER14_items$Q302, `1` = 100, `2` = 0) %>% 
  replace_labels(ESENER14_items$Q302, labels = c("Yes" = 100, "No" = 0)) %>% as_numeric(., drop.levels = TRUE)

### Saving the resulting dataset for the data reduction step

save(ESENER14_items, file = "./Data/Working_data/ESENER14_items.rda") # r data
write.xlsx(ESENER14_items, file = "./Data/Working_data/ESENER14_items.xlsx") # excel file

rm(list = ls()) # delete workspace

#########################################################################################################

## EWCS2015 - for JDR model 

### loading the original dataset 

EWCS2015_base <- read_spss("./Data/UKDA-8098-spss/spss/spss19/ewcs6_2015_ukda_1904.sav")

l_EWCS2015_base <- get_labels(EWCS2015_base, values = "n") # value labels
q_EWCS2015_base <- as.data.frame(label(EWCS2015_base))

### items 

items_EWCS2015 <- c(
  
  # filtering variables
  
  "Q2c",
  "Q7",
  "Q24", 
  
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

### select variables from EWCS 2015

EWCS2015_items <- EWCS2015_base %>% select(all_of(items_EWCS2015)) %>% as_tibble()

q_EWCS2015_items <- as.data.frame(label(EWCS2015_items))

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


### Saving the resulting dataset for the data reduction step

save(EWCS2015_items, file = "./Data/Working_data/EWCS2015_items.rda") # r data

rm(list = ls()) # delete workspace

# End data wrangling 
