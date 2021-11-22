
# 4 # filtering datasets

load(file = "./Data/Working_data/ESENER14_items.rda") 

load(file = "./Data/Working_data/EWCS2015_items.rda") 

load(file = "./Data/Working_data/policy_index.rda")



### filtering databases by size and employment criteria 

#### ESENER

ESENER14_filtered <- ESENER14_items %>% filter(size_esener != 1) # equal or more than 10 employees

ESENER14_filtered %>% group_by(country) %>% summarise(total = n())  # summary per country

ESENER14_filtered %>% nrow()

ESENER14_filtered %>% group_by(size_esener) %>% summarise(total = n()) 


#### EWCS 

EWCS2015_filtered <- EWCS2015_items %>% filter(size_ewcs != 1) # more than 10 employees company size

EWCS2015_filtered <- EWCS2015_filtered %>% filter(employment_status == 1
                                                    & worker_type == 1
                                                   # & hours >= 20 # not included
                                                  ) # in employment, employee, [20 or more hours]

EWCS2015_filtered %>% nrow()

EWCS2015_filtered %>% group_by(size_ewcs) %>% summarise(total = n())   # summary per country

EWCS2015_filtered %>% count(sex) %>% summarise(percentage = n / sum(n) * 100) # sex percentage

EWCS2015_filtered$sex

EWCS2015_filtered %>% filter(!is.na(age)) %>% summarise(mean_age = mean(age), sd_age = sd(age)) # sex percentage

### saving ready to link datasets

policy_ready <- policy_index

JDR_ready <- EWCS2015_filtered

practices_ready <- ESENER14_filtered

save(policy_ready, file = "./Data/Working_data/policy_ready.rda") # r data

save(JDR_ready, file = "./Data/Working_data/JDR_ready.rda") # r data for CFA

save(practices_ready, file = "./Data/Working_data/practices_ready.rda") # r data

# End data wrangling 

rm(list = ls())

