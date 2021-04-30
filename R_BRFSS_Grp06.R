# set up R (clear workspace)
rm(list = ls())
setwd("/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data/BRFSS/XPT Files/")

# Load Libraries
##load foreign to read SAS file
#install.packages("foreign")
library(foreign)
library(tidyverse)
# plyr allows you to combine data frames that do not have perfectly matching column names
library(plyr)
library(table1)

# load brfss data from CDC website
brfss_15 = read.xport("LLCP2015.XPT")
brfss_15_v1 = read.xport("LLCP15V1.XPT")
#brfss_15_v2 = read.xport("LLCP15V2.XPT")
#brfss_15_v3= read.xport("LLCP15V3.XPT")

brfss_16 = read.xport("LLCP2016.XPT")
#brfss_16_v1 = read.xport("LLCP16V1.XPT")
#brfss_16_v2 = read.xport("LLCP16V2.XPT")
#brfss_16_v3= read.xport("LLCP16V3.XPT")

brfss_17 = read.xport("LLCP2017.XPT")
#brfss_17_v1 = read.xport("LLCP17V1.XPT")
#brfss_17_v2 = read.xport("LLCP17V2.XPT")
#brfss_17_v3= read.xport("LLCP17V3.XPT")

brfss_18 = read.xport("LLCP2018.XPT")
#brfss_18_v1 = read.xport("LLCP18V1.XPT")
brfss_18_v2 = read.xport("LLCP18V2.XPT")
#brfss_18_v3= read.xport("LLCP18V3.XPT")

brfss_19 = read.xport("LLCP2019.XPT")
#brfss_19_v1 = read.xport("LLCP19V1.XPT")
#brfss_19_v2 = read.xport("LLCP19V2.XPT")
#brfss_19_v3= read.xport("LLCP19V3.XPT")

##See what states are in each version
#table(brfss_15$X_STATE)
#table(brfss_15_v1$X_STATE)
#table(brfss_15_v2$X_STATE)
#table(brfss_15_v3$X_STATE)

### Pull data from each year
# 2015
# brfss_15_sub <- brfss_15 %>% dplyr::select(study_id, xx)
# subset to just states that used this module from the original verision of the survey
brfss_15_sub <- brfss_15[brfss_15$X_STATE %in% c(8,9,10,13,15,16,17,18,
                                                 20,24,25,27,29,32,36,39,
                                                 42,48,51,54,55), ]
# Rename the final weight variable for the main data set
brfss_15_sub$finalwt <-brfss_15_sub$X_LLCPWT
# Choose the states that used this module in version 1 in 2015
brfss_15_sub_v1 <- brfss_15_v1[brfss_15_v1$X_STATE %in% c(19), ]
# Rename the final weight variable for the version 1 data set
brfss_15_sub_v1$finalwt <-brfss_15_sub_v1$X_LCPWTV1

# Combine the data sets
brfss15 <-rbind.fill(brfss_15_sub, brfss_15_sub_v1) 
# Confirmed rows: 196,141+2,972=199,113 rows
# NOTE SURE WHAT IS HAPPENING WITH columns? 
    #331 in brfss_15_sub (330 orig + 1 new finalwt)
    #330 in brfss_15_sub_v1 (329 orig + 1 new finalwt)
    #333 in combined brfss15 (2 new- must be non-overlaping?)

# 2016
# subset to just states that used this module 
# NOTE: only use original verision of the survey (no states used other versions)
brfss16 <- brfss_16[brfss_16$X_STATE %in% c(6,9,10,13,15,16,17,18,19,21,
                                                 22,25,27,28,29,32,36,39,42,
                                                 44,48,50,51,53,55,66), ]
# Rename the final weight variable 
brfss16$finalwt <-brfss16$X_LLCPWT

# 2017
# subset to just states that used this module 
# NOTE: only use original verision of the survey (no states used other versions)
brfss17 <- brfss_17[brfss_17$X_STATE %in% c(6,9,10,12,13,15,17,18,19,22,
                                            25,27,28,30,32,36,37,39,40,42,
                                            44,45,48,50,51,53,55,66), ]
# Rename the final weight variable 
brfss17$finalwt <-brfss17$X_LLCPWT

# 2018
# subset to just states that used this module from the original verision of the survey
brfss_18_sub <- brfss_18[brfss_18$X_STATE %in% c(9,10,12,15,16,17,20,22,
                                                 24,27,28,29,30,32,36,
                                                 37,39,40,42,44,45,47,
                                                 48,50,51,53,54,55,66), ]
# Rename the final weight variable for the main data set
brfss_18_sub$finalwt <-brfss_18_sub$X_LLCPWT
# Choose the states that used this module in version 2 in 2018
brfss_18_sub_v2 <- brfss_18_v2[brfss_18_v2$X_STATE %in% c(4), ]
# Rename the final weight variable for the version 2 data set
brfss_18_sub_v2$finalwt <-brfss_18_sub_v2$X_LCPWTV1
# Combine the data sets
brfss18 <-rbind.fill(brfss_18_sub, brfss_18_sub_v2) 
# Confirmed rows: 258,128+3,739=261,867 rows
# NOTE SURE WHAT IS HAPPENING WITH columns? 
#276 in brfss_18_sub (275 orig + 1 new finalwt)
#275 in brfss_18_sub_v2 (275 orig + 0??? new finalwt)
#278 in combined brfss18 (2 new- must be non-overlaping?)

#Recode variables
brfss18 <- brfss18 %>% dplyr::rename(SEX = SEX1)
brfss18 <- brfss18 %>% mutate(SXORIENT = case_when(SOMALE == 2 | SOFEMALE == 2 ~ 1,
                                                   SOMALE == 1 | SOFEMALE == 1 ~ 2,
                                                   SOMALE == 3 | SOFEMALE == 3 ~ 3,
                                                   SOMALE == 4 | SOFEMALE == 4 ~ 4,
                                                   SOMALE == 7 | SOFEMALE == 7 ~ 7,
                                                   SOMALE == 9 | SOFEMALE == 9 ~ 9))
#Check
table(brfss18$SXORIENT)
table(brfss18$SOMALE)
table(brfss18$SOFEMALE)

# 2019
# subset to just states that used this module 
# NOTE: only use original verision of the survey (no states used other versions)
brfss19 <- brfss_19[brfss_19$X_STATE %in% c(2,4,8,9,10,12,13,15,16,19,
                                            20,22,24,27,28,30,36,37,
                                            39,40,44,45,47,48,49,50,
                                            51,53,54,55,66), ]
# Rename the final weight variable 
brfss19$finalwt <-brfss19$X_LLCPWT
#Recode variables
brfss19 <- brfss19 %>% dplyr::rename(FLUSHOT6 = FLUSHOT7)
brfss19 <- brfss19 %>% dplyr::rename(SEX = BIRTHSEX)
brfss19 <- brfss19 %>% mutate(SXORIENT = case_when(SOMALE == 2 | SOFEMALE == 2 ~ 1,
                                                   SOMALE == 1 | SOFEMALE == 1 ~ 2,
                                                   SOMALE == 3 | SOFEMALE == 3 ~ 3,
                                                   SOMALE == 4 | SOFEMALE == 4 ~ 4,
                                                   SOMALE == 7 | SOFEMALE == 7 ~ 7,
                                                   SOMALE == 9 | SOFEMALE == 9 ~ 9))
#Check
table(brfss19$SXORIENT)
table(brfss19$SOMALE)
table(brfss19$SOFEMALE)

# Count overall observiations in data set to determine distribution for final sample
count_15 <- nrow(brfss15)
count_16 <- nrow(brfss16)
count_17 <- nrow(brfss17)
count_18 <- nrow(brfss18)
count_19 <- nrow(brfss19)

total <- count_15 + count_16 + count_17 + count_18 + count_19

prop_15<-count_15/total
prop_16<-count_16/total
prop_17<-count_17/total
prop_18<-count_18/total
prop_19<-count_19/total

# apply distribution to final weights
brfss15$finalwt <- brfss15$finalwt*prop_15
brfss16$finalwt <- brfss16$finalwt*prop_16
brfss17$finalwt <- brfss17$finalwt*prop_17
brfss18$finalwt <- brfss18$finalwt*prop_18
brfss19$finalwt <- brfss19$finalwt*prop_19


# combine all years
brfss <-rbind.fill (brfss15, brfss16, brfss17, brfss18, brfss19) 


###################### Data management ######################

######################TO DO#####################################

#Factorize variables

#Gender
#check TRNSGNDR
table(brfss$TRNSGNDR)
#create new transgender variable: recode 7 (Don't know/not sure) and 9 (Refused) to NA
brfss <- brfss %>% mutate(transgender = case_when(TRNSGNDR == 1 ~ 1, #Transfemale
                                                TRNSGNDR == 2 ~ 2, #Transmale
                                                TRNSGNDR == 3 ~ 3, #Non-binary
                                                TRNSGNDR == 4 ~ 4)) #Cisgender
brfss$transgender  <-  factor(brfss$transgender, 
                            levels = c(1, 2, 3, 4), 
                            labels=c("Transfemale", 
                                     "Transmale",
                                     "Non-binary",
                                     "Cisgender"))	
table(brfss$TRNSGNDR)
table(brfss$transgender)

#create new gender variable: recode 7 (Don't know/not sure) and 9 (Refused) to NA
brfss <- brfss %>% mutate(gender = case_when(TRNSGNDR == 4 & SEX == 1 ~ 1, #Cisgender male
                                             TRNSGNDR == 4 & SEX == 2 ~ 2, #Cisgender female
                                             TRNSGNDR == 2 ~ 3, #Transmale
                                             TRNSGNDR == 1 ~ 4, #Transfemale
                                             TRNSGNDR == 3 ~ 5)) #Non-binary
brfss$gender  <-  factor(brfss$gender, 
                              levels = c(1, 2, 3, 4, 5), 
                              labels=c("Cisgender Men",
                                       "Cisgender Women",
                                       "Transgender Men", 
                                       "Transgender Women",
                                       "Non-binary"))	
#Check
table(brfss$TRNSGNDR)
table(brfss$gender)
table(brfss$transgender)
## ************check this coding as 175,991 cisgender drop when combine with sex**********

#Sexual Orientation
#check SXORIENT
table(brfss$SXORIENT)
#create new sexorient: recode 7 (Don't know/not sure) and 9 (Refused) to NA
brfss <- brfss %>% mutate(sexorient = case_when(SXORIENT == 1 ~ 1, #Straight
                                                SXORIENT == 2 ~ 2, #Lesbian or gay
                                                SXORIENT == 3 ~ 3, #Bisexual
                                                SXORIENT == 4 ~ 4)) #Other
brfss$sexorient  <-  factor(brfss$sexorient, 
                                    levels = c(1, 2, 3, 4), 
                                    labels=c("Straight", 
                                             "Lesbian or gay",
                                             "Bisexual",
                                             "Other"))	
table(brfss$SXORIENT)
summary(brfss$SXORIENT)
table(brfss$sexorient)
summary(brfss$sexorient)

#Sex at Birth
#check SEX
table(brfss$SEX)
#create new sexbirth: recode 7 (Don't know/not sure) and 9 (Refused) to NA
brfss <- brfss %>% mutate(birthsex = case_when(SEX == 1 ~ 1, #Male
                                                SEX == 2 ~ 2)) #Female
brfss$birthsex  <-  factor(brfss$birthsex, 
                            levels = c(1, 2), 
                            labels=c("Male", 
                                     "Female"))	
table(brfss$SEX)
summary(brfss$SEX)
table(brfss$birthsex)
summary(brfss$birthsex)

#Label variables for Table 1
#table1::label(brfss$age_grp) <- "Age (years)"
table1::label(brfss$birthsex) <- "Sex at Birth"
table1::label(brfss$sexorient) <-"Sexual Orientation"
#*******continue********#

## Table 1
table1 <- table1(~ birthsex
#                + add all variables for table1
                 + sexorient| gender, data = brfss,
                 render.missing=NULL, render.categorical ="FREQ (PCTnoNA%)", overall= "Total")
table1 