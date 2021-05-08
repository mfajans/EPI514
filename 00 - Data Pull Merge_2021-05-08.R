+##########################################
#EPI514 Project
#Coders: Collrane Frivold, Sarah Cox, Mark Fajans
#Data Pull & Merging, Unweighted Table 1
#Last Updated: 2021-05-08
##########################################


# set up R (clear workspace)
rm(list = ls())
#set memory limit
memory.limit(size=500000)
#Sarah's Directory
setwd("/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data/BRFSS/XPT Files/")
#Mark's directory
#setwd("~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data/Compiled Files")
#Collrane's directory
#setwd("C:/epi514/data/")


# Load Libraries
##load foreign to read SAS file
#install.packages("foreign")
library(foreign)
library(tidyverse)
# plyr allows you to combine data frames that do not have perfectly matching column names
library(plyr)
library(dplyr)
library(table1)
library(knitr)

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
brfss_15_sub <- brfss_15[brfss_15$X_STATE %in% c(8, #Colorado
                                                 9, #Connecticut
                                                 10,#Delaware 
                                                 13,#Georgia
                                                 15,#Hawaii
                                                 16,#Idaho
                                                 17,#Illinois
                                                 18,#Indiana
                                                 20,#Kansas
                                                 24,#Maryland
                                                 25,#Mass
                                                 27,#Minnesota
                                                 29,#Missouri
                                                 32,#Nevada
                                                 36,#NY
                                                 39,#Ohio
                                                 42,#Penn
                                                 48,#TX
                                                 51,#VA
                                                 54,#WV
                                                 55), ] #WI

#Checking states in subset
#table(brfss_15_sub$X_STATE)

# Rename the final weight variable for the main data set
brfss_15_sub$finalwt <-brfss_15_sub$X_LLCPWT

#Checking weight;
#wtchk15<- brfss_15_sub %>% select(finalwt, X_LLCPWT)
#wtchk15


# Choose the states that used this module in version 1 in 2015
brfss_15_sub_v1 <- brfss_15_v1[brfss_15_v1$X_STATE %in% c(19), ] #Iowa

#Checking states in V1 subset
#table(brfss_15_sub_v1$X_STATE)

# Rename the final weight variable for the version 1 data set
brfss_15_sub_v1$finalwt <-brfss_15_sub_v1$X_LCPWTV1

#Checking weight;
#wtchk15v1<- brfss_15_sub_v1 %>% select(finalwt, X_LCPWTV1)
#wtchk15v1


# Combine the data sets

#dim(brfss_15_sub)
#196141 rows 331 col

#dim(brfss_15_sub_v1)
#2972  rows 330 col


#Merge original and V1
brfss15 <-rbind.fill(brfss_15_sub, brfss_15_sub_v1) 

#dim(brfss15)
#199113    333

# Confirmed rows: 196,141+2,972=199,113 rows
# NOTE SURE WHAT IS HAPPENING WITH columns? 
#331 in brfss_15_sub (330 orig + 1 new finalwt)
#330 in brfss_15_sub_v1 (329 orig + 1 new finalwt)
#333 in combined brfss15 (2 new- must be non-overlaping?)

#Checking to see where the 2 extra variables are coming from
varor<-names(brfss_15_sub)
varv1<-names(brfss_15_sub_v1)
varmerge<-names(brfss15)

setdiff(varmerge, varv1)
#"MSCODE"    "X_CLLCPWT" "X_LLCPWT" 
setdiff(varmerge, varor)
#"X_CLCWTV1" "X_LCPWTV1"

#It looks like there there are 5 total variables here that are in one version 
#and not the other (3 in the original not in v1,
#2 vice versa (see above)), I think we'll probably end up dropping in our final analytical dataset!

#Create year variable for 2015 data 
brfss15$dataYear <- 2015

summary(brfss15$dataYear) #check 

# 2016
# subset to just states that used this module 
# NOTE: only use original verison of the survey (no states used other versions)

brfss16 <- brfss_16[brfss_16$X_STATE %in% c(6, #CA
                                            9, #CT
                                            10,#Delaware
                                            13,#GA
                                            15,#HI
                                            16,#ID
                                            17,#IL
                                            18,#Indiana
                                            19,#Iowa
                                            21,#Kentucky
                                            22,#LA
                                            25,#MA
                                            27,#MN
                                            28,#Mississippi
                                            29,#Missouri
                                            32,#NV
                                            36,#NY
                                            39,#OH
                                            42,#Penn
                                            44,#RI
                                            48,#TX
                                            50,#VT
                                            51,#VA
                                            53,#WA
                                            55,#WI
                                            66), ] #Guam




#Checking states in subset
#table(brfss16$X_STATE)


# Rename the final weight variable 
brfss16$finalwt <-brfss16$X_LLCPWT

#Checking weight creation
#wtchk16<- brfss16 %>% select(finalwt, X_LLCPWT)
#wtchk16

dim(brfss16)
#232881 rows    276 col

#Create year variable for 2016 data 
brfss16$dataYear <- 2016

summary(brfss16$dataYear) #check 

# 2017
# subset to just states that used this module 
# NOTE: only use original verision of the survey (no states used other versions)
brfss17 <- brfss_17[brfss_17$X_STATE %in% c(6, #CA
                                            9, #CT
                                            10,#Delaware
                                            12,#FL
                                            13,#GA
                                            15,#HI
                                            17,#IL
                                            18,#Indiana
                                            19,#Iowa
                                            22,#LA
                                            25,#MA
                                            27,#MN
                                            28,#Mississippi
                                            30,#Montana
                                            32,#NV
                                            36,#NY
                                            37,#NC
                                            39,#OH
                                            40,#OK
                                            42,#Penn
                                            44,#RI
                                            45,#SC
                                            48,#TX
                                            50,#VT
                                            51,#VA
                                            53,#WA
                                            55,#WI
                                            66), ] #Guam



#Checking states in subset
#table(brfss17$X_STATE)


# Rename the final weight variable 
brfss17$finalwt <-brfss17$X_LLCPWT

#Checking weight creation
#wtchk17<- brfss17 %>% select(finalwt, X_LLCPWT)
#wtchk17

#Create year variable for 2017 data 
brfss17$dataYear <- 2017

summary(brfss17$dataYear) #check 


# 2018
# subset to just states that used this module from the original verision of the survey
brfss_18_sub <- brfss_18[brfss_18$X_STATE %in% c(9, #CT
                                                 10,#Delaware
                                                 12,#FL
                                                 15,#HI
                                                 16,#ID
                                                 17,#IL
                                                 20,#KS
                                                 22,#LA
                                                 24,#Maryland
                                                 27,#MN
                                                 28,#Mississippi
                                                 29,#Missouri
                                                 30,#Montana
                                                 32,#NV
                                                 36,#NY
                                                 37,#NC
                                                 39,#OH
                                                 40,#OK
                                                 42,#Penn
                                                 44,#RI
                                                 45,#SC
                                                 47,#TN
                                                 48,#TX
                                                 50,#VT
                                                 51,#VA
                                                 53,#WA
                                                 54,#WV
                                                 55,#WI
                                                 66), ] #Guam

#Checking states in subset
#table(brfss_18_sub$X_STATE)

# Rename the final weight variable for the main data set
brfss_18_sub$finalwt <-brfss_18_sub$X_LLCPWT

#Checking weight creation
#wtchk18sub<- brfss_18_sub %>% select(finalwt, X_LLCPWT)
#wtchk18sub

# Choose the states that used this module in version 2 in 2018
brfss_18_sub_v2 <- brfss_18_v2[brfss_18_v2$X_STATE %in% c(4), ]#AZ
# Rename the final weight variable for the version 2 data set
brfss_18_sub_v2$finalwt <-brfss_18_sub_v2$X_LCPWTV2

#Checking weight creation
#wtchk18v2<- brfss_18_sub_v2 %>% select(finalwt, X_LCPWTV2)
#wtchk18v2



# Combine the data sets

dim(brfss_18_sub)
#258128 rows 276 col

dim(brfss_18_sub_v2)
#3739 rows  276 col



brfss18 <-rbind.fill(brfss_18_sub, brfss_18_sub_v2) 
# Confirmed rows: 258,128+3,739=261,867 rows
# NOTE SURE WHAT IS HAPPENING WITH columns? 
#276 in brfss_18_sub (275 orig + 1 new finalwt)
#275 in brfss_18_sub_v2 (275 orig + 0??? new finalwt)   - Fixed this, typo in the raw weight variable
#278 in combined brfss18 (2 new- must be non-overlaping?)

dim(brfss18)
#261867 rows 278 col

varor<-names(brfss_18_sub)
varv2<-names(brfss_18_sub_v2)
varmerge<-names(brfss18)

setdiff(varmerge, varv2)
#X_CLLCPWT" "X_LLCPWT" 
setdiff(varmerge, varor)
#"X_CLCWTV2" "X_LCPWTV2"

#It looks like there there are 4 total variables here that are in one version and not the other (2 in the original not in v1,
#2 vice versa (see above)), I think we'll probably end up dropping in our final analytical dataset



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

table(brfss18$SOMALE,brfss18$SXORIENT)
table(brfss18$SOFEMALE,brfss18$SXORIENT)

#Create year variable for 2018 data 
brfss18$dataYear <- 2018

summary(brfss18$dataYear) #check 


# 2019
# subset to just states that used this module 
# NOTE: only use original version of the survey (no states used other versions)
brfss19 <- brfss_19[brfss_19$X_STATE %in% c(2, #AK
                                            4, #AZ
                                            8, #CO
                                            9, #CT
                                            10,#Delaware
                                            12,#FL
                                            13,#GA
                                            15,#HI
                                            16,#ID
                                            19,#IA
                                            20,#KS
                                            22,#LA
                                            24,#Maryland
                                            27,#MN
                                            28,#Mississippi
                                            30,#MN
                                            36,#NY
                                            37,#NC
                                            39,#OH
                                            40,#OK
                                            44,#RI
                                            45,#SC
                                            47,#TN
                                            48,#TX
                                            49,#UT
                                            50,#VT
                                            51,#VA
                                            53,#WA
                                            54,#WV
                                            55,#WI
                                            66), ] #Guam



# Rename the final weight variable 
brfss19$finalwt <-brfss19$X_LLCPWT

#Checking weight creation
wtchk19<- brfss19 %>% select(finalwt, X_LLCPWT)
wtchk19

#Recode variables

#Check sex variable
table(brfss_19$BIRTHSEX, useNA = "always")
table(brfss19$BIRTHSEX, useNA = "always")
table(brfss_19$SEXVAR, useNA = "always")
table(brfss19$SEXVAR, useNA = "always")
#Use SEXVAR (NOT BIRTHSEX)
brfss19 <- brfss19 %>% dplyr::rename(SEX = SEXVAR)
table(brfss19$SEX, useNA = "always")

brfss19 <- brfss19 %>% dplyr::rename(FLUSHOT6 = FLUSHOT7)
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

table(brfss19$SOMALE,brfss19$SXORIENT)
table(brfss19$SOFEMALE,brfss19$SXORIENT)

# Count overall observations in data set to determine distribution for final sample
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

#Create year variable for 2019 data 
brfss19$dataYear <- 2019

summary(brfss19$dataYear) #check 


# combine all years
dim(brfss15)
#199113    334
dim(brfss16)
#232881    277
dim(brfss17)
#239173    360
dim(brfss18)
#261867    280
dim(brfss19)
#266291    345

brfss <-rbind.fill (brfss15, brfss16, brfss17, brfss18, brfss19) 
dim(brfss)
#1199325     609

###################### Data management ######################
#Rename 
# Rename the final weight variable for the main data set
brfss$ststr <- brfss$X_STSTR 
brfss$psu <- brfss$X_PSU

#Factorize variables

#Gender
#check TRNSGNDR
table(brfss$TRNSGNDR, useNA = "always")
#create new transgender variable
brfss <- brfss %>% mutate(transgender = case_when(TRNSGNDR == 1 ~ 1, #Transfemale
                                                  TRNSGNDR == 2 ~ 2, #Transmale
                                                  TRNSGNDR == 3 ~ 3, #Non-binary
                                                  TRNSGNDR == 4 ~ 4, #Cisgender
                                                  TRNSGNDR == 7 ~ 7, #Don't Know
                                                  TRNSGNDR == 9 ~ 9)) #Refused

brfss$transgender  <-  factor(brfss$transgender, 
                              levels = c(1, 2, 3, 4, 7, 9), 
                              labels=c("Transfemale", 
                                       "Transmale",
                                       "Non-binary",
                                       "Cisgender",
                                       "Don't Know",
                                       "Refused"))	
table(brfss$TRNSGNDR)
table(brfss$transgender)
table(brfss$transgender, brfss$TRNSGNDR)


#create new gender variable: with cisgender male vs female
brfss <- brfss %>% mutate(gender = case_when(TRNSGNDR == 4 & SEX == 1 ~ 1, #Cisgender male
                                             TRNSGNDR == 4 & SEX == 2 ~ 2, #Cisgender female
                                             TRNSGNDR == 2 ~ 3, #Transmale
                                             TRNSGNDR == 1 ~ 4, #Transfemale
                                             TRNSGNDR == 3 ~ 5,#Non-binary
                                             TRNSGNDR == 7 ~ 7, #Don't Know
                                             TRNSGNDR == 9 ~ 9)) #Refused

brfss$gender  <-  factor(brfss$gender, 
                         levels = c(1, 2, 3, 4, 5, 7, 9), 
                         labels=c("Cisgender Men",
                                  "Cisgender Women",
                                  "Transgender Men", 
                                  "Transgender Women",
                                  "Non-binary",
                                  "Don't Know",
                                  "Refused"))
#Check
table(brfss$TRNSGNDR)
table(brfss$gender, useNA = "always")
table(brfss$transgender)

table(brfss$gender, brfss$TRNSGNDR, brfss$SEX, useNA = "always", deparse.level = 2)

#create new gender_re variable: recode 7 (Don't know/not sure) and 9 (Refused) to NA
brfss <- brfss %>% mutate(gender_re = case_when(TRNSGNDR == 4 & SEX == 1 ~ 1, #Cisgender male
                                                TRNSGNDR == 4 & SEX == 2 ~ 2, #Cisgender female
                                                TRNSGNDR == 2 ~ 3, #Transmale
                                                TRNSGNDR == 1 ~ 4, #Transfemale
                                                TRNSGNDR == 3 ~ 5)) #Non-binary
brfss$gender_re  <-  factor(brfss$gender_re, 
                            levels = c(1, 2, 3, 4, 5), 
                            labels=c("Cisgender Men",
                                     "Cisgender Women",
                                     "Transgender Men", 
                                     "Transgender Women",
                                     "Non-binary"))	

#create new gender_re2 variable: use gender_re and then combine cisgender male and female 
brfss <- brfss %>% mutate(gender_re2 = case_when(TRNSGNDR == 4 ~ 1, #Cisgender 
                                                TRNSGNDR == 2 ~ 2, #Transmale
                                                TRNSGNDR == 1 ~ 3, #Transfemale
                                                TRNSGNDR == 3 ~ 4)) #Non-binary
brfss$gender_re2  <-  factor(brfss$gender_re2, 
                            levels = c(1, 2, 3, 4), 
                            labels=c("Cisgender",
                                     "Transgender Men", 
                                     "Transgender Women",
                                     "Non-binary"))	
#Check
table(brfss$TRNSGNDR)
table(brfss$transgender)
table(brfss$gender_re, useNA = "always") #NA=162244 
table(brfss$gender_re2, useNA = "always") #NA=161678 
#Why does gender_re2 have less missingness? Likely because some missing SEX
table(brfss15$SEX, useNA = "always") #NA=0
table(brfss16$SEX, useNA = "always") #NA=0, 9= 15
table(brfss17$SEX, useNA = "always") #NA=0, 9= 141
table(brfss18$SEX, useNA = "always") #NA=0, 7=333, 9= 431
table(brfss19$SEX, useNA = "always") #NA=0
# 15+141+333+431 = 920
# 162244 - 161678 = 566
# ****************************************WHY 920 vs. 566?******************************************************
table(brfss$gender_re, brfss$TRNSGNDR, brfss$SEX, useNA = "always", deparse.level = 2)

#It looks like it has something to do with participants reporting "No"(4) for TRNSGNDR but NA for SEX

## ************check this coding as 566 cisgender drop when combine with sex**********

#Collrane: I think it has to do with how to question changed over time. It seems like NA is only an option for the 2019 dataset
#also do we need a dataset year variable? Right now we just have IYEAR which corresponds to the year of the interview

#table(brfss$TRNSGNDR, brfss$SEX, brfss$IYEAR, useNA = "always", deparse.level = 2)

#Sarah: now that we fixed SEX variable, NA is never an option but...
#2016-2018 all have 9 = refused and 2018 also has 7= Don't Know/Not Sure. Stull these #s don't line up




#Sexual Orientation
#check SXORIENT
table(brfss$SXORIENT)
table(brfss$SXORIENT, useNA = "always")
#create new sexorient: 
brfss <- brfss %>% mutate(sexorient = case_when(SXORIENT == 1 ~ 1, #Straight
                                                SXORIENT == 2 ~ 2, #Lesbian or gay
                                                SXORIENT == 3 ~ 3, #Bisexual
                                                SXORIENT == 4 ~ 4, #Other
                                                SXORIENT == 7 ~ 7, #Don't Know/ not sure
                                                SXORIENT == 9 ~ 9)) #Refused

#Code Missing to show up in table since >5%
brfss$sexorient[is.na(brfss$sexorient)] <- 999
brfss$sexorient  <-  factor(brfss$sexorient, 
                            levels = c(1, 2, 3, 4, 7, 9, 999),
                            labels=c("Straight", 
                                     "Lesbian or gay",
                                     "Bisexual",
                                     "Other",
                                     "Don't Know/ not sure",
                                     "Refused",
                                     "Missing"))	
table(brfss$SXORIENT, useNA = "always")
table(brfss$sexorient, useNA = "always")



#create new sexorient_re: recode 7 (Don't know/not sure) and 9 (Refused) to NA
# this has >5% missingness so don't use
brfss <- brfss %>% mutate(sexorient_re = case_when(SXORIENT == 1 ~ 1, #Straight
                                                   SXORIENT == 2 ~ 2, #Lesbian or gay
                                                   SXORIENT == 3 ~ 3, #Bisexual
                                                   SXORIENT == 4 ~ 4)) #Other
brfss$sexorient_re  <-  factor(brfss$sexorient_re, 
                               levels = c(1, 2, 3, 4), 
                               labels=c("Straight", 
                                        "Lesbian or gay",
                                        "Bisexual",
                                        "Other"))	
table(brfss$SXORIENT,  useNA = "always")
table(brfss$sexorient_re,  useNA = "always")



#Sex at Birth
#check SEX
table(brfss$SEX, useNA = "always")
#create new sexbirth: 
brfss <- brfss %>% mutate(birthsex = case_when(SEX == 1 ~ 1, #Male
                                               SEX == 2 ~ 2, #Female
                                               SEX == 7 ~ 7, #Don't Know/ not sure
                                               SEX == 9 ~ 9)) #Refused
table(brfss$birthsex, useNA = "always")
#<5% missing

#create new sexbirth_re: recode 7 (Don't know/not sure) and 9 (Refused) to NA
# this has <5% missingness so ok to use
brfss <- brfss %>% mutate(birthsex_re = case_when(SEX == 1 ~ 1, #Male
                                                  SEX == 2 ~ 2)) #Female
brfss$birthsex_re  <-  factor(brfss$birthsex_re, 
                              levels = c(1, 2), 
                              labels=c("Male", 
                                       "Female"))	
table(brfss$SEX, useNA = "always")
#Checking missingness
table(brfss$birthsex_re, useNA = "always")
# Male    Female     <NA> 
# 532417  665988    920



#BMI Category - 5.2.21 Mark

table(brfss$X_BMI5CAT, useNA = "always")

brfss<-brfss %>% mutate(bmi_cat = case_when(X_BMI5CAT==1 ~ 1, #Underweight
                                            X_BMI5CAT==2 ~ 2, #Normal Weight
                                            X_BMI5CAT==3 ~ 3, #Overweight
                                            X_BMI5CAT==4 ~ 4  #Obese  
))

brfss$bmi_cat  <-  factor(brfss$bmi_cat, 
                          levels = c(1, 2, 3, 4), 
                          labels=c("Underweight", 
                                   "Normal Weight",
                                   "Overweight",
                                   "Obese"))	

#Checking variable creation
table(brfss$X_BMI5CAT, brfss$bmi_cat, useNA = "always")

#Checking missingness
table(brfss$bmi_cat, useNA="always")
# Underweight   Normal Weight    Overweight         Obese          <NA> 
# 18738        344632            393591             340443         101921 
#>5% missingness so dont re-code


#Household income
table(brfss$X_INCOMG, useNA = "always")
#1      2      3      4      5      9           <NA> 
#93606  160241 104248 137881 496053 207296      0 
#Need to recode 9 as NA

brfss<-brfss %>% mutate(hsld_inc = case_when(X_INCOMG==1 ~ 1, #<15,000
                                             X_INCOMG==2 ~ 2, #15,000 - <25,000 
                                             X_INCOMG==3 ~ 3, #25,000 - <35,000
                                             X_INCOMG==4 ~ 4, #35,000 - <50,000
                                             X_INCOMG==5 ~ 5, #50,000+
                                             X_INCOMG==9 ~ 9 #Don't Know/Not Sure/Missing
))        

brfss$hsld_inc  <-  factor(brfss$hsld_inc, 
                           levels = c(1, 2, 3, 4, 5, 9), 
                           labels=c("<15,000", 
                                    "15,000 - <25,000",
                                    "25,000 - <35,000",
                                    "35,000 - <50,000",
                                    "50,000+",
                                    "Don't Know/Not Sure/Missing"))	

#Checking variable creation
table(brfss$X_INCOMG, brfss$hsld_inc, useNA = "always")

#Checking missingness
table(brfss$hsld_inc, useNA = "always")

#Recoded missing version
brfss<-brfss %>% mutate(hsld_inc_re = case_when(X_INCOMG==1 ~ 1, #<15,000
                                                X_INCOMG==2 ~ 2, #15,000 - <25,000 
                                                X_INCOMG==3 ~ 3, #25,000 - <35,000
                                                X_INCOMG==4 ~ 4, #35,000 - <50,000
                                                X_INCOMG==5 ~ 5, #50,000+
))   

brfss$hsld_inc_re[brfss$X_INCOMG==9]<-NA

brfss$hsld_inc_re  <-  factor(brfss$hsld_inc, 
                              levels = c(1, 2, 3, 4, 5), 
                              labels=c("<15,000", 
                                       "15,000 - <25,000",
                                       "25,000 - <35,000",
                                       "35,000 - <50,000",
                                       "50,000+"
                              ))


#Checking variable creation
table(brfss$X_INCOMG, brfss$hsld_inc_re, useNA = "always")

#Checking missingness
table(brfss$hsld_inc_re, useNA = "always")

#1     2      3      4      5      <NA> 
#93606 160241 104248 137881 496053 207296 
#>5% missingness so dont use re-coded version

#Education level - 5.3.21 Mark
table(brfss$X_EDUCAG, useNA="always")


#1      2      3      4        9        <NA> 
#89224  326726 325049 453365   4961      0 

#9 needs to be recoded as missing, <5%

brfss<-brfss %>% mutate(edu_cat = case_when(X_EDUCAG==1 ~ 1, #Less than a high school degree
                                            X_EDUCAG==2 ~ 2, #High school degree
                                            X_EDUCAG==3 ~ 3, #Some college but no degree
                                            X_EDUCAG==4 ~ 4 #College Degree
))        
brfss$edu_cat[brfss$X_EDUCAG==9]<-NA

brfss$edu_cat  <-  factor(brfss$edu_cat, 
                          levels = c(1, 2, 3, 4), 
                          labels=c("Less than a high school degree", 
                                   "High school degree",
                                   "Some college but no degree",
                                   "College Degree"
                          ))	

#Checking variable creation
table(brfss$X_EDUCAG, brfss$edu_cat, useNA = "always")

#Checking missingness
table(brfss$edu_cat, useNA = "always")

#Less than a high school degree             High school degree     Some college but no degree 
#89224                                      326726                 325049 

#College Degree                           <NA> 
#453365                                   4961 



#State;
table(brfss$X_STATE, useNA="always")

brfss$state<-brfss$X_STATE

table(brfss$X_STATE, brfss$state, useNA="always")

brfss$state  <-  factor(brfss$state, 
                        levels = c(2, 4, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21,
                                   22, 24, 25, 27, 28, 29, 30, 32, 36, 37, 39, 40, 42, 44,
                                   45, 47, 48, 49, 50, 51, 53, 54, 55, 66), 
                        labels=c("Alaska",
                                 "Arizona",
                                 "California",
                                 "Colorado",
                                 "Connecticut",
                                 "Delaware",
                                 "Florida",
                                 "Georgia",
                                 "Hawaii",
                                 "Idaho",
                                 "Illinois",
                                 "Indiana",
                                 "Iowa",
                                 "Kansas",
                                 "Kentucky",
                                 "Louisiana",
                                 "Maryland",
                                 "Massachusetts",
                                 "Minnesota",
                                 "Mississippi",
                                 "Missouri",
                                 "Montana",
                                 "Nevada",
                                 "New York",
                                 "North Carolina",
                                 "Ohio",
                                 "Oklahoma",
                                 "Pennsylvania",
                                 "Rhode Island",
                                 "South Carolina",
                                 "Tennessee",
                                 "Texas",
                                 "Utah",
                                 "Vermont",
                                 "Virginia",
                                 "Washington",
                                 "West Virginia",
                                 "Wisconsin",
                                 "Guam"))	

table(brfss$state, useNA="always")


#Health coverage

table(brfss$HLTHPLN1, useNA = "always")
#1         2        7       9          <NA> 
#1100134   94037    3097    2050       7 

#Recode 7 & 9s as missing, 0.4% of responses

brfss<-brfss %>% mutate(health_coverage_re = case_when(HLTHPLN1==1 ~ 1, #Yes
                                                    HLTHPLN1==2 ~ 2, #No
))        
brfss$health_coverage_re[brfss$HLTHPLN1==7]<-NA
brfss$health_coverage_re[brfss$HLTHPLN1==9]<-NA

brfss$health_coverage_re  <-  factor(brfss$health_coverage_re, 
                                  levels = c(1,2),
                                  labels = c("Yes",
                                             "No"))

table(brfss$HLTHPLN1,brfss$health_coverage_re, useNA="always")

table(brfss$health_coverage_re, useNA="always")

#Any healthcare coverage  No healthcare coverage    <NA> 
#1100134                  94037                    5154 


#Medical cost

table(brfss$MEDCOST, useNA = "always")

#1       2          7        9       <NA> 
#122240  1072860    2576     778     871 

#Recode 7 & 9 as NA, combo is 0.35%

brfss<-brfss %>% mutate(medcost_re = case_when(MEDCOST==1 ~ 1, #Yes
                                            MEDCOST==2 ~ 2, #No
)) 

brfss$medcost_re[brfss$MEDCOST==7]<-NA
brfss$medcost_re[brfss$MEDCOST==9]<-NA



table(brfss$MEDCOST, brfss$medcost_re, useNA = "always")

brfss$medcost_re  <-  factor(brfss$medcost_re, 
                          levels = c(1,2),
                          labels = c("Could not see doctor due to cost",
                                     "No cost barriers"))

table(brfss$MEDCOST, brfss$medcost_re, useNA = "always")

table(brfss$medcost, useNA="always")

#Could not see doctor due to cost    No cost barriers   <NA> 
#122240                              1072860            4225 


# Health access < 5% missing ok to recode
table(brfss$MEDCOST, useNA="always")
brfss <- brfss %>% mutate(medcost_re = case_when(MEDCOST == 1 ~ 1, #Yes
                                                 MEDCOST == 2 ~ 2)) #No

brfss$medcost_re<-factor(brfss$medcost_re,
                         levels=c(1, 2),
                         labels=c("Yes",
                                  "No"))
table(brfss$MEDCOST, useNA="always")
table(brfss$medcost_re, useNA="always")


#Employment status

table(brfss$EMPLOY1, useNA="always")
#1      2       3      4      5      6     7       8      9       <NA> 
#495287 101502  24992  23974  60818  32004 360831  86693  10486   2738 
brfss<-brfss %>% mutate(employ_recat= case_when(EMPLOY1==1 ~ 1, #Employed for wages
                                                    EMPLOY1==2 ~ 1, #Self-employed
                                                    EMPLOY1==3 ~ 2, #Out of work for 1 year or more
                                                    EMPLOY1==4 ~ 2, #Out of work for <1 year
                                                    EMPLOY1==5 ~ 3, #Homemaker
                                                    EMPLOY1==6 ~ 4, #Student
                                                    EMPLOY1==7 ~ 5, #Retired
                                                    EMPLOY1==8 ~ 5, #Unable to work
                                                    EMPLOY1==9 ~ 9)) #Refused
brfss$employ_recat <-  factor(brfss$employ_recat, 
                                 levels = c(1,2,3,4,5,9),
                                 labels = c("Employed/Self-Employed",
                                            "Unemployed",
                                            "Homemaker",
                                            "Student",
                                            "Retired/Unable to work",
                                            "Refused"))
table(brfss$employ_recat, useNA="always")
#NA & Refused together make up only 1.1%, 

brfss<-brfss %>% mutate(employ_recat_re = case_when(EMPLOY1==1 ~ 1, #Employed for wages
                                                 EMPLOY1==2 ~ 1, #Self-employed
                                                 EMPLOY1==3 ~ 2, #Out of work for 1 year or more
                                                 EMPLOY1==4 ~ 2, #Out of work for <1 year
                                                 EMPLOY1==5 ~ 3, #Homemaker
                                                 EMPLOY1==6 ~ 4, #Student
                                                 EMPLOY1==7 ~ 5, #Retired
                                                 EMPLOY1==8 ~ 5, #Unable to work
)) 

brfss$employ_recat_re[brfss$EMPLOY1==9]<-NA

brfss$employ_recat_re <-  factor(brfss$employ_recat_re, 
                              levels = c(1,2,3,4,5),
                              labels = c("Employed/Self-Employed",
                                         "Unemployed",
                                         "Homemaker",
                                         "Student",
                                         "Retired/Unable to work"
                              ))



table(brfss$employ_recat_re, brfss$EMPLOY1, useNA="always")

table(brfss$employ_recat_re, useNA="always")

#Employed/Self-Employed  Unemployed   Homemaker   Student 
#596789                  48966        60818       32004 

#Retired/Unable to work   <NA> 
#447524                   13224 


#Poor mental health status 

table(brfss$MENTHLTH,useNA="always")

#1      2      3      4      5      6      7      8      9     10     11     12     13     14     15     16 
#38483  58253  34881  17851  42640  4919  16646  3447    599  31483   224    2364   351    6184   29156  590 

#17     18     19     20     21     22     23     24     25     26     27     28     29     30     77     88 
#403    626    89     17894  1154   358    208    245    6153   248    474    1809   1099   65057  15725  793694 

#99       <NA> 
#6003     15 

#Combination of don't know (77), refused (99) and NA make up 1.8% missingness, OK to recode

brfss<-brfss %>% mutate(mentalhlth_re = case_when((MENTHLTH>=14 & MENTHLTH<=30) ~ 1, #>=14 days 
                                               (MENTHLTH<14 | MENTHLTH==88)  ~ 2 #<14 days
))

brfss$mentalhlth_re[brfss$MENTHLTH==77]<-NA                                                 
brfss$mentalhlth_re[brfss$MENTHLTH==99]<-NA



brfss$mentalhlth_re <-  factor(brfss$mentalhlth_re, 
                            levels = c(1,2),
                            labels = c(">=14 days",
                                       "<14 days"))


table(brfss$MENTHLTH, brfss$mentalhlth_re, useNA="always")
table(brfss$mentalhlth_re, useNA="always")

# >=14 days  <14 days      <NA> 
# 131747     1045835       21743 

#Urban vs Rural
table(brfss15$MSCODE, useNA = "always")
table(brfss16$MSCODE, useNA = "always")
table(brfss17$MSCODE, useNA = "always")
table(brfss18$MSCODE, useNA = "always")
table(brfss19$MSCODE, useNA = "always")

table(brfss$MSCODE, useNA = "always")

# 1      2      3     5      <NA> 
# 188437 97964  84188 152387 676349 

#56.4% missing, don't use

#Age Category

table(brfss$X_AGE_G, useNA = "always")

# 1     2      3      4      5      6        <NA> 
# 69331 124585 139839 188309 255492 421769    0 

brfss$age_grp<-brfss$X_AGE_G

brfss$age_grp<-factor(brfss$age_grp,
                      levels=c(1:6),
                      labels=c("18-24",
                               "25-34",
                               "35-44",
                               "45-54",
                               "55-64",
                               "65+"
                      ))

table(brfss$age_grp, brfss$X_AGE_G, useNA = "always")

table(brfss$age_grp)
#18-24  25-34  35-44  45-54  55-64    65+ 
#69331  124585 139839 188309 255492   421769 

# Year
table(brfss$dataYear, useNA="always")
brfss$dataYear_cat<-brfss$dataYear
brfss$dataYear_cat<-factor(brfss$dataYear_cat,
                           levels=c(2015, 2016, 2017, 2018, 2019),
                           labels=c("2015",
                                    "2016",
                                    "2017",
                                    "2018",
                                    "2019"))
table(brfss$dataYear, useNA="always")
table(brfss$dataYear_cat, useNA="always")
#2015   2016   2017   2018   2019   <NA> 
#199113 232881 239173 261867 266291      0 

# Fluvax
table(brfss$FLUSHOT6, useNA="always")
# 7.9% 7/9/missing
#1      2      7      9   <NA> 
#  505290 598090   3577   2226  90142 
brfss$fluvax<-brfss$FLUSHOT6
brfss$fluvax<-factor(brfss$fluvax,
                     levels=c(1, 2, 7, 9),
                     labels=c("Yes",
                              "No",
                              "Maybe",
                              "Refused"))
table(brfss$FLUSHOT6, useNA="always")
table(brfss$fluvax, useNA="always")
#(3577+2226+90142)/(505290+598090+3577+2226+90142)
# ~8% missing, but recode bc this is outcome variable
brfss <- brfss %>% mutate(fluvax_re = case_when(FLUSHOT6 == 1 ~ 1, #Yes
                                                FLUSHOT6 == 2 ~ 2)) #No
brfss$fluvax_re  <-  factor(brfss$fluvax_re, 
                               levels = c(1, 2), 
                               labels=c("Yes", 
                                        "No"))	
#Check
table(brfss$FLUSHOT6,  useNA = "always")
table(brfss$fluvax,  useNA = "always")
table(brfss$fluvax_re,  useNA = "always")


table(brfss$gender_re, brfss$fluvax, useNA = "always")
table(brfss$gender_re, brfss$fluvax_re, useNA = "always")
table(brfss$gender_re2, brfss$fluvax, useNA = "always")
table(brfss$gender_re2, brfss$fluvax_re, useNA = "always")

table(brfss$gender_re, useNA = "always")
table(brfss$gender_re2, useNA = "always")

# Gender Equality Tally
brfss <- brfss %>% mutate(equality = case_when(state == "Alaska" ~ 0,
                                               state == "Arizona" ~ 1,
                                               state == "California" ~ 0,
                                               state == "Colorado" ~ 0,
                                               state == "Connecticut" ~ 0,
                                               state == "Delaware" ~ 0,
                                               state == "Florida" ~ 1,
                                               state == "Georgia" ~ 1,
                                               state == "Hawaii" ~ 0,
                                               state == "Idaho" ~ 1,
                                               state == "Illinois" ~ 0,
                                               state == "Indiana" ~ 1,
                                               state == "Iowa" ~ 0,
                                               state == "Kansas" ~ 1,
                                               state == "Kentucky" ~ 1,
                                               state == "Louisiana" ~ 1,
                                               state == "Maryland" ~ 0,
                                               state == "Massachusetts" ~ 0,
                                               state == "Minnesota" ~ 0,
                                               state == "Mississippi" ~ 1,
                                               state == "Missouri" ~ 1,
                                               state == "Montana" ~ 1,
                                               state == "Nevada" ~ 0,
                                               state == "New York" ~ 0,
                                               state == "North Carolina" ~ 1,
                                               state == "Ohio" ~ 1,
                                               state == "Oklahoma" ~ 1,
                                               state == "Pennsylvania" ~ 0,
                                               state == "Rhode Island" ~ 0,
                                               state == "South Carolina" ~ 1,
                                               state == "Tennessee" ~ 1,
                                               state == "Texas" ~ 1,
                                               state == "Utah" ~ 0,
                                               state == "Vermont" ~ 0,
                                               state == "Virginia" ~ 0,
                                               state == "Washington" ~ 0,
                                               state == "West Virginia" ~ 1,
                                               state == "Wisconsin" ~ 1,
                                               state == "Guam" ~ 1))

brfss$equality<-factor(brfss$equality,
                       levels=c(0, 1),
                       labels=c("Protective",
                                "Restrictive"))
table(brfss$equality, useNA="always")
#Protective    Restrictive        <NA> 
#  674213      525112              0


#Race

#Add labels to race variable
brfss$race <- factor(brfss$X_RACE,
                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                     labels = c("White, Non-Hispanic", 
                                "Black, Non-Hispanic",
                                "American Indian or Alaskan Native, Non-Hispanic",
                                "Asian, non-Hispanic",
                                "Native Hawaiian or other Pacific Islander",
                                "Other race, Non-Hispanic",
                                "Multiracial, Non-Hispanic",
                                "Hispanic",
                                "Don't know/Not sure/Refused"))

#Check variable creation
table(brfss$X_RACE, brfss$race, useNA = "always")

#Check missingness
table(brfss$race, useNA = "always")

#White_nh            Black_nh             AIAN_nh            Asian_nh 
#897508               98774               14484               34533 
#NHOPI_nh            Other_nh      Multiracial_nh            Hispanic 
#6902                7373               27356               88613 
#Don't know/not sure                <NA> 
#              23779                   3 

#Don't know/not sure cat is 1.79% -> change to NA 
brfss$race_re <- NA
brfss <- brfss %>% mutate(race_re = case_when(X_RACE == 1 ~ 1, 
                                              X_RACE == 2 ~ 2,
                                              X_RACE == 3 ~ 3, 
                                              X_RACE == 4 ~ 4,
                                              X_RACE == 5 ~ 5,
                                              X_RACE == 6 ~ 6,
                                              X_RACE == 7 ~ 7,
                                              X_RACE == 8 ~ 8)) 
brfss$race_re <- factor(brfss$race_re,
                     levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                     labels = c("White, Non-Hispanic", 
                                "Black, Non-Hispanic",
                                "American Indian or Alaskan Native, Non-Hispanic",
                                "Asian, non-Hispanic",
                                "Native Hawaiian or other Pacific Islander",
                                "Other race, Non-Hispanic",
                                "Multiracial, Non-Hispanic",
                                "Hispanic"))
table(brfss$race_re, useNA = "always")


#Preferred race variable 

table(brfss$X_PRACE1,useNA="always") #check 

#2.94% missingness for don't know/not sure; refused; and NA


#Add labels to race variable
brfss$prace <- factor(brfss$X_PRACE1,
                      levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                      labels = c("White", 
                                 "Black",
                                 "American Indian or Alaskan Native",
                                 "Asian",
                                 "Native Hawaiian or other Pacific Islander",
                                 "Other race",
                                 "No preferred race",
                                 "Multiracial but preferred race not answered"))

brfss$prace[brfss$X_PRACE1==77]<-NA #Change don't know/not sure to NA                                                 
brfss$prace[brfss$X_PRACE1==99]<-NA #missing


#Check variable creation
table(brfss$X_PRACE1, brfss$prace, useNA = "always")

#Check missingness
table(brfss$prace, useNA = "always")

#cross-tab preferred race and imputted race/ethnicity 
table(brfss$prace, brfss$race, useNA = "always")


#########################
#Save abbreviated dataset 
#########################

#Drop variables we don't need
brfss2015_2019 <-  brfss %>% dplyr::select(psu, ststr, state, finalwt,
                                           gender_re, gender_re2,
                                           fluvax, fluvax_re, 
                                           equality,
                                           birthsex_re,
                                           sexorient,
                                           age_grp,
                                           race, race_re,
                                           bmi_cat,
                                           edu_cat,
                                           hsld_inc, 
                                           employ_recat_re,
                                           health_coverage_re,
                                           medcost_re,
                                           mentalhlth_re, 
                                           dataYear_cat)
                                           
#Tried this, but code isn't working
#brfss2015_2019 <- brfss[, c("psu", "ststr", "state", "finalwt", "MSCODE",
#                            "fluvax",
#                            "birthsex",
#                            "sexorient",
#                            "age_grp",
#                            "race", "prace",
#                            "bmi_cat",
#                            "edu_cat",
#                            "hsld_inc",
#                            "employ_recat",
#                            "health_coverage",
#                            "medcost_re",
#                            "mentalhlth", 
#                            "dataYear_cat", "equality",
#                            "gender_re")]

glimpse(brfss2015_2019) #check

#Drop anyone without our exposure (gender_re= NA) or outcome (fluvax_re = NA) 

#First look at missingness in exposure and outcome
table(brfss2015_2019$gender_re, useNA = "always") #13.5% missingness
table(brfss2015_2019$equality, useNA = "always") #0% missingness
table(brfss2015_2019$fluvax_re, useNA = "always") #8.0% missingness
table(brfss$gender_re, brfss2015_2019$fluvax_re, useNA = "always")

#Now drop
brfss2015_2019<-brfss2015_2019 %>% filter(!is.na(gender_re) & !is.na(fluvax_re))

#Look at missingness in exposure and outcome once dropped
table(brfss2015_2019$gender_re, useNA = "always") #0% missingness
table(brfss2015_2019$equality, useNA = "always") #0% missingness
table(brfss2015_2019$fluvax_re, useNA = "always") #0% missingness


#Save clean data set for analysis
#dataDir <- "C:/epi514/data/"
#Sarah's data directory
dataDir<-"/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data/"

write_rds(brfss2015_2019, paste0(dataDir, "brfss2015_2019.rds")) #save
#brfss <- read_rds(paste0(dataDir, "brfss2015_2019.rds")) #check
#View(brfss) #check x2


####################
#Unweighted Table 1 Creation (using orig dataset)
####################

#Label variables for Table 1
table1::label(brfss$birthsex_re) <- "Sex at Birth"
table1::label(brfss$sexorient) <-"Sexual Orientation"
table1::label(brfss$age_grp) <- "Age (years)"
table1::label(brfss$race_re) <- "Race/ethnicity"
table1::label(brfss$bmi_cat) <- "Body Mass Index (kg/m^2)"
table1::label(brfss$edu_cat) <- "Highest Level of Education"
table1::label(brfss$hsld_inc) <-"Annual Household Income"
table1::label(brfss$employ_recat_re) <-"Employment Status"
table1::label(brfss$health_coverage_re) <-"Health Insurance"
table1::label(brfss$medcost_re) <-"Poor Health Access (Could not see a doctor because of cost)"
table1::label(brfss$mentalhlth_re) <-"Poor Mental Health Status"
table1::label(brfss$dataYear_cat) <-"BRFSS Year"
#*******continue********#

## Table 1a
table1a <- table1(~ birthsex_re
                  + sexorient
                  + age_grp
                  + race_re 
                  + bmi_cat
                  + edu_cat
                  + hsld_inc 
                  + employ_recat_re
                  + health_coverage_re
                  + medcost_re
                  + mentalhlth_re
                  + dataYear_cat | gender_re, data = brfss,
                  render.missing=NULL, render.categorical ="FREQ (PCTnoNA%)", overall= "Total (DONT USE THIS BC INCLUDES MISSING)")
table1a 

## Table 1b
table1b <- table1(~ birthsex_re
                  + sexorient
                  + age_grp
                  + race_re 
                  + bmi_cat
                  + edu_cat
                  + hsld_inc 
                  + employ_recat_re
                  + health_coverage_re
                  + medcost_re
                  + mentalhlth_re 
                  + dataYear_cat | equality, data = brfss,
                  render.missing=NULL, render.categorical ="FREQ (PCTnoNA%)", overall= "Total")
table1b 

####################
#Unweighted Table 1 Creation (using clean dataset)
####################

#Label variables for Table 1 New
table1::label(brfss2015_2019$birthsex_re) <- "Sex at Birth"
table1::label(brfss2015_2019$sexorient) <-"Sexual Orientation"
table1::label(brfss2015_2019$age_grp) <- "Age (years)"
table1::label(brfss2015_2019$race_re) <- "Race/ethnicity"
table1::label(brfss2015_2019$bmi_cat) <- "Body Mass Index (kg/m^2)"
table1::label(brfss2015_2019$edu_cat) <- "Highest Level of Education"
table1::label(brfss2015_2019$hsld_inc) <-"Annual Household Income"
table1::label(brfss2015_2019$employ_recat_re) <-"Employment Status"
table1::label(brfss2015_2019$health_coverage_re) <-"Health Insurance"
table1::label(brfss2015_2019$medcost_re) <-"Poor Health Access (Could not see a doctor because of cost)"
table1::label(brfss2015_2019$mentalhlth_re) <-"Poor Mental Health Status"
table1::label(brfss2015_2019$dataYear_cat) <-"BRFSS Year"
#*******continue********#

## Table 1a New
table1a_new <- table1(~ birthsex_re
                  + sexorient
                  + age_grp
                  + race_re 
                  + bmi_cat
                  + edu_cat
                  + hsld_inc 
                  + employ_recat_re
                  + health_coverage_re
                  + medcost_re
                  + mentalhlth_re
                  + dataYear_cat | gender_re, data = brfss2015_2019,
                  render.missing=NULL, render.categorical ="FREQ (PCTnoNA%)", overall= "Total")
table1a_new 

## Table 1b New
table1b_new <- table1(~ birthsex_re
                  + sexorient
                  + age_grp
                  + race_re 
                  + bmi_cat
                  + edu_cat
                  + hsld_inc 
                  + employ_recat_re
                  + health_coverage_re
                  + medcost_re
                  + mentalhlth_re 
                  + dataYear_cat | equality, data = brfss2015_2019,
                  render.missing=NULL, render.categorical ="FREQ (PCTnoNA%)", overall= "Total")
table1b_new 

