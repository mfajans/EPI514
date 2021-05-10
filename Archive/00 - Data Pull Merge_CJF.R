##########################################
#EPI514 Project
#Coders: Collrane Frivold, Sarah Cox, Mark Fajans
#Data Pull & Merging
##########################################


# set up R (clear workspace)
rm(list = ls())
#Mark's directory
#setwd("~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data/Compiled Files")
#Collrane's directory
setwd("C:/epi514/data/")


# Load Libraries
##load foreign to read SAS file
#install.packages("foreign")
library(foreign)
library(tidyverse)
# plyr allows you to combine data frames that do not have perfectly matching column names
library(plyr)
library(dplyr)
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
# NOTE: only use original verision of the survey (no states used other versions)

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

#Save merged files as a clean dataset (we can move this until after recoding all our variables)
dataDir <- "C:/epi514/data/"

write_rds(brfss, paste0(dataDir, "brfss2015_2019.rds")) #save
brfss <- read_rds(paste0(dataDir, "brfss2015_2019.rds")) #check
View(brfss2015_2019) #check x2

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
table(brfss$transgender, brfss$TRNSGNDR)

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

table(brfss$gender, brfss$TRNSGNDR, brfss$SEX, useNA = "always", deparse.level = 2)

#It looks like it has something to do with participants reporting "No"(4) for TRNSGNDR but NA for SEX, maybe 
#we should discuss how 

## ************check this coding as 175,991 cisgender drop when combine with sex**********

#It looks like it has something to do with participants reporting "No"(4) for TRNSGNDR but NA for SEX, maybe 
#we should discuss how to treat?

#Collrane: I think it has to do with how to question changed over time. It seems like NA is only an option for the 2019 dataset
#also do we need a dataset year variable? Right now we just have IYEAR which corresponds to the year of the interview

#table(brfss$TRNSGNDR, brfss$SEX, brfss$IYEAR, useNA = "always", deparse.level = 2)


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

