################################################################################
#EPI514 Project
#Coders: Collrane Frivold, Sarah Cox, Mark Fajans
#Table 2 and Figure 2 script  ****UPDATED FOR SURVEY WEIGHTING
#Updated: July 27, 2021
################################################################################


##Housekeeping## 
################################################################################
# set up R (clear workspace)
rm(list = ls())
#set memory limit
memory.limit(size=500000)
#Widen the screen so that you can see the commands better 
options(width=150)
#Set directory 
#Sarah's Directory
#setwd("/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data")
#dataDir <- "/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data/"
#Mark's directory
setwd("~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data")
dataDir <- "~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data/Compiled Files"
#Collrane's directory
#setwd("C:/epi514/data/")
#dataDir <- "C:/epi514/data/"
#outputDir <- "C:/epi514/output/"
#Packages 
library(tidyverse)
library(epiR)
library(kableExtra)  
library(survey)
library(usmap)
library(ggplot2)
library(patchwork)
library(haven)
library(jtools)
library(remotes)
#library(svrepmisc)



#Load data 
#Load data created in 00 - data pull script 
brfss2015_2019 <- readRDS(paste0(dataDir, "brfss2015_2019.rds")) #check
brfss_t <- readRDS(paste0(dataDir, "brfss_t.rds")) #check

brfss2015_2019 <- readRDS("Databrfss2015_2019.rds")
brfss_t <- readRDS("Databrfss_t.rds")

################################################################################
#UPDATE - Weighted PRs using survey weighted logistic regression to recreate
#Tables 2a & 2b

#7.27.21

##Table 2a - Weighted ## 
################################################################################

# Recreating nofluvax as a (0,1) binomial variable

brfss2015_2019 <- brfss2015_2019 %>% mutate(
        nofluvax_bin = case_when(
                nofluvax_re=="No Influenza Vaccination"~1,
                nofluvax_re=="Influenza Vaccination"~0,),
        
        nofluvax_bin.f=factor(nofluvax_bin, labels=c("Influenza Vaccination", "No Influenza Vaccination"))
        )


#Checking variable creation

table(brfss2015_2019$nofluvax_bin, brfss2015_2019$nofluvax_bin.f, useNA = "always", deparse.level = 2) #looks OK
table(brfss2015_2019$gender_re, brfss2015_2019$gender_re2, useNA = "always", deparse.level = 2) #looks OK


#Taking survey design elements into weighted calculations, using cisgender men as reference group


brfss_svy_cismen<-svydesign(
                data=brfss2015_2019,
                id=~1,
                strata=~ststr,
                weights=~finalwt
                )

options(survey.lonely.psu="adjust")

brfss_svy<-svydesign(
        data=brfss2015_2019,
        id=~1,
        strata=~ststr,
        weights=~finalwt
        )

summary(brfss_svy)

#Creating separate brfss_svy object with cisgender women as reference group

brfss2015_2019$gender_re<-relevel(brfss2015_2019$gender_re, "Cisgender Women")

brfss_svy_ciswomen<-svydesign(
        data=brfss2015_2019,
        id=~1,
        strata=~ststr,
        weights=~finalwt
)


#################################
#2A - Weighted
##Gender identity and flu vax##
#################################

#Weighted prevalences by gender identity

novaxprev_gentab<-svytable(~nofluvax_bin + gender_re, brfss_svy)
prop.table(novaxprev_gentab, margin = 2)*100


novaxprev_genre2_tab<-svytable(~nofluvax_bin + gender_re2, brfss_svy)
prop.table(novaxprev_genre2_tab, margin = 2)*100

#Running log binomial regression incorporating survey weights to calculate PRs

#Using Cisgender women as reference

reg2a_cwref<-svyglm(nofluvax_bin~gender_re, design=brfss_svy_ciswomen, family = quasibinomial(link = "log"))
reg2a_cwref


exp(coef(reg2a_cwref))
exp(confint(reg2a_cwref))


#Using Cisgender men as reference
reg2a_cmref<-svyglm(nofluvax_bin~gender_re, design=brfss_svy_cismen, family = quasibinomial(link = "log"))
reg2a_cmref



exp(coef(reg2a_cmref))
exp(confint(reg2a_cmref))

#Using gender_re variable to 


summary(brfss2015_2019$nofluvax_re)

summary(svyglm(nofluvax_bin~gender_re, design=brfss_svy, family = quasibinomial(link = "log")))


####################################
#2B - Weighted
# State policies on gender identity
####################################


#Recalculating weighted prevalences and PRs

###Cisgender

#Prevalence
cis_prev2b<-subset(brfss_svy,gender_re2=="Cisgender")

novaxprev_cis<-svytable(~nofluvax_bin + equality, cis_prev2b) 
prop.table(novaxprev_cis, margin=2)*100

#PR
reg2b_cis<-svyglm(nofluvax_bin~equality, design=cis_prev2b, family = quasibinomial(link = "log"))
reg2b_cis

exp(coef(reg2b_cis))
exp(confint(reg2b_cis))

##Transgender women

#Prevalence
tgw_prev2b<-subset(brfss_svy,gender_re2=="Transgender Women")

novaxprev_tgw<-svytable(~nofluvax_bin + equality, tgw_prev2b) 
prop.table(novaxprev_tgw, margin=2)*100

#PR
reg2b_tgw<-svyglm(nofluvax_bin~equality, design=tgw_prev2b, family = quasibinomial(link = "log"))
reg2b_tgw

exp(coef(reg2b_tgw))
exp(confint(reg2b_tgw))

##Transgender men

#Prevalence
tgm_prev2b<-subset(brfss_svy,gender_re2=="Transgender Men")

novaxprev_tgm<-svytable(~nofluvax_bin + equality, tgm_prev2b) 
prop.table(novaxprev_tgm, margin=2)*100

#PR
reg2b_tgm<-svyglm(nofluvax_bin~equality, design=tgm_prev2b, family = quasibinomial(link = "log"))
reg2b_tgm

exp(coef(reg2b_tgm))
exp(confint(reg2b_tgm))

##Non-Binary

#Prevalence
nb_prev2b<-subset(brfss_svy,gender_re2=="Non-binary")

novaxprev_nb<-svytable(~nofluvax_bin + equality, nb_prev2b) 
prop.table(novaxprev_nb, margin=2)*100


#PR
reg2b_nb<-svyglm(nofluvax_bin~equality, design=nb_prev2b, family = quasibinomial(link = "log"))
reg2b_nb

exp(coef(reg2b_nb))
exp(confint(reg2b_nb))

#Double checking to make sure subsetting prior to running survey table doesn't cause an issue
novaxprev_genpoltab<-svytable(~nofluvax_bin + equality + gender_re2, brfss_svy)

novaxprev_genpoltab #Hand-checked proportions, weighted prevalences are identical






























#Prevalence of flu vaccination (unweighted)
rrTab <- table(brfss2015_2019$gender_re2, brfss2015_2019$nofluvax_re,
               deparse.level = 2)
#Proportions
prop.table(rrTab, 1)*100

#Table 2a: gender identity vs no flu vaccine 

#Create indicator exposure variables for bivariate comparisons
brfss2015_2019 <- brfss2015_2019 %>% mutate(
        transM_bin = case_when(
                gender_re2=="Transgender Men"~1,
                gender_re2=="Cisgender" ~ 2),
        transM_bin = factor(transM_bin, labels=c("Transgender Men", "Cisgender")), 
        transF_bin = case_when(
                gender_re2=="Transgender Women"~1,
                gender_re2=="Cisgender" ~ 2),
        transF_bin = factor(transF_bin, labels=c("Transgender Women", "Cisgender")),
        nb_bin = case_when(
                gender_re2=="Non-binary" ~ 1, 
                gender_re2=="Cisgender" ~ 2),
        nb_bin = factor(nb_bin, labels=c("Non-binary", "Cisgender")), 
        nofluvax_re = factor(nofluvax_re, levels=c("No Influenza Vaccination", "Influenza Vaccination"))
)


#Create strata 
strat_1 <- with(brfss2015_2019,
                table(transM_bin, nofluvax_re))
strat_2 <- with(brfss2015_2019,
                table(transF_bin, nofluvax_re))
strat_3 <- with(brfss2015_2019, 
                table(nb_bin, nofluvax_re))

#run analysis | estimate RRs by gender identity for no flu vax in past year
epi.2by2(strat_1) #Trans men
epi.2by2(strat_2) #Trans women
epi.2by2(strat_3) #Non-binary










