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
setwd("/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data")
dataDir <- "/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data/"
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
library(survey)
library(table1)
library(epiR)
library(kableExtra) 
library(gtsummary) 
library(flextable) 
library(dplyr)
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

#check
table(brfss2015_2019$nofluvax_bin)

#8.23.21 update - collapsing 55-64 and 65+ together

brfss2015_2019 <- brfss2015_2019 %>% mutate(
        age_grp2 = case_when(
                age_grp=="18-24"~1,
                age_grp=="25-34"~2,
                age_grp=="35-44"~3,
                age_grp=="45-54"~4,
                age_grp=="55-64"~5,
                age_grp=="65+"~5,),
        
        age_grp2=factor(age_grp2, labels=c("18-24", "25-34", "35-44", "45-54", "55+")))



table(brfss2015_2019$age_grp, brfss2015_2019$age_grp2, useNA = "always", deparse.level=2)


#Checking variable creation

table(brfss2015_2019$nofluvax_bin, brfss2015_2019$nofluvax_bin.f, useNA = "always", deparse.level = 2) #looks OK
table(brfss2015_2019$gender_re, brfss2015_2019$gender_re2, useNA = "always", deparse.level = 2) #looks OK


table(brfss2015_2019$age_grp, brfss2015_2019$age_grp2, useNA = "always", deparse.level=2)


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
#2A - Weighted Unadjusted
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
#2B - Weighted Unadjusted
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


################################################
#3A - Adjusted PRs
#Assessing for confounding & Effect modification
################################################

#####No flu Vaccine & Gender Identity

##Assessing for confounding

#Based on DAG, assessing for potential confounding by Age, Health insurance status, healthcare access, poor mental health status

#Age

svytable(~age_grp2 + nofluvax_bin, brfss_svy) %>% prop.table(margin=1) #looks like age might be acting more as an effect modifier
svytable(~age_grp2 + gender_re, brfss_svy) %>% prop.table(margin=1) #Same

age_nofluvax<-svyglm(nofluvax_bin~age_grp2, brfss_svy, family=quasibinomial(link = "log"))
        summary(age_nofluvax)
        exp(coef(age_nofluvax))
        exp(confint(age_nofluvax))  #I would be more inclined to include age as an effect modifier
        
#Health insurance status
        

        
svytable(~health_coverage_re + nofluvax_bin, brfss_svy) %>% prop.table(margin=1) #Looks like it might be associated with outcome
svytable(~health_coverage_re + gender_re, brfss_svy) %>% prop.table(margin=2) #Not so sure about exposure

healthcov_nofluvax<-svyglm(nofluvax_bin~health_coverage_re, brfss_svy, family=quasibinomial(link = "log"))
        summary(healthcov_nofluvax)
        exp(coef(healthcov_nofluvax))
        exp(confint(healthcov_nofluvax)) #yes, moderately associated

healthcov_gender_re<-svyglm(health_coverage_re~gender_re, brfss_svy, family=quasibinomial(link = "log"))
        summary(healthcov_gender_re)
        exp(coef(healthcov_gender_re))
        exp(confint(healthcov_gender_re)) #Looks like it's more of an effect modifier 

#Poor healthcare access
        
svytable(~medcost_re + nofluvax_bin, brfss_svy) %>% prop.table(margin=1) #Looks like it might be associated with outcome
svytable(~medcost_re + gender_re, brfss_svy) %>% prop.table(margin=2) #Potentially,  but looks more like an effect modifier
        
medcost_nofluvax<-svyglm(nofluvax_bin~medcost_re, brfss_svy, family=quasibinomial(link = "log"))
        summary(medcost_nofluvax)
        exp(coef(medcost_nofluvax))
        exp(confint(medcost_nofluvax)) #Yes, looks like it's moderately associated with outcome 
        
medcost_gender_re<-svyglm(medcost_re~gender_re, brfss_svy, family=quasibinomial(link = "log")) #Doesn't run
        summary(medcost_gender_re)
        exp(coef(medcost_gender_re))
        exp(confint(medcost_gender_re)) 

#note: using "poisson" family gets around a common error "Error: no valid set 
# of coefficients has been found: please supply starting values". Individual assignment provided by Alyson uses
# a poisson regression to get by this; unsure if this is appropriate
        
medcost_gender_re<-svyglm(as.numeric(medcost_re)~gender_re, brfss_svy, family=poisson) #Runs
        summary(medcost_gender_re)
        exp(coef(medcost_gender_re))
        exp(confint(medcost_gender_re))  #Doesn't seem to be an issue       
        

        
#Poor mental health status
        
svytable(~mentalhlth_re+ nofluvax_bin, brfss_svy) %>% prop.table(margin=1) #Doesn't look significant
svytable(~mentalhlth_re + gender_re, brfss_svy) %>% prop.table(margin=2) #Potentially an effect modifier?
        
mentalhtlh_nofluvax<-svyglm(nofluvax_bin~mentalhlth_re, brfss_svy, family=quasibinomial(link = "log"))
        summary(mentalhtlh_nofluvax)
        exp(coef(mentalhtlh_nofluvax))
        exp(confint(mentalhtlh_nofluvax)) #Significant, but doesn't look by much
        
        mentalhlth_gender_re<-svyglm(mentalhlth_re~gender_re, brfss_svy, family=quasibinomial(link = "log")) #Doesn't run
        summary(mentalhlth_gender_re)
        exp(coef(mentalhlth_gender_re))
        exp(confint(mentalhlth_gender_re)) 

#Changing to poisson regression
mentalhlth_gender_re<-svyglm(as.numeric(mentalhlth_re)~gender_re, brfss_svy, family=poisson) #Runs, might be an effect modifier? 
        summary(mentalhlth_gender_re)
        exp(coef(mentalhlth_gender_re))
        exp(confint(mentalhlth_gender_re)) 
        
#All 3 might be potential effect modifiers, would be good just to check via stratified analysis
        


##Assessing for effect modification (Collapsed age group 8.24.21)
#Age
        
        svytable(~nofluvax_bin + gender_re + age_grp2, brfss_svy) %>%
                prop.table(margin = 2)
        
        age1 <- svyglm(nofluvax_bin ~ gender_re, 
                       design = subset(brfss_svy, age_grp2 == "18-24"),
                       family = quasibinomial(link = "log"))
        
        age2 <- svyglm(nofluvax_bin ~ gender_re, 
                              design = subset(brfss_svy, age_grp2 == "25-34"),
                              family = quasibinomial(link = "log"))
        
        age3 <- svyglm(nofluvax_bin ~ gender_re, 
                              design = subset(brfss_svy, age_grp2 == "35-44"),
                              family = quasibinomial(link = "log"))
        
        age4 <- svyglm(nofluvax_bin ~ gender_re, 
                              design = subset(brfss_svy, age_grp2 == "45-54"),
                              family = quasibinomial(link = "log"))
        
        age5 <- svyglm(nofluvax_bin ~ gender_re, 
                              design = subset(brfss_svy, age_grp2 == "55+"),
                              family = quasibinomial(link = "log"))
        
        #age6 <- svyglm(nofluvax_bin ~ gender_re, 
        #                      design = subset(brfss_svy, age_grp == "65+"),
        #                      family = quasibinomial(link = "log"))   


        exp(coef(age1))
        exp(coef(age2))
        exp(coef(age3))
        exp(coef(age4))
        exp(coef(age5))
        #exp(coef(age6)) #Pretty significant increase across all gender identities in this age group, would consider incorporating as an effect modifier
        
        exp(confint(age1))
        exp(confint(age2))
        exp(confint(age3))
        exp(confint(age4))
        exp(confint(age5))
        #exp(confint(age6))


#Health insurance status
        svytable(~nofluvax_bin + gender_re + health_coverage_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        healthcov_yes <- svyglm(nofluvax_bin ~ gender_re, 
                       design = subset(brfss_svy, health_coverage_re == "Yes"),
                       family = quasibinomial(link = "log"))
        
        healthcov_no <- svyglm(nofluvax_bin ~ gender_re, 
                       design = subset(brfss_svy, health_coverage_re == "No"),
                       family = quasibinomial(link = "log"))

        exp(coef(healthcov_yes))
        exp(coef(healthcov_no))  #Possibly among non-binary?

        exp(confint(healthcov_yes))
        exp(confint(healthcov_no))

#Possibly among non-binary? CIs seem wide and overlap. I would probably say not to include.          
        

#Poor Healthcare access
        svytable(~nofluvax_bin + gender_re + medcost_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        medcost_yes <- svyglm(nofluvax_bin ~ gender_re, 
                              design = subset(brfss_svy, medcost_re == "Yes"),
                              family = quasibinomial(link = "log"))
        
        medcost_no <- svyglm(nofluvax_bin ~ gender_re, 
                             design = subset(brfss_svy, medcost_re == "No"),
                             family = quasibinomial(link = "log"))
        
        exp(coef(medcost_yes))
        exp(coef(medcost_no))
        
        exp(confint(medcost_yes))
        exp(confint(medcost_no))

#Mental Health Status
        svytable(~nofluvax_bin + gender_re + mentalhlth_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        mentalhtlh_bad <- svyglm(nofluvax_bin ~ gender_re, 
                              design = subset(brfss_svy, mentalhlth_re == ">=14 days"),
                              family = quasibinomial(link = "log"))
        
        mentalhlth_notbad <- svyglm(nofluvax_bin ~ gender_re, 
                             design = subset(brfss_svy, mentalhlth_re == "<14 days"),
                             family = quasibinomial(link = "log"))
        
        exp(coef(medcost_yes))
        exp(coef(medcost_no))
        
        exp(confint(medcost_yes))
        exp(confint(medcost_no))
        
#Doesn't look like a major difference, would not include as an effect modifier

#Effect modifier: Age
        
####8.12.21 change - Switching back to combining cisgender women and men, and using as reference group
        
#Crude PR-combined cisgender for reference
crudePR3a<-svyglm(nofluvax_bin ~ gender_re2, 
                                  design =brfss_svy,
                                  family = quasibinomial(link = "log"))
         
        summary(crudePR3a)
        exp(coef(crudePR3a))
        exp(confint(crudePR3a))

##Assessing for effect modification
#Age
                
                svytable(~nofluvax_bin + gender_re2 + age_grp2, brfss_svy) %>%
                prop.table(margin = 2)
        
        age1 <- svyglm(nofluvax_bin ~ gender_re2, 
                       design = subset(brfss_svy, age_grp2 == "18-24"),
                       family = quasibinomial(link = "log"))
        
        age2 <- svyglm(nofluvax_bin ~ gender_re2, 
                       design = subset(brfss_svy, age_grp2 == "25-34"),
                       family = quasibinomial(link = "log"))
        
        age3 <- svyglm(nofluvax_bin ~ gender_re2, 
                       design = subset(brfss_svy, age_grp2 == "35-44"),
                       family = quasibinomial(link = "log"))
        
        age4 <- svyglm(nofluvax_bin ~ gender_re2, 
                       design = subset(brfss_svy, age_grp2 == "45-54"),
                       family = quasibinomial(link = "log"))
        
        age5 <- svyglm(nofluvax_bin ~ gender_re2, 
                       design = subset(brfss_svy, age_grp2 == "55+"),
                       family = quasibinomial(link = "log"))
        
        #age6 <- svyglm(nofluvax_bin ~ gender_re2, 
        #               design = subset(brfss_svy, age_grp == "65+"),
        #               family = quasibinomial(link = "log"))   
        
        
        exp(coef(age1))
        exp(coef(age2))
        exp(coef(age3))
        exp(coef(age4))
        exp(coef(age5))
       # exp(coef(age6)) #Pretty significant increase across all gender identities in this age group, would consider incorporating as an effect modifier
        
        exp(confint(age1))
        exp(confint(age2))
        exp(confint(age3))
        exp(confint(age4))
        exp(confint(age5))
       # exp(confint(age6))
        
        
        #Health insurance status
        svytable(~nofluvax_bin + gender_re2 + health_coverage_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        healthcov_yes <- svyglm(nofluvax_bin ~ gender_re2, 
                                design = subset(brfss_svy, health_coverage_re == "Yes"),
                                family = quasibinomial(link = "log"))
        
        healthcov_no <- svyglm(nofluvax_bin ~ gender_re2, 
                               design = subset(brfss_svy, health_coverage_re == "No"),
                               family = quasibinomial(link = "log"))
        
        exp(coef(healthcov_yes))
        exp(coef(healthcov_no))  #Possibly among non-binary?
        
        exp(confint(healthcov_yes))
        exp(confint(healthcov_no))
        
        #Possibly among non-binary? CIs seem wide and overlap. I would probably say not to include.          
        
        
        #Poor Healthcare access
        svytable(~nofluvax_bin + gender_re2 + medcost_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        medcost_yes <- svyglm(nofluvax_bin ~ gender_re2, 
                              design = subset(brfss_svy, medcost_re == "Yes"),
                              family = quasibinomial(link = "log"))
        
        medcost_no <- svyglm(nofluvax_bin ~ gender_re2, 
                             design = subset(brfss_svy, medcost_re == "No"),
                             family = quasibinomial(link = "log"))
        
        exp(coef(medcost_yes))
        exp(coef(medcost_no))
        
        exp(confint(medcost_yes))
        exp(confint(medcost_no))
        
        #Mental Health Status
        svytable(~nofluvax_bin + gender_re2 + mentalhlth_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        mentalhtlh_bad <- svyglm(nofluvax_bin ~ gender_re2, 
                                 design = subset(brfss_svy, mentalhlth_re == ">=14 days"),
                                 family = quasibinomial(link = "log"))
        
        mentalhlth_notbad <- svyglm(nofluvax_bin ~ gender_re2, 
                                    design = subset(brfss_svy, mentalhlth_re == "<14 days"),
                                    family = quasibinomial(link = "log"))
        
        exp(coef(medcost_yes))
        exp(coef(medcost_no))
        
        exp(confint(medcost_yes))
        exp(confint(medcost_no))        
        
        
####No flu Vaccine & state policy, by gender identity


   
        
##Assessing for confounding
        
#Based on DAG, assessing for potential confounding by Age, Health insurance status, healthcare access, poor mental health status
        
#Age
        
svytable(~age_grp2 + equality, brfss_svy) %>% prop.table(margin=1) #Looks pretty similar
svytable(~age_grp2 + nofluvax_bin, brfss_svy) %>% prop.table(margin=1) #Looks like a strong association, but again could be an effect modifier
        
age_nofluvax<-svyglm(nofluvax_bin~age_grp2, brfss_svy, family=quasibinomial(link = "log"))
        summary(age_nofluvax)
        exp(coef(age_nofluvax))
        exp(confint(age_nofluvax))  #I would be more inclined to include age as an effect modifier
        
age_equality<-svyglm(equality~age_grp2, brfss_svy, family=quasibinomial(link = "log"))
        summary(age_equality)
        exp(coef(age_equality))
        exp(confint(age_equality))  #Doesn't appear to any significant difference
        
        
        #Health insurance status
        
svytable(~health_coverage_re + equality, brfss_svy) %>% prop.table(margin=1) #Looks like it might be associated with outcome
svytable(~health_coverage_re + nofluvax_bin, brfss_svy) %>% prop.table(margin=2) #Strong association

healthcov_nofluvax<-svyglm(nofluvax_bin~health_coverage_re, brfss_svy, family=quasibinomial(link = "log"))
        summary(healthcov_nofluvax)
        exp(coef(healthcov_nofluvax))
        exp(confint(healthcov_nofluvax)) 
        #Looks like there is an association 
                
healthcov_equality<-svyglm(health_coverage_re~equality, brfss_svy, family=quasibinomial(link = "log")) 
        summary(healthcov_equality)
        exp(coef(healthcov_equality))
        exp(confint(healthcov_equality)) 
        #Strong association
        #Maybe consider as a confounder?
                
        
#comparing unadjusted vs adjusting for healthcare coverage (have to change to poisson to get to run)
 
nofluvax_equality_unadj<-svyglm(nofluvax_bin~equality, brfss_svy, family=poisson)       
        summary(healthcov_equality)
        exp(coef(healthcov_equality))
        exp(confint(healthcov_equality))     
        
nofluvax_equality_healthcovadj<-svyglm(as.numeric(nofluvax_bin)~equality + health_coverage_re, 
                                       brfss_svy, 
                                       family=poisson)       
        summary(nofluvax_equality_healthcovadj)
        exp(coef(nofluvax_equality_healthcovadj))
        exp(confint(nofluvax_equality_healthcovadj))   
        #Pretty significant decrease, maybe we should consider including as a confounder? But considering the DAG, more likely
        #that coverage is most likely downstream from state policies through some intermediary variable.
        
#Poor healthcare access
        
svytable(~medcost_re + nofluvax_bin, brfss_svy) %>% prop.table(margin=1) #Appears fairly significant 
svytable(~medcost_re + equality, brfss_svy) %>% prop.table(margin=2) #Seems fairly similar
        
medcost_nofluvax<-svyglm(nofluvax_bin~medcost_re, brfss_svy, family=quasibinomial(link = "log")) #moderate association
        summary(medcost_nofluvax)
        exp(coef(medcost_nofluvax))
        exp(confint(medcost_nofluvax)) 
        
medcost_equality<-svyglm(medcost_re~equality, brfss_svy, family=quasibinomial(link = "log")) #Doesn't run
        summary(medcost_equality)
        exp(coef(medcost_equality))
        exp(confint(medcost_equality)) 
        
#note: using "poisson" family gets around a common error "Error: no valid set 
# of coefficients has been found: please supply starting values". Individual assignment provided by Alyson uses
# a poisson regression to get by this; unsure if this is appropriate
        
medcost_equality<-svyglm(as.numeric(medcost_re)~equality, brfss_svy, family=poisson) #Runs
        summary(medcost_equality)
        exp(coef(medcost_equality))
        exp(confint(medcost_equality))  #Just barely significant, not convinced it should be included as confounder     
        
        
        
        #Poor mental health status
        
svytable(~mentalhlth_re+ nofluvax_bin, brfss_svy) %>% prop.table(margin=1) #Doesn't look significant
svytable(~mentalhlth_re + equality, brfss_svy) %>% prop.table(margin=2) #Looks similar


mentalhtlh_nofluvax<-svyglm(nofluvax_bin~mentalhlth_re, brfss_svy, family=quasibinomial(link = "log"))
        summary(mentalhtlh_nofluvax)
        exp(coef(mentalhtlh_nofluvax))
        exp(confint(mentalhtlh_nofluvax)) #Significant, but doesn't look by much
        
mentalhlth_equality<-svyglm(mentalhlth_re~equality, brfss_svy, family=quasibinomial(link = "log")) #Doesn't run
        summary(mentalhlth_equality)
        exp(coef(mentalhlth_equality))
        exp(confint(mentalhlth_equality)) 
        
#Changing to poisson regression
mentalhlth_equality<-svyglm(as.numeric(mentalhlth_re)~equality, brfss_svy, family=poisson) #Runs, might be an effect modifier? 
        summary(mentalhlth_equality)
        exp(coef(mentalhlth_equality))
        exp(confint(mentalhlth_equality)) 
        #Same as above, barely significant, would not consider as confounder
        
#Potential confounders:Health insurance status
        
        
##Assessing for effect modification

#Age

#Cisgender
svytable(~nofluvax_bin +equality + age_grp2, brfss_svy) %>%
                prop.table(margin = 2)
        
        age1_cis <- svyglm(nofluvax_bin ~ equality, 
                       design = subset(brfss_svy, age_grp2 == "18-24" & gender_re2=="Cisgender"),
                       family = quasibinomial(link = "log"))
        
        age2_cis <- svyglm(nofluvax_bin ~ equality, 
                       design = subset(brfss_svy, age_grp2 == "25-34"& gender_re2=="Cisgender"),
                       family = quasibinomial(link = "log"))
        
        age3_cis <- svyglm(nofluvax_bin ~ equality, 
                       design = subset(brfss_svy, age_grp2 == "35-44"& gender_re2=="Cisgender"),
                       family = quasibinomial(link = "log"))
        
        age4_cis<- svyglm(nofluvax_bin ~ equality,
                       design = subset(brfss_svy, age_grp2 == "45-54"& gender_re2=="Cisgender"),
                       family = quasibinomial(link = "log"))
        
        age5_cis <- svyglm(nofluvax_bin ~ equality, 
                       design = subset(brfss_svy, age_grp2 == "55+"& gender_re2=="Cisgender"),
                       family = quasibinomial(link = "log"))
        
        #age6_cis <- svyglm(nofluvax_bin ~ equality,
        #               design = subset(brfss_svy, age_grp == "65+"& gender_re2=="Cisgender"),
        #               family = quasibinomial(link = "log"))   
        
        
        exp(coef(age1_cis))
        exp(coef(age2_cis))
        exp(coef(age3_cis))
        exp(coef(age4_cis))
        exp(coef(age5_cis))
        #exp(coef(age6_cis)) #Seems pretty consistent across age groups
         
        exp(confint(age1_cis))
        exp(confint(age2_cis))
        exp(confint(age3_cis))
        exp(confint(age4_cis))
        exp(confint(age5_cis))
        #exp(confint(age6_cis))  
        
#Transgender Women
svytable(~nofluvax_bin +equality + age_grp2, brfss_svy) %>%
                prop.table(margin = 2)
        
        age1_tgw <- svyglm(nofluvax_bin ~ equality, 
                           design = subset(brfss_svy, age_grp2 == "18-24" & gender_re2=="Transgender Women"),
                           family = quasibinomial(link = "log"))
        
        age2_tgw <- svyglm(nofluvax_bin ~ equality, 
                           design = subset(brfss_svy, age_grp2 == "25-34"& gender_re2=="Transgender Women"),
                           family = quasibinomial(link = "log"))
        
        age3_tgw <- svyglm(nofluvax_bin ~ equality, 
                           design = subset(brfss_svy, age_grp2 == "35-44"& gender_re2=="Transgender Women"),
                           family = quasibinomial(link = "log"))
        
        age4_tgw<- svyglm(nofluvax_bin ~ equality,
                          design = subset(brfss_svy, age_grp2 == "45-54"& gender_re2=="Transgender Women"),
                          family = quasibinomial(link = "log"))
        
        age5_tgw <- svyglm(nofluvax_bin ~ equality, 
                           design = subset(brfss_svy, age_grp2 == "55+"& gender_re2=="Transgender Women"),
                           family = quasibinomial(link = "log"))
        
        #age6_tgw <- svyglm(nofluvax_bin ~ equality,
         #                 design = subset(brfss_svy, age_grp == "65+"& gender_re2=="Transgender Women"),
         #                 family = quasibinomial(link = "log"))   
        
        
        exp(coef(age1_tgw))
        exp(coef(age2_tgw))
        exp(coef(age3_tgw))
        exp(coef(age4_tgw))
        exp(coef(age5_tgw))
        #exp(coef(age6_tgw)) #Looks like there might be evidence of effect modification, especially for 45-54 age group
        
        exp(confint(age1_tgw))
        exp(confint(age2_tgw))
        exp(confint(age3_tgw))
        exp(confint(age4_tgw))
        exp(confint(age5_tgw))
        #exp(confint(age6_tgw))
        
        
#Transgender Men
svytable(~nofluvax_bin +equality + age_grp2, brfss_svy) %>%
                prop.table(margin = 2)
        
        age1_tgm <- svyglm(nofluvax_bin ~ equality, 
                           design = subset(brfss_svy, age_grp2 == "18-24" & gender_re2=="Transgender Men"),
                           family = quasibinomial(link = "log"))
        
        age2_tgm <- svyglm(nofluvax_bin ~ equality, 
                           design = subset(brfss_svy, age_grp2 == "25-34"& gender_re2=="Transgender Men"),
                           family = quasibinomial(link = "log"))
        
        age3_tgm <- svyglm(nofluvax_bin ~ equality, 
                           design = subset(brfss_svy, age_grp2 == "35-44"& gender_re2=="Transgender Men"),
                           family = quasibinomial(link = "log"))
        
        age4_tgm<- svyglm(nofluvax_bin ~ equality,
                          design = subset(brfss_svy, age_grp2 == "45-54"& gender_re2=="Transgender Men"),
                          family = quasibinomial(link = "log"))
        
        age5_tgm <- svyglm(nofluvax_bin ~ equality, 
                           design = subset(brfss_svy, age_grp2 == "55+"& gender_re2=="Transgender Men"),
                           family = quasibinomial(link = "log"))
        
        #age6_tgm <- svyglm(nofluvax_bin ~ equality,
        #                   design = subset(brfss_svy, age_grp == "65+"& gender_re2=="Transgender Men"),
        #                   family = quasibinomial(link = "log"))   
        
        
        exp(coef(age1_tgm))
        exp(coef(age2_tgm))
        exp(coef(age3_tgm))
        exp(coef(age4_tgm))
        exp(coef(age5_tgm))
        #exp(coef(age6_tgm)) #Looks like there might be evidence of effect modification, especially for 25-54 age group, but really wide CI
        
        exp(confint(age1_tgm))
        exp(confint(age2_tgm))
        exp(confint(age3_tgm))
        exp(confint(age4_tgm))
        exp(confint(age5_tgm))
        #exp(confint(age6_tgm))
        
#Non-binary
svytable(~nofluvax_bin +equality + age_grp2, brfss_svy) %>%
                prop.table(margin = 2)
        
        age1_nb <- svyglm(nofluvax_bin ~ equality, 
                           design = subset(brfss_svy, age_grp2 == "18-24" & gender_re2=="Non-binary"),
                           family = quasibinomial(link = "log"))
        
        age2_nb <- svyglm(nofluvax_bin ~ equality, 
                           design = subset(brfss_svy, age_grp2 == "25-34"& gender_re2=="Non-binary"),
                           family = quasibinomial(link = "log"))
        
        age3_nb <- svyglm(nofluvax_bin ~ equality, 
                           design = subset(brfss_svy, age_grp2 == "35-44"& gender_re2=="Non-binary"),
                           family = quasibinomial(link = "log"))
        
        age4_nb<- svyglm(nofluvax_bin ~ equality,
                          design = subset(brfss_svy, age_grp2 == "45-54"& gender_re2=="Non-binary"),
                          family = quasibinomial(link = "log"))
        
        age5_nb <- svyglm(nofluvax_bin ~ equality, 
                           design = subset(brfss_svy, age_grp2 == "55+"& gender_re2=="Non-binary"),
                           family = quasibinomial(link = "log"))
        
        #age6_nb <- svyglm(nofluvax_bin ~ equality,
        #                   design = subset(brfss_svy, age_grp == "65+"& gender_re2=="Non-binary"),
        #                   family = quasibinomial(link = "log"))   
        
        
        exp(coef(age1_nb))
        exp(coef(age2_nb))
        exp(coef(age3_nb))
        exp(coef(age4_nb))
        exp(coef(age5_nb))
        #exp(coef(age6_nb)) 
        
        exp(confint(age1_nb))
        exp(confint(age2_nb))
        exp(confint(age3_nb))
        exp(confint(age4_nb))
        exp(confint(age5_nb))
        #exp(confint(age6_nb))

#Pretty significant drop for 45-54 age range
        
        
#Health insurance status
#Cisgender
        
svytable(~nofluvax_bin + equality + health_coverage_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        healthcov_yes_cis <- svyglm(nofluvax_bin ~ equality, 
                                design = subset(brfss_svy, health_coverage_re == "Yes" & gender_re2=="Cisgender"),
                                family = quasibinomial(link = "log"))
        
        healthcov_no_cis <- svyglm(nofluvax_bin ~ equality, 
                               design = subset(brfss_svy, health_coverage_re == "No" & gender_re2=="Cisgender"),
                               family = quasibinomial(link = "log"))
        
        exp(coef(healthcov_yes_cis))
        exp(coef(healthcov_no_cis))  
        
        exp(confint(healthcov_yes_cis))
        exp(confint(healthcov_no_cis))
        
#Transgender women
        
svytable(~nofluvax_bin + equality + health_coverage_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        healthcov_yes_tgw <- svyglm(as.numeric(nofluvax_bin) ~ equality, 
                                    design = subset(brfss_svy, health_coverage_re == "Yes" & gender_re2=="Transgender Women"),
                                    family = poisson) #Changing to poisson to get it to run
        
        healthcov_no_tgw <- svyglm(as.numeric(nofluvax_bin)~ equality, 
                                   design = subset(brfss_svy, health_coverage_re == "No" & gender_re2=="Transgender Women"),
                                   family = poisson)
        
        exp(coef(healthcov_yes_tgw))
        exp(coef(healthcov_no_tgw))  
        
        exp(confint(healthcov_yes_tgw))
        exp(confint(healthcov_no_tgw))     #slight difference, but again pretty wide CIs   
        
   
#Transgender Men
        
svytable(~nofluvax_bin + equality + health_coverage_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        healthcov_yes_tgm <- svyglm(as.numeric(nofluvax_bin) ~ equality, 
                                    design = subset(brfss_svy, health_coverage_re == "Yes" & gender_re2=="Transgender Men"),
                                    family = poisson) #Changing to poisson to get it to run
        
        healthcov_no_tgm <- svyglm(as.numeric(nofluvax_bin)~ equality, 
                                   design = subset(brfss_svy, health_coverage_re == "No" & gender_re2=="Transgender Men"),
                                   family = poisson)
        
        exp(coef(healthcov_yes_tgm))
        exp(coef(healthcov_no_tgm))  
        
        exp(confint(healthcov_yes_tgm))
        exp(confint(healthcov_no_tgm))     #Slight difference, but wide CIs  
        
 
#Non-binary
        
svytable(~nofluvax_bin + equality + health_coverage_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        healthcov_yes_nb <- svyglm(as.numeric(nofluvax_bin) ~ equality, 
                                    design = subset(brfss_svy, health_coverage_re == "Yes" & gender_re2=="Non-binary"),
                                    family = poisson) #Changing to poisson to get it to run
        
        healthcov_no_nb <- svyglm(as.numeric(nofluvax_bin)~ equality, 
                                   design = subset(brfss_svy, health_coverage_re == "No" & gender_re2=="Non-binary"),
                                   family = poisson)
        
        exp(coef(healthcov_yes_nb))
        exp(coef(healthcov_no_nb))  
        
        exp(confint(healthcov_yes_nb))
        exp(confint(healthcov_no_nb))     #Slight difference, but wide CIs       

        

        
        
        
#Poor Healthcare access
#Cisgender
svytable(~nofluvax_bin + equality + medcost_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        medcost_yes_cis <- svyglm(nofluvax_bin ~ equality, 
                              design = subset(brfss_svy, medcost_re == "Yes" & gender_re2=="Cisgender"),
                              family = quasibinomial(link = "log"))
        
        medcost_no_cis <- svyglm(nofluvax_bin ~ equality, 
                             design = subset(brfss_svy, medcost_re == "No" & gender_re2=="Cisgender"),
                             family = quasibinomial(link = "log"))
        
        exp(coef(medcost_yes_cis))
        exp(coef(medcost_no_cis))
        
        exp(confint(medcost_yes_cis))
        exp(confint(medcost_no_cis))  #Seems fairly similar 
        

#Transgender Women
        svytable(~nofluvax_bin + equality + medcost_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        medcost_yes_tgw <- svyglm(nofluvax_bin ~ equality, 
                                  design = subset(brfss_svy, medcost_re == "Yes" & gender_re2=="Transgender Women"),
                                  family = quasibinomial(link = "log"))
        
        medcost_no_tgw <- svyglm(nofluvax_bin ~ equality, 
                                 design = subset(brfss_svy, medcost_re == "No" & gender_re2=="Transgender Women"),
                                 family = quasibinomial(link = "log"))
        
        exp(coef(medcost_yes_tgw))
        exp(coef(medcost_no_tgw))
        
        exp(confint(medcost_yes_tgw))
        exp(confint(medcost_no_tgw))   #Seems fairly similar     
        
        
#Transgender Men
svytable(~nofluvax_bin + equality + medcost_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        medcost_yes_tgm <- svyglm(nofluvax_bin ~ equality, 
                                  design = subset(brfss_svy, medcost_re == "Yes" & gender_re2=="Transgender Men"),
                                  family = quasibinomial(link = "log"))
        
        medcost_no_tgm <- svyglm(nofluvax_bin ~ equality, 
                                 design = subset(brfss_svy, medcost_re == "No" & gender_re2=="Transgender Men"),
                                 family = quasibinomial(link = "log"))
        
        exp(coef(medcost_yes_tgm))
        exp(coef(medcost_no_tgm))
        
        exp(confint(medcost_yes_tgm))
        exp(confint(medcost_no_tgm))   #Seems fairly similar 

#Non-binary
        svytable(~nofluvax_bin + equality + medcost_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        medcost_yes_nb <- svyglm(nofluvax_bin ~ equality, 
                                  design = subset(brfss_svy, medcost_re == "Yes" & gender_re2=="Non-binary"),
                                  family = quasibinomial(link = "log"))
        
        medcost_no_nb <- svyglm(nofluvax_bin ~ equality, 
                                 design = subset(brfss_svy, medcost_re == "No" & gender_re2=="Non-binary"),
                                 family = quasibinomial(link = "log"))
        
        exp(coef(medcost_yes_nb))
        exp(coef(medcost_no_nb))
        
        exp(confint(medcost_yes_nb))
        exp(confint(medcost_no_nb))   #Seems fairly similar     
        
        
#Mental Health Status
#Cisgender
svytable(~nofluvax_bin + equality + mentalhlth_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        mentalhtlh_bad_cis <- svyglm(nofluvax_bin ~ equality, 
                                 design = subset(brfss_svy, mentalhlth_re == ">=14 days" & gender_re2=="Cisgender"),
                                 family = quasibinomial(link = "log"))
        
        mentalhlth_notbad_cis <- svyglm(nofluvax_bin ~ equality, 
                                    design = subset(brfss_svy, mentalhlth_re == "<14 days" & gender_re2=="Cisgender"),
                                    family = quasibinomial(link = "log"))
        
        exp(coef(mentalhtlh_bad_cis))
        exp(coef(mentalhlth_notbad_cis))
        
        exp(confint(mentalhtlh_bad_cis))
        exp(confint(mentalhlth_notbad_cis))  #Seems pretty similar
        
#Transgender Women
svytable(~nofluvax_bin + equality + mentalhlth_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        mentalhtlh_bad_tgw <- svyglm(nofluvax_bin ~ equality, 
                                     design = subset(brfss_svy, mentalhlth_re == ">=14 days" & gender_re2=="Transgender Women"),
                                     family = quasibinomial(link = "log"))
        
        mentalhlth_notbad_tgw <- svyglm(nofluvax_bin ~ equality, 
                                        design = subset(brfss_svy, mentalhlth_re == "<14 days" & gender_re2=="Transgender Women"),
                                        family = quasibinomial(link = "log"))
        
        exp(coef(mentalhtlh_bad_tgw))
        exp(coef(mentalhlth_notbad_tgw))
        
        exp(confint(mentalhtlh_bad_tgw))
        exp(confint(mentalhlth_notbad_tgw ))  #Seems pretty similar, wide CIs
        
#Transgender Men
svytable(~nofluvax_bin + equality + mentalhlth_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        mentalhtlh_bad_tgm <- svyglm(nofluvax_bin ~ equality, 
                                     design = subset(brfss_svy, mentalhlth_re == ">=14 days" & gender_re2=="Transgender Men"),
                                     family = quasibinomial(link = "log"))
        
        mentalhlth_notbad_tgm <- svyglm(nofluvax_bin ~ equality, 
                                        design = subset(brfss_svy, mentalhlth_re == "<14 days" & gender_re2=="Transgender Men"),
                                        family = quasibinomial(link = "log"))
        
        exp(coef(mentalhtlh_bad_tgw))
        exp(coef( mentalhlth_notbad_tgm))
        
        exp(confint(mentalhtlh_bad_tgm))
        exp(confint(mentalhlth_notbad_tgm ))  #On the verge, but again wide CIs        
        
        
#Non-binary
svytable(~nofluvax_bin + equality + mentalhlth_re, brfss_svy) %>%
                prop.table(margin = 2)
        
        mentalhtlh_bad_nb <- svyglm(nofluvax_bin ~ equality, 
                                     design = subset(brfss_svy, mentalhlth_re == ">=14 days" & gender_re2=="Non-binary"),
                                     family = quasibinomial(link = "log"))
        
        mentalhlth_notbad_nb <- svyglm(nofluvax_bin ~ equality, 
                                        design = subset(brfss_svy, mentalhlth_re == "<14 days" & gender_re2=="Non-binary"),
                                        family = quasibinomial(link = "log"))
        
        exp(coef(mentalhtlh_bad_nb))
        exp(coef( mentalhlth_notbad_nb))
        
        exp(confint(mentalhtlh_bad_nb))
        exp(confint(mentalhlth_notbad_nb ))  #Pretty close     
        
        
        
####################################
#Prevalence differences (PDs)
####################################
        
#PR code from above for reference
#summary(svyglm(nofluvax_bin~gender_re, design=brfss_svy, family = quasibinomial(link = "log")))
        
#test PD
riskmodel<-svyglm(nofluvax_bin~gender_re, design=brfss_svy)
riskmodel
        
confint(riskmodel)
        
###Table 2a### 
        
#PD: cis women ref group
pd2a_cwref <-svyglm(nofluvax_bin~gender_re, design=brfss_svy_ciswomen)
pd2a_cwref
confint(pd2a_cwref)
        
#round coefficients/CIs for table 2a
pd2a_cwref$coefficients %>% signif(3)
pd2a_cwref %>% coef %>% round(3)
pd2a_cwref %>% confint %>% round(3)
        
        #PD: cis men ref group
        pd2a_cmref <-svyglm(nofluvax_bin~gender_re, design=brfss_svy_cismen)
        pd2a_cmref
        confint(pd2a_cmref, digits = 3)
        coef(pd2a_cmref, digits = 3)
        
        #round coefficients/CIs for table 2a
        pd2a_cmref$coefficients %>% signif(3)
        pd2a_cmref %>% coef %>% round(3)
        pd2a_cmref %>% confint %>% round(3)
        
        ###Table 2b### 
        
        ###Cisgender
        
        #PD cisgender
        pd2b_cis <-svyglm(nofluvax_bin~equality, design=cis_prev2b)
        pd2b_cis 
        pd2b_cis %>% coef %>% round(3)
        pd2b_cis %>% confint %>% round(3)    
        
        ##Transgender women
        
        #PD trans women
        pd2b_tgw <-svyglm(nofluvax_bin~equality, design=tgw_prev2b)
        pd2b_tgw 
        pd2b_tgw %>% coef %>% round(3)
        pd2b_tgw %>% confint %>% round(3)       
        
        ##Transgender men
        
        #PD trans women
        pd2b_tgm <-svyglm(nofluvax_bin~equality, design=tgm_prev2b)
        pd2b_tgm 
        pd2b_tgm %>% coef %>% round(3)
        pd2b_tgm %>% confint %>% round(3) 
        
        
        ##Non-Binary
        
        #PD non-binary
        pd2b_nb <-svyglm(nofluvax_bin~equality, design=nb_prev2b)
        pd2b_nb 
        pd2b_nb %>% coef %>% round(3)
        pd2b_nb %>% confint %>% round(3)   
        
        


finalmodelA <- svyglm(as.numeric(nofluvax_bin) ~ gender_re2 * age_grp2, 
                        design =brfss_svy,
                        family = quasibinomial(link = "log"))
        
exp(coef(finalmodelA))
exp(confint(finalmodelA))
        
        
finalmodelB <- svyglm(as.numeric(nofluvax_bin) ~ gender_re2 * equality*age_grp2, 
                      design =brfss_svy,
                      family = quasibinomial(link = "log"))

exp(coef(finalmodelB))
exp(confint(finalmodelB))       
        
        
        
        
        
        
        
        
        
        ##8.24.21 - Redoing 1A & 1B age with collapsed age group
        
       #1A
        #Overall
         agefix1a_overall<-svytable(~age_grp2, brfss_svy)
         prop.table(agefix1a_overall)*100
        #By gender identity
         agefix1a<-svytable(~age_grp2 + gender_re, brfss_svy)
         prop.table(agefix1a, 2)*100
         
         #Unweighted counts
        table(brfss2015_2019$age_grp2)
        table(brfss2015_2019$age_grp2, brfss2015_2019$gender_re) 
        
        #1A
        #Overall
        
        agefix1b_overall<-svytable(~age_grp2, brfss_svy)
        prop.table(agefix1b_overall)*100
        
        #By protective vs restrictive
        agefix1b<-svytable(~age_grp2 + equality, brfss_svy)
        prop.table(agefix1b, 2)*100
        
        #Unweighted counts
        table(brfss2015_2019$age_grp2)
        table(brfss2015_2019$age_grp2, brfss2015_2019$equality) 
        
        