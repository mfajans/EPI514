+##########################################
#EPI514 Project
#Coders: Collrane Frivold, Sarah Cox, Mark Fajans
#Stratified Analysis - Table 3
#Last Updated: 2021-05-12
##########################################


# set up R (clear workspace)
rm(list = ls())
#set memory limit
memory.limit(size=500000)
#Set directory 
#Sarah's Directory
#setwd("/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data)")
#dataDir <- "/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data/"
#Mark's directory
setwd("~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data")
#dataDir <- "~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data"
#Collrane's directory
#setwd("C:/epi514/data/")
#dataDir <- "C:/epi514/data/"
#outputDir <- "C:/epi514/output/"
#Packages 
library(epiR)
library(kableExtra)  
library(survey)


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


#Load data 
#Load data created in 00 - data pull script 
brfss <- readRDS("Databrfss2015_2019.rds") #check
brfss_t <- readRDS("Databrfss_t.rds") #check

#Create indicator exposure variables for bivariate comparisons
brfss <- brfss %>% mutate(
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

##########################################################
#Table 3A - Evaluating confounding/effect modification 
# via stratified analysis - Transgender men, transgender 
#women & non binary compared to cisgender
#Evaluating: Age, insurance coverage, reported cost of healthcare
#being a barrier and poor mental health
#
##########################################################

#crude
crude_transM<-with(brfss,
                   table(transM_bin, nofluvax_re))
crude_transF<- with(brfss,
                table(transF_bin, nofluvax_re))
crude_nb <- with(brfss, 
                table(nb_bin, nofluvax_re))

epi.2by2(crude_transM, method='cross.sectional')
epi.2by2(crude_transF, method='cross.sectional')
epi.2by2(crude_nb, method='cross.sectional')

#Age
table(brfss$age_grp, useNA="always")

#Age - Transgender Men
strat_1_transM <- with(subset(brfss, age_grp == "18-24"),
                      table(transM_bin, nofluvax_re))
strat_2_transM <- with(subset(brfss, age_grp == "25-34"),
                      table(transM_bin, nofluvax_re))
strat_3_transM <- with(subset(brfss, age_grp == "35-44"),
                      table(transM_bin, nofluvax_re))
strat_4_transM <- with(subset(brfss, age_grp == "45-54"),
                      table(transM_bin, nofluvax_re))
strat_5_transM <- with(subset(brfss, age_grp == "55-64"),
                      table(transM_bin, nofluvax_re))
strat_6_transM <- with(subset(brfss, age_grp == "65+"),
                      table(transM_bin, nofluvax_re))

epi.2by2(strat_1_transM, method='cross.sectional')
epi.2by2(strat_2_transM, method='cross.sectional')
epi.2by2(strat_3_transM, method='cross.sectional')
epi.2by2(strat_4_transM, method='cross.sectional')
epi.2by2(strat_5_transM, method='cross.sectional')
epi.2by2(strat_6_transM, method='cross.sectional')

#Age - Transgender Female
strat_1_transF <- with(subset(brfss, age_grp == "18-24"),
                       table(transF_bin, nofluvax_re))
strat_2_transF <- with(subset(brfss, age_grp == "25-34"),
                       table(transF_bin, nofluvax_re))
strat_3_transF <- with(subset(brfss, age_grp == "35-44"),
                       table(transF_bin, nofluvax_re))
strat_4_transF <- with(subset(brfss, age_grp == "45-54"),
                       table(transF_bin, nofluvax_re))
strat_5_transF <- with(subset(brfss, age_grp == "55-64"),
                       table(transF_bin, nofluvax_re))
strat_6_transF <- with(subset(brfss, age_grp == "65+"),
                       table(transF_bin, nofluvax_re))

epi.2by2(strat_1_transF, method='cross.sectional')
epi.2by2(strat_2_transF, method='cross.sectional')
epi.2by2(strat_3_transF, method='cross.sectional')
epi.2by2(strat_4_transF, method='cross.sectional')
epi.2by2(strat_5_transF, method='cross.sectional')
epi.2by2(strat_6_transF, method='cross.sectional')

#Age - Non-binary

strat_1_nb <- with(subset(brfss, age_grp == "18-24"),
                       table(nb_bin, nofluvax_re))
strat_2_nb <- with(subset(brfss, age_grp == "25-34"),
                       table(nb_bin, nofluvax_re))
strat_3_nb <- with(subset(brfss, age_grp == "35-44"),
                       table(nb_bin, nofluvax_re))
strat_4_nb <- with(subset(brfss, age_grp == "45-54"),
                       table(nb_bin, nofluvax_re))
strat_5_nb <- with(subset(brfss, age_grp == "55-64"),
                       table(nb_bin, nofluvax_re))
strat_6_nb <- with(subset(brfss, age_grp == "65+"),
                       table(nb_bin, nofluvax_re))

epi.2by2(strat_1_nb, method='cross.sectional')
epi.2by2(strat_2_nb, method='cross.sectional')
epi.2by2(strat_3_nb, method='cross.sectional')
epi.2by2(strat_4_nb, method='cross.sectional')
epi.2by2(strat_5_nb, method='cross.sectional')
epi.2by2(strat_6_nb, method='cross.sectional')



#Insurance coverage
table(brfss$health_coverage_re, useNA="always")

#Insurance coverage - Transgender Men
strat_1_transM <- with(subset(brfss, health_coverage_re == "Yes"),
                       table(transM_bin, nofluvax_re))
strat_2_transM <- with(subset(brfss, health_coverage_re == "No"),
                       table(transM_bin, nofluvax_re))


epi.2by2(strat_1_transM, method='cross.sectional')
epi.2by2(strat_2_transM, method='cross.sectional')


#Insurance coverage- Transgender Female
strat_1_transF <- with(subset(brfss, health_coverage_re == "Yes"),
                       table(transF_bin, nofluvax_re))
strat_2_transF <- with(subset(brfss, health_coverage_re == "No"),
                       table(transF_bin, nofluvax_re))

epi.2by2(strat_1_transF, method='cross.sectional')
epi.2by2(strat_2_transF, method='cross.sectional')


##Insurance coverage- Non-binary

strat_1_nb <- with(subset(brfss, health_coverage_re == "Yes"),
                       table(nb_bin, nofluvax_re))
strat_2_nb <- with(subset(brfss, health_coverage_re == "No"),
                       table(nb_bin, nofluvax_re))

epi.2by2(strat_1_nb, method='cross.sectional')
epi.2by2(strat_2_nb, method='cross.sectional')


#Reported healthcare cost being a barrier to access
table(brfss$medcost_re, useNA="always")

#Insurance coverage - Transgender Men
strat_1_transM <- with(subset(brfss, medcost_re == "Yes"),
                       table(transM_bin, nofluvax_re))
strat_2_transM <- with(subset(brfss, medcost_re == "No"),
                       table(transM_bin, nofluvax_re))


epi.2by2(strat_1_transM, method='cross.sectional')
epi.2by2(strat_2_transM, method='cross.sectional')


#Insurance coverage- Transgender Female
strat_1_transF <- with(subset(brfss, medcost_re == "Yes"),
                       table(transF_bin, nofluvax_re))
strat_2_transF <- with(subset(brfss, medcost_re == "No"),
                       table(transF_bin, nofluvax_re))

epi.2by2(strat_1_transF, method='cross.sectional')
epi.2by2(strat_2_transF, method='cross.sectional')


##Insurance coverage- Non-binary

strat_1_nb <- with(subset(brfss, medcost_re == "Yes"),
                   table(nb_bin, nofluvax_re))
strat_2_nb <- with(subset(brfss, medcost_re == "No"),
                   table(nb_bin, nofluvax_re))

epi.2by2(strat_1_nb, method='cross.sectional')
epi.2by2(strat_2_nb, method='cross.sectional')


#Reported poor mental health within last 30 days (>=14 days)
table(brfss$mentalhlth_re, useNA="always")

#Reported poor mental health within last 30 days - Transgender Men
strat_1_transM <- with(subset(brfss, mentalhlth_re == ">=14 days"),
                       table(transM_bin, nofluvax_re))
strat_2_transM <- with(subset(brfss, mentalhlth_re == "<14 days"),
                       table(transM_bin, nofluvax_re))


epi.2by2(strat_1_transM, method='cross.sectional')
epi.2by2(strat_2_transM, method='cross.sectional')


#Reported poor mental health within last 30 days- Transgender Female
strat_1_transF <- with(subset(brfss, mentalhlth_re == ">=14 days"),
                       table(transF_bin, nofluvax_re))
strat_2_transF <- with(subset(brfss, mentalhlth_re == "<14 days"),
                       table(transF_bin, nofluvax_re))

epi.2by2(strat_1_transF, method='cross.sectional')
epi.2by2(strat_2_transF, method='cross.sectional')


##Reported poor mental health within last 30 days- Non-binary

strat_1_nb <- with(subset(brfss, mentalhlth_re == ">=14 days"),
                   table(nb_bin, nofluvax_re))
strat_2_nb <- with(subset(brfss, mentalhlth_re == "<14 days"),
                   table(nb_bin, nofluvax_re))

epi.2by2(strat_1_nb, method='cross.sectional')
epi.2by2(strat_2_nb, method='cross.sectional')



#MF: Any other variables to add?



##########################################################
#Table 3B - Evaluating confounding/effect modification 
# via stratified analysis - Protective vs restrictive, stratified by 
#Transgender men, transgender women, non binary & cisgender
#Evaluating: Age, insurance coverage, reported cost of healthcare
#being a barrier and poor mental health
#
##########################################################


#Create restrictive bin  
brfss <- brfss %>% mutate(
        restrictive_bin = case_when(
                equality=="Restrictive"~1,
                equality=="Protective" ~2),
        restrictive_bin = factor(restrictive_bin, labels=c("Restrictive", "Protective")), 
        nofluvax_re = factor(nofluvax_re, levels=c("No Influenza Vaccination", "Influenza Vaccination"))
)


#Crude
table(brfss$gender_re2)

crude_cis_state<-with(subset(brfss, gender_re2 == "Cisgender"),
                   table(restrictive_bin, nofluvax_re))

crude_transM_state<-with(subset(brfss, gender_re2 == "Transgender Men"),
                         table(restrictive_bin, nofluvax_re))

crude_transF_state<- with(subset(brfss, gender_re2 == "Transgender Women"),
                          table(restrictive_bin, nofluvax_re))

crude_nb_state <- with(subset(brfss, gender_re2 == "Non-binary"),
                       table(restrictive_bin, nofluvax_re))

epi.2by2(crude_cis_state, method='cross.sectional')
epi.2by2(crude_transM_state, method='cross.sectional')
epi.2by2(crude_transF_state, method='cross.sectional')
epi.2by2(crude_nb_state, method='cross.sectional')

#Matches Table 2B, looks good!

#Age
table(brfss$age_grp, useNA="always")


#Age - Cisgender
strat_1_cis_state <- with(subset(brfss, age_grp == "18-24" & gender_re2 == "Cisgender"),
                          table(restrictive_bin, nofluvax_re))
strat_2_cis_state <- with(subset(brfss, age_grp == "25-34" & gender_re2 == "Cisgender"),
                          table(restrictive_bin, nofluvax_re))
strat_3_cis_state <- with(subset(brfss, age_grp == "35-44" & gender_re2 == "Cisgender"),
                          table(restrictive_bin, nofluvax_re))
strat_4_cis_state <- with(subset(brfss, age_grp == "45-54" & gender_re2 == "Cisgender"),
                          table(restrictive_bin, nofluvax_re))
strat_5_cis_state<- with(subset(brfss, age_grp == "55-64" & gender_re2 == "Cisgender"),
                         table(restrictive_bin, nofluvax_re))
strat_6_cis_state <- with(subset(brfss, age_grp == "65+" & gender_re2 == "Cisgender"),
                          table(restrictive_bin, nofluvax_re))

epi.2by2(strat_1_cis_state, method='cross.sectional')
epi.2by2(strat_2_cis_state, method='cross.sectional')
epi.2by2(strat_3_cis_state, method='cross.sectional')
epi.2by2(strat_4_cis_state, method='cross.sectional')
epi.2by2(strat_5_cis_state, method='cross.sectional')
epi.2by2(strat_6_cis_state, method='cross.sectional')

#Age - Transgender Men
strat_1_transM_state <- with(subset(brfss, age_grp == "18-24" & gender_re2 == "Transgender Men"),
                          table(restrictive_bin, nofluvax_re))
strat_2_transM_state <- with(subset(brfss, age_grp == "25-34" & gender_re2 == "Transgender Men"),
                          table(restrictive_bin, nofluvax_re))
strat_3_transM_state <- with(subset(brfss, age_grp == "35-44" & gender_re2 == "Transgender Men"),
                          table(restrictive_bin, nofluvax_re))
strat_4_transM_state <- with(subset(brfss, age_grp == "45-54" & gender_re2 == "Transgender Men"),
                          table(restrictive_bin, nofluvax_re))
strat_5_transM_state<- with(subset(brfss, age_grp == "55-64" & gender_re2 == "Transgender Men"),
                         table(restrictive_bin, nofluvax_re))
strat_6_transM_state <- with(subset(brfss, age_grp == "65+" & gender_re2 == "Transgender Men"),
                          table(restrictive_bin, nofluvax_re))

epi.2by2(strat_1_transM_state, method='cross.sectional')
epi.2by2(strat_2_transM_state, method='cross.sectional')
epi.2by2(strat_3_transM_state, method='cross.sectional')
epi.2by2(strat_4_transM_state, method='cross.sectional')
epi.2by2(strat_5_transM_state, method='cross.sectional')
epi.2by2(strat_6_transM_state, method='cross.sectional')

#Age - Transgender Female
strat_1_transF_state <- with(subset(brfss, age_grp == "18-24" & gender_re2 == "Transgender Women"),
                             table(restrictive_bin, nofluvax_re))
strat_2_transF_state <- with(subset(brfss, age_grp == "25-34" & gender_re2 == "Transgender Women"),
                             table(restrictive_bin, nofluvax_re))
strat_3_transF_state <- with(subset(brfss, age_grp == "35-44" & gender_re2 == "Transgender Women"),
                             table(restrictive_bin, nofluvax_re))
strat_4_transF_state <- with(subset(brfss, age_grp == "45-54" & gender_re2 == "Transgender Women"),
                             table(restrictive_bin, nofluvax_re))
strat_5_transF_state<- with(subset(brfss, age_grp == "55-64" & gender_re2 == "Transgender Women"),
                            table(restrictive_bin, nofluvax_re))
strat_6_transF_state <- with(subset(brfss, age_grp == "65+" & gender_re2 == "Transgender Women"),
                             table(restrictive_bin, nofluvax_re))

epi.2by2(strat_1_transF_state, method='cross.sectional')
epi.2by2(strat_2_transF_state, method='cross.sectional')
epi.2by2(strat_3_transF_state, method='cross.sectional')
epi.2by2(strat_4_transF_state, method='cross.sectional')
epi.2by2(strat_5_transF_state, method='cross.sectional')
epi.2by2(strat_6_transF_state, method='cross.sectional')

#Age - Non-binary

strat_1_nb_state <- with(subset(brfss, age_grp == "18-24" & gender_re2 == "Non-binary"),
                             table(restrictive_bin, nofluvax_re))
strat_2_nb_state <- with(subset(brfss, age_grp == "25-34" & gender_re2 == "Non-binary"),
                             table(restrictive_bin, nofluvax_re))
strat_3_nb_state <- with(subset(brfss, age_grp == "35-44" & gender_re2 == "Non-binary"),
                             table(restrictive_bin, nofluvax_re))
strat_4_nb_state <- with(subset(brfss, age_grp == "45-54" & gender_re2 == "Non-binary"),
                             table(restrictive_bin, nofluvax_re))
strat_5_nb_state<- with(subset(brfss, age_grp == "55-64" & gender_re2 == "Non-binary"),
                            table(restrictive_bin, nofluvax_re))
strat_6_nb_state <- with(subset(brfss, age_grp == "65+" & gender_re2 == "Non-binary"),
                             table(restrictive_bin, nofluvax_re))

epi.2by2(strat_1_nb_state, method='cross.sectional')
epi.2by2(strat_2_nb_state, method='cross.sectional')
epi.2by2(strat_3_nb_state, method='cross.sectional')
epi.2by2(strat_4_nb_state, method='cross.sectional')
epi.2by2(strat_5_nb_state, method='cross.sectional')
epi.2by2(strat_6_nb_state, method='cross.sectional')


#Insurance Coverage

#Insurance coverage - Cisgender
strat_1_cis_state <- with(subset(brfss, health_coverage_re == "Yes" & gender_re2 == "Cisgender"),
                             table(restrictive_bin, nofluvax_re))
strat_2_cis_state <- with(subset(brfss, health_coverage_re == "No" & gender_re2 == "Cisgender"),
                             table(restrictive_bin, nofluvax_re))


epi.2by2(strat_1_cis_state, method='cross.sectional')
epi.2by2(strat_2_cis_state, method='cross.sectional')



#Insurance coverage - Transgender Men
strat_1_transM_state <- with(subset(brfss, health_coverage_re == "Yes" & gender_re2 == "Transgender Men"),
                       table(restrictive_bin, nofluvax_re))
strat_2_transM_state <- with(subset(brfss, health_coverage_re == "No" & gender_re2 == "Transgender Men"),
                       table(restrictive_bin, nofluvax_re))


epi.2by2(strat_1_transM_state, method='cross.sectional')
epi.2by2(strat_2_transM_state, method='cross.sectional')



#Insurance coverage - Transgender Women
strat_1_transF_state <- with(subset(brfss, health_coverage_re == "Yes" & gender_re2 == "Transgender Women"),
                             table(restrictive_bin, nofluvax_re))
strat_2_transF_state <- with(subset(brfss, health_coverage_re == "No" & gender_re2 == "Transgender Women"),
                             table(restrictive_bin, nofluvax_re))


epi.2by2(strat_1_transF_state, method='cross.sectional')
epi.2by2(strat_2_transF_state, method='cross.sectional')

#Insurance coverage - Non-binary
strat_1_nb_state <- with(subset(brfss, health_coverage_re == "Yes" & gender_re2 == "Non-binary"),
                             table(restrictive_bin, nofluvax_re))
strat_2_nb_state <- with(subset(brfss, health_coverage_re == "No" & gender_re2 == "Non-binary"),
                             table(restrictive_bin, nofluvax_re))


epi.2by2(strat_1_nb_state, method='cross.sectional')
epi.2by2(strat_2_nb_state, method='cross.sectional')


#Reported cost being a barrier to healthcare access

#cost barrier - Cisgender
strat_1_cis_state <- with(subset(brfss, medcost_re == "Yes" & gender_re2 == "Cisgender"),
                          table(restrictive_bin, nofluvax_re))
strat_2_cis_state <- with(subset(brfss, medcost_re == "No" & gender_re2 == "Cisgender"),
                          table(restrictive_bin, nofluvax_re))


epi.2by2(strat_1_cis_state, method='cross.sectional')
epi.2by2(strat_2_cis_state, method='cross.sectional')



#cost barrier - Transgender Men
strat_1_transM_state <- with(subset(brfss, medcost_re == "Yes" & gender_re2 == "Transgender Men"),
                             table(restrictive_bin, nofluvax_re))
strat_2_transM_state <- with(subset(brfss, medcost_re == "No" & gender_re2 == "Transgender Men"),
                             table(restrictive_bin, nofluvax_re))


epi.2by2(strat_1_transM_state, method='cross.sectional')
epi.2by2(strat_2_transM_state, method='cross.sectional')



#cost barrier - Transgender Women
strat_1_transF_state <- with(subset(brfss, medcost_re == "Yes" & gender_re2 == "Transgender Women"),
                             table(restrictive_bin, nofluvax_re))
strat_2_transF_state <- with(subset(brfss, medcost_re == "No" & gender_re2 == "Transgender Women"),
                             table(restrictive_bin, nofluvax_re))


epi.2by2(strat_1_transF_state, method='cross.sectional')
epi.2by2(strat_2_transF_state, method='cross.sectional')

#cost barrier - Non-binary
strat_1_nb_state <- with(subset(brfss, medcost_re == "Yes" & gender_re2 == "Non-binary"),
                         table(restrictive_bin, nofluvax_re))
strat_2_nb_state <- with(subset(brfss, medcost_re == "No" & gender_re2 == "Non-binary"),
                         table(restrictive_bin, nofluvax_re))


epi.2by2(strat_1_nb_state, method='cross.sectional')
epi.2by2(strat_2_nb_state, method='cross.sectional')


#Reported poor mental health within the last 30 days (>=14 days)

#Poor mental health - Cisgender
strat_1_cis_state <- with(subset(brfss, mentalhlth_re == ">=14 days" & gender_re2 == "Cisgender"),
                          table(restrictive_bin, nofluvax_re))
strat_2_cis_state <- with(subset(brfss, mentalhlth_re == "<14 days" & gender_re2 == "Cisgender"),
                          table(restrictive_bin, nofluvax_re))


epi.2by2(strat_1_cis_state, method='cross.sectional')
epi.2by2(strat_2_cis_state, method='cross.sectional')



#Poor mental health - Transgender Men
strat_1_transM_state <- with(subset(brfss, mentalhlth_re == ">=14 days" & gender_re2 == "Transgender Men"),
                             table(restrictive_bin, nofluvax_re))
strat_2_transM_state <- with(subset(brfss, mentalhlth_re == "<14 days" & gender_re2 == "Transgender Men"),
                             table(restrictive_bin, nofluvax_re))


epi.2by2(strat_1_transM_state, method='cross.sectional')
epi.2by2(strat_2_transM_state, method='cross.sectional')



#Poor mental health - Transgender Women
strat_1_transF_state <- with(subset(brfss, mentalhlth_re == ">=14 days" & gender_re2 == "Transgender Women"),
                             table(restrictive_bin, nofluvax_re))
strat_2_transF_state <- with(subset(brfss, mentalhlth_re == "<14 days" & gender_re2 == "Transgender Women"),
                             table(restrictive_bin, nofluvax_re))


epi.2by2(strat_1_transF_state, method='cross.sectional')
epi.2by2(strat_2_transF_state, method='cross.sectional')

#Poor mental health - Non-binary
strat_1_nb_state <- with(subset(brfss, mentalhlth_re == ">=14 days" & gender_re2 == "Non-binary"),
                         table(restrictive_bin, nofluvax_re))
strat_2_nb_state <- with(subset(brfss, mentalhlth_re == "<14 days" & gender_re2 == "Non-binary"),
                         table(restrictive_bin, nofluvax_re))


epi.2by2(strat_1_nb_state, method='cross.sectional')
epi.2by2(strat_2_nb_state, method='cross.sectional')



