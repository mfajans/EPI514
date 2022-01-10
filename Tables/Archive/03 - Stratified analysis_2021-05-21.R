+##########################################
#EPI514 Project
#Coders: Collrane Frivold, Sarah Cox, Mark Fajans
#Stratified Analysis - Table 3, Figure 2
#Last Updated: 2021-05-21
##########################################


# set up R (clear workspace)
rm(list = ls())
#set memory limit
memory.limit(size=500000)
#Set directory 
#Sarah's Directory
setwd("/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data")
#dataDir <- "/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data/"
#Mark's directory
#setwd("~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data")
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
#meta for forest plots
library(meta)


#Load data 
#Load data created in 00 - data pull script 
brfss <- readRDS("Databrfss2015_2019.rds") #check
brfss_t <- readRDS("Databrfss_t.rds") #check
#Mark- are these different files from the 00- Data pull merge? That output labels without "Data"?
brfss <- readRDS("brfss2015_2019.rds") #check
brfss_t <- readRDS("brfss_t.rds") #check

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
table(brfss$health_coverage_re, useNA="always")

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


#Reported healthcare cost being a barrier to access
table(brfss$medcost_re, useNA="always")

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
table(brfss$mentalhlth_re, useNA="always")

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


##########################################################
#Figure 2 - Forest Plot
##########################################################

# Example code

######## SECOND EXAMPLE - FOREST PLOT #########

#graph prevalence ratios from Individual Assignment Q10 Answer Key Estimates
#adjusted for income


#create a data frame of coefficients and CI (high and lo) to graph
df <- data.frame("Vars" = c("BMI <18.5 - Males", "BMI <18.5 - Females",
                            "BMI 25.0-29.9 - Males", "BMI 25.0-29.9 - Females",
                            "BMI 30+ - Males", "BMI 30+ - Females"),
                 "PR" = c(0.91, 0.62, 1.73, 2.11, 3.37, 3.80),
                 "CI_lo" = c(0.76, 0.53, 1.67, 2.03, 3.25, 3.67),
                 "CI_hi" = c(1.09, 0.73, 1.81, 2.19, 3.51, 3.94))

#create a label variable for each point; not totally necessary, but can be nice
df$label <- paste0(round(df$PR, 2),
                   " (",
                   round(df$CI_lo, 2),
                   ",",
                   round(df$CI_hi, 2),
                   ")")
#create a variable to label sex
df$sex <- c("Male", "Female",
            "Male", "Female",
            "Male", "Female")


#the geom_pointrange function creates our "forest plot" vertically by default, 
#so we have to set it up vertically and then flip it horizontally below
ggplot(df, aes(x = Vars, y = PR)) +
        geom_pointrange(aes(ymin = CI_lo,
                            ymax = CI_hi,
                            color= sex),
                        shape = 1) +
        scale_y_continuous(trans = "log", #show axis on log scale since these are multiplicative comparisons (ratios)
                           breaks = c(.5, .75, 1, 1.25, 2, 3)) +
        scale_x_discrete(labels = c("", "BMI <18.5", #Leaving the first group blank - use colors to show sex instead
                                    "", "BMI 25.0-29.9",
                                    "", "BMI 30+")) +
        geom_text(aes(label = label), #add the created labels
                  size = 2.5,
                  vjust = -1) + 
        geom_hline(yintercept = 1,
                   linetype = "dashed") +
        coord_flip() + #flip the graph horizontally
        scale_color_manual(values = c("#4b2e83", "#85754d")) + #telling what colors to use for sex
        theme_classic() +
        theme(axis.line = element_line(),
              plot.title = element_text(hjust = .5),
              plot.subtitle = element_text(face = "italic"),
              legend.position = "bottom",
              text = element_text(family = "Times")) +
        labs(y = "PR (95% CI)",
             x = "BMI Category",
             title = "Prevalence Ratio and 95% Confidence Intervals for Stratified Analysis of \nDiabetes Prevalence: \nWashington State BRFSS 2011-2018",
             subtitle = "Reference Group: BMI 18.5-24.9")
