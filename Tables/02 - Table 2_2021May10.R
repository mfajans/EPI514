################################################################################
#EPI514 Project
#Coders: Collrane Frivold, Sarah Cox, Mark Fajans
#Weighted Table 2 script 
#Updated: May 10, 2021
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
#setwd("/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data/BRFSS/XPT Files/")
#dataDir <- "/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data/BRFSS/XPT Files/"
#Mark's directory
#setwd("~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data/Compiled Files")
#dataDir <- "~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data/Compiled Files"
#Collrane's directory
#setwd("C:/epi514/data/")
#dataDir <- "C:/epi514/data/"
#outputDir <- "C:/epi514/output/"
#Packages 
library(epiR)
library(kableExtra)  
library(survey)
#Load data 
#Load data created in 00 - data pull script 
brfss2015_2019 <- readRDS(paste0(dataDir, "brfss2015_2019.rds")) #check
brfss_t <- readRDS(paste0(dataDir, "brfss_t.rds")) #check
################################################################################


##Table 2a## 
################################################################################

##Gender identity and flu vax##

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

##Table 2b##
################################################################################

#state policies on gender identity vs no flu vaccine 

#Create 2x2 table for state policies on gender identity versus no flu vax 
(rrTab2 <- table(brfss_t$equality, brfss_t$nofluvax_re, 
                 deparse.level = 2))
rrTab2

#Proportions
prop.table(rrTab2, 1)*100

#Create restrictive bin  
brfss_t <- brfss_t %>% mutate(
  restrictive_bin = case_when(
    equality=="Restrictive"~1,
    equality=="Protective" ~2),
  restrictive_bin = factor(restrictive_bin, labels=c("Restrictive", "Protective")), 
  nofluvax_re = factor(nofluvax_re, levels=c("No Influenza Vaccination", "Influenza Vaccination"))
)

#Create stratum
strat_1b <- with(brfss_t,
                 table(restrictive_bin, nofluvax_re))
#run analysis
epi.2by2(strat_1b) 


###Assess confounding###

#Assess confounding
#potential confounders: sexual orientation, mental health status, 
    #sexorient, mentalhlth_re, health_coverage_re

#Create 2x2 table for state policies on gender identity versus no flu vax 
    #Adjusting for poor mental health
    (rrTab3 <- table(brfss_t$equality, brfss_t$nofluvax_re, brfss_t$gender_re2, mentalhlth_re,
                 deparse.level = 2))
      rrTab3

#Proportions
prop.table(rrTab3, 1)*100

#Estimate RRs
rrStrat2 <- epi.2by2(dat=rrTab3, method="cohort.count")
rrStrat2


###Stratify by gender identity###

#tabulate 
table(brfss_t$equality, brfss_t$nofluvax_re, brfss_t$gender_re2, useNA = "always")

#Gender identity strata
strat_1_equality <- with(subset(brfss_t, gender_re2 == "Transgender Men"),
                         table(restrictive_bin, nofluvax_re))
strat_2_equality <- with(subset(brfss_t, gender_re2 == "Transgender Women"),
                         table(restrictive_bin, nofluvax_re))
strat_3_equality <- with(subset(brfss_t, gender_re2 == "Non-binary"),
                         table(restrictive_bin, nofluvax_re))


#Run analysis 
epi.2by2(strat_1_equality)
epi.2by2(strat_2_equality)
epi.2by2(strat_3_equality)
