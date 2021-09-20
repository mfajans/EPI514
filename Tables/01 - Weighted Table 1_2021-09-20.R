################################################################################
#EPI514 Project
#Coders: Collrane Frivold, Sarah Cox, Mark Fajans
#Weighted Table 1 script 
#Updated: September 20, 2021
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
    setwd("/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/")
   dataDir <- "/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data/"
   outputDir <- "/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data/"
  #Mark's directory
    #setwd("~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data/Compiled Files")
    #dataDir <- "~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data/Compiled Files"
  #Collrane's directory
    setwd("C:/epi514/data/")
    dataDir <- "C:/epi514/data/"
    outputDir <- "C:/epi514/output/"
#Packages 
  library(survey)
  library(table1)
  library(epiR)
  library(kableExtra) 
  library(gtsummary) 
  library(flextable)  
  library(foreign)
  library(tidyverse)
  library(plyr)
  library(dplyr)
  library(knitr)
#Load data 
  #File created at the end of the data pull merge script
  brfss2015_2019 <- readRDS(paste0(dataDir, "brfss2015_2019.rds")) #check
################################################################################
  
##Weighting##
################################################################################
#Create survey weights 
#set how the survey package should handle rows with only 1 psu
options(survey.lonely.psu = "adjust")

#Assign weights
#Used "id = ~1" based on Nicole's feedback (5/10/21)
design <- svydesign(data = brfss2015_2019, 
                    id = ~1, 
                    strata = ~ststr, 
                    weights = ~finalwt)

design # check

table(brfss2015_2019$ethnicity_re,  useNA = "always")
names(brfss2015_2019)
#save design object 
write_rds(design, paste0(dataDir, "design.rds")) #save
  design <- readRDS(paste0(dataDir, "design.rds")) #check

################################################################################
  
##Weighted table using survey package##
################################################################################        
#Use results from survey package to verify gtsummary output and that weighting is correct 
  

#Table for race variable only 
#Assign svytable to variables
    raceTab <- svytable(~race_re + gender_re, design)
    raceTab # check
#Create frequency tables with proportions
    raceTabOutput <- data.frame(cbind(raceTab, prop.table(raceTab, 2)*100))
#Create CSV to copy tables to Word
    write.csv(raceTabOutput, paste0(outputDir, "raceTabOutput.csv"), row.names = TRUE) 
#Format table 
    p <- print(raceTabOutput, printToggle = FALSE, noSpaces = TRUE, digits = 2)

    #print pretty table 
    p %>%
      kbl(caption = "Weighted table 1 - Race only", digits = 2) %>%
      kable_classic(full_width = F, html_font = "Calibri")

#Table for sexual identity variable 
    
    #Assign svytable to variables
    sexualidentTab <- svytable(~sexualident + gender_re, design)
    sexualidentTab # check
    #Create frequency tables with proportions
    sexualidentOutput <- data.frame(cbind(sexualidentTab, prop.table(sexualidentTab, 2)*100))
    #Create CSV to copy tables to Word
    write.csv(sexualidentOutput, paste0(outputDir, "sexualidentOutput.csv"), row.names = TRUE) 
    #Format table 
    p <- print(sexualidentOutput, printToggle = FALSE, noSpaces = TRUE, digits = 2)
    
    #print pretty table 
    p %>%
      kbl(caption = "Weighted table 1 - Sexual identity only", digits = 2) %>%
      kable_classic(full_width = F, html_font = "Calibri")
    
################################################################################        
    
    
##Weighted prevalence##    
################################################################################        
    #Prevalence of flu vaccination (weighted)
    svytable(~fluvax_re, design) %>% 
      prop.table()
    
    #Prevalence of flu vaccination within each gender identity category (weighted)
    svytable(~gender_re2 + fluvax_re, design) %>% 
      prop.table(margin = 2)
    
################################################################################        

    
##gtsummary package##
##weighted table 1## 
################################################################################        

#Create survey weights
    svy_brfss <- 
      survey::svydesign(
        id = ~1, 
        weights = ~finalwt, 
        data = brfss2015_2019, 
        strata = ~ststr
      )

#save survey weight object 
    saveRDS(svy_brfss, paste0(dataDir, "svy_brfss.rds")) #save
    svy_brfss <- readRDS(paste0(dataDir, "svy_brfss.rds")) #check
    
#Table 1a: unweighted frequencies, weighted percentages
    #NOTE: I cannot figure out how to incorporate unweighted n values into the header 
    #so those values must be updated manually; all other frequencies are unweighted
    
table1a <- svy_brfss %>%
  tbl_svysummary(
    # stratify summary statistics by gender identity
    by = gender_re, 
    #Select descriptive stats of interest 
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
    # summarize a subset of the covariates 
    include = c(age_grp, race_re, ethnicity_re, edu_cat, sexualident, hsld_inc, 
                employ_recat_re, health_coverage_re, medcost_re, mentalhlth_re, gender_re),
    #digits
    digits = all_categorical() ~ c(0,1),
    # adding labels to table
    label = list(age_grp ~ "Age (years)",
                 race_re ~ "Race",
                 ethnicity_re ~ "Ethnicity",
                 edu_cat ~ "Highest Level of Education",
                 sexualident ~ "Sexual identity",
                 hsld_inc ~ "Annual Household Income",
                 employ_recat_re ~ "Employment Status",
                 health_coverage_re ~ "Health Insurance",
                 medcost_re ~ "Poor Health Access (Could not see a doctor because of cost)",
                 mentalhlth_re ~ "Poor Mental Health Status")
  ) %>%
  add_overall() %>% #overall column
  as_flex_table() #create word compatible file 

#Save to Word doc 
save_as_docx("Table 1a" = table1a, path = paste0(outputDir, "Table1a.docx"))


# updating headers, remove all footnotes, add spanning header
    #NOTE: I have not been able to get the header code to run, but leaving for now 
    #in case we figure it out 

#modify_table1a <- table1a %>%
  #modify_header(
   # update = all_stat_cols() ~ "**{gender_re}**, N = {N_unweighted} ({style_percent(p)}%)")


#Table 1b: unweighted frequencies, weighted percentages

table1b <- svy_brfss %>%
  tbl_svysummary(
    # stratify summary statistics by protective/restrictive state policies
    by = equality, 
    #Select descriptive stats of interest 
    statistic = list(all_categorical() ~ "{n_unweighted} ({p}%)"),
    # summarize a subset of the covariates 
    include = c(gender_re, age_grp, race_re, ethnicity_re, edu_cat, sexualident, hsld_inc, 
                employ_recat_re, health_coverage_re, medcost_re, mentalhlth_re, equality),
    #digits
    digits = all_categorical() ~ c(0,1),
    # adding labels to table
    label = list(gender_re ~ "Gender identity",
                 age_grp ~ "Age (years)",
                 race_re ~ "Race",
                 ethnicity_re ~ "Ethnicity",
                 edu_cat ~ "Highest level of education",
                 sexualident ~ "Sexual identity",
                 hsld_inc ~ "Annual household income",
                 employ_recat_re ~ "Employment status",
                 health_coverage_re ~ "Health insurance status",
                 medcost_re ~ "Poor health access (could not see a doctor because of cost)",
                 mentalhlth_re ~ "Poor mental health status (≥14 poor mental health days in last 30 days)")) %>%
  add_overall() %>% #overall column
  as_flex_table() #create word compatible file 

#Save to Word doc 
save_as_docx("Table 1b" = table1b, path = paste0(outputDir, "Table1b.docx"))


################################################################################        

##unweighted table for comparison##
##Use this to pull out n values for column headers
################################################################################        

#Create unweighted table1a
table1a_unweighted <- brfss2015_2019 %>%
  tbl_summary(
    # stratify summary statistics by gender identity
    by = gender_re, 
    #add summary statistics
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    # summarize a subset of the columns
    include = c(age_grp, race_re, ethnicity_re, edu_cat, sexualident, hsld_inc, 
                employ_recat_re, health_coverage_re, medcost_re, mentalhlth_re, gender_re),
    #digits
    digits = all_categorical() ~ c(0,1),
    # adding labels to table
    label = list(age_grp ~ "Age (years)",
                  race_re ~ "Race",
                  ethnicity_re ~ "Ethnicity",
                  edu_cat ~ "Highest level of education",
                  sexualident ~ "Sexual identity",
                  hsld_inc ~ "Annual household income",
                  employ_recat_re ~ "Employment status",
                  health_coverage_re ~ "Health insurance status",
                  medcost_re ~ "Poor health access (Could not see a doctor because of cost)",
                  mentalhlth_re ~ "Poor mental health status (≥14 poor mental health days in last 30 days)")
  ) %>%
  add_overall() %>% #overall column
  as_flex_table() #create word compatible file 

#Save to Word doc 
save_as_docx("Table 1a_UNWEIGHTED" = table1a_unweighted, path = paste0(outputDir, "Table1a_UNWEIGHTED.docx"))


#Create unweighted table1b

table1b_unweighted <- brfss2015_2019 %>%
  tbl_summary(
    # stratify summary statistics by restrictive/protective states 
    by = equality, 
    #add summary statistics
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    # summarize a subset of the columns
    include = c(gender_re, age_grp, race_re, ethnicity_re, edu_cat, sexualident, hsld_inc, 
                employ_recat_re, health_coverage_re, medcost_re, mentalhlth_re, equality),
    #digits
    digits = all_categorical() ~ c(0,1),
    # adding labels to table
    label = list(gender_re ~ "Gender identity",
                 age_grp ~ "Age (years)",
                 race_re ~ "Race",
                 ethnicity_re ~ "Ethnicity",
                 edu_cat ~ "Highest level of education",
                 sexualident ~ "Sexual identity",
                 hsld_inc ~ "Annual household income",
                 employ_recat_re ~ "Employment status",
                 health_coverage_re ~ "Health insurance status",
                 medcost_re ~ "Poor health access (could not see a doctor because of cost)",
                 mentalhlth_re ~ "Poor mental health status (≥14 poor mental health days in last 30 days)")
  ) %>%
  add_overall() %>% #overall column
  as_flex_table() #create word compatible file 

#Save to Word doc 
save_as_docx("Table 1b_UNWEIGHTED" = table1b_unweighted, path = paste0(outputDir, "Table1b_UNWEIGHTED.docx"))



################################################################################        


####################
#RETIRED: Unweighted Table 1 Creation (using clean dataset)
#Using Table1 package
####################

#Label variables for Table 1 New
table1::label(brfss2015_2019$birthsex_re) <- "Sex at Birth"
table1::label(brfss2015_2019$sexualident) <-"Sexual Orientation"
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

## Table 1a Old
table1a_old <- table1(~ birthsex_re
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
table1a_old

## Table 1b Old
table1b_old <- table1(~ birthsex_re
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
table1b_old 

