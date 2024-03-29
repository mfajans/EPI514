################################################################################
#EPI514 Project
#Coders: Collrane Frivold, Sarah Cox, Mark Fajans
#Unweighted & Weighted Table 1 script 
#Updated: May 11, 2021
################################################################################


##Housekeeping## 
################################################################################
# set up R (clear workspace)
rm(list = ls())
#set memory limit (PC only)
memory.limit(size=500000)
#set memory limit (Mac)
Sys.setenv('R_MAX_VSIZE'=32000000000)
#Widen the screen so that you can see the commands better 
options(width=150)
#Set directory 
  #Sarah's Directory
    setwd("/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data/")
    dataDir <- "/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data/"
    outputDir <- "/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/output"
  #Mark's directory
    #setwd("~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data/Compiled Files")
    #dataDir <- "~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data/Compiled Files"
  #Collrane's directory
    #setwd("C:/epi514/data/")
    #dataDir <- "C:/epi514/data/"
    #outputDir <- "C:/epi514/output/"
#Packages 
  library(survey)
  library(tableone)
  library(table1)
  library(progress)
  library(epiR)
  library(kableExtra) 
  library(readr)
#Load data 
  #File created at the end of the data pull merge script
  brfss2015_2019 <- readRDS(paste0(dataDir, "brfss2015_2019.rds")) 
  #Check
  #dim(brfss2015_2019 )
  # 1016012      24
################################################################################


##Unweighted Table 1 Creation (using clean dataset)##
################################################################################  
#Label variables for Table 1 Unweighted
table1::label(brfss2015_2019$sexorient) <-"Sexual Orientation"
table1::label(brfss2015_2019$age_grp) <- "Age (years)"
table1::label(brfss2015_2019$race_re) <- "Race/ethnicity"
table1::label(brfss2015_2019$edu_cat) <- "Highest Level of Education"
table1::label(brfss2015_2019$employ_recat_re) <-"Employment Status"
table1::label(brfss2015_2019$health_coverage_re) <-"Health Insurance"
table1::label(brfss2015_2019$medcost_re) <-"Poor Health Access (Could not see a doctor because of cost)"
table1::label(brfss2015_2019$mentalhlth_re) <-"Poor Mental Health Status"
table1::label(brfss2015_2019$dataYear_cat) <-"BRFSS Year"
#table1::label(brfss2015_2019$birthsex_re) <- "Sex at Birth"  
#table1::label(brfss2015_2019$bmi_cat) <- "Body Mass Index (kg/m^2)"
#table1::label(brfss2015_2019$hsld_inc) <-"Annual Household Income"

## Table 1a Unweighted
table1a_unweighted <- table1(~ sexorient
                        + age_grp
                        + race_re 
                        + edu_cat
                        + employ_recat_re
                        + health_coverage_re
                        + medcost_re
                        + mentalhlth_re
                      # + birthsex_re
                      # + bmi_cat
                      # + hsld_inc
                        + dataYear_cat | gender_re, data = brfss2015_2019,
                        render.missing=NULL, render.categorical ="FREQ (PCTnoNA%)", overall= "Total")
table1a_unweighted 

table1a_unweighted <- table1(~ sexorient
                             + age_grp
                             + race_re 
                             + edu_cat
                             + employ_recat_re
                             + health_coverage_re
                             + medcost_re
                             + mentalhlth_re
                             # + birthsex_re
                             # + bmi_cat
                             # + hsld_inc
                             + dataYear_cat | gender_re, data = brfss2015_2019,
                             render.missing=NULL, render.categorical ="FREQ (PCTnoNA%)", overall= "Total")
table1a_unweighted 
  
## Table 1b Unweighted
table1b_unweighted  <- table1(~ sexorient
                        + age_grp
                        + race_re 
                        + edu_cat
                        + employ_recat_re
                        + health_coverage_re
                        + medcost_re
                        + mentalhlth_re
                        # + birthsex_re
                        # + bmi_cat
                        # + hsld_inc
                        + dataYear_cat | equality, data = brfss2015_2019,
                        render.missing=NULL, render.categorical ="FREQ (PCTnoNA%)", overall= "Total")
table1b_unweighted  
################################################################################

  
##Weighted Table 1 Creation (using clean dataset)##
################################################################################
#Create survey weights 
#set how the survey package should handle rows with only 1 psu
options(survey.lonely.psu = "adjust")

#Assign weights
#Used "id = ~1" (instead of "id = ~psu") based on Nicole's feedback (5/10/21)
design <- svydesign(data = brfss2015_2019, 
                    id = ~1, 
                    strata = ~ststr, 
                    weights = ~finalwt)

design # check

#save design object 
#Save file and then you don't have to rerun the weighting 
write_rds(design, paste0(dataDir, "design.rds")) #save
  design <- readRDS(paste0(dataDir, "design.rds")) #check

################################################################################


##Create weighted Table 1 using TableOne## Archived bc very slow    
################################################################################
#Covariates for table 
#vars <- c("sexorient","age_grp", "race", "edu_cat", "hsld_inc",
          "employ_recat", "health_coverage", "medcost_re", 
          "mentalhlth", "dataYear_cat")
  
#Use TableOne to created weighted table
#tab1_weighted <- svyCreateTableOne(vars = vars, strata = "gender_re", 
#                                   data = design, test = FALSE)

#Use TableOne to created weighted table with race only 
#tab1_weighted <- svyCreateTableOne(vars = "race_re", strata = "gender_re", 
#                                   data = design, test = FALSE)

#Create a for loop 
#Made this, but it never fully ran so don't know if it would work 
#for (x in vars) {
#  tab1_weighted <- svyCreateTableOne(vars = x, strata = "gender_re", 
#                                     data = design, test = FALSE)
#}

#show table
#tab1_weighted #default formatting 
#summary(tab1_weighted) #detailed output 

#format table using kable 
#options(knitr.table.format = "word") 
#p <- print(tab1_weighted, printToggle = FALSE, noSpaces = TRUE)
#kable(p, format = "latex")

# directly with the tableone wrapper for the kable function
#kableone(tab1_weighted, booktabs = TRUE, format = "latex")

#tab1_weighted <- print(tab1_weighted)
#kbl(p, booktabs = TRUE, format = "latex")

#print pretty table 
#tab1_weighted %>%
#  kbl(caption = "Weighted table 1 - Race only") %>%
#  kable_classic(full_width = F, html_font = "Arial")

################################################################################        


##Weighted table using survey package## NOTE: still workin on this for all variables
################################################################################        

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
    
    