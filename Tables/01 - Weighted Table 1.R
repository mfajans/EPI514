################################################################################
#EPI514 Project
#Coders: Collrane Frivold, Sarah Cox, Mark Fajans
#Weighted Table 1 script 
#Updated: May 7, 2021
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
#Packages 
  library(survey)
  library(tableone)
#Load data 
  #File created at the end of the data pull merge script
  brfss2015_2019 <- read_rds(paste0(dataDir, "brfss2015_2019.rds")) #check
################################################################################


##Weighting##
################################################################################
#Create survey weights 
#set how the survey package should handle rows with only 1 psu
options(survey.lonely.psu = "adjust")

#assign weights
#Individual assignment used 1 here, but I don't think that's appropriate unless 
#all the weights as the same
#nest=T also from EPI 510
design <- svydesign(data = brfss2015_2019, 
                    id = ~psu, 
                    strata = ~ststr, 
                    weights = ~finalwt,
                    nest = TRUE)

design # check

#save design object 
#Save file and then you don't have to rerun the weighting which takes ~20 mins
write_rds(design, paste0(dataDir, "design.rds")) #save
  design <- read_rds(paste0(dataDir, "design.rds")) #check

################################################################################


##Create weighted Table 1##     
################################################################################
#Covariates for table 
vars <- c("sexorient","age_grp", "race", "edu_cat", "hsld_inc",
          "employ_recat", "health_coverage", "medcost_re", 
          "mentalhlth", "dataYear_cat")

#Use TableOne to created weighted table
tab1_weighted <- svyCreateTableOne(vars = vars, strata = "gender_re", 
                                   data = design, test = FALSE)
################################################################################

