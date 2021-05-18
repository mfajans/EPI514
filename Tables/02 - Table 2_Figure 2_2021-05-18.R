################################################################################
#EPI514 Project
#Coders: Collrane Frivold, Sarah Cox, Mark Fajans
#Table 2 and Figure 2 script 
#Updated: May 18, 2021
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
#setwd("~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data/Compiled Files")
#dataDir <- "~/Mark's Work/UW/Classes/Spring 2021/EPI 514 - Applications of Epi/Data/Compiled Files"
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

##Figure 2/ Supplemental Table
## Number of Adult Participants and Percent Unvaccinated 
#           Against Influenza in the Past Year 
#           by State and Gender Identity, BRFSS, 2015-2019
################################################################################

#Create datasets for each gender
brfss<- brfss2015_2019
brfss_cis<-brfss %>% filter(gender_re2== "Cisgender")
brfss_cism<-brfss %>% filter(gender_re== "Cisgender Men")
brfss_cisw<-brfss %>% filter(gender_re== "Cisgender Women")
brfss_tm<-brfss %>% filter(gender_re2== "Transgender Men")
brfss_tf<-brfss %>% filter(gender_re2== "Transgender Women")
brfss_nb<-brfss %>% filter(gender_re2== "Non-binary")

# Number of Adults by State
table(brfss$state, brfss$gender_re2)
table(brfss$state, brfss$gender_re)

#Create 2x2 table for state policies on gender identity versus no flu vax 
(rrTab2c_overall <- table(brfss$state, brfss$nofluvax_re,
                 deparse.level = 2))
(rrTab2c_cis <- table(brfss_cis$state, brfss_cis$nofluvax_re,
                          deparse.level = 2))
(rrTab2c_cism <- table(brfss_cism$state, brfss_cism$nofluvax_re,
                      deparse.level = 2))
(rrTab2c_cisw <- table(brfss_cisw$state, brfss_cisw$nofluvax_re,
                      deparse.level = 2))
(rrTab2c_tm <- table(brfss_tm$state, brfss_tm$nofluvax_re,
                          deparse.level = 2))
(rrTab2c_tf <- table(brfss_tf$state, brfss_tf$nofluvax_re,
                          deparse.level = 2))
(rrTab2c_nb <- table(brfss_nb$state, brfss_nb$nofluvax_re,
                          deparse.level = 2))

#Proportions
prop.table(rrTab2c_overall, 1)
prop.table(rrTab2c_cis, 1)
prop.table(rrTab2c_cism, 1)
prop.table(rrTab2c_cisw, 1)
prop.table(rrTab2c_tm, 1)
prop.table(rrTab2c_tf, 1)
prop.table(rrTab2c_nb, 1)

#Create Tables for maps
#Overall
map_overall<-as.data.frame(prop.table(rrTab2c_overall, 1))
map_overall<-map_overall %>% dplyr::rename(state = brfss.state,
                                           unvaccinated = Freq)
map_overall<-map_overall %>% mutate(unvaccinated_per = paste(round(unvaccinated*100,0), "%", sep= ""))
map_overall<-map_overall %>% mutate(unvaccinated_cat = case_when((unvaccinated > 0 & unvaccinated <= 0.1) ~ 1, 
                                                                 (unvaccinated > 0.1 & unvaccinated <= 0.2) ~ 2, 
                                                                 (unvaccinated > 0.2 & unvaccinated <= 0.3) ~ 3,
                                                                 (unvaccinated > 0.3 & unvaccinated <= 0.4) ~ 4,
                                                                 (unvaccinated > 0.4 & unvaccinated <= 0.5) ~ 5,
                                                                 (unvaccinated > 0.5 & unvaccinated <= 0.6) ~ 6,
                                                                 (unvaccinated > 0.6 & unvaccinated <= 0.7) ~ 7,
                                                                 (unvaccinated > 0.7 & unvaccinated <= 0.8) ~ 8,
                                                                 (unvaccinated > 0.8 & unvaccinated <= 0.9) ~ 9,
                                                                 (unvaccinated > 0.9 & unvaccinated <= 1) ~ 10))

map_overall$unvaccinated_cat  <-  factor(map_overall$unvaccinated_cat, 
                                         levels = c(1,2,3,4,5,6,7,8,9,10), 
                                         labels=c(">0% - 10%", 
                                                  ">10% - 20%", 
                                                  ">20% - 30%", 
                                                  ">30% - 40%", 
                                                  ">40% - 50%", 
                                                  ">50% - 60%", 
                                                  ">60% - 70%", 
                                                  ">70% - 80%", 
                                                  ">80% - 90%",
                                                  ">90% - 100%"))	
#Cisgender
map_cis<-as.data.frame(prop.table(rrTab2c_cis, 1))
map_cis<-map_cis %>% dplyr::rename(state = brfss_cis.state,
                                 unvaccinated = Freq)
map_cis<-map_cis %>% mutate(unvaccinated_per = paste(round(unvaccinated*100,0), "%", sep= ""))
map_cis<-map_cis %>% mutate(unvaccinated_cat = case_when((unvaccinated > 0 & unvaccinated <= 0.1) ~ 1, 
                                                       (unvaccinated > 0.1 & unvaccinated <= 0.2) ~ 2, 
                                                       (unvaccinated > 0.2 & unvaccinated <= 0.3) ~ 3,
                                                       (unvaccinated > 0.3 & unvaccinated <= 0.4) ~ 4,
                                                       (unvaccinated > 0.4 & unvaccinated <= 0.5) ~ 5,
                                                       (unvaccinated > 0.5 & unvaccinated <= 0.6) ~ 6,
                                                       (unvaccinated > 0.6 & unvaccinated <= 0.7) ~ 7,
                                                       (unvaccinated > 0.7 & unvaccinated <= 0.8) ~ 8,
                                                       (unvaccinated > 0.8 & unvaccinated <= 0.9) ~ 9,
                                                       (unvaccinated > 0.9 & unvaccinated <= 1) ~ 10))
map_cis$unvaccinated_cat  <-  factor(map_cis$unvaccinated_cat, 
                                    levels = c(1,2,3,4,5,6,7,8,9,10), 
                                    labels=c(">0% - 10%", 
                                             ">10% - 20%", 
                                             ">20% - 30%", 
                                             ">30% - 40%", 
                                             ">40% - 50%", 
                                             ">50% - 60%", 
                                             ">60% - 70%", 
                                             ">70% - 80%", 
                                             ">80% - 90%",
                                             ">90% - 100%"))	

#Non-binary
map_nb<-as.data.frame(prop.table(rrTab2c_nb, 1))
map_nb<-map_nb %>% dplyr::rename(state = brfss_nb.state,
                                           unvaccinated = Freq)
map_nb<-map_nb %>% mutate(unvaccinated_per = paste(round(unvaccinated*100,0), "%", sep= ""))
map_nb<-map_nb %>% mutate(unvaccinated_cat = case_when((unvaccinated > 0 & unvaccinated <= 0.1) ~ 1, 
                                                       (unvaccinated > 0.1 & unvaccinated <= 0.2) ~ 2, 
                                                       (unvaccinated > 0.2 & unvaccinated <= 0.3) ~ 3,
                                                       (unvaccinated > 0.3 & unvaccinated <= 0.4) ~ 4,
                                                       (unvaccinated > 0.4 & unvaccinated <= 0.5) ~ 5,
                                                       (unvaccinated > 0.5 & unvaccinated <= 0.6) ~ 6,
                                                       (unvaccinated > 0.6 & unvaccinated <= 0.7) ~ 7,
                                                       (unvaccinated > 0.7 & unvaccinated <= 0.8) ~ 8,
                                                       (unvaccinated > 0.8 & unvaccinated <= 0.9) ~ 9,
                                                       (unvaccinated > 0.9 & unvaccinated <= 1) ~ 10))
map_nb$unvaccinated_cat  <-  factor(map_nb$unvaccinated_cat, 
                                    levels = c(1,2,3,4,5,6,7,8,9,10), 
                                    labels=c(">0% - 10%", 
                                             ">10% - 20%", 
                                             ">20% - 30%", 
                                             ">30% - 40%", 
                                             ">40% - 50%", 
                                             ">50% - 60%", 
                                             ">60% - 70%", 
                                             ">70% - 80%", 
                                             ">80% - 90%",
                                             ">90% - 100%"))	

#Transgender Men
map_tm<-as.data.frame(prop.table(rrTab2c_tm, 1))
map_tm<-map_tm %>% dplyr::rename(state = brfss_tm.state,
                                 unvaccinated = Freq)
map_tm<-map_tm %>% mutate(unvaccinated_per = paste(round(unvaccinated*100,0), "%", sep= ""))
map_tm<-map_tm %>% mutate(unvaccinated_cat = case_when((unvaccinated > 0 & unvaccinated <= 0.1) ~ 1, 
                                                       (unvaccinated > 0.1 & unvaccinated <= 0.2) ~ 2, 
                                                       (unvaccinated > 0.2 & unvaccinated <= 0.3) ~ 3,
                                                       (unvaccinated > 0.3 & unvaccinated <= 0.4) ~ 4,
                                                       (unvaccinated > 0.4 & unvaccinated <= 0.5) ~ 5,
                                                       (unvaccinated > 0.5 & unvaccinated <= 0.6) ~ 6,
                                                       (unvaccinated > 0.6 & unvaccinated <= 0.7) ~ 7,
                                                       (unvaccinated > 0.7 & unvaccinated <= 0.8) ~ 8,
                                                       (unvaccinated > 0.8 & unvaccinated <= 0.9) ~ 9,
                                                       (unvaccinated > 0.9 & unvaccinated <= 1) ~ 10))
map_tm$unvaccinated_cat  <-  factor(map_tm$unvaccinated_cat, 
                                    levels = c(1,2,3,4,5,6,7,8,9,10), 
                                    labels=c(">0% - 10%", 
                                             ">10% - 20%", 
                                             ">20% - 30%", 
                                             ">30% - 40%", 
                                             ">40% - 50%", 
                                             ">50% - 60%", 
                                             ">60% - 70%", 
                                             ">70% - 80%", 
                                             ">80% - 90%",
                                             ">90% - 100%"))	


#Transgender Women
map_tf<-as.data.frame(prop.table(rrTab2c_tf, 1))
map_tf<-map_tf %>% dplyr::rename(state = brfss_tf.state,
                                 unvaccinated = Freq)
map_tf<-map_tf %>% mutate(unvaccinated_per = paste(round(unvaccinated*100,0), "%", sep= ""))
map_tf<-map_tf %>% mutate(unvaccinated_cat = case_when((unvaccinated > 0 & unvaccinated <= 0.1) ~ 1, 
                                                       (unvaccinated > 0.1 & unvaccinated <= 0.2) ~ 2, 
                                                       (unvaccinated > 0.2 & unvaccinated <= 0.3) ~ 3,
                                                       (unvaccinated > 0.3 & unvaccinated <= 0.4) ~ 4,
                                                       (unvaccinated > 0.4 & unvaccinated <= 0.5) ~ 5,
                                                       (unvaccinated > 0.5 & unvaccinated <= 0.6) ~ 6,
                                                       (unvaccinated > 0.6 & unvaccinated <= 0.7) ~ 7,
                                                       (unvaccinated > 0.7 & unvaccinated <= 0.8) ~ 8,
                                                       (unvaccinated > 0.8 & unvaccinated <= 0.9) ~ 9,
                                                       (unvaccinated > 0.9 & unvaccinated <= 1) ~ 10))
map_tf$unvaccinated_cat  <-  factor(map_tf$unvaccinated_cat, 
                                    levels = c(1,2,3,4,5,6,7,8,9,10), 
                                    labels=c(">0% - 10%", 
                                             ">10% - 20%", 
                                             ">20% - 30%", 
                                             ">30% - 40%", 
                                             ">40% - 50%", 
                                             ">50% - 60%", 
                                             ">60% - 70%", 
                                             ">70% - 80%", 
                                             ">80% - 90%",
                                             ">90% - 100%"))	
#Create maps 
#Overall
map_overall_plot <- plot_usmap(
    data = map_overall, values = "unvaccinated_cat", 
    color = "black") + 
  scale_fill_manual(values = c(#"#fff5f0",
                               #"#fee0d2",
                               "#fcbba1",
                               "#fc9272",
                               "#fb6a4a",
                               "#ef3b2c",
                               "#cb181d",
                               "#a50f15",
                               "#67000d",
                               "#000000"), 
                    na.value = "#d9d9d9", name = "% unvaccinated")+
  labs(#title = "Percentage of U.S. Adults Unvaccinated Against Influenza, BRFSS, 2015-2019", 
       subtitle = "Overall") +
  theme(legend.position = "right")

map_overall_plot

#Cisgender 
map_cis_plot <-  plot_usmap(
  data = map_cis, values = "unvaccinated_cat", 
  color = "black") + 
  scale_fill_manual(values = c(#"#fff5f0",
                               #"#fee0d2",
                               "#fcbba1",
                               "#fc9272",
                               "#fb6a4a",
                               "#ef3b2c",
                               "#cb181d",
                               "#a50f15",
                               "#67000d",
                               "#000000"), 
                    na.value = "#d9d9d9", name = "% unvaccinated")+
  labs(#title = "Percentage of U.S. Adults Unvaccinated Against Influenza, BRFSS, 2015-2019", 
       subtitle = "Cisgender") +
  theme(legend.position = "none")

map_cis_plot


#Non-binary 
map_nb_plot <-  plot_usmap(
    data = map_nb, values = "unvaccinated_cat", 
    color = "black") + 
  scale_fill_manual(values = c("#fff5f0",
                               "#fee0d2",
                               "#fcbba1",
                               "#fc9272",
                               "#fb6a4a",
                               "#ef3b2c",
                               "#cb181d",
                               "#a50f15",
                               "#67000d",
                               "#000000"), 
                    na.value = "#d9d9d9", name = "% unvaccinated")+
  labs(#title = "Percentage of U.S. Adults Unvaccinated Against Influenza, BRFSS, 2015-2019", 
       subtitle = "Non-binary") +
  theme(legend.position = "right")
  
  map_nb_plot  

#Transgender Men
  map_tm_plot <-  plot_usmap(
    data = map_tm, values = "unvaccinated_cat", 
    color = "black") + 
    scale_fill_manual(values = c("#fff5f0",
                                 "#fee0d2",
                                 "#fcbba1",
                                 "#fc9272",
                                 "#fb6a4a",
                                 "#ef3b2c",
                                 "#cb181d",
                                 "#a50f15",
                                 "#67000d",
                                 "#000000"), 
                      na.value = "#d9d9d9", name = "% unvaccinated")+
    labs(#title = "Percentage of U.S. Adults Unvaccinated Against Influenza, BRFSS, 2015-2019", 
      subtitle = "Transgender Men") +
    theme(legend.position = "none")
  
  map_tm_plot  

#Transgender Women
  map_tf_plot <-  plot_usmap(
    data = map_tf, values = "unvaccinated_cat", 
    color = "black") + 
    scale_fill_manual(values = c(#"#fff5f0",
                                 "#fee0d2",
                                 "#fcbba1",
                                 "#fc9272",
                                 "#fb6a4a",
                                 "#ef3b2c",
                                 "#cb181d",
                                 "#a50f15",
                                 "#67000d",
                                 "#000000"), 
                      na.value = "#d9d9d9", name = "% unvaccinated")+
    labs(#title = "Percentage of U.S. Adults Unvaccinated Against Influenza, BRFSS, 2015-2019", 
      subtitle = "Transgender Women") +
    theme(legend.position = "none")
  
  map_tf_plot  

#Create figures
#2a. Overall
map_overall_plot 
#2b. By Gender 
figure2b<-  map_cis_plot + map_nb_plot +  map_tf_plot + map_tm_plot +
    plot_layout(ncol = 2)
figure2b  


#Old code for maps (ignore)
  #gradient scale
  plot_usmap(
    data = map_overall, values = "unvaccinated", 
    #include = c("California", "Washington"),
    color = "black") + 
    scale_fill_continuous(low = "white", 
                          high = "red", 
                          name = "%",
                          label = scales::comma) + 
    labs(title = "Percentage of U.S. Adults Unvaccinated Against Influenza, BRFSS, 2015-2019", 
         subtitle = "All Genders") +
    theme(legend.position = "right")
  
  #reds
  plot_usmap(
    data = map_overall, values = "unvaccinated_cat", 
    #include = c("California", "Washington"),
    color = "black") + 
    scale_fill_brewer(palette = "Reds",  na.value = "#f0f0f0", name = "% unvaccinated") +
    scale_color_brewer(palette = "Set1") +
    labs(title = "Percentage of U.S. Adults Unvaccinated Against Influenza, BRFSS, 2015-2019", 
         subtitle = "All Genders") +
    theme(legend.position = "right")
  
  #4 colors red 
  plot_usmap(
    data = map_overall, values = "unvaccinated_cat", 
    #include = c("California", "Washington"),
    color = "black") + 
    scale_fill_manual(values = c("#fee5d9", "#fcae91", "#de2d26", "#a50f15"), na.value = "#f0f0f0", name = "% unvaccinated")+
    #scale_color_brewer(palette = "Set1") +
    labs(title = "Percentage of U.S. Adults Unvaccinated Against Influenza, BRFSS, 2015-2019", 
         subtitle = "All Genders") +
    theme(legend.position = "right")
  theme(legend.position = "right")
  
  

