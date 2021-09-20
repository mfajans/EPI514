################################################################################
#EPI514 Project
#Coders: Collrane Frivold, Sarah Cox, Mark Fajans
#Figure 2 script 
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


# Recreating nofluvax as a (0,1) binomial variable
brfss2015_2019 <- brfss2015_2019 %>% mutate(
  nofluvax_bin = case_when(
    nofluvax_re=="No Influenza Vaccination"~1,
    nofluvax_re=="Influenza Vaccination"~0,),
  
  nofluvax_bin.f=factor(nofluvax_bin, labels=c("Influenza Vaccination", "No Influenza Vaccination"))
)


#Subset by gender
table(brfss2015_2019$gender_re)
brfss_cism<-brfss2015_2019 %>% filter(gender_re== "Cisgender Men")
#check #table(brfss_cism$gender_re)
brfss_cisw<-brfss2015_2019 %>% filter(gender_re== "Cisgender Women")
brfss_tm<-brfss2015_2019 %>% filter(gender_re== "Transgender Men")
brfss_tf<-brfss2015_2019 %>% filter(gender_re== "Transgender Women")
brfss_nb<-brfss2015_2019 %>% filter(gender_re== "Non-binary")


#Create survey weights
#overall
svy_brfss <- 
  survey::svydesign(
    id = ~1, 
    weights = ~finalwt, 
    data = brfss2015_2019, 
    strata = ~ststr
  )

#cismen
svy_brfss_cism <- 
  survey::svydesign(
    id = ~1, 
    weights = ~finalwt, 
    data = brfss_cism, 
    strata = ~ststr
  )

#ciswomen
svy_brfss_cisw <- 
  survey::svydesign(
    id = ~1, 
    weights = ~finalwt, 
    data = brfss_cisw, 
    strata = ~ststr
  )

#transmen
svy_brfss_tm<- 
  survey::svydesign(
    id = ~1, 
    weights = ~finalwt, 
    data = brfss_tm, 
    strata = ~ststr
  )

#transwomen
svy_brfss_tf<- 
  survey::svydesign(
    id = ~1, 
    weights = ~finalwt, 
    data = brfss_tf, 
    strata = ~ststr
  )

#nonbinary
svy_brfss_nb<- 
  survey::svydesign(
    id = ~1, 
    weights = ~finalwt, 
    data = brfss_nb, 
    strata = ~ststr
  )


#Prevalence by State: Create tables for maps
#Overall
novaxprev_state<-svytable(~nofluvax_bin + state, svy_brfss) 
weightedprev_overall <- prop.table(novaxprev_state, margin=2)*100
write.csv(weightedprev_overall, "/Users/sarahcox/Desktop/PhD/2021 Spring/S21_EPI514_ApplicationEpiMethods/data/weightedprev_overall.csv", row.names = TRUE) 

map_overall<-as.data.frame(weightedprev_overall)
map_overall<-map_overall %>% filter(nofluvax_bin == 1) %>% select(state, Freq) %>% dplyr::rename(unvaccinated = Freq)
map_overall<-map_overall %>% mutate(unvaccinated_per = paste(round(unvaccinated,0), "%", sep= ""))

map_overall<-map_overall %>% mutate(unvaccinated_cat = case_when((unvaccinated > 20 & unvaccinated <= 30) ~ 1,
                                                                 (unvaccinated > 30 & unvaccinated <= 40) ~ 2,
                                                                 (unvaccinated > 40 & unvaccinated <= 50) ~ 3,
                                                                 (unvaccinated > 50 & unvaccinated <= 55) ~ 4,
                                                                 (unvaccinated > 55 & unvaccinated <= 60) ~ 5,
                                                                 (unvaccinated > 60 & unvaccinated <= 65) ~ 6,
                                                                 (unvaccinated > 65 & unvaccinated <= 70) ~ 7,
                                                                 (unvaccinated > 70 & unvaccinated <= 80) ~ 8,
                                                                 (unvaccinated > 80 & unvaccinated <= 90) ~ 9,
                                                                 (unvaccinated > 90 & unvaccinated <= 100) ~ 10))

map_overall$unvaccinated_cat  <-  factor(map_overall$unvaccinated_cat, 
                                         levels = c(1,2,3,4,5,6,7,8,9,10), 
                                         labels=c(">20% - 30%", 
                                                  ">30% - 40%", 
                                                  ">40% - 50%", 
                                                  ">50% - 55%", 
                                                  ">55% - 60%",
                                                  ">60% - 65%",
                                                  ">65% - 70%", 
                                                  ">70% - 80%", 
                                                  ">80% - 90%",
                                                  ">90% - 100%"))	

table(map_overall$unvaccinated_cat)

#Cismen
novaxprev_state_cism<-svytable(~nofluvax_bin + state, svy_brfss_cism) 
weightedprev_cism <- prop.table(novaxprev_state_cism, margin=2)*100
map_cism<-as.data.frame(weightedprev_cism)
map_cism<-map_cism %>% filter(nofluvax_bin == 1) %>% select(state, Freq) %>% dplyr::rename(unvaccinated = Freq)
map_cism<-map_cism %>% mutate(unvaccinated_per = paste(round(unvaccinated,0), "%", sep= ""))

map_cism<-map_cism %>% mutate(unvaccinated_cat = case_when((unvaccinated > 20 & unvaccinated <= 30) ~ 1,
                                                                 (unvaccinated > 30 & unvaccinated <= 40) ~ 2,
                                                                 (unvaccinated > 40 & unvaccinated <= 50) ~ 3,
                                                                 (unvaccinated > 50 & unvaccinated <= 55) ~ 4,
                                                                 (unvaccinated > 55 & unvaccinated <= 60) ~ 5,
                                                                 (unvaccinated > 60 & unvaccinated <= 65) ~ 6,
                                                                 (unvaccinated > 65 & unvaccinated <= 70) ~ 7,
                                                                 (unvaccinated > 70 & unvaccinated <= 80) ~ 8,
                                                                 (unvaccinated > 80 & unvaccinated <= 90) ~ 9,
                                                                 (unvaccinated > 90 & unvaccinated <= 100) ~ 10))

map_cism$unvaccinated_cat  <-  factor(map_cism$unvaccinated_cat, 
                                         levels = c(1,2,3,4,5,6,7,8,9,10), 
                                         labels=c(">20% - 30%", 
                                                  ">30% - 40%", 
                                                  ">40% - 50%", 
                                                  ">50% - 55%", 
                                                  ">55% - 60%",
                                                  ">60% - 65%",
                                                  ">65% - 70%", 
                                                  ">70% - 80%", 
                                                  ">80% - 90%",
                                                  ">90% - 100%"))	

table(map_cism$unvaccinated_cat) #39 confirmed

#Ciswomen
novaxprev_state_cisw<-svytable(~nofluvax_bin + state, svy_brfss_cisw) 
weightedprev_cisw <- prop.table(novaxprev_state_cisw, margin=2)*100
map_cisw<-as.data.frame(weightedprev_cisw)
map_cisw<-map_cisw %>% filter(nofluvax_bin == 1) %>% select(state, Freq) %>% dplyr::rename(unvaccinated = Freq)
map_cisw<-map_cisw %>% mutate(unvaccinated_per = paste(round(unvaccinated,0), "%", sep= ""))

map_cisw<-map_cisw %>% mutate(unvaccinated_cat = case_when((unvaccinated > 20 & unvaccinated <= 30) ~ 1,
                                                           (unvaccinated > 30 & unvaccinated <= 40) ~ 2,
                                                           (unvaccinated > 40 & unvaccinated <= 50) ~ 3,
                                                           (unvaccinated > 50 & unvaccinated <= 55) ~ 4,
                                                           (unvaccinated > 55 & unvaccinated <= 60) ~ 5,
                                                           (unvaccinated > 60 & unvaccinated <= 65) ~ 6,
                                                           (unvaccinated > 65 & unvaccinated <= 70) ~ 7,
                                                           (unvaccinated > 70 & unvaccinated <= 80) ~ 8,
                                                           (unvaccinated > 80 & unvaccinated <= 90) ~ 9,
                                                           (unvaccinated > 90 & unvaccinated <= 100) ~ 10))

map_cisw$unvaccinated_cat  <-  factor(map_cisw$unvaccinated_cat, 
                                      levels = c(1,2,3,4,5,6,7,8,9,10), 
                                      labels=c(">20% - 30%", 
                                               ">30% - 40%", 
                                               ">40% - 50%", 
                                               ">50% - 55%", 
                                               ">55% - 60%",
                                               ">60% - 65%",
                                               ">65% - 70%", 
                                               ">70% - 80%", 
                                               ">80% - 90%",
                                               ">90% - 100%"))	

table(map_cisw$unvaccinated_cat) #39 confirmed

#Transmen
novaxprev_state_tm<-svytable(~nofluvax_bin + state, svy_brfss_tm) 
weightedprev_tm <- prop.table(novaxprev_state_tm, margin=2)*100
map_tm<-as.data.frame(weightedprev_tm)
map_tm<-map_tm %>% filter(nofluvax_bin == 1) %>% select(state, Freq) %>% dplyr::rename(unvaccinated = Freq)
map_tm<-map_tm %>% mutate(unvaccinated_per = paste(round(unvaccinated,0), "%", sep= ""))
#Supress where n <= 15
table(brfss_tm$state, brfss_tm$gender_re)
#Create subset of states we want
tm_state_sub15<-c("Arizona",
                  "Colorado",
                  "Connecticut",
                  "Delaware",
                  "Florida",
                  "Georgia",
                  "Hawaii",
                  "Indiana",
                  "Iowa",
                  "Kansas",
                  "Kentucky",
                  "Louisiana",
                  "Maryland",
                  "Massachusetts",
                  "Minnesota",
                  "Mississippi",
                  "Missouri",
                  "Montana",
                  "Nevada",
                  "New York",
                  "North Carolina",
                  "Ohio",
                  "Oklahoma",
                  "Pennsylvania",
                  "Rhode Island",
                  "South Carolina",
                  "Texas",
                  "Utah",
                  "Vermont",
                  "Virginia",
                  "Washington",
                  "Guam")
#Filter to subsets >=15
map_tm_sub <- map_tm %>% filter(state %in% tm_state_sub15)

map_tm_sub<-map_tm_sub %>% mutate(unvaccinated_cat = case_when((unvaccinated > 20 & unvaccinated <= 30) ~ 1,
                                                           (unvaccinated > 30 & unvaccinated <= 40) ~ 2,
                                                           (unvaccinated > 40 & unvaccinated <= 50) ~ 3,
                                                           (unvaccinated > 50 & unvaccinated <= 55) ~ 4,
                                                           (unvaccinated > 55 & unvaccinated <= 60) ~ 5,
                                                           (unvaccinated > 60 & unvaccinated <= 65) ~ 6,
                                                           (unvaccinated > 65 & unvaccinated <= 70) ~ 7,
                                                           (unvaccinated > 70 & unvaccinated <= 80) ~ 8,
                                                           (unvaccinated > 80 & unvaccinated <= 90) ~ 9,
                                                           (unvaccinated > 90 & unvaccinated <= 100) ~ 10))

map_tm_sub$unvaccinated_cat  <-  factor(map_tm_sub$unvaccinated_cat, 
                                      levels = c(1,2,3,4,5,6,7,8,9,10), 
                                      labels=c(">20% - 30%", 
                                               ">30% - 40%", 
                                               ">40% - 50%", 
                                               ">50% - 55%", 
                                               ">55% - 60%",
                                               ">60% - 65%",
                                               ">65% - 70%", 
                                               ">70% - 80%", 
                                               ">80% - 90%",
                                               ">90% - 100%"))	

table(map_tm_sub$unvaccinated_cat) #32 confirmed


#Transwomen
novaxprev_state_tf<-svytable(~nofluvax_bin + state, svy_brfss_tf) 
weightedprev_tf <- prop.table(novaxprev_state_tf, margin=2)*100
map_tf<-as.data.frame(weightedprev_tf)
map_tf<-map_tf %>% filter(nofluvax_bin == 1) %>% select(state, Freq) %>% dplyr::rename(unvaccinated = Freq)
map_tf<-map_tf %>% mutate(unvaccinated_per = paste(round(unvaccinated,0), "%", sep= ""))
#Supress where n <= 15
table(brfss_tf$state, brfss_tf$gender_re)
#Create subset of states we want
tf_state_sub15<-c("Arizona",
                  "California",
                  "Colorado",
                  "Connecticut",
                  "Delaware",
                  "Florida",
                  "Georgia",
                  "Hawaii",
                  "Idaho",
                  "Illinois",
                  "Indiana",
                  "Iowa",
                  "Kansas",
                  "Kentucky",
                  "Louisiana",
                  "Maryland",
                  "Massachusetts",
                  "Minnesota",
                  "Mississippi",
                  "Montana",
                  "New York",
                  "North Carolina",
                  "Ohio",
                  "Oklahoma",
                  "Pennsylvania",
                  "Rhode Island",
                  "South Carolina",
                  "Texas",
                  "Vermont",
                  "Virginia",
                  "Washington",
                  "West Virginia",
                  "Wisconsin",
                  "Guam")
#Filter to subsets >=15
map_tf_sub <- map_tf %>% filter(state %in% tf_state_sub15)

map_tf_sub<-map_tf_sub %>% mutate(unvaccinated_cat = case_when((unvaccinated > 20 & unvaccinated <= 30) ~ 1,
                                                               (unvaccinated > 30 & unvaccinated <= 40) ~ 2,
                                                               (unvaccinated > 40 & unvaccinated <= 50) ~ 3,
                                                               (unvaccinated > 50 & unvaccinated <= 55) ~ 4,
                                                               (unvaccinated > 55 & unvaccinated <= 60) ~ 5,
                                                               (unvaccinated > 60 & unvaccinated <= 65) ~ 6,
                                                               (unvaccinated > 65 & unvaccinated <= 70) ~ 7,
                                                               (unvaccinated > 70 & unvaccinated <= 80) ~ 8,
                                                               (unvaccinated > 80 & unvaccinated <= 90) ~ 9,
                                                               (unvaccinated > 90 & unvaccinated <= 100) ~ 10))

map_tf_sub$unvaccinated_cat  <-  factor(map_tf_sub$unvaccinated_cat, 
                                        levels = c(1,2,3,4,5,6,7,8,9,10), 
                                        labels=c(">20% - 30%", 
                                                 ">30% - 40%", 
                                                 ">40% - 50%", 
                                                 ">50% - 55%", 
                                                 ">55% - 60%",
                                                 ">60% - 65%",
                                                 ">65% - 70%", 
                                                 ">70% - 80%", 
                                                 ">80% - 90%",
                                                 ">90% - 100%"))	

table(map_tf_sub$unvaccinated_cat) #34 confirmed

#Nonbinary
novaxprev_state_nb<-svytable(~nofluvax_bin + state, svy_brfss_nb) 
weightedprev_nb <- prop.table(novaxprev_state_nb, margin=2)*100
map_nb<-as.data.frame(weightedprev_nb)
map_nb<-map_nb %>% filter(nofluvax_bin == 1) %>% select(state, Freq) %>% dplyr::rename(unvaccinated = Freq)
map_nb<-map_nb %>% mutate(unvaccinated_per = paste(round(unvaccinated,0), "%", sep= ""))
#Supress where n <= 15
table(brfss_nb$state, brfss_nb$gender_re)
#Create subset of states we want
nb_state_sub15<-c("California",
                  "Connecticut",
                  "Delaware",
                  "Florida",
                  "Hawaii",
                  "Idaho",
                  "Illinois",
                  "Indiana",
                  "Iowa",
                  "Kansas",
                  "Maryland",
                  "Massachusetts",
                  "Minnesota",
                  "Montana",
                  "Nevada",
                  "New York",
                  "North Carolina",
                  "Ohio",
                  "Oklahoma",
                  "Pennsylvania",
                  "Rhode Island",
                  "South Carolina",
                  "Texas",
                  "Vermont",
                  "Virginia",
                  "Washington",
                  "West Virginia")
#Filter to subsets >=15
map_nb_sub <- map_nb %>% filter(state %in% nb_state_sub15)

map_nb_sub<-map_nb_sub %>% mutate(unvaccinated_cat = case_when((unvaccinated > 20 & unvaccinated <= 30) ~ 1,
                                                               (unvaccinated > 30 & unvaccinated <= 40) ~ 2,
                                                               (unvaccinated > 40 & unvaccinated <= 50) ~ 3,
                                                               (unvaccinated > 50 & unvaccinated <= 55) ~ 4,
                                                               (unvaccinated > 55 & unvaccinated <= 60) ~ 5,
                                                               (unvaccinated > 60 & unvaccinated <= 65) ~ 6,
                                                               (unvaccinated > 65 & unvaccinated <= 70) ~ 7,
                                                               (unvaccinated > 70 & unvaccinated <= 80) ~ 8,
                                                               (unvaccinated > 80 & unvaccinated <= 90) ~ 9,
                                                               (unvaccinated > 90 & unvaccinated <= 100) ~ 10))

map_nb_sub$unvaccinated_cat  <-  factor(map_nb_sub$unvaccinated_cat, 
                                        levels = c(1,2,3,4,5,6,7,8,9,10), 
                                        labels=c(">20% - 30%", 
                                                 ">30% - 40%", 
                                                 ">40% - 50%", 
                                                 ">50% - 55%", 
                                                 ">55% - 60%",
                                                 ">60% - 65%",
                                                 ">65% - 70%", 
                                                 ">70% - 80%", 
                                                 ">80% - 90%",
                                                 ">90% - 100%"))	

table(map_nb_sub$unvaccinated_cat) #27 confirmed

##Figure 2/ Supplemental Table
## Number of Adult Participants and Percent Unvaccinated 
#           Against Influenza in the Past Year 
#           by State and Gender Identity, BRFSS, 2015-2019
################################################################################


#Create maps 
#Overall
map_overall_plot <- plot_usmap(
  data = map_overall, values = "unvaccinated_cat", 
  color = "black") + 
  scale_fill_manual(values = c(#"#fff5f0",
    #"#fee0d2",
   # "#fcbba1",
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
  theme(legend.position = "none")

map_overall_plot


#Cisgender Men
map_cism_plot <-  plot_usmap(
  data = map_cism, values = "unvaccinated_cat", 
  color = "black") + 
  scale_fill_manual(values = c(#"#fff5f0",
    #"#fee0d2",
    #"#fcbba1",
    #"#fc9272",
    "#fb6a4a",
    "#ef3b2c",
    "#cb181d",
    "#a50f15",
    "#67000d",
    "#000000"), 
    na.value = "#d9d9d9", name = "% unvaccinated")+
  labs(#title = "Percentage of U.S. Adults Unvaccinated Against Influenza, BRFSS, 2015-2019", 
    subtitle = "Cisgender men") +
  theme(legend.position = "none")

map_cism_plot


#Cisgender women
map_cisw_plot <-  plot_usmap(
  data = map_cisw, values = "unvaccinated_cat", 
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
    subtitle = "Cisgender women") +
  theme(legend.position = "none")

map_cisw_plot

#Non-binary Supressed subset
map_nb_sub_plot <-  plot_usmap(
  data = map_nb_sub, values = "unvaccinated_cat", 
  color = "black") + 
  scale_fill_manual(values = c("#fff5f0",
    "#fee0d2",
   # "#fcbba1",
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
  theme(legend.position = "none")

map_nb_sub_plot 

#Transgender Men Supressed subset
map_tm_sub_plot <-  plot_usmap(
  data = map_tm_sub, values = "unvaccinated_cat", 
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
    subtitle = "Transgender men") +
  theme(legend.position = "none")

map_tm_sub_plot  

#Transgender Women Supressed subset
map_tf_sub_plot <-  plot_usmap(
  data = map_tf_sub, values = "unvaccinated_cat", 
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
    subtitle = "Transgender women") +
  theme(legend.position = "none")

map_tf_sub_plot  


#Create figures
#2. Overall
map_overall_plot 
#2. By Gender 
figure2<-  map_overall_plot + map_cisw_plot + map_cism_plot + map_nb_sub_plot +  map_tf_sub_plot + map_tm_sub_plot +
  plot_layout(ncol = 3)
figure2  

#Guam
map_overall#71%
map_cism #71%
map_cisw #72%
map_tf_sub #66%
map_tm_sub #60%
map_nb_sub #NA

  

