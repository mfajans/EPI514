
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
pd2a_cwref %>% coef*100 %>% round(3)
pd2a_cwref %>% confint*100 %>% round(3)

#PD: cis men ref group
pd2a_cmref <-svyglm(nofluvax_bin~gender_re, design=brfss_svy_cismen)
pd2a_cmref
confint(pd2a_cmref, digits = 3)
coef(pd2a_cmref, digits = 3)

#round coefficients/CIs for table 2a
pd2a_cmref$coefficients %>% signif(3)
pd2a_cmref %>% coef*100 %>% round(3)
pd2a_cmref %>% confint*100 %>% round(3)

###Table 2b### 

###Cisgender

#PD cisgender women
pd2b_ciswomen <-svyglm(nofluvax_bin~equality, design=ciswomen_prev2b)
pd2b_ciswomen 
pd2b_ciswomen %>% coef*100 %>% round(3)
pd2b_ciswomen %>% confint*100 %>% round(3)    

#PD cisgender men
pd2b_cismen <-svyglm(nofluvax_bin~equality, design=cismen_prev2b)
pd2b_cismen 
pd2b_cismen %>% coef*100 %>% round(3)
pd2b_cismen %>% confint*100 %>% round(3) 

##Transgender women

#PD trans women
pd2b_tgw <-svyglm(nofluvax_bin~equality, design=tgw_prev2b)
pd2b_tgw 
pd2b_tgw %>% coef*100 %>% round(3)
pd2b_tgw %>% confint*100 %>% round(3)       

##Transgender men

#PD trans men
pd2b_tgm <-svyglm(nofluvax_bin~equality, design=tgm_prev2b)
pd2b_tgm 
pd2b_tgm %>% coef*100 %>% round(3)
pd2b_tgm %>% confint*100 %>% round(3) 


##Non-Binary

#PD non-binary
pd2b_nb <-svyglm(nofluvax_bin~equality, design=nb_prev2b)
pd2b_nb 
pd2b_nb %>% coef*100 %>% round(3)
pd2b_nb %>% confint*100 %>% round(3)   



###Table 3a### 
#Stratified analysis with PDs 
#reference group = cis women 

svytable(~nofluvax_bin + gender_re + age_grp, brfss_svy_ciswomen) %>%
  prop.table(margin = 2)

age1 <- svyglm(nofluvax_bin ~ gender_re, 
               design = subset(brfss_svy_ciswomen, age_grp == "18-24"))

age2 <- svyglm(nofluvax_bin ~ gender_re, 
               design = subset(brfss_svy_ciswomen, age_grp == "25-44"))

age3 <- svyglm(nofluvax_bin ~ gender_re, 
               design = subset(brfss_svy_ciswomen, age_grp == "45-64"))

age4 <- svyglm(nofluvax_bin ~ gender_re, 
               design = subset(brfss_svy_ciswomen, age_grp == "65+"))

#18-24
age1 %>% coef*100 %>% round(3)
age1 %>% confint*100 %>% round(3) 

#25-44  
age2 %>% coef*100 %>% round(3)
age2 %>% confint*100 %>% round(3) 

#45-64  
age3 %>% coef*100 %>% round(3)
age3 %>% confint*100 %>% round(3) 

#65+  
age4 %>% coef*100 %>% round(3)
age4 %>% confint*100 %>% round(3) 

#Health insurance status
svytable(~nofluvax_bin + gender_re + health_coverage_re, brfss_svy_ciswomen) %>%
  prop.table(margin = 2)

healthcov_yes <- svyglm(nofluvax_bin ~ gender_re, 
                        design = subset(brfss_svy_ciswomen, health_coverage_re == "Yes"))

healthcov_no <- svyglm(nofluvax_bin ~ gender_re, 
                       design = subset(brfss_svy_ciswomen, health_coverage_re == "No"))

#PD for health insurance (yes)
  healthcov_yes %>% coef*100 %>% round(3)
  healthcov_yes %>% confint*100 %>% round(3) 

#PD for health insurance (no)
  healthcov_no %>% coef*100 %>% round(3)
  healthcov_no %>% confint*100 %>% round(3) 


#Poor Health care access
svytable(~nofluvax_bin + gender_re + medcost_re, brfss_svy_ciswomen) %>%
  prop.table(margin = 2)

medcost_yes <- svyglm(nofluvax_bin ~ gender_re, 
                      design = subset(brfss_svy_ciswomen, medcost_re == "Yes"))

medcost_no <- svyglm(nofluvax_bin ~ gender_re, 
                     design = subset(brfss_svy_ciswomen, medcost_re == "No"))

#PD for poor health care access (yes)
  medcost_yes %>% coef*100 %>% round(3)
  medcost_yes %>% confint*100 %>% round(3) 

#PD for poor health care access (no)
  medcost_no %>% coef*100 %>% round(3)
  medcost_no %>% confint*100 %>% round(3) 

#Mental Health Status
svytable(~nofluvax_bin + gender_re + mentalhlth_re, brfss_svy_ciswomen) %>%
  prop.table(margin = 2)

mentalhtlh_bad <- svyglm(nofluvax_bin ~ gender_re, 
                         design = subset(brfss_svy_ciswomen, mentalhlth_re == ">=14 days"))

mentalhlth_notbad <- svyglm(nofluvax_bin ~ gender_re, 
                            design = subset(brfss_svy_ciswomen, mentalhlth_re == "<14 days"))

#PD for poor mental health (yes)
  mentalhtlh_bad %>% coef*100 %>% round(3)
  mentalhtlh_bad %>% confint*100 %>% round(3) 

#PD for poor mental health (no)
  mentalhlth_notbad %>% coef*100 %>% round(3)
  mentalhlth_notbad %>% confint*100 %>% round(3) 

#State policies on gender identity (equality)

svytable(~nofluvax_bin + gender_re + equality, brfss_svy_ciswomen) %>%
  prop.table(margin = 2)

equality_p <- svyglm(nofluvax_bin ~ gender_re, 
                     design = subset(brfss_svy_ciswomen, equality == "Protective"))

equality_r <- svyglm(nofluvax_bin ~ gender_re, 
                     design = subset(brfss_svy_ciswomen, equality == "Restrictive"))

#PD for protective states
equality_p %>% coef*100 %>% round(3)
equality_p %>% confint*100 %>% round(3) 

#PD for restrictive states
equality_r %>% coef*100 %>% round(3)
equality_r %>% confint*100 %>% round(3) 


