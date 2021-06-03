
######################################################
#Forest Plot
######################################################

#Graph prevalence ratios from Individual Assignment Q10 Answer Key Estimates
#adjusted for income




#create a data frame of coefficients and CI (high and lo) to graph

#Table 2a

forest2a <- data.frame("Vars" = c("Transgender Men", "Transgender Women",
                                  "Non-Binary"),
                       "PR" = c(1.06, 1.09, 1.10),
                       "CI_lo" = c(1.02, 1.05, 1.04),
                       "CI_hi" = c(1.11, 1.14, 1.16))

forest2a$label <- paste0(round(forest2a$PR, 2),
                         " (",
                         round(forest2a$CI_lo, 2),
                         ",",
                         round(forest2a$CI_hi, 2),
                         ")")

#the geom_pointrange function creates our "forest plot" vertically by default, 
#so we have to set it up vertically and then flip it horizontally below
ggplot(forest2a, aes(x = Vars, y = PR)) +
        geom_pointrange(aes(ymin = CI_lo,
                            ymax = CI_hi),
                        shape = 1) +
        scale_y_continuous(trans = "log", #show axis on log scale since these are multiplicative comparisons (ratios)
                           breaks = c(0.8, 0.9, 1, 1.1, 1.2, 1.3)
        ) +
        scale_x_discrete(labels = c("Transgender Men", #Leaving the first group blank - use colors to show sex instead
                                    "Transgender Women", 
                                    "Non-Binary")) +
        geom_text(aes(label = label), #add the created labels
                  size = 2.5,
                  vjust = -1) + 
        geom_hline(yintercept = 1,
                   linetype = "dashed") +
        coord_flip() + #flip the graph horizontally
        #scale_color_manual(values = c("#4b2e83", "#85754d")) + #telling what colors to use for sex
        theme_classic() +
        theme(axis.line = element_line(),
              plot.title = element_text(hjust = .5),
              plot.subtitle = element_text(face = "italic"),
              legend.position = "bottom",
              text = element_text(family = "Times")) +
        labs(y = "PR (95% CI)",
             x = "Gender Identity",
             title = "Prevalence rations between gender identity and not receiving an influenza vaccine (unvaccinated) in the last year  among adults, BRFSS, 2015-2019 (N = 1,016,012)",
             subtitle = "Reference Group: Cisgender")



#Table 2B

forest2b <- data.frame("Vars" = c("Cisgender", "Transgender Men", "Transgender Women",
                                  "Non-Binary"),
                       "PR" = c(1.04, 0.95, 0.96, 1.11),
                       "CI_lo" = c(1.03, 0.87, 0.89, 1.00),
                       "CI_hi" = c(1.04, 1.04, 1.04, 1.23))

forest2b$label <- paste0(round(forest2b$PR, 2),
                         " (",
                         round(forest2b$CI_lo, 2),
                         ",",
                         round(forest2b$CI_hi, 2),
                         ")")

#the geom_pointrange function creates our "forest plot" vertically by default, 
#so we have to set it up vertically and then flip it horizontally below
ggplot(forest2b, aes(x = Vars, y = PR)) +
        geom_pointrange(aes(ymin = CI_lo,
                            ymax = CI_hi),
                        shape = 1) +
        scale_y_continuous(trans = "log", #show axis on log scale since these are multiplicative comparisons (ratios)
                           breaks = c(0.8, 0.9, 1, 1.1, 1.2, 1.3)
        ) +
        scale_x_discrete(labels = c("Cisgender", 
                                    "Transgender Men", 
                                    "Transgender Women", 
                                    "Non-Binary")) +
        geom_text(aes(label = label), #add the created labels
                  size = 2.5,
                  vjust = -1) + 
        geom_hline(yintercept = 1,
                   linetype = "dashed") +
        coord_flip() + #flip the graph horizontally
        #scale_color_manual(values = c("#4b2e83", "#85754d")) + #telling what colors to use for sex
        theme_classic() +
        theme(axis.line = element_line(),
              plot.title = element_text(hjust = .5),
              plot.subtitle = element_text(face = "italic"),
              legend.position = "bottom",
              text = element_text(family = "Times")) +
        labs(y = "PR (95% CI)",
             x = "Gender Identity",
             title = "Prevalence ratios between state policies on gender identity and not receiving an influenza vaccine (unvaccinated) in the last year  among adults, BRFSS, 2015-2019 (N = 1,016,012)",
             subtitle = "Reference Group: Protective States")


#Stratified analysis - 3A
forest3a <- data.frame("Vars" = c("Crude Transgender Men", "Crude Transgender Women", "Crude Non Binary",
                                  
                                  "Age 18-24 Transgender Men", "Age 18-24  Transgender Women", "Age 18-24  Non Binary",
                                  "Age 25-34:Transgender Men", "Age 25-34: Transgender Women", "Age 25-34: Non Binary",
                                  "Age 35-44:Transgender Men", "Age 35-44: Transgender Women", "Age 35-44: Non Binary",
                                  "Age 45-54:Transgender Men", "Age 45-54: Transgender Women", "Age 45-54: Non Binary",
                                  "Age 55-64:Transgender Men", "Age 55-64: Transgender Women", "Age 55-64: Non Binary",
                                  "Age 65+:Transgender Men", "Age 65+: Transgender Women", "Age 65+: Non Binary",
                                  
                                  "Health Insurance - Yes TM",  "Health Insurance - Yes TW",  "Health Insurance - Yes NB",
                                  "Health Insurance - No TM",  "Health Insurance - No TW",  "Health Insurance - No NB",
                                  
                                  "Poor Health Care Access - Yes TM", "Poor Health Care Access - Yes TW", "Poor Health Care Access - Yes",
                                  "Poor Health Care Access - No TM", "Poor Health Care Access - No TW", "Poor Health Care Access - No",
                                  
                                  "Poor Mental Health Status - Yes TM", "Poor Mental Health Status - Yes TW", "Poor Mental Health Status - Yes NB",
                                  "Poor Mental Health Status - No TM", "Poor Mental Health Status - No TW", "Poor Mental Health Status - No NB"
                                  
                                  
),
"PR" = c(1.06, 1.09, 1.10, #Crude
         
         0.92, 0.93, 0.90, #Age
         0.93, 1.00, 0.95,
         0.95, 1.09, 0.93,
         0.98, 1.03, 0.94,
         1.16, 1.05, 0.89,
         1.05, 1.16, 1.31,
         
         1.04, 1.07, 1.09, #Health insurance Status        
         1.03, 1.02, 0.95,
         
         1.03, 1.02, 0.95, #Poor health Care Access
         1.04, 1.09, 1.10,
         
         1.08, 1.07, 1.05, #Poor Mental Health
         1.04, 1.09, 1.08
),



"CI_lo" = c(1.02, 1.05, 1.04,#Crude
            0.83, 0.84, 0.81,# Age 
            0.84, 0.92, 0.86,
            0.84, 1.00, 0.80,
            0.88, 0.94, 0.94,
            1.06, 0.96, 0.76,
            0.94, 1.06, 1.16,
            
            0.99, 1.03, 1.03, #Health insurance Status  
            0.91, 0.94, 0.85,
            
            0.96, 0.94, 0.86, #Poor health care access
            0.99, 1.04, 1.03,
            
            0.99, 0.99, 0.96, #Poor Mental Health Status
            0.99, 1.04, 1.01),





"CI_hi" =  c(1.11, 1.14, 1.16, #Crude
             1.01, 1.04, 1.00, #Age
             1.04, 1.09, 1.06,
             1.06, 1.19, 1.09,
             1.09, 1.12, 1.22,
             1.27, 1.14, 1.05,
             1.18, 1.28, 1.48,
             
             1.10, 1.12, 1.16, #Health insurance Status  
             1.06, 1.07, 1.05,
             
             1.12, 1.09, 1.06, #Poor health care access
             1.10, 1.14, 1.17,
             
             1.17, 1.16, 1.15, #Poor Mental Health Status
             1.10, 1.14, 1.15))





forest3a$label <- paste0(round(forest3a$PR, 2),
                         " (",
                         round(forest3a$CI_lo, 2),
                         ",",
                         round(forest3a$CI_hi, 2),
                         ")")


forest3a$genid <- c("Transgender Men", "Transgender Women", "Non Binary",
                    "Transgender Men", "Transgender Women", "Non Binary",
                    "Transgender Men", "Transgender Women", "Non Binary",
                    "Transgender Men", "Transgender Women", "Non Binary",
                    "Transgender Men", "Transgender Women", "Non Binary",
                    "Transgender Men", "Transgender Women", "Non Binary",
                    "Transgender Men", "Transgender Women", "Non Binary",
                    "Transgender Men", "Transgender Women", "Non Binary",
                    "Transgender Men", "Transgender Women", "Non Binary",
                    "Transgender Men", "Transgender Women", "Non Binary",
                    "Transgender Men", "Transgender Women", "Non Binary",
                    "Transgender Men", "Transgender Women", "Non Binary",
                    "Transgender Men", "Transgender Women", "Non Binary")


ggplot(forest3a, aes(x = Vars, y = PR)) +
        geom_pointrange(aes(ymin = CI_lo,
                            ymax = CI_hi,
                            color=genid),
                        shape = 1) +
        scale_y_continuous(trans = "log", #show axis on log scale since these are multiplicative comparisons (ratios)
                           breaks = c(0.7,0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4)
        ) +
        scale_x_discrete(labels = c("Crude","Crude","Crude",
                                    "Age 18-24","Age 18-24","Age 18-24",
                                    "Age 25-34","Age 25-34","Age 25-34",
                                    "Age 35-44","Age 35-44","Age 35-44",
                                    "Age 45-54","Age 45-54","Age 45-54",
                                    "Age 55-64","Age 55-64","Age 55-64",
                                    "Age 65+","Age 65+","Age 65+",
                                    
                                    "Health Insurance - Yes", "Health Insurance - Yes", "Health Insurance - Yes",
                                    "Health Insurance - No", "Health Insurance - No", "Health Insurance - No",
                                    
                                    "Poor Health Care Access - Yes", "Poor Health Care Access - Yes", "Poor Health Care Access - Yes",
                                    "Poor Health Care Access - No", "Poor Health Care Access - No", "Poor Health Care Access - No",
                                    
                                    "Poor Mental Health Status - Yes", "Poor Mental Health Status - Yes", "Poor Mental Health Status - Yes",
                                    "Poor Mental Health Status - No", "Poor Mental Health Status - No", "Poor Mental Health Status - No"
                                    
        )) +
        geom_text(aes(label = label), #add the created labels
                  size = 2.5,
                  vjust = -1) + 
        geom_hline(yintercept = 1,
                   linetype = "dashed") +
        coord_flip() + #flip the graph horizontally
        scale_color_manual(values = c("#4d856a", "#3544b5", "#cf1f48")) + #telling what colors to use for sex
        theme_classic() +
        theme(axis.line = element_line(),
              plot.title = element_text(hjust = .5),
              plot.subtitle = element_text(face = "italic"),
              legend.position = "bottom",
              text = element_text(family = "Times")) +
        labs(y = "PR (95% CI)",
             x = "Strata",
             title = "Stratified Prevalence ratios between gender identity and not receiving an influenza vaccine (unvaccinated)",
             subtitle = "Reference Group: Cisgender")



#Stratified analysis - 3B
forest3b <- data.frame("Vars" = c("Crude Cisgender","Crude Transgender Men", "Crude Transgender Women", "Crude Non Binary",
                                  
                                  "Age 18-24 Cisgender","Age 18-24 Transgender Men", "Age 18-24  Transgender Women", "Age 18-24  Non Binary",
                                  "Age 25-34:Cisgender","Age 25-34:Transgender Men", "Age 25-34: Transgender Women", "Age 25-34: Non Binary",
                                  "Age 35-44:Cisgender", "Age 35-44:Transgender Men", "Age 35-44: Transgender Women", "Age 35-44: Non Binary",
                                  "Age 45-54:Cisgender", "Age 45-54:Transgender Men", "Age 45-54: Transgender Women", "Age 45-54: Non Binary",
                                  "Age 55-64:Cisgender","Age 55-64:Transgender Men", "Age 55-64: Transgender Women", "Age 55-64: Non Binary",
                                  "Age 65+:Cisgender","Age 65+:Transgender Men", "Age 65+: Transgender Women", "Age 65+: Non Binary",
                                  
                                  "Health Insurance - Yes C","Health Insurance - Yes TM",  "Health Insurance - Yes TW",  "Health Insurance - Yes NB",
                                  "Health Insurance - No C",  "Health Insurance - No TM",  "Health Insurance - No TW",  "Health Insurance - No NB",
                                  
                                  "Poor Health Care Access - Yes C", "Poor Health Care Access - Yes TM", "Poor Health Care Access - Yes TW", "Poor Health Care Access - Yes NB",
                                  "Poor Health Care Access - No C", "Poor Health Care Access - No TM", "Poor Health Care Access - No TW", "Poor Health Care Access - No NB",
                                  
                                  "Poor Mental Health Status - Yes C", "Poor Mental Health Status - Yes TM", "Poor Mental Health Status - Yes TW", "Poor Mental Health Status - Yes NB",
                                  "Poor Mental Health Status - No C", "Poor Mental Health Status - No TM", "Poor Mental Health Status - No TW", "Poor Mental Health Status - No NB"
                                  
                                  
),
"PR" = c(1.04,	0.95,	0.96,	1.11, #Crude
         
         1.05,	1.19,	0.94,	1.08, #Age
         1.05,	0.85,	1.06,	1.22,
         1.07,	1.1,	1.01,	1.1,
         1.06,	1.19,	0.89,	0.98,
         1.06,	0.95,	1.09,	1.35,
         1.04,	0.79,	0.95,	1.09,
         
         1.02,	0.91,	0.95,	1.12, #Health insurance Status 
         1.05,	1.05,	0.96,	1.02,
         
         1.04,	1.09,	0.98,	1.08, #Poor health care access
         1.02,	0.91,	0.96,	1.11,
         
         1.03,	1.12,	1.05,	1.12, #Poor Mental Health Status
         1.03,	0.91,	0.93,	1.12	
         
),



"CI_lo" = c(1.03, 0.87,	0.89, 1.00, #Crude
            
            1.04, 0.98,	0.76, 0.87, #Age
            1.04, 0.68,	0.90, 0.99,
            1.06, 0.87,	0.85, 0.80,
            1.05, 0.96,	0.74, 0.76,
            1.05, 0.79,	0.92, 0.97,
            1.03, 0.63,	0.79, 0.85,
            
            1.01, 0.82,	0.87, 0.99, #Health insurance Status 
            1.04, 0.90,	0.84, 0.83,
            
            1.04, 0.93,	0.84, 0.88, #Poor health care access
            1.02, 0.82,	0.88, 0.98,
            
            1.02, 0.95,	0.90, 0.94, #Poor Mental Health Status
            1.03, 0.82,	0.85, 0.98	
            
),





"CI_hi" =  c(1.04, 1.04, 1.04, 1.23,#Crude
             
             1.06, 1.44, 1.16, 1.34,#Age
             1.06, 1.06, 1.26, 1.49,
             1.08, 1.39, 1.21, 1.49,
             1.07, 1.47, 1.06, 1.28,
             1.07, 1.14, 1.29, 1.86,
             1.05, 1.00, 1.14, 1.40,
             
             1.02, 1.01, 1.04, 1.26, #Health insurance Status 
             1.06, 1.23, 1.09, 1.25,
             
             1.05, 1.27, 1.13, 1.33, #Poor health care access
             1.03, 1.02, 1.05, 1.25,
             
             1.04, 1.32, 1.23, 1.35, #Poor Mental Health Status
             1.04, 1.01, 1.02, 1.28	
             
))

forest3b$label <- paste0(round(forest3b$PR, 2),
                         " (",
                         round(forest3b$CI_lo, 2),
                         ",",
                         round(forest3b$CI_hi, 2),
                         ")")


forest3b$genid <- c("Cisgender", "Transgender Men", "Transgender Women", "Non Binary",
                    "Cisgender", "Transgender Men", "Transgender Women", "Non Binary",
                    "Cisgender", "Transgender Men", "Transgender Women", "Non Binary",
                    "Cisgender", "Transgender Men", "Transgender Women", "Non Binary",
                    "Cisgender", "Transgender Men", "Transgender Women", "Non Binary",
                    "Cisgender", "Transgender Men", "Transgender Women", "Non Binary",
                    "Cisgender", "Transgender Men", "Transgender Women", "Non Binary",
                    "Cisgender", "Transgender Men", "Transgender Women", "Non Binary",
                    "Cisgender", "Transgender Men", "Transgender Women", "Non Binary",
                    "Cisgender", "Transgender Men", "Transgender Women", "Non Binary",
                    "Cisgender", "Transgender Men", "Transgender Women", "Non Binary",
                    "Cisgender", "Transgender Men", "Transgender Women", "Non Binary",
                    "Cisgender", "Transgender Men", "Transgender Women", "Non Binary")



ggplot(forest3b, aes(x = Vars, y = PR)) +
        geom_pointrange(aes(ymin = CI_lo,
                            ymax = CI_hi,
                            color=genid),
                        shape = 1) +
        scale_y_continuous(trans = "log", #show axis on log scale since these are multiplicative comparisons (ratios)
                           breaks = c(0.7,0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4)
        ) +
        scale_x_discrete(labels = c("Crude","Crude", "Crude", "Crude",
                                    
                                    "Age 18-24","Age 18-24", "Age 18-24", "Age 18-24",
                                    "Age 25-34","Age 25-34", "Age 25-34", "Age 25-34",
                                    "Age 35-44", "Age 35-44", "Age 35-44", "Age 35-44",
                                    "Age 45-54", "Age 45-54", "Age 45-54", "Age 45-54",
                                    "Age 55-64","Age 55-64", "Age 55-64", "Age 55-64",
                                    "Age 65+","Age 65+", "Age 65+", "Age 65+",
                                    
                                    "Health Insurance - Yes","Health Insurance - Yes",  "Health Insurance - Yes",  "Health Insurance - Yes",
                                    "Health Insurance - No",  "Health Insurance - No",  "Health Insurance - No",  "Health Insurance - No",
                                    
                                    "Poor Health Care Access - Yes", "Poor Health Care Access - Yes", "Poor Health Care Access - Yes", "Poor Health Care Access - Yes",
                                    "Poor Health Care Access - No", "Poor Health Care Access - No TM", "Poor Health Care Access - No TW", "Poor Health Care Access - No",
                                    
                                    "Poor Mental Health Status - Yes", "Poor Mental Health Status - Yes", "Poor Mental Health Status - Yes", "Poor Mental Health Status - Yes",
                                    "Poor Mental Health Status - No", "Poor Mental Health Status - No", "Poor Mental Health Status - No", "Poor Mental Health Status - No"
                                    
        )) +
        geom_text(aes(label = label), #add the created labels
                  size = 2.5,
                  vjust = -1) + 
        geom_hline(yintercept = 1,
                   linetype = "dashed") +
        coord_flip() + #flip the graph horizontally
        scale_color_manual(values = c("#db4e1a","#4d856a", "#3544b5", "#cf1f48" )) + #telling what colors to use for sex
        theme_classic() +
        theme(axis.line = element_line(),
              plot.title = element_text(hjust = .5),
              plot.subtitle = element_text(face = "italic"),
              legend.position = "bottom",
              text = element_text(family = "Times")) +
        labs(y = "PR (95% CI)",
             x = "Strata",
             title = "Prevalence ratios between state policies on gender identity and not receiving an influenza vaccine (unvaccinated)",
             subtitle = "Reference Group: Cisgender")

