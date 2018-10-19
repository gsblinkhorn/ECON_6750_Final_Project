#######################################################
## WRITTEN BY GARRETT BLINKHORN
## ----------------------------------------------------
## This is the third script of our project, where the
## analysis of the data will be conducted. The point 
## of this project is to analyze the quantitative data 
## available about top schools in order to predict their 
## US News & World Report Ranking
#######################################################

############ TODO #################
## 1 - Consider rescaling endowment to better capture its impact
## 2 - Consider rescaling accept_rate, grad_rate to better
##     illustrate their impact per unit increase
## 3 - investigate joint significance of setting
## 4 - How to handle high collinearity between instate & outstate
## Continue Investigation
###################################

library(stargazer)

# SetWD
setwd("C:/Users/jakes/Desktop/Course Work/ECON_6750_Introduction_to_Econometrics/ECON_6750_Final_Project")
sink(file="analysis.out",append=FALSE,split=TRUE) 

# Read in Data
school_data <- read.csv("school_data_clean.csv", header=TRUE, sep=",")

lm_model <- lm(ranking ~ instate_tuition + 
                 outstate_tuition +
                 room_and_board + 
                 enrollment +
                 school_type +
                 religious_affiliation +
                 acad_calendar +
                 setting_urban +
                 setting_suburb +
                 endowment +
                 salary +
                 accept_rate +
                 stud_fac_ratio +
                 four_year_grad_rate, school_data)

stargazer(lm_model,
          type="text")

sink()
