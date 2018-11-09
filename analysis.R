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
library(ggplot2)

# SetWD
setwd("C:/Users/jakes/Desktop/CourseWork/ECON_6750_Introduction_to_Econometrics/ECON_6750_Final_Project")
sink(file="analysis.out",append=FALSE,split=TRUE) 

# Read in Data
school_data <- read.csv("school_data_clean.csv", header=TRUE, sep=",")

# Full Model
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

# Scales the endowmen to billions of dollars
school_data_scaled <- transform(school_data, endow_billions = endowment/1000000000)

lm_model_endow_scaled <- lm(ranking ~ instate_tuition + 
                              outstate_tuition +
                              room_and_board + 
                              enrollment +
                              school_type +
                              religious_affiliation +
                              acad_calendar +
                              setting_urban +
                              setting_suburb +
                              endow_billions +
                              salary +
                              accept_rate +
                              stud_fac_ratio +
                              four_year_grad_rate, school_data_scaled)

lm_model_log <- lm(ranking ~ instate_tuition + 
                     outstate_tuition +
                     room_and_board + 
                     enrollment +
                     school_type +
                     religious_affiliation +
                     acad_calendar +
                     setting_urban +
                     setting_suburb +
                     log(endow_billions) +
                     salary +
                     accept_rate +
                     stud_fac_ratio +
                     four_year_grad_rate, school_data_scaled)



step <- step(lm_model_log, direction="backward")

stargazer(lm_model, lm_model_endow_scaled,lm_model_log, step,
          column.labels=c("Full Model", "Endowment in Billions", "Log of Endowment (Billions)", "Backwards Step Model"),
          type="text")

# Create and Save Scatterplots
attach(school_data_scaled)

log <- plot(ranking, log(endow_billions), main="Scatterplot of Endowmenet (Billions) Versus Ranking",
     xlab="School Ranking", ylab="Log(Endowment (Billions))", col="blue")

png("normal.png")
normal <- plot(ranking, endow_billions, main="Scatterplot of Endowmenet (Billions) Versus Ranking",
               xlab="School Ranking", ylab="Endowment (Billions)", col="red")
dev.off()

png("log.png")
log <- plot(ranking, log(endow_billions), main="Scatterplot of Endowmenet (Billions) Versus Ranking",
            xlab="School Ranking", ylab="Log(Endowment (Billions))", col="blue")
dev.off()


sink()
