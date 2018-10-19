#######################################################
## WRITTEN BY GARRETT BLINKHORN
## ----------------------------------------------------
## This is the second script of our project, where the
## data will be encoded into numerical formats. The point 
## of this project is to analyze the quantitative data 
## available about top schools in order to predict their 
## US News & World Report Ranking
#######################################################

# SetWD
setwd("C:/Users/jakes/Desktop/Course Work/ECON_6750_Introduction_to_Econometrics/ECON_6750_Final_Project")

# Read in Data
school_data <- read.csv("school_data_raw.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

# Function for extracting number from a string which includes a comma
numextract_comma <- function(string){ 
  str_extract(string, "\\-*\\d+\\,*\\d*")
}

# Function for extracting number from a string which includes a decimal
numextract_decimal <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
}

# Clean instate_tuition - Remove extra content and convert to numeric
school_data$instate_tuition <- str_replace_all(school_data$instate_tuition, "(2018-19)", "")
school_data$instate_tuition <- numextract_comma(school_data$instate_tuition)
school_data$instate_tuition <- str_replace_all(school_data$instate_tuition, ",", "")
school_data$instate_tuition <- as.numeric(school_data$instate_tuition)

# Clean outstate_tuition - Remove extra content and convert to numeric
school_data$outstate_tuition <- str_replace_all(school_data$outstate_tuition, "(2018-19)", "")
school_data$outstate_tuition <- numextract_comma(school_data$outstate_tuition)
school_data$outstate_tuition <- str_replace_all(school_data$outstate_tuition, ",", "")
school_data$outstate_tuition <- as.numeric(school_data$outstate_tuition)

# Clean room_and_board - Remove extra content and convert to numeric
school_data$room_and_board <- str_replace_all(school_data$room_and_board, "(2018-19)", "")
school_data$room_and_board <- numextract_comma(school_data$room_and_board)
school_data$room_and_board <- str_replace_all(school_data$room_and_board, ",", "")
school_data$room_and_board <- as.numeric(school_data$room_and_board)

# Clean enrollment - Extract and Convert Number
school_data$enrollment <- numextract_comma(school_data$enrollment)
school_data$enrollment <- str_replace_all(school_data$enrollment, ",", "")
school_data$enrollment <- as.numeric(school_data$enrollment)

# Clean school_type - Parse public/private and encode (1 = private, 0 = public)
school_data$school_type <- substr(school_data$school_type,1,regexpr(",",school_data$school_type)-1)
for (i in 1:nrow(school_data)){
  if(identical(school_data$school_type[i], "Private")) {
    school_data$school_type[i] <- 1
  } else {
    school_data$school_type[i] <- 0
  }
}
school_data$school_type <- as.numeric(school_data$school_type)

# Clean religious_affiliation - Parse religion and encode (1 = Religious, 0 = None)
for (i in 1:nrow(school_data)){
  if(identical(school_data$religious_affiliation[i], "None")) {
    school_data$religious_affiliation[i] <- 0
  } else {
    school_data$religious_affiliation[i] <- 1
  }
}
school_data$religious_affiliation <- as.numeric(school_data$religious_affiliation)

# Clean acad_calendar - Parse calendar and encode (1 = Semester, 0 = Other)
for (i in 1:nrow(school_data)){
  if(identical(school_data$acad_calendar[i], "Semester")) {
    school_data$acad_calendar[i] <- 1
  } else {
    school_data$acad_calendar[i] <- 0
  }
}
school_data$acad_calendar <- as.numeric(school_data$acad_calendar)

# Clean setting_urban - Parse setting_urban and encode (1 = Urban/City, 0 = Other)
for (i in 1:nrow(school_data)){
  if(identical(school_data$setting_urban[i], "Urban") ||
     identical(school_data$setting_urban[i], "City")) {
    school_data$setting_urban[i] <- 1
  } else {
    school_data$setting_urban[i] <- 0
  }
}
school_data$setting_urban <- as.numeric(school_data$setting_urban)

# Clean setting_suburb - Parse setting_suburb and encode (1 = Suburban/Suburb, 0 = Other)
for (i in 1:nrow(school_data)){
  if(identical(school_data$setting_suburb[i], "Suburban") ||
     identical(school_data$setting_suburb[i], "Suburb")) {
    school_data$setting_suburb[i] <- 1
  } else {
    school_data$setting_suburb[i] <- 0
  }
}
school_data$setting_urban <- as.numeric(school_data$setting_urban)

# Clean endowment - Convert to true value in numeric
for (i in 1:nrow(school_data)){
  # If endowment contains billion
  if(grepl("billion", school_data$endowment[i])){
    school_data$endowment[i] <- as.numeric(numextract_decimal(school_data$endowment[i])) * 1000000000
  } else if (grepl("million", school_data$endowment[i])) {
    school_data$endowment[i] <- as.numeric(numextract_decimal(school_data$endowment[i])) * 1000000
  } else {
    # Endowment error
    school_data$endowment[i] <- -1
  }
}
school_data$endowment <- as.numeric(school_data$endowment)

# Clean salary - Extract and convert number
school_data$salary <- numextract_comma(school_data$salary)
school_data$salary <- str_replace_all(school_data$salary, ",", "")
school_data$salary <- as.numeric(school_data$salary)

# Clean accept_rate - Extract and convert number
school_data$accept_rate <- numextract_comma(school_data$accept_rate)
school_data$accept_rate <- as.numeric(school_data$accept_rate)
school_data$accept_rate <- school_data$accept_rate / 100

# Clean stud_fac_ratio (numextract returns first number) - Extract and convert number
school_data$stud_fac_ratio <- numextract_comma(school_data$stud_fac_ratio)
school_data$stud_fac_ratio <- as.numeric(school_data$stud_fac_ratio)

# Clean four_year_grad_rate - Extract and convert number
school_data$four_year_grad_rate <- numextract_comma(school_data$four_year_grad_rate)
school_data$four_year_grad_rate <- as.numeric(school_data$four_year_grad_rate)
school_data$four_year_grad_rate <- school_data$four_year_grad_rate / 100

# Final Formatting
school_data <- cbind(ranking = rownames(school_data), school_data) # adds rankings as column using df index as rankings
school_data <- school_data[complete.cases(school_data), ] # removes rows containing NA
school_data <- school_data[!grepl("-1", as.character(school_data$endowment)),] # removes rows where endowment = -1

# Write to csv
write.csv(school_data, "school_data_clean.csv", row.names=FALSE)
