#######################################################
## WRITTEN BY GARRETT BLINKHORN
## ----------------------------------------------------
## This is the first script of our project, where the
## actually data gathering through web scraping will
## occur. The point of this project is to analyze
## the quantitative data available about top schools
## in order to predict their US News & World Report
## Ranking
##
## Target: https://www.usnews.com/best-colleges/rankings/national-universities?_mode=table
#######################################################

# Install scraping package
# install.packages('rvest')

# Load scraping package
library('rvest')
library("stringr")

# SetWD
setwd("C:/Users/jakes/Desktop/Course Work/ECON_6750_Introduction_to_Econometrics/ECON_6750_Final_Project")

# The landing page is located at first_url
first_url <- "https://www.usnews.com/best-colleges/rankings/national-universities?_mode=table"

# Each subsequent page will be accessed via appending the url_suffix and page number to first_url
url_suffix <- "&_page="

# Initialize a list of urls starting with first_url
url_list <- list(first_url)

# Loops over page numbers to be added
for (i in 2:12){
  url_list <- append(url_list, paste(first_url,url_suffix, i, sep=""))
}

# Initialize data frame with empty vectors
school_data <- data.frame("school_name"=character(),
                          "instate_tuition"=character(),
                          "outstate_tuition"=character(),
                          "room_and_board"=character(),
                          "enrollment"=character(),
                          "school_type"=character(),
                          "religious_affiliation"=character(),
                          "acad_calendar"=character(),
                          "setting_urban"=character(),
                          "setting_suburb"=character(),
                          "endowment"=character(),
                          "salary"=character(),
                          "accept_rate"=character(),
                          "stud_fac_ratio"=character(),
                          "four_year_grad_rate"=character(),
                          stringsAsFactors=FALSE)

# Each url in url_list contains summaries of 20 schools.
# Each of these summaries links to full descriptions of each school.
# The desired data is in the full description, so the initial
# processing of each url is to retrieve the list of sub_urls
for (url in url_list) {
  domain <- 'https://www.usnews.com'
  sub_url_list = list()

  # Returns list of urls, where links 1:20 are the sub_urls of interest
  link_containers <- read_html(url) %>%
                     html_nodes(".text-strong") %>%
                     html_nodes("a") %>% 
                     html_attr('href')
  
  for (i in 1:20){
    sub_url <- paste(domain, link_containers[i], sep="")
    sub_url_list <- append(sub_url_list, sub_url)
  }
  
  for (sub_url in sub_url_list) {
    print(sub_url)
    Sys.sleep(1) # Wait one second between requests
    webpage <- read_html(sub_url)
    
    # Scrape University Name
    school_name <- webpage %>%
                   html_nodes(".hero-heading") %>%
                   html_text()
    
    school_name <- str_trim(str_replace_all(school_name, "\n" , ""))

    quick_stats_li <- webpage %>%
                      html_nodes(".hero-stats-widget-stats") %>%
                      html_nodes("ul") %>%
                      html_nodes("li") 
    
    quick_stats_span <- quick_stats_li %>% 
                        html_nodes("span") %>%
                        html_text()
    
    quick_stats_text <- webpage %>%
                        html_nodes(".hero-stats-widget-stats") %>%
                        html_nodes("ul") %>%
                        html_nodes("li") %>%
                        html_text()
    
    if (identical(quick_stats_span[1], "Tuition & Fees")){
      # School reports only a single tuition, use for in and out of state
      instate_tuition <- quick_stats_text[1]
      outstate_tuition <- quick_stats_text[1]
      room_and_board <- quick_stats_text[2]
      enrollment <- quick_stats_text[3]
    } else {
      # School reports in & out of state tuition
      instate_tuition <- quick_stats_text[1]
      outstate_tuition <- quick_stats_text[2]
      room_and_board <- quick_stats_text[3]
      enrollment <- quick_stats_text[4]
    }
    
    general_info <- webpage %>%
                    html_nodes(".flex-small") %>%
                    html_nodes(".text-tight") %>%
                    html_text()
    
    school_type <- general_info[1]
    religious_affiliation <- general_info[3]
    acad_calendar <- general_info[4]
    setting_urban <- general_info[5]
    setting_suburb <- general_info[5]
    
    endowment <- general_info[6]
    
    spans <- webpage %>%
              html_nodes(".show-for-small-only") %>%
              html_nodes(".text-strong") %>%
              html_text()
    
    spans <- str_replace_all(spans, "\n", "")
    
    salary <- spans[1]
    accept_rate <- spans[3]
    stud_fac_ratio <- spans[6]
    four_year_grad_rate <- spans[7]
    
    school_data[nrow(school_data) + 1,] = list(school_name,
                                               instate_tuition,
                                               outstate_tuition,
                                               room_and_board,
                                               enrollment,
                                               school_type,
                                               religious_affiliation,
                                               acad_calendar,
                                               setting_urban,
                                               setting_suburb,
                                               endowment,
                                               salary,
                                               accept_rate,
                                               stud_fac_ratio,
                                               four_year_grad_rate)
  }
}

# Export dataframe to CSV
write.csv(school_data, "school_data_raw.csv", row.names=FALSE)
