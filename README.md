# Introduction to Econometrics Final Project
## Statistical Analysis of US News & World Reports College Rankings

*Rankings URL*: https://www.usnews.com/best-colleges/rankings/national-universities?_mode=table

This project attempts to build a model for US News' College Rankings using
the data available on their website. I've designed this project in three stages:

1) web_scrape.R - The R script for scraping the data from the website and outputing the raw data into a csv file (school_data_raw.csv)
2) encode_data.R - The R script responsible for wrangling the data into a useable format; it reads in the school_data_raw.csv and outputs
the clean/encoded data in school_data_clean.csv
3) analysis.R - The R script responsible for conducting the statistical analysis and building the model for rankings. It reads in the data
from school_data_clean.csv for this purpose.

