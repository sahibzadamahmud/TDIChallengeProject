
# R Script to 
# 1. Explore the Indeed data set for data science jobs
# 2. Check the skill set for each discipline
# 3. Check if there is strong correlation between variables to determine a linear realtionship for further analysis
# 4. If not, then non-linear relationships can be studied as future work


install.packages("bigmemory")   #If required: Just in case

library(dplyr)
library(tidyverse)
library(caret)
library(readr)
library(bigmemory)

# Read from the .csv file
ds_jobs_data <- read_csv("F:\\TDI Data Sets\\Indeed Dataset\\indeed_job_dataset.csv")

str(ds_jobs_data)

head(ds_jobs_data)

# Prelimenary exploratory data analysis

# Check Statewise number of opportunities for data scientists: CA highest with 723 followed by NY
ds_state_trend <- ds_jobs_data %>% filter(Job_Type == "data_scientist") %>% group_by(Location) %>% tally()
ds_state_trend

print(ds_state_trend, n=46)

# Check Statewise number of opportunities for data analysts: CA highest with 376 followed by Ny
da_state_trend <- ds_jobs_data %>% filter(Job_Type == "data_analyst") %>% group_by(Location) %>% tally()
da_state_trend

print(da_state_trend, n=50)

# Dividing the data set into three subsets i.e. for data scientist jobs,
# for data analyst jobs and data engineer jobs
ds_subset <- ds_jobs_data %>% filter(Job_Type == "data_scientist")

da_subset <- ds_jobs_data %>% filter(Job_Type == "data_analyst")

de_subset <- ds_jobs_data %>% filter(Job_Type == "data_engineer")

# Checking the number of records for each discipline
nrow(ds_subset)
nrow(da_subset)
nrow(de_subset)

# Cheking the mean of No. of Skills required for each discipline
mean(ds_subset$No_of_Skills)
mean(da_subset$No_of_Skills)
mean(de_subset$No_of_Skills)

# % Requirement of r for each discipline
((ds_subset %>% filter(r == 1) %>% count())/ nrow(ds_subset)) * 100 # 60.9%
((da_subset %>% filter(r == 1) %>% count())/ nrow(da_subset)) * 100 # 25.4%
((de_subset %>% filter(r == 1) %>% count())/ nrow(de_subset)) * 100 # 16.5%

# % Requirement of Python for each discipline
# for data engineers, python is more preferred instead of r
((ds_subset %>% filter(python == 1) %>% count())/ nrow(ds_subset)) * 100 # 75.1%
((da_subset %>% filter(python == 1) %>% count())/ nrow(da_subset)) * 100 # 28.5%
((de_subset %>% filter(python == 1) %>% count())/ nrow(de_subset)) * 100 # 65.3%

# % Requirement of r and python for each discipline
((ds_subset %>% filter(r == 1 & python == 1) %>% count())/ nrow(ds_subset)) * 100 # 54.4%
((da_subset %>% filter(r == 1 & python == 1) %>% count())/ nrow(da_subset)) * 100 # 19.4%
((de_subset %>% filter(r == 1 & python == 1) %>% count())/ nrow(de_subset)) * 100 # 13.8%


# % Requirement of r and sql for each discipline
((ds_subset %>% filter(r == 1 & sql == 1) %>% count())/ nrow(ds_subset)) * 100 # 36.2%
((da_subset %>% filter(r == 1 & sql == 1) %>% count())/ nrow(da_subset)) * 100 # 19.6%
((de_subset %>% filter(r == 1 & sql == 1) %>% count())/ nrow(de_subset)) * 100 # 13.3%

# % Requirement of python and sql for each discipline
((ds_subset %>% filter(python == 1 & sql == 1) %>% count())/ nrow(ds_subset)) * 100 # 41.2%
((da_subset %>% filter(python == 1 & sql == 1) %>% count())/ nrow(da_subset)) * 100 # 22.8%
((de_subset %>% filter(python == 1 & sql == 1) %>% count())/ nrow(de_subset)) * 100 # 44.7%

# % Requirement of hadoop and spark for each discipline
((ds_subset %>% filter(hadoop == 1 & spark == 1) %>% count())/ nrow(ds_subset)) * 100 # 20.0%
((da_subset %>% filter(hadoop == 1 & spark == 1) %>% count())/ nrow(da_subset)) * 100 # 2.0%
((de_subset %>% filter(hadoop == 1 & spark == 1) %>% count())/ nrow(de_subset)) * 100 # 42.2%

# % Requirement of all skills for each discipline
((ds_subset %>% filter(hadoop == 1 & spark == 1 & r == 1 & python == 1 & sql == 1 & java == 1 & tableau == 1 & sas == 1) %>% count())/ nrow(ds_subset)) * 100 # 0.19%
((da_subset %>% filter(hadoop == 1 & spark == 1 & r == 1 & python == 1 & sql == 1 & java == 1 & tableau == 1 & sas == 1) %>% count())/ nrow(da_subset)) * 100 # 0.0%
((de_subset %>% filter(hadoop == 1 & spark == 1 & r == 1 & python == 1 & sql == 1 & java == 1 & tableau == 1 & sas == 1) %>% count())/ nrow(de_subset)) * 100 # 0.14%

# Setting the grounds for Linear Regression Analysis as a baseline
# Predict number of skills as accurately as possible 
# Check correlation between variables
cor(ds_subset$No_of_Reviews, ds_subset$No_of_Skills, use="complete.obs")
cor(ds_subset$No_of_Skills, ds_subset$No_of_Stars, use="complete.obs")
cor(ds_subset$No_of_Skills, ds_subset$python, use="complete.obs")

plot(ds_subset$No_of_Reviews, ds_subset$No_of_Skills)
# The linear relationship between the variables is weak i.e. weak correlation

# Get some better insight through relevant plots

# This plot shows the No of Skills required per Location
ds_jobs_data %>% ggplot(aes(Date_Since_Posted, No_of_Skills , col = Location)) + geom_point() + facet_wrap(~Location)
   
# This plot shows the Job Type in demand per Location           
ds_jobs_data %>% ggplot(aes(Job_Type, No_of_Skills , col = Job_Type)) + geom_point() + facet_wrap(~Location) + theme(axis.text.x = element_text(angle = 90))
  
