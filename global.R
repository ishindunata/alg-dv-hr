library(tidyverse)
library(ggpubr)
library(scales)
library(lubridate)
library(dplyr)
library(glue)
library(ggplot2)
library(plotly) 
library(shiny)
library(shinydashboard)
library(echarts4r)
library(highcharter)
library(htmlwidgets)
library(DT)

options(scipen=123)

# read data
hrdt <- read.csv("HRDataset_v14.csv", stringsAsFactors = T, fileEncoding="UTF-8-BOM") 

## data wrangling
hrdt_clean <-  hrdt %>% 
  mutate(
    DOB = mdy(DOB),
    DOBYr = ifelse(year(DOB) > 2022,
                   paste0(str_replace(substr(DOB,1,2),"20","19"), substr(DOB,3,4)),year(DOB)),
    DOBMt = month(DOB),
    DOBDy = day(DOB),
    DOBNew = as.Date(glue("{DOBYr}-{DOBMt}-{DOBDy}"),"%Y-%m-%d"),
    DateofHire = mdy(DateofHire),
    HireYr = year(DateofHire),
    HireMt = month(DateofHire),
    DateofTermination = mdy(DateofTermination),
    TermYr = year(DateofTermination),
    TermMt = month(DateofTermination),
    LastPerformanceReview_Date = mdy(LastPerformanceReview_Date),
    Department = trimws(Department),
    WorkExperience = ifelse(
      is.na(DateofTermination),
      round((as.double(difftime(Sys.Date(),DateofHire, units="days"))/365), digits=2 ),
      round((as.double(difftime(DateofTermination,DateofHire, units="days"))/365), digits=2 )),
    Age = round((as.double(difftime(Sys.Date(),DOBNew, units="days"))/365), digits=2 ),
    Generation = ifelse(year(DOBNew) <= 1964,"Baby Boomers",
                        ifelse(year(DOBNew) >= 1965 & year(DOBNew) < 1980,"Gen X",
                               ifelse(year(DOBNew) >= 1980 & year(DOBNew) < 1996,"Millennials",
                                      ifelse(year(DOBNew) >= 1996 && year(DOBNew) < 2012,"Gen Z",
                                             "Gen Alpha")
                               )
                        )
    ),
    WorkStage = ifelse(WorkExperience < 1,"Idealize",
                       ifelse(WorkExperience >= 1 & WorkExperience <= 2,"Honeymoon",
                              ifelse(WorkExperience >= 3 & WorkExperience <= 5,"Real Reconciliation",
                                     ifelse(WorkExperience >= 6 & WorkExperience <= 10,"Peak Performance",
                                            "Plateau")
                              )
                       )
    ),
  ) %>% 
  # mutate_at(c("ï..Employee_Name","ManagerName"), as.character) %>% 
  mutate(ManagerName=as.character("ManagerName")) %>% 
  mutate_at(c("Generation","WorkStage"), as.factor) %>% 
  rename(EmployeeName = Employee_Name)

hrdt_trend <- hrdt_clean %>% 
  group_by(HireYr) %>% 
  summarise(cnt_emp = n_distinct(EmployeeName)) %>% 
  ungroup() %>% 
  mutate(label = glue(
    "Year: {HireYr}
    Total Employee: {comma(cnt_emp)}"
  ))

hrdt_active_trend <- hrdt_clean %>% 
  filter(EmploymentStatus=="Active") %>% 
  group_by(HireYr) %>% 
  summarise(cnt_emp = n_distinct(EmployeeName)) %>% 
  ungroup() %>% 
  mutate(label = glue(
    "Year: {HireYr}
    Total Employee: {comma(cnt_emp)}"
  ))

hrdt_termv_trend <- hrdt_clean %>% 
  filter(EmploymentStatus=="Voluntarily Terminated") %>% 
  group_by(HireYr) %>% 
  summarise(cnt_emp = n_distinct(EmployeeName)) %>% 
  ungroup() %>% 
  mutate(label = glue(
    "Year: {HireYr}
    Total Employee: {comma(cnt_emp)}"
  ))

hrdt_termc_trend <- hrdt_clean %>% 
  filter(EmploymentStatus=="Terminated for Cause") %>% 
  group_by(HireYr) %>% 
  summarise(cnt_emp = n_distinct(EmployeeName)) %>% 
  ungroup() %>% 
  mutate(label = glue(
    "Year: {HireYr}
    Total Employee: {comma(cnt_emp)}"
  ))

hrdt_active <- hrdt_clean %>% 
  filter(EmploymentStatus=="Active") %>% 
  group_by(EmploymentStatus) %>% 
  summarise(cnt=n_distinct(EmployeeName)) %>% 
  ungroup

hrdt_termv <- hrdt_clean %>% 
  filter(EmploymentStatus=="Voluntarily Terminated") %>% 
  group_by(EmploymentStatus) %>% 
  summarise(cnt=n_distinct(EmployeeName)) %>% 
  ungroup

hrdt_termc <- hrdt_clean %>% 
  filter(EmploymentStatus=="Terminated for Cause") %>% 
  group_by(EmploymentStatus) %>% 
  summarise(cnt=n_distinct(EmployeeName)) %>% 
  ungroup

