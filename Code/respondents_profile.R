# Load package

library(tidyverse)
library(googlesheets4)
library(stringr)
library(gtsummary)
library(collapse)
library(flextable)

# Load data
url <- "https://docs.google.com/spreadsheets/d/12jxEV9rqIOrIw70lVXfSegorv25owciTMS-hjSotioo/edit?resourcekey#gid=1120526525"
dat <- read_sheet(url)
url2 <- "https://docs.google.com/spreadsheets/d/1ZMLHBtBn83r69u0IHmtAyjJ_QU8XV9uevYIuomj8x1g/edit#gid=146447740"

respondents <- dat |> 
  rename(email = 2) |> 
  mutate(use_ird_email = str_detect(email, "ird\\.global")) 

registrants <- read_sheet(url2) |> 
  rename(email = 2, 
         gender = 6,
         country = 7,
         type = 10,
         org = 11,
         SSRG = 16
         ) |> 
  filter(row_number() == n(), .by = email) |> # Take the last record if duplicated
  select(email, gender, country, type, org, SSRG) |> 
  mutate(email = recode(email, 'lapeonalaja@gmail.com' = 'lape.onalaja@ird.global', 'israr.ahmad.hri@nih.org.pk' = 'israrpmrc@gmail.com', 
  'Mondar.Ahmed@ird.global' = 'mondar.ahmed@ird.global', 'Maryam.younus@ird.global' = 'maryam.younus@ird.global',
  'aftab.khan.hri@nih.org.pk' = 'aftabropmrc@gmail.com', 'minhal.hyder@ird.golbal' = 'minhal.hyder@ird.global',
  'mohsin.h.alvi@pill.org.pk' = 'mohsinhassanalvi@gmail.com')) |> 
  mutate(country = recode(country, 'Karachi' = 'Pakistan'))

# merge details from registrant list to respondents 
combi <- respondents |> 
  left_join(registrants)

# flattened feedback of respondents and rename columns
feedbacks <- combi |> 
  group_by(email) |> 
  summarise_all(~paste(ifelse(is.na(.), "", .), collapse = "|")) |> 
  rename("Organisation - What was done well" = 7,
         "Organisation - Suggestions to improve" = 8,
         "Intro to R - What was done well" = 11,
         "Intro to R - Suggestions to improve" = 12,
         "Data Viz - What was done well" = 15,
         "Data Viz - Suggestions to improve" = 16,
         "Data Wrangling - What was done well" = 19,
         "Data Wrangling - Suggestions to improve" = 20,
         "Descriptive Analysis - What was done well" = 23,
         "Descriptive Analysis - Suggestions to improve" = 24,
         "Intro to Quarto - What was done well" = 27,
         "Intro to Quarto - Suggestions to improve" = 28,
         "Report generation - What was done well" = 31,
         "Report generation - Suggestions to improve" = 32,
         "General comments or suggestions" = 33
  ) |> 
  select(1,7,8,11,12,15,16,19,20,23,24,27,28,31,32,33)

# Summary table for registrants
registrants_details <- registrants |> 
  filter(row_number() == n(), .by = email) |> 
  select(2 : 6) |> 
  tbl_summary()
  
# Summary table for respondents
respondents_details <- combi |> 
  filter(row_number() == n(), .by = email) |>
  select(35 : 39) |> 
  tbl_summary()

feedback_org <- feedbacks |> 
  select(starts_with("Organisation")) |> 
  flextable() |> 
  set_table_properties(layout = "autofit") |> 
  set_caption(caption = "Feedback on pre-workshop Organisation")

feedback_intro <- feedbacks |> 
  select(starts_with("Intro to R")) |> 
  flextable() |> 
  set_table_properties(layout = "autofit") |> 
  set_caption(caption = "Feedback on Intro to R")

feedback_viz <- feedbacks |> 
  select(starts_with("Data Viz")) |> 
  flextable() |> 
  set_table_properties(layout = "autofit") |> 
  set_caption(caption = "Feedback on Data Viz")

feedback_wrang <- feedbacks |> 
  select(starts_with("Data Wrangling")) |> 
  flextable() |> 
  set_table_properties(layout = "autofit") |> 
  set_caption(caption = "Feedback on Data Wrangling")

feedback_descr <- feedbacks |> 
  select(starts_with("Descriptive")) |> 
  flextable() |> 
  set_table_properties(layout = "autofit") |> 
  set_caption(caption = "Feedback on Descriptive Analysis")

feedback_quarto <- feedbacks |> 
  select(starts_with("Intro to Quarto")) |> 
  flextable() |> 
  set_table_properties(layout = "autofit") |> 
  set_caption(caption = "Feedback on Intro to Quarto")

feedback_report <- feedbacks |> 
  select(starts_with("Report generation"))|> 
  flextable() |> 
  set_table_properties(layout = "autofit") |> 
  set_caption(caption = "Feedback on Report generation")

feedback_general <- feedbacks |> 
  select(starts_with("General")) |> 
  rename("Remarks" = 1) |> 
  flextable() |> 
  set_table_properties(layout = "autofit") |> 
  set_caption(caption = "General comments or suggestions")

# Summary table for registrants
registrants_details <- registrants |> 
  filter(row_number() == n(), .by = email) |> 
  select(2 : 6) |> 
  tbl_summary()

# Summary table for respondents
respondents_details <- combi |> 
  filter(row_number() == n(), .by = email) |>
  select(35 : 39) |> 
  mutate(
    gender = setLabels(gender, "Gender"),
    country = setLabels(country, "Country"),
    type = setLabels(type, "Type"),
    org = setLabels(org, "Organization"),
    SSRG = setLabels(SSRG, "Part of SSRG")
  )|> 
  tbl_summary()

