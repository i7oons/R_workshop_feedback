# Load package

library(tidyverse)
library(googlesheets4)

# Load data
url <- "https://docs.google.com/spreadsheets/d/12jxEV9rqIOrIw70lVXfSegorv25owciTMS-hjSotioo/edit?resourcekey#gid=1120526525"
dat <- read_sheet(url)
url2 <- "https://docs.google.com/spreadsheets/d/1ZMLHBtBn83r69u0IHmtAyjJ_QU8XV9uevYIuomj8x1g/edit#gid=146447740"

respondents <- dat |> 
  rename(email = 2) |> 
  filter(row_number() == n(), .by = email) |> # Take the last record if duplicated
  select(email)

registrants <- read_sheet(url2) |> 
  rename(email = 2, 
         gender = 6,
         country = 7,
         type = 9,
         org = 10,
         designation = 11,
         SSRG = 15
         ) |> 
  filter(row_number() == n(), .by = email) |> # Take the last record if duplicated
  select(email, gender, country, type, org, designation, SSRG) |> 
  

