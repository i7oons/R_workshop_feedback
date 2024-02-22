# Load package

library(tidyverse)
library(googlesheets4)

# Load data
url <- "https://docs.google.com/spreadsheets/d/12jxEV9rqIOrIw70lVXfSegorv25owciTMS-hjSotioo/edit?resourcekey#gid=1120526525"
dat <- read_sheet(url)

dat1 <- dat |> 
  rename(email = 2) |> 
  filter(row_number() == n(), .by = email) |> # Take the last record if duplicated
  select(-Timestamp)

#load data for pre-workshop organisation
pre_wksp <- dat1 |> 
  select(email, matches("1.[1-4]")) |> 
  pivot_longer(-email,
               names_to = "qns",
               values_to = "rating") |> 
  mutate(
    qns = str_sub(qns, 3, 3) |> as.numeric(), # 3 is the index position of the str_sub for qns
    qns = case_when(
      qns == 1 ~ "It was easy to register for the workshop",
      qns == 2 ~ "Communications for the workshop were clear",
      qns == 3 ~ "Communications for the workshop were timely",
      qns == 4 ~ "Instructions for workshop preparation were clear",
      .default = "Unknown"
    ),
    qns = factor(qns,
                 levels = c("It was easy to register for the workshop","Communications for the workshop were clear",
                            "Communications for the workshop were timely","Instructions for workshop preparation were clear")),
    rating = case_when(
      rating == 1 ~ "Strongly disagree",
      rating == 2 ~ "Disagree",
      rating == 3 ~ "Neutral",
      rating == 4 ~ "Agree",
      rating == 5 ~ "Strongly agree",
      .default = "Unknown"
    ),
    rating = factor(rating,
                    levels = c("Strongly disagree", "Disagree", "Neutral",
                               "Agree", "Strongly agree"))
  )

pre_wksp_1 <- pre_wksp |> 
  count(qns, rating) |> 
  mutate(prop = n/sum(n),
         .by = qns)

# load data for Facilitator explanation
explain <- dat1 |> 
  select(email, matches("^[2-7]\\.1")) |> 
  pivot_longer(-email,
               names_to = "topic",
               values_to = "rating") |> 
  mutate(
    topic = str_sub(topic, 1, 1) |> as.numeric(),
    topic = case_when(
      topic == 2 ~ "Intro to R",
      topic == 3 ~ "Data Viz",
      topic == 4 ~ "Data Wrangling",
      topic == 5 ~ "Descriptive Analysis",
      topic == 6 ~ "Intro to Quarto",
      topic == 7 ~ "Report generation",
      .default = "Unknown"
    ),
    topic = factor(topic,
                   levels = c("Intro to R", "Data Viz", "Data Wrangling",
                              "Descriptive Analysis", "Intro to Quarto",
                              "Report generation")),
    rating = case_when(
      rating == 1 ~ "Strongly disagree",
      rating == 2 ~ "Disagree",
      rating == 3 ~ "Neutral",
      rating == 4 ~ "Agree",
      rating == 5 ~ "Strongly agree",
      .default = "Unknown"
    ),
    rating = factor(rating,
                    levels = c("Strongly disagree", "Disagree", "Neutral",
                               "Agree", "Strongly agree")),
    qns = "The facilitator's explanation was easy to follow"
  )

explain_1 <- explain |> 
  count(topic, rating, qns) |> 
  mutate(prop = n/sum(n),
         .by = topic)

# load data for materials
materials <- dat1 |> 
  select(email, matches("^[2-7]\\.2")) |> 
  pivot_longer(-email,
               names_to = "topic",
               values_to = "rating") |> 
  mutate(
    topic = str_sub(topic, 1, 1) |> as.numeric(),
    topic = case_when(
      topic == 2 ~ "Intro to R",
      topic == 3 ~ "Data Viz",
      topic == 4 ~ "Data Wrangling",
      topic == 5 ~ "Descriptive Analysis",
      topic == 6 ~ "Intro to Quarto",
      topic == 7 ~ "Report generation",
      .default = "Unknown"
    ),
    topic = factor(topic,
                   levels = c("Intro to R", "Data Viz", "Data Wrangling",
                              "Descriptive Analysis", "Intro to Quarto",
                              "Report generation")),
    rating = case_when(
      rating == 1 ~ "Strongly disagree",
      rating == 2 ~ "Disagree",
      rating == 3 ~ "Neutral",
      rating == 4 ~ "Agree",
      rating == 5 ~ "Strongly agree",
      .default = "Unknown"
    ),
    rating = factor(rating,
                    levels = c("Strongly disagree", "Disagree", "Neutral",
                               "Agree", "Strongly agree")),
    qns = "The training materials clear and adequate"
  )

materials_1 <- materials |> 
  count(topic, rating, qns) |> 
  mutate(prop = n/sum(n),
         .by = topic)

p_pre_wksp <-  ggplot(pre_wksp_1, aes(x = prop, y = fct_rev(qns), fill = fct_rev(rating))) +
  geom_col(alpha = 0.8, show.legend = TRUE) +
  labs(x = "", y = "", fill = "",
       title = "Pre-workshop Organisation") +
  scale_fill_manual(values = c("#236EC3", "#78AFE6", "#E1E1E1", "#E6AAA0", "#C32314")) +
  guides(fill = guide_legend(reverse = TRUE)) + # reverse legend order
  scale_x_continuous(labels = scales::label_percent()) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()   
  )

p_explain <- ggplot(explain_1, aes(x = prop, y = fct_rev(topic), fill = fct_rev(rating))) +
  geom_col(alpha = 0.8) +
  labs(x = "", y = "", fill = "",
       title = "The facilitator's explanation was easy to follow") +
  scale_fill_manual(values = c("#236EC3", "#78AFE6", "#E1E1E1", "#E6AAA0", "#C32314")) +
  guides(fill = guide_legend(reverse = TRUE)) + # reverse legend order
  scale_x_continuous(labels = scales::label_percent()) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

p_materials <- ggplot(materials_1, aes(x = prop, y = fct_rev(topic), fill = fct_rev(rating))) +
  geom_col(alpha = 0.8) +
  labs(x = "", y = "", fill = "",
       title = "The training materials clear and adequate") +
  scale_fill_manual(values = c("#236EC3", "#78AFE6", "#E1E1E1", "#E6AAA0", "#C32314")) +
  guides(fill = guide_legend(reverse = TRUE)) + # reverse legend order
  scale_x_continuous(labels = scales::label_percent()) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

