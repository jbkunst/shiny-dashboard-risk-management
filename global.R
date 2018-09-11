library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
library(highcharter)
library(lubridate)
library()
# http://130.60.24.205/shinydashboardPlus/

# data(credit, package = "riskr")
# saveRDS(credit, "data/credit.rds")
credit <- tbl_df(readRDS("data/credit.rds"))

# irks::describe_bivariate(credit, bad) 
# biv <- .Last.value
# biv %>% 
#   arrange(desc(iv))

mod <- glm(data = credit, bad ~ age + marital_status + months_in_the_job + flag_res_phone)

get_sample_credit <- function(n = 1000, br = 0.123) {
  
  set.seed(123)
  d1 <- credit %>% filter(bad == 1) %>% sample_n(round(n * br), replace = TRUE)
  
  set.seed(123)
  d0 <- credit %>% filter(bad == 0) %>% sample_n(round(n * (1 - br)), replace = TRUE)
  
  d <- bind_rows(d1, d0)
  
  set.seed(123)
  d <- sample_frac(d, size = 1)
  
  d
  
}

get_data <- function() {
  
  date_ini <- ymd(20180101)
  date_end <- Sys.Date()
  date_seq <- seq.Date(from = date_ini, to = date_end, by = "day")
  
  data <- data_frame(
    date = date_seq
  ) %>% 
    mutate(
      month = ymd(format(date, "%Y%m01")),
      wday_lbl = wday(date, label = TRUE),
      wday_num = wday(date)
    ) %>% 
    # remuevo los feriados
    filter(
      ! wday_num %in% c(7, 1)
    )
  
  set.seed(123)
  data <- data %>% 
    mutate(
      n = round(row_number()^0.7) + 200 + sample(1:50, size = nrow(.), replace = TRUE),
      # agrego si es lunes o viernes
      n = n + ifelse(wday_num %in% c(2, 6), 25, 0),
      br = rbeta(nrow(.), 10, 90)
    )
  
  data <- data %>% 
    mutate(data_sample = map2(n, br, get_sample_credit))
  
  ggplot(data) + 
    geom_line(aes(date, n))
  
  data %>% 
    group_by(month) %>% 
    summarise(
      brm = round(sum(br*n))/sum(n)
    ) %>% 
    ggplot() + 
    geom_line(aes(month, brm)) + 
    scale_y_continuous(limits = c(0, 1))
  
  
  data %>% 
    count(month, wt = n) %>% 
    ggplot() + 
    geom_line(aes(month, nn))
  
  data <- data %>% 
    select(date, month, data_sample) %>% 
    unnest()
  
  data
  
}





