# packages ----------------------------------------------------------------
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)
library(highcharter)
library(lubridate)
# http://130.60.24.205/shinydashboardPlus/

# variables ---------------------------------------------------------------
COLORS <- c("#F13E48", "#1C6AC7", "#FFE933", "#53A155", "#666666")

# options -----------------------------------------------------------------
options(
  highcharter.theme = 
    hc_theme_smpl(
      colors = COLORS,
      title = list(style = list(fontSize = "1.2em", fontFamily = "Overpass")),
      chart = list(
        backgroundColor = "transparent",
        style = list(fontFamily = "Overpass")
      ),
      plotOptions = list(
        series = list(
          dataLabels = list(color = "#222d32", style = list(fontWeight = "normal", textShadow = FALSE, textOutline = FALSE)),
          animation = list(duration = 2500)
        )
      ),
      legend = list(
        itemStyle =  list(
          fontWeight = "normal"
        )
      )
    ),
  shiny.launch.browser = TRUE
)



# data --------------------------------------------------------------------
# data(credit, package = "riskr")
# saveRDS(credit, "data/credit.rds")
credit <- tbl_df(readRDS("data/credit.rds"))

# irks::describe_bivariate(credit, bad) 
# biv <- .Last.value
# biv %>% 
#   arrange(desc(iv))

mod <- glm(data = credit %>% sample_n(2000), bad ~ age + marital_status + months_in_the_job, family="binomial")
mod_brks <- c(0, quantile(round(1000 * predict(mod, type = "response")), 1:9/10), 1000)
mod_dev_dist <- round(1000 * predict(mod, type = "response")) %>% 
  cut(mod_brks) %>% 
  table() %>% 
  as_data_frame() %>% 
  set_names(c("score_cut", "n_dev")) %>% 
  mutate(p_dev = n_dev/sum(n_dev))


# helpers -----------------------------------------------------------------
get_sample_credit <- function(n = 1000, br = 0.123, noise = 0.1) {
  
  set.seed(123)
  d1 <- credit %>% filter(bad == 1) %>% sample_n(round(n * abs(br - noise)), replace = TRUE)
  
  set.seed(123)
  d0 <- credit %>% filter(bad == 0) %>% sample_n(round(n * abs(1 - br + noise)), replace = TRUE)
  
  d <- bind_rows(d1, d0)
  
  set.seed(123)
  d <- sample_frac(d, size = 1)
  
  # count(d, bad)
  
  set.seed(123)
  d <- d %>% 
    mutate(bad = ifelse(runif(nrow(.)) < noise, 1 - bad, bad))
  
  # count(d, bad)
  
  d
  
}

get_data <- function() {
  
  date_ini <- ymd(20140601)
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
      n = round(row_number()^0.7 * 0.5) + 100 + sample(1:50, size = nrow(.), replace = TRUE),
      # agrego si es lunes o viernes
      n = n + ifelse(wday_num %in% c(2, 6), 25, 0),
      br = rbeta(nrow(.), 20, 80),
      noise = seq(0, by = 0.015, length.out = nrow(.))/100
    )
  
  tail(data)
  
  datas <- data %>% 
    select(n, br, noise) %>% 
    pmap(get_sample_credit)
    
  data <- data %>% 
    mutate(data_sample = datas)
    
  ggplot(data, aes(date, n)) + 
    geom_line() +
    geom_smooth() +
    scale_y_continuous(limits = c(0, NA))
  
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
  
  data %>% 
    group_by(month) %>% 
    summarise(
      brm = mean(bad)
    ) %>% 
    ggplot() + 
    geom_line(aes(month, brm)) + 
    scale_y_continuous(limits = c(0, 1))
  
  data 
  
}
