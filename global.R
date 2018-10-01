# packages ----------------------------------------------------------------
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(highcharter)
library(lubridate)
library(ROCR)
library(scales)
# http://130.60.24.205/shinydashboardPlus/

# variables ---------------------------------------------------------------
COLORS <- c("#F13E48", "#1C6AC7", "#FFE933", "#53A155", "#666666")
# scales::show_col(COLORS)
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

# clcredit <- tbl_df(readRDS("data/chileancredit.rds"))
# clcredit <- clcredit %>% 
#   mutate(bad = 1 - fgood) %>% 
#   rename(periodo = period)

# biv <- irks::describe_bivariate(credit, bad)
# biv %>%
#   arrange(desc(iv))


# helpers -----------------------------------------------------------------
ks <- function(target, score){
  
  stopifnot(
    setequal(target, c(0, 1)),
    length(target) == length(score)
  )
  
  pred <- prediction(score, target)
  perf <- performance(pred, "tpr", "fpr")
  
  ks <- max(abs(attr(perf, "y.values")[[1]] - attr(perf, "x.values")[[1]]))
  
  return(as.numeric(ks))
}

get_sample_credit <- function(n = 1000, br = 0.123, noise = 0.1) {
  
  # message("n: ", n, " - br: ", br, " - noise: ", noise)
  
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
  
  periodo_ini <- ymd(20140601)
  periodo_end <- Sys.Date() 
  periodo_end <- (periodo_end - day(periodo_end) + days(1)) - months(1)
  periodo_seq <- seq.Date(from = periodo_ini, to = periodo_end, by = "month")
  
  data <- data_frame(periodo = periodo_seq)
  
  nr <- nrow(data)
  
  set.seed(123)
  
  data <- data %>% 
    mutate(
      n = ((1:nr)^0.3 + 1) * 1000,
      n_noise = (rnorm(nr) * (1:nr)^1.5),
      br = rbeta(nr, 20, 80),
      noise = seq(0, by = 0.0005, length.out = nr)
    )
  
  head(data)
  tail(data)
  
  data <- data %>% 
    mutate(
      n = n + n_noise,
      br = br + noise
    ) 
  
  data %>% 
    gather(k, v, -periodo) %>% 
    ggplot(aes(periodo, v)) +
    geom_line() +
    facet_wrap(vars(k), scales = "free_y")
  
  datas <- data %>% 
    select(n, br, noise) %>% 
    pmap(get_sample_credit)
    
  data <- data %>% 
    mutate(data_sample = datas)
    
  ggplot(data, aes(periodo, n)) + 
    geom_line() +
    geom_smooth() +
    scale_y_continuous(limits = c(0, NA))
  
  data %>% 
    group_by(periodo) %>% 
    summarise(brm = round(sum(br*n))/sum(n)) %>% 
    ggplot() + 
    geom_line(aes(periodo, brm)) + 
    scale_y_continuous(limits = c(0, 1))
  
  data %>% 
    count(periodo, wt = n) %>% 
    ggplot() + 
    geom_line(aes(periodo, nn))
  
  data <- data %>% 
    select(periodo, periodo, data_sample) %>% 
    unnest()
  
  data %>% 
    group_by(periodo) %>% 
    summarise(brm = mean(bad)) %>% 
    ggplot() + 
    geom_line(aes(periodo, brm)) + 
    scale_y_continuous(limits = c(0, 1))
  
  data 
  
}

valueBox2 <- function(value, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL, minititle = NULL,
                      info = "Este texto te puede ayudar a entender el valor que estas viendo") {
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon)) 
    shinydashboard:::tagAssert(icon, type = "i")
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(class = "inner",
        tags$small(minititle),
        tags$small(tags$i(class = "fa fa-info-circle fa-2x", title = info, class = "ttip"), class = "pull-right"),
        h3(value),
        subtitle) #, if (!is.null(icon)) div(class = "icon-large", icon)
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), boxContent)
}

hc_spark <- function(d, height = 100, suffix = "", prefix = "", type = "line", ...) {
  
  highchart() %>% 
    hc_add_series(
      zIndex = 5,
      data = list_parse2(d),
      color = 'rgba(255,255,255,0.55)',
      type = type,
      lineWidth = 2.4,
      marker = list(enabled = FALSE),
      showInLegend = FALSE,
      fillColor = list(
        linearGradient = c(0, 0, 0, 300),
        stops = list(
          list(0, 'rgba(255,255,255,0.45)'),
          list(1, 'rgba(255,255,255,0.05)')
        )
      ),
      ...
    ) %>% 
    hc_tooltip(
      zIndex = 100,
      headerFormat = "",
      pointFormat = "{point.x:%Y-%m-%d}: <b>{point.y}</b>",
      valueSuffix = suffix,
      valuePrefix = prefix
    ) %>% 
    hc_xAxis(visible = FALSE) %>% 
    hc_yAxis(visible = FALSE, endOnTick = FALSE, startOnTick = FALSE) %>% 
    hc_size(height = height) %>% 
    hc_chart(margins = c(0, 0, 0, 0))
  
}


# get data ----------------------------------------------------------------
data <- get_data()

mod1 <- glm(
  data = credit %>% filter(age <= 30) %>% head(1000),
  bad ~ age + marital_status + months_in_the_job + flag_res_phone,
  family = "binomial"
)

mod_brks1 <- c(0, quantile(round(1000 * predict(mod1, type = "response")), 1:9/10), 1000)

mod_dev_dist1 <- round(1000 * predict(mod1, type = "response")) %>% 
  cut(mod_brks1) %>% 
  table() %>% 
  as_data_frame() %>% 
  set_names(c("score_cut", "n_dev")) %>% 
  mutate(p_dev = n_dev/sum(n_dev))

mod2 <- glm(
  data = credit %>% filter(months_in_the_job >= 12 * 3) %>% head(2000),
  bad ~ marital_status + months_in_the_job + months_in_residence + profession_code,
  family = "binomial"
)

mod_brks2 <- c(0, quantile(round(1000 * predict(mod2, type = "response")), 1:9/10), 1000)

mod_dev_dist2 <- round(1000 * predict(mod2, type = "response")) %>% 
  cut(mod_brks2) %>% 
  table() %>% 
  as_data_frame() %>% 
  set_names(c("score_cut", "n_dev")) %>% 
  mutate(p_dev = n_dev/sum(n_dev))

set.seed(123)
data <- data %>%
  mutate(
    producto = paste("Producto", sample(x = 1:4, size = nrow(.), replace = TRUE, prob = sqrt(4:1))),
    score1 = round(1000 * predict(mod1, newdata = data, type = "response")),
    score_cut1 = cut(score1, breaks = mod_brks1),
    score2 = round(1000 * predict(mod2, newdata = data, type = "response")),
    score_cut2 = cut(score2, breaks = mod_brks2),
    saldo = round(rnorm(nrow(.), 7e6, 1e6)),
    provision = (saldo/1000) * runif(nrow(.), .2, .4) * (score1 + score2)/2,
    venta = ifelse(runif(nrow(.)) < .25, TRUE, FALSE),
    ri = cut(score1, breaks = mod_brks1[-c(2, 4, 9, 5, 6)], include.lowest = TRUE, labels = LETTERS[1:5])
  )

filter(data, is.na(ri)) %>% glimpse()
count(data , ri)
glimpse(data)
