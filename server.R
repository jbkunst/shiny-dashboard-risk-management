# Define server logic required to draw a histogram
function(input, output) {
  
  data <- get_data()
  data <- data %>% 
    mutate(
      score = round(1000 * predict(mod, newdata = data, type = "response")),
      score_cut = cut(score, breaks = mod_brks)
      ) %>% 
    select(date, month, bad, score, score_cut)
  
  glimpse(data)
  
  output$chart_backtest <- renderHighchart({
    
    bind_rows(
      data %>% 
        group_by(month) %>% 
        sample_n(100) %>% 
        summarise(value = mean(score)/1000) %>% 
        mutate(key = "PD"),
      data %>% 
        group_by(month) %>% 
        summarise(value = mean(bad)) %>% 
        head(-13) %>%
        mutate(key = "Tasa Incumplimiento")
    ) %>% 
      mutate(value = round(value, 2)) %>% 
      hchart("line", hcaes(month, value, group = key)) %>% 
      hc_yAxis(min = 0, title = list(text = "")) %>% 
      hc_xAxis(title = list(text = "Fecha"))
    
  })
  
  output$chart_perf <- renderHighchart({
    
    data %>% 
      group_by(month) %>% 
      summarise(aucroc = round(100 * ModelMetrics::auc(bad, score), 2)) %>% 
      head(-13) %>% 
      hchart("line", hcaes(month, aucroc), name = "AUCRoc") %>% 
      hc_yAxis(
        min = 0, max = 100,
        title = list(text = "AUCRoc"),
        plotLines =  
          list(
            list(label = list(text = "Modelo Aleatorio"),
                 color = "#999999",
                 width = 2,
                 value = 50,
                 zIndex = 2
                 )
            )
        ) %>% 
      hc_xAxis(title = list(text = "Fecha"))
    
  })
  
  output$chart_psi <- renderHighchart({
    
    data %>% 
      count(month, score_cut) %>% 
      group_by(month) %>% 
      mutate(p = n/sum(n)) %>% 
      left_join(mod_dev_dist) %>% 
      mutate(psi = (p - p_dev) * log(p/p_dev)) %>% 
      group_by(month) %>% 
      summarise(psi = round(100 * sum(psi), 2)) %>% 
      hchart("line", hcaes(month, psi), name = "AUCRoc") %>% 
      hc_yAxis(
        min = 0, max = 50,
        title = list(text = "PSI"),
        plotLines =  
          list(
            list(label = list(text = "Gran cambio en población"),
                 color = "red",
                 width = 2,
                 value = 25,
                 zIndex = 2
            ),
            list(label = list(text = "Algún cambio menor"),
                 color = "yellow",
                 width = 2,
                 value = 10,
                 zIndex = 2
            )
          )
      ) %>% 
      hc_xAxis(title = list(text = "Fecha"))
    
  })
  
}
