# input <- list(prods = c("Producto 1", "Producto 2"), time = 3)
function(input, output) {
  
  glimpse(data)
  
  d <- reactive({
    
    d <- data %>% 
      filter(producto %in% input$prods) %>%
      filter(periodo %in% (data %>% pull(periodo) %>% unique() %>% sort() %>% tail(input$time))) %>% 
      rename(score = score2, score_cut = score_cut2)
      
  })
  
  output$vb_coloc <- renderValueBox({
    d <- d()
    data2 <- d %>% 
      group_by(datetime_to_timestamp(periodo)) %>% 
      summarise(y = sum(saldo/1e6)) %>% 
      mutate(y = round(y, 2))
    
    lbl <- data2 %>% pull(y) %>% last() %>% comma() %>% paste0("$", ., "mm") 
    
    hc <- hc_spark(d = data2, prefix = "$", suffix = "mm", type = "area") %>% 
      hc_xAxis(type = "datetime")
    
    valueBox2(
      lbl, subtitle = tagList(hc, p("Colocaciones Mensuales")),
      icon = icon("money"), color = "red", minititle = "Último periodo",
      info = "Información extra de colocaciones"
    )
    
  })
  
  output$vb_venta <- renderValueBox({
    d <- d()
    data2 <- d %>% 
      filter(venta) %>% 
      group_by(datetime_to_timestamp(periodo)) %>% 
      summarise(y = sum(saldo/1e6)) %>% 
      mutate(y = round(y, 2))
    
    lbl <- data2 %>% pull(y) %>% last() %>% comma() %>% paste0("$", ., "mm") 
    
    hc <- hc_spark(d = data2, prefix = "$", suffix = "mm", type = "area") %>% 
      hc_xAxis(type = "datetime")
    
    valueBox2(
      lbl, subtitle = tagList(hc, p("Ventas Mensuales")),
      icon = icon("money"), color = "green", minititle = "Último periodo",
      info = "Información extra"
    )
    
  })
  
  output$vb_provi <- renderValueBox({
    d <- d()
    data2 <- d %>% 
      group_by(datetime_to_timestamp(periodo)) %>% 
      summarise(y = sum(provision/1e6)) %>% 
      mutate(y = round(y, 2))
    
    lbl <- data2 %>% pull(y) %>% last() %>% comma() %>% paste0("$", ., "mm") 
    
    hc <- hc_spark(d = data2, prefix = "$", suffix = "mm", type = "area") %>% 
      hc_xAxis(type = "datetime")
    
    valueBox2(
      lbl, subtitle = tagList(hc, p("Provisión")),
      icon = icon("money"), color = "blue", minititle = "Último periodo",
      info = "Información extra"
    )
    
  })  
  
  output$vb_iries <- renderValueBox({
    d <- d()
    data2 <- d %>% 
      group_by(datetime_to_timestamp(periodo)) %>% 
      summarise(y = 100 * sum(provision/1e6)/sum(saldo/1e6)) %>% 
      mutate(y = round(y, 0))
    
    lbl <- data2 %>% pull(y) %>% last() %>% comma() %>% paste0("", ., " %") 
    
    hc <- hc_spark(d = data2, prefix = "IR ", suffix = " %", type = "area") %>% 
      hc_xAxis(type = "datetime")
    
    valueBox2(
      lbl, subtitle = tagList(hc, p("Índice de Riesgo")),
      icon = icon("money"), color = "black", minititle = "Último periodo",
      info = "Información extra"
    )
    
  })  
  
  output$chart_colocaiones_prod <- renderHighchart({
    d <- d()
    d %>% 
      group_by(periodo, producto) %>% 
      summarise(saldo = round(sum(saldo/1e6), 0)) %>% 
      hchart("area", hcaes(x = periodo, y = saldo, group = producto)) %>% 
      hc_tooltip(sort = TRUE, table = TRUE) %>% 
      hc_title(text = "Colocación por Producto") %>%
      hc_plotOptions(area = list(stacking = "normal")) %>% 
      hc_exporting(enabled = TRUE)
    
  })
  
  output$chart_venta_riesgo <- renderHighchart({
    d <- d()
    d %>% 
      filter(venta) %>% 
      group_by(producto, `Risk Indicator` = ri) %>% 
      summarise(Venta = round(sum(saldo/1e6), 0)) %>% 
      hchart("column", hcaes(x = `Risk Indicator`, group = producto, y = Venta)) %>% 
      hc_tooltip(sort = TRUE, table = TRUE) %>% 
      hc_title(text = "Venta por Riesgo") %>% 
      hc_exporting(enabled = TRUE)
  })
  
  output$chart_ks <- renderHighchart({
    d <- d()    
    d %>% 
      group_by(periodo) %>% 
      summarise(aucroc = round(100 * ks(bad, score), 2)) %>% 
      hchart("line", hcaes(periodo, aucroc), name = "KS") %>% 
      hc_yAxis(min = 0, title = list(text = "KS")) %>% 
      hc_xAxis(title = list(text = "Fecha")) %>% 
      hc_title(text = "Kolmogorov-Smirnov")
    
  })
  
  output$chart_auc <- renderHighchart({
    d <- d()
    d %>% 
      group_by(periodo) %>% 
      summarise(aucroc = round(100 * ModelMetrics::auc(bad, score), 2)) %>% 
      hchart("line", hcaes(periodo, aucroc), name = "AUCRoc") %>% 
      hc_yAxis(min = 0, title = list(text = "AUCRoc"),
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
      hc_xAxis(title = list(text = "Fecha")) %>% 
      hc_title(text = "Area Under Curve ROC")
    
  })
  
  output$chart_psi <- renderHighchart({
    d <- d()
    d %>% 
      count(periodo, score_cut) %>% 
      group_by(periodo) %>% 
      mutate(p = n/sum(n)) %>% 
      left_join(mod_dev_dist2) %>% 
      mutate(psi = (p - p_dev) * log(p/p_dev)) %>% 
      group_by(periodo) %>% 
      summarise(psi = round(100 * sum(psi), 2)) %>% 
      hchart("line", hcaes(periodo, psi), name = "PSI") %>% 
      hc_yAxis(
        min = 0, max = 30,
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
      hc_xAxis(title = list(text = "Fecha")) %>% 
      hc_title(text = "Population Stability Index")
    
  })
  
  output$chart_backtest <- renderHighchart({
    
    bind_rows(
      data %>% 
        group_by(periodo) %>% 
        summarise(value = mean(score1)/1000) %>% 
        mutate(key = "PD"),
      data %>% 
        group_by(periodo) %>% 
        summarise(value = mean(bad)) %>% 
        head(-13) %>%
        mutate(key = "Tasa Incumplimiento")
    ) %>% 
      mutate(value = round(value, 2)) %>% 
      hchart("line", hcaes(periodo, value, group = key)) %>% 
      hc_yAxis(min = 0, title = list(text = "")) %>% 
      hc_xAxis(title = list(text = "Fecha")) %>% 
      hc_title(text = "Backtest")
    
  })
  
}
