# input <- list(prods = c("Producto 1", "Producto 2"), time = 3, mod = "score1", var = "age")
function(input, output, session) {
  
  glimpse(data)
  
  d <- reactive({
    score_num <- input$mod
    score_num_cat <- paste0(score_num, "_cut")
    
    d <- data %>% 
      filter(producto %in% input$prods) %>%
      filter(periodo %in% (data %>% pull(periodo) %>% unique() %>% sort() %>% tail(input$time))) %>% 
      mutate(score = !!sym(score_num), score_cut = !!sym(score_num_cat))
    
  })
  
  observe({
    
    input$mod
    
    updateSelectInput(session, inputId = "var", choices = names(mods[[input$mod]]$xlevels))
    
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
                 value = 50
            )
          )
      ) %>% 
      hc_xAxis(title = list(text = "Fecha")) %>% 
      hc_title(text = "Area Under Curve ROC")
    
  })
  
  output$chart_backtest <- renderHighchart({
    d <- d()
    bind_rows(
      d %>% 
        group_by(periodo) %>% 
        summarise(value = mean(score)/1000) %>% 
        mutate(key = "PD"),
      d %>% 
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
  
  output$chart_psi <- renderHighchart({
    d <- d()
    
    d %>% 
      count(periodo, score_cut) %>% 
      group_by(periodo) %>% 
      mutate(p = n/sum(n)) %>% 
      left_join(mod_dev_dist_list[[input$mod]]) %>% 
      mutate(psi = (p - p_dev) * log(p/p_dev)) %>% 
      group_by(periodo) %>%
      summarise(psi = round(100 * sum(psi, na.rm = TRUE), 2)) %>% 
      hchart("line", hcaes(periodo, psi), name = "PSI") %>% 
      hc_yAxis(
        min = 0, max = 80,
        title = list(text = "PSI"),
        plotLines =  
          list(
            list(label = list(text = "Gran cambio en población"),
                 color = "red",
                 width = 2,
                 value = 25
            ),
            list(label = list(text = "Algún cambio menor"),
                 color = "yellow",
                 width = 2,
                 value = 10
            )
          )
      ) %>% 
      hc_xAxis(title = list(text = "Fecha")) %>% 
      hc_title(text = "Population Stability Index")
    
  })
  
  output$chart_psi_cut <- renderHighchart({
    d <- d()
    
    dmod_dist <- mod_dev_dist_list[[input$mod]] %>% 
      rename_all(str_remove, "_dev") %>% 
      mutate(cat = "desarrollo")
    
    d %>% 
      count(score_cut) %>% 
      mutate(p = n/sum(n)) %>% 
      mutate(cat = "produccion") %>% 
      bind_rows(dmod_dist) %>% 
      hchart("column", hcaes(score_cut, p, group = cat)) %>% 
      hc_yAxis(title = list(text = "Distribución")) %>% 
      hc_xAxis(title = list(text = "Categoría")) %>% 
      hc_title(text = "Distribución Score Desarrollo/Producción")
    
  })
  
  output$chart_psi_vars <- renderHighchart({
    d <- d()
    
    mod <- mods[[input$mod]]
    
    mod_dist <- mod$data %>% 
      select_(.dots = names(mod$xlevels)) %>% 
      mutate_all(as.character) %>% 
      gather(variable, valor) %>% 
      count(variable, valor) %>% 
      group_by(variable) %>% 
      mutate(p_dev = sum(n), p_dev = n/p_dev) %>% 
      rename(n_dev = n)
    
    d %>% 
      select_(.dots = c("periodo", names(mod$xlevels))) %>% 
      mutate_at(vars(-periodo), as.character) %>% 
      gather(variable, valor, -periodo) %>% 
      count(periodo, variable, valor) %>% 
      group_by(periodo, variable) %>% 
      mutate(p = sum(n), p = n/p) %>% 
      left_join(mod_dist,  by = c("variable", "valor")) %>% 
      mutate(psi = (p - p_dev) * log(p/p_dev)) %>% 
      group_by(periodo, variable) %>%
      summarise(psi = round(100 * sum(psi, na.rm = TRUE), 2)) %>% 
      hchart("line", hcaes(periodo, psi, group = variable)) %>% 
      hc_yAxis(
        min = 0, max = 80,
        title = list(text = "PSI"),
        plotLines =  
          list(
            list(label = list(text = "Gran cambio en población"),
                 color = "red",
                 width = 2,
                 value = 25
            ),
            list(label = list(text = "Algún cambio menor"),
                 color = "yellow",
                 width = 2,
                 value = 10
            )
          )
      ) %>% 
      hc_xAxis(title = list(text = "Fecha")) %>% 
      hc_title(text = "Population Stability Index por Variables") %>% 
      hc_tooltip(shadred = TRUE, sort = TRUE)
    
  })
  
  output$chart_var_psi_cut <- renderHighchart({
    
    d <- d()
    
    dmod_dist <- mods[[input$mod]]$data %>% 
      rename(variable = !!sym(input$var)) %>% 
      count(variable) %>% 
      mutate(p = sum(n), p = n/p) %>% 
      mutate(cat = "desarrollo") 
    
    d %>% 
      rename(variable = !!sym(input$var)) %>% 
      count(variable) %>% 
      mutate(p = n/sum(n)) %>% 
      mutate(cat = "produccion") %>% 
      bind_rows(dmod_dist) %>% 
      hchart("column", hcaes(variable, p, group = cat)) %>% 
      hc_yAxis(title = list(text = "Distribución")) %>% 
      hc_xAxis(title = list(text = "Categoría")) %>% 
      hc_title(text = "Distribución Score Desarrollo/Producción") %>% 
      hc_tooltip(shared = TRUE)
    
  })
  
  output$chart_var_psi <- renderHighchart({
    
    d <- d()
    
    dmod_dist <- mods[[input$mod]]$data %>% 
      rename(variable = !!sym(input$var)) %>% 
      count(variable) %>% 
      mutate(p_dev = sum(n), p_dev = n/p_dev) %>% 
      rename(n_dev = n) 
    
    d %>% 
      rename(variable = !!sym(input$var)) %>% 
      count(periodo, variable) %>% 
      group_by(periodo) %>% 
      mutate(p = n/sum(n)) %>% 
      left_join(dmod_dist) %>% 
      mutate(psi = (p - p_dev) * log(p/p_dev)) %>% 
      group_by(periodo) %>%
      summarise(psi = round(100 * sum(psi, na.rm = TRUE), 2)) %>% 
      hchart("line", hcaes(periodo, psi), name = "PSI") %>% 
      hc_yAxis(
        min = 0, max = 80,
        title = list(text = "PSI"),
        plotLines =  
          list(
            list(label = list(text = "Gran cambio en población"),
                 color = "red",
                 width = 2,
                 value = 25
            ),
            list(label = list(text = "Algún cambio menor"),
                 color = "yellow",
                 width = 2,
                 value = 10
            )
          )
      ) %>% 
      hc_xAxis(title = list(text = "Fecha")) %>% 
      hc_title(text = "Population Stability Index")
    
  })
  
}
