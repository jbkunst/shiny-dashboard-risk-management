# Define server logic required to draw a histogram
function(input, output) {
  
  data <- get_data()
  data <- data %>% 
    mutate(score = round(1000 * predict(mod, newdata = data, type = "response")))
  
  glimpse(data)
  
  output$perf_plot <- renderHighchart({
    
    data %>% 
      group_by(month) %>% 
      summarise(aucroc = ModelMetrics::auc(bad, score)) %>% 
      hchart("line", hcaes(month, aucroc))
    
    
  })
}
