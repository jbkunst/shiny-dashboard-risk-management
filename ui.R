dashboardPagePlus(
  skin = "black-light",
  collapse_sidebar = TRUE,
  dashboardHeader(
    title = shiny::icon("university")
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        text = "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
        ),
      menuItem(
        text = "Modelo",
        tabName = "modelo",
        icon = icon("bullseye")
        ),
      menuItem(
        text = "Características",
        tabName = "caracteristicas",
        icon = icon("area-chart")
        )
      )
    ),
  dashboardBody(
    setShadow("box"),
    setShadow("small-box"),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")),
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          column(
            6, 
            selectizeInput(
              "prods",
              label = "Productos",
              width = "100%",
              multiple = TRUE,
              selected = data %>% distinct(producto) %>% pull() %>% first(), 
              choices = data %>% distinct(producto) %>% pull() %>% sort())
            ),
          column(
            6, 
            selectizeInput(
              "time",
              label = "Periodo de Tiempo",
              width = "100%",
              choices = c("Desde siempre" = Inf, "Ultimo año" = 12, "Último trimestre" = 3, "Ultimo Periodo" = 1)
              )
          )
        ),
        fluidRow(
          column(valueBoxOutput("vb_coloc", NULL), width = 3),
          column(valueBoxOutput("vb_venta", NULL), width = 3),
          column(valueBoxOutput("vb_provi", NULL), width = 3),
          column(valueBoxOutput("vb_iries", NULL), width = 3)  
          ),
        fluidRow(
          box(highchartOutput("chart_colocaiones_prod")),
          box(highchartOutput("chart_venta_riesgo"))
          )
        ),
      tabItem(
        tabName = "modelo",
        fluidRow(
          column(
            6, 
            selectizeInput(
              "mod",
              label = NULL,
              width = "100%",
              choices = data %>% select(matches("score\\d+$")) %>% names()
            )
          )
        ),
        fluidRow(
          box(highchartOutput("chart_auc")),
          box(highchartOutput("chart_ks")),
          box(highchartOutput("chart_backtest")),
          box(highchartOutput("chart_psi"))
          )
        )
      )
    )
  )