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
        )
      )
    ),
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "css/custom.css")),
    tabItems(
      tabItem(
        tabName = "dashboard",
        h2("Dashboard tab content")
        ),
      tabItem(
        tabName = "modelo",
        box(title = "Backtest", highchartOutput("chart_backtest")),
        box(title = "Performance", highchartOutput("chart_perf")),
        box(title = "PSI", highchartOutput("chart_psi"))
        )
      )
    )
  )