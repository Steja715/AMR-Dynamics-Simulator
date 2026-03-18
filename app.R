# 0. REQUIRED LIBRARIES (Strict Case Sensitivity for Linux)
library(deSolve)  # Note the capital 'S'
library(shiny)
library(plotly)
library(bslib)
library(tidyverse)

# 1. THE BRAIN: Mathematical Model
amr_sir_model <- function(time, state, parameters){
  with(as.list(c(state, parameters)), {
    # dS: Susceptible, dIs: Sensitive Infected, dIr: Resistant Infected, dR: Recovered
    dS <- -beta * S * (Is + Ir)
    dIs <- beta * S * Is - (gamma + alpha) * Is
    dIr <- beta * S * Ir + alpha * Is - gamma * Ir
    dR <- gamma * (Is + Ir)
    return(list(c(dS, dIs, dIr, dR)))
  })
}

# 2. THE FACE: Professional UI Design
ui <- page_navbar(
  title = "AMR Dynamics Simulator",
  theme = bs_theme(
    version = 5, 
    bootswatch = "lux", 
    primary = "#1a1a1a"
  ),
  
  header = tags$style(HTML("
    body { font-family: 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; }
    .bslib-sidebar-layout { background-color: #f8f9fa; }
    .card { border-radius: 10px; border: none; box-shadow: 0 4px 6px rgba(0,0,0,0.05); }
    .value-box { border-radius: 10px !important; }
  ")),
  
  nav_panel(
    title = "Baddi Case Study Analysis",
    layout_sidebar(
      sidebar = sidebar(
        width = 380,
        title = "Parameters & Indicators",
        
        card(
          card_header("Social Awareness (Demand Side)"),
          sliderInput("literacy", "AMR Literacy Score (0-10):",
                      min = 0, max = 10, value = 4.8, step = 0.1),
          helpText("Baddi Baseline (Ophelia): 4.8")
        ),
        
        card(
          card_header("Pharmacy Practice (Supply Side)"),
          sliderInput("non_presc", "Non-Prescription Sales (%):",
                      min = 0, max = 100, value = 73),
          sliderInput("watch_use", "Watch-Category Usage (%):",
                      min = 0, max = 100, value = 65)
        ),
        
        actionButton("reset", "Reset to Thesis Baseline", 
                     class = "btn-outline-dark w-100 mb-3"),
        
        uiOutput("dynamic_risk_box")
      ),
      
      card(
        full_screen = TRUE,
        card_header(
          div(class = "d-flex justify-content-between align-items-center",
              span("Pathogen Modeling: Baddi hub Projection", style="font-weight:600;"),
              span("LIVE SIMULATION", class = "badge bg-danger")
          )
        ),
        card_body(
          plotlyOutput("sir_plot", height = "550px")
        ),
        card_footer(
          "Data Source: Muramalla (2026) - Mixed-method cross-sectional study in Baddi, India."
        )
      )
    )
  )
)

# 3. THE NERVOUS SYSTEM: Server Logic
server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    updateSliderInput(session, "literacy", value = 4.8)
    updateSliderInput(session, "non_presc", value = 73)
    updateSliderInput(session, "watch_use", value = 65)
  })
  
  projections <- reactive({
    req(input$literacy, input$non_presc, input$watch_use)
    
    # N = 10,000 population
    init <- c(S = 9999, Is = 1, Ir = 0, R = 0)
    times <- seq(0, 180, by = 1)
    
    # Parameter Scaling based on Literacy and Practice
    beta_val <- max(0, 0.45 * (1 - (input$literacy / 9.5)))
    alpha_val <- (input$non_presc / 100) * (input$watch_use / 100) * 0.35 * (1 - (input$literacy / 12))
    
    params <- c(beta = beta_val, alpha = alpha_val, gamma = 0.1)
    
    # Run ODE solver
    out <- deSolve::ode(y = init, times = times, func = amr_sir_model, parms = params)
    as.data.frame(out)
  })
  
  output$dynamic_risk_box <- renderUI({
    df <- projections()
    req(df) 
    
    peak_val <- (max(df$Ir, na.rm = TRUE) / 10000) * 100
    
    # Risk Logic
    if (peak_val < 5) {
      box_theme <- "success"
      status_text <- if(peak_val < 0.01) "Contained" else paste0(round(peak_val, 2), "%")
    } else if (peak_val < 20) {
      box_theme <- "warning"
      status_text <- paste0(round(peak_val, 1), "%")
    } else {
      box_theme <- "danger"
      status_text <- paste0(round(peak_val, 1), "%")
    }
    
    value_box(
      title = "Resistance Risk Peak",
      value = status_text,
      theme = box_theme
    )
  })
  
  output$sir_plot <- renderPlotly({
    df <- projections()
    req(df) 
    
    p <- ggplot(df, aes(x = time)) +
      geom_area(aes(y = Is, fill = "Sensitive Strain"), alpha = 0.15) +
      geom_area(aes(y = Ir, fill = "Resistant Strain"), alpha = 0.15) +
      geom_line(aes(y = Is, color = "Sensitive Strain"), linewidth = 1.1) +
      geom_line(aes(y = Ir, color = "Resistant Strain"), linewidth = 1.1, linetype = "dashed") +
      scale_color_manual(values = c("Sensitive Strain" = "#2c3e50", "Resistant Strain" = "#d9534f")) +
      scale_fill_manual(values = c("Sensitive Strain" = "#2c3e50", "Resistant Strain" = "#d9534f")) +
      theme_minimal() +
      labs(x = "Days Post-Infection", y = "Estimated Cases", color = "Pathogen Type", fill = "Pathogen Type") +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>% 
      layout(hovermode = "x unified", legend = list(orientation = "h", y = -0.2))
  })
}

# 4. RUN APP
shinyApp(ui = ui, server = server)
