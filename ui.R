# ui.R

library(shiny)

ui <- fluidPage(
  titlePanel("Arteriolar Diameter and Glomerular Hemodynamics"),
  
  # Styling for text outputs
  tags$style(HTML("
    #pressureText {
      white-space: pre-wrap;
    }
    #flowText {
      white-space: pre;
      overflow-x: auto;
      width: 100%;
      display: block;
    }
  ")),
  
  # Main layout row: controls | pressures/diameters | flows/resistances
  fluidRow(
    # -------- LEFT COLUMN: CONTROLS --------
    column(
      width = 2,
      h4("Adjust hemodynamics"),
      
      sliderInput(
        "Pa", "Renal arterial pressure (mmHg)",
        min   = 60,
        max   = 180,
        value = baselinePa,
        step  = 5
      ),
      sliderInput(
        "d_aff", "Afferent arteriole diameter (µm)",
        min   = 10,
        max   = 30,
        value = d_aff_baseline,
        step  = 1
      ),
      sliderInput(
        "d_eff", "Efferent arteriole diameter (µm)",
        min   = 10,
        max   = 30,
        value = d_eff_baseline,
        step  = 1
      ),
      
      radioButtons(
        "res_mode",
        label   = "Arteriolar constraint",
        choices = c(
          "Free"                 = "free",
          "Lock Ra/Re ratio"     = "ratio",
          "Lock (Ra + Re) total" = "sum"
        ),
        selected = "free"
      )
    ),
    
    # -------- MIDDLE COLUMN: PRESSURES & DIAMETERS --------
    column(
      width = 5,
      h4("Pressures and arteriolar diameters"),
      plotOutput("diameterPlot", height = "500px"),
      h5("Key pressures (mmHg)"),
      verbatimTextOutput("pressureText")
    ),
    
    # -------- RIGHT COLUMN: FLOWS & RESISTANCES --------
    column(
      width = 5,
      h4("Flows and arteriolar resistances"),
      fluidRow(
        column(
          width = 6,
          h5("RBF and GFR"),
          plotOutput("flowHist", height = "500px")
        ),
        column(
          width = 6,
          h5("Ra and Re"),
          plotOutput("resHist", height = "500px")
        )
      )
    )
  ),
  
  # Full-width row: flow & filtration text (no wrapping)
  fluidRow(
    column(
      width = 12,
      h4("Flow & filtration (numerical values)"),
      verbatimTextOutput("flowText")
    )
  ),
  
  # Optional diagnostics at the bottom when DIAGNOSTICS == TRUE
  if (exists("DIAGNOSTICS") && isTRUE(DIAGNOSTICS)) {
    fluidRow(
      column(
        width = 12,
        hr(),
        h4("Diagnostics (server environment)"),
        verbatimTextOutput("diagnostics")
      )
    )
  }
)