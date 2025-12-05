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
      ),
      checkboxInput(
        "show_flow",
        label = "Show RBF & GFR plot",
        value = TRUE
      )
    ),
    
    # -------- MIDDLE COLUMN: PRESSURES & RESISTANCES --------
    column(
      width = 7,
     # h4("Pressures and arteriolar diameters"),
      fluidRow(
        column(
          width = 8,
          h5("Pressure profile and diameters"),
          plotOutput("diameterPlot", height = "700px")
        ),
        column(
          width = 4,
          h5("Arteriolar resistances"),
          plotOutput("resHist", height = "700px")
        )
      ),
      # (optional) text summary under the plots:
      # h5("Key pressures (mmHg)"),
      # verbatimTextOutput("pressureText")
    ),
    
    # -------- RIGHT COLUMN: FLOWS --------
    column(
      width = 3,
      #h4("Flows"),
      h5("RBF and GFR"),
      plotOutput("flowHist", height = "700px")
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
  
  tags$hr(),
  div(
    style = "text-align:center; font-size:12px; color:#555; margin-top:10px;",
    HTML('Renal hemodynamics interactive simulator v. 1.0 - 2025-12-05 <br>© 2025 David W. Adelson, Ph.D. — 
        This work is licensed under a 
        <a href="https://creativecommons.org/licenses/by-nc-sa/4.0/" target="_blank">
        Creative Commons Attribution–NonCommercial–ShareAlike 4.0 International License</a>.')
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