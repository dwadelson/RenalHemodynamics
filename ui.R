# ui.R

ui <- fluidPage(
  titlePanel("Arteriolar Diameter and Glomerular Hemodynamics"),
  
  # Styling for the text outputs:
  # - pressureText: wraps nicely
  # - flowText: no word wrap, horizontal scroll if long
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
  
  # Main 3-column layout: sliders | main hemo plot | RBF/GFR bars
  fluidRow(
    # Column 1: controls
    column(
      width = 3,
      h4("Adjust hemodynamics"),
      sliderInput(
        "Pa", "Renal arterial pressure (mmHg)",
        min   = 60, max = 180,
        value = baselinePa,
        step  = 5
      ),
      sliderInput(
        "d_aff", "Afferent arteriole diameter (µm)",
        min   = 10, max = 30,
        value = d_aff_baseline,
        step  = 1
      ),
      sliderInput(
        "d_eff", "Efferent arteriole diameter (µm)",
        min   = 10, max = 30,
        value = d_eff_baseline,
        step  = 1
      ),
      radioButtons(
        "res_mode",
        label   = "Arteriolar constraint",
        choices = c(
          "Free"                     = "free",
          "Lock Ra/Re ratio"         = "ratio",
          "Lock (Ra + Re) total"     = "sum"
        ),
        selected = "free",
        inline   = FALSE
      )
    ),
    
    # Column 2: combined pressures + diameter plot + pressure text
    column(
      width = 5,
      h4("Pressures and arteriolar diameters"),
      plotOutput("diameterPlot", height = "500px"),
    ),
    
    # Column 3: absolute RBF & GFR bar plot
    column(
      width = 4,
      h4("Renal blood flow and GFR"),
      plotOutput("flowHist", height = "500px")
    )
  ),
  
  # Full-width row: flow & filtration text (no wrapping)
  fluidRow(
    column(
      width = 12,
      h4("Assumptions"),
      verbatimTextOutput("flowText")
    )
  ),
  
  # Optional diagnostics at the bottom when DIAGNOSTICS == TRUE
  if (DIAGNOSTICS) {
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