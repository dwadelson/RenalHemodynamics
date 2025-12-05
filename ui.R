# ui.R

ui <- fluidPage(
  titlePanel("Arteriolar Diameter and Glomerular Hemodynamics"),
  
  # Make verbatimTextOutput wrap instead of overflowing horizontally
  tags$style(HTML("
    #pressureText, #flowText {
      white-space: pre-wrap;
    }
  ")),
  
  sidebarLayout(
    sidebarPanel(
      h4("Adjust hemodynamics"),
      sliderInput(
        inputId = "Pa",
        label   = "Renal arterial pressure (mmHg)",
        min     = 70,
        max     = 130,
        value   = 90,
        step    = 1
      ),
      sliderInput(
        inputId = "d_aff",
        label   = "Afferent arteriole diameter (µm)",
        min     = 10,
        max     = 30,
        value   = 20,
        step    = 1
      ),
      sliderInput(
        inputId = "d_eff",
        label   = "Efferent arteriole diameter (µm)",
        min     = 10,
        max     = 30,
        value   = 20,
        step    = 1
      ),
      hr(),
      helpText(
        "Smaller diameter = higher resistance (Poiseuille). ",
        "Changes in resistance and arterial pressure alter renal blood flow ",
        "and the pressure profile along the vasculature."
      )
    ),
    
    mainPanel(
      fluidRow(
        column(
          width = 12,
          plotOutput("diameterPlot", height = "400px")
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 6,
          h4("Pressures"),
          verbatimTextOutput("pressureText")
        ),
        column(
          width = 6,
          h4("Flow & Filtration (relative)"),
          verbatimTextOutput("flowText")
        )
      )
    )
  )
)