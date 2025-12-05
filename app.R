# app.R

library(shiny)
library(ggplot2)
library(dplyr)   # for pipes etc.

# Source the hemodynamic model (defines compute_hemo, baseline_hemo, Pv, baselinePa, dref)
source("model.R")

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

server <- function(input, output, session) {
  
  # Reactives wrapping the model functions from model.R ----------------------
  
  hemo <- reactive({
    compute_hemo(
      Pa   = input$Pa,
      d_aff = input$d_aff,
      d_eff = input$d_eff
    )
  })
  
  base <- reactive({
    baseline_hemo()
  })
  
  # --- Layout constants for x positions -------------------------------------
  # Short segments left-to-right: artery -> glomerulus -> peritubular -> vein
  x_RA_start <- 0.0; x_RA_end <- 0.2
  x_GC_start <- 0.4; x_GC_end <- 0.6
  x_PC_start <- 0.8; x_PC_end <- 1.0
  x_RV_start <- 1.2; x_RV_end <- 1.4
  
  x_RA_mid <- (x_RA_start + x_RA_end) / 2
  x_GC_mid <- (x_GC_start + x_GC_end) / 2
  x_PC_mid <- (x_PC_start + x_PC_end) / 2
  x_RV_mid <- (x_RV_start + x_RV_end) / 2
  
  # --- Combined plot: pressures + arteriole diameters -----------------------
  
  output$diameterPlot <- renderPlot({
    h <- hemo()
    
    Pa   <- h$Pa
    P_gc <- h$P_gc
    P_pc <- h$P_pc
    
    # Pv comes from model.R as a global constant
    # (could also hard-code 6 here if you prefer)
    local_Pv <- Pv
    
    # Segment data with labels (multiline)
    seg_df <- tibble::tibble(
      segment = factor(
        c("Renal artery", "Glomerular capillary", "Peritubular capillary", "Renal vein"),
        levels = c("Renal artery", "Glomerular capillary",
                   "Peritubular capillary", "Renal vein")
      ),
      x_start = c(x_RA_start, x_GC_start, x_PC_start, x_RV_start),
      x_end   = c(x_RA_end,   x_GC_end,   x_PC_end,   x_RV_end),
      x_mid   = c(x_RA_mid,   x_GC_mid,   x_PC_mid,   x_RV_mid),
      P       = c(Pa,         P_gc,       P_pc,       local_Pv),
      label   = c(
        paste0("Renal artery\n(",           round(Pa, 1), " mmHg)"),
        paste0("Glomerular capillaries\n(", round(P_gc, 1), " mmHg)"),
        paste0("Peritubular capillaries\n(", round(P_pc, 1), " mmHg)"),
        paste0("Renal vein\n(",             round(local_Pv, 1), " mmHg)")
      )
    ) |>
      # label offset; cap at 128 so labels aren't clipped at top
      
      mutate(
        # Put every label above its line, with a 12-mmHg gap,
        # and cap at 128 so it doesn't hit the top.
        P_label = pmin(P + 10, 138)
      )
    
    # Circle positions:
    #   - Afferent: below the renal artery segment
    #   - Efferent: below the glomerular capillary segment
    aff_x <- x_RA_mid
    eff_x <- x_GC_mid
    aff_y <- Pa  - 12      # 12 mmHg below renal artery line
    eff_y <- P_gc - 12     # 12 mmHg below glomerular line
    
    circle_df <- tibble::tibble(
      vessel = factor(c("Afferent arteriole", "Efferent arteriole"),
                      levels = c("Afferent arteriole", "Efferent arteriole")),
      x      = c(aff_x, eff_x),
      y      = c(aff_y, eff_y),
      diam   = c(h$d_aff, h$d_eff),
      label  = c("Afferent", "Efferent")
    )
    
    ggplot() +
      # Horizontal pressure segments
      geom_segment(
        data = seg_df,
        aes(x = x_start, xend = x_end, y = P, yend = P),
        linewidth = 2
      ) +
      # Pressure labels
      geom_text(
        data = seg_df,
        aes(x = x_mid, y = P_label, label = label),
        size = 4
      ) +
      # Arteriole circles
      geom_point(
        data = circle_df,
        aes(x = x, y = y, size = diam, fill = vessel),
        shape = 21, alpha = 0.7, color = "black"
      ) +
      # Labels on the circles
      geom_text(
        data = circle_df,
        aes(x = x, y = y, label = label),
        size = 4.5,
        fontface = "bold"
      ) +
      scale_size_continuous(
        range  = c(8, 24),
        limits = c(10, 30),   # fixed to slider range so sizes aren't coupled
        guide  = "none"
      ) +
      scale_y_continuous(
        limits = c(0, 140),
        breaks = seq(0, 140, by = 10),
        name   = "Pressure (mmHg)"
      ) +
      coord_cartesian(xlim = c(-0.05, 1.45)) +
      labs(
        title = "Pressure profile and arteriolar diameters",
        x = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x  = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.margin = margin(5.5, 20, 5.5, 5.5)
      )
  })
  
  # --- Text outputs ---------------------------------------------------------
  
  output$pressureText <- renderPrint({
    h <- hemo()
    
    cat(
      sprintf("Renal mean arterial pressure:       %5.1f mmHg\n", h$Pa),
      sprintf("Glomerular capillary pressure: %5.1f mmHg\n", h$P_gc),
      sprintf("Peritubular capillary pressure:%5.1f mmHg\n", h$P_pc),
      sprintf("Renal venous pressure:         %5.1f mmHg\n", Pv)
    )
  })
  
  output$flowText <- renderPrint({
    h  <- hemo()
    bl <- base()
    
    rel_Q   <- h$Q   / bl$Q0
    rel_NFP <- h$NFP / bl$NFP0
    
    cat(
      sprintf("Relative renal blood flow (Q):   %5.2f (baseline = 1.00 at Pa = %g mmHg, d_aff = d_eff = %g µm)\n",
              rel_Q,  baselinePa, dref),
      sprintf("Net filtration pressure (NFP*):  %5.2f (same baseline)\n", rel_NFP),
      "\n",
      "*NFP is a simplified proxy for filtration:\n",
      "  NFP ≈ P_gc - P_Bowman's space - π_gc\n",
      "  (Bowman's space and oncotic pressures held constant in this model.)\n"
    )
  })
  
}

shinyApp(ui, server)