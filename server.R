# server.R

server <- function(input, output, session) {
  
  sync_state <- reactiveValues(syncing = FALSE)
  
  install_constraint_observers(
    input, session,
    d_aff_baseline = d_aff_baseline,
    d_eff_baseline = d_eff_baseline,
    Ra0 = Ra0, Re0 = Re0,
    sync_state = sync_state
  )
  source("diagnostics.R") 
  #if DIAGNOSTICS are on then they'll be reported when running on the server
  
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
  
  # Relative changes vs baseline
  rel_changes <- reactive({
    compute_relative_changes(hemo())
  })
  
  # Absolute RBF and GFR (mL/min), scaled to physiological baselines
  abs_values <- reactive({
    compute_absolute_rbf_gfr(hemo())
  })
  
  # Resistances of afferent and efferent arterioles
  resistances <- reactive({
    h <- hemo()
    data.frame(
      vessel = factor(
        c("Afferent arteriole", "Efferent arteriole"),
        levels = c("Afferent arteriole", "Efferent arteriole")
      ),
      Resistance = c(h$Ra, h$Re)
    )
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
        paste0("Glomerular\ncapillaries\n(", round(P_gc, 1), " mmHg)"),
        paste0("Peritubular\ncapillaries\n(", round(P_pc, 1), " mmHg)"),
        paste0("Renal vein\n(",             round(local_Pv, 1), " mmHg)")
      )
    ) |>
      mutate(
        # label offset; cap at 138 so labels aren't clipped at top
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
      label  = c("Aff", "Eff")
    )
    
    # Define names for each vascular segment, in the same order as seg_df rows
    vessel_labels <- c(
      "Renal artery",
      "Glomerular capillaries",
      "Peritubular capillaries",
      "Renal vein"
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
        size = 4.5
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
        size = 5,
        fontface = "bold"
      ) +
      # Diameter text below each circle (e.g., "18 µm")
      geom_text(
        data = circle_df,
        aes(
          x = x,
          y = y - 7,
          label = paste0(round(diam, 1), " µm")
        ),
        size = 5,
        fontface = "bold"
      ) +
      scale_size_continuous(
        range  = c(8, 24),
        limits = c(10, 30),
        guide  = "none"
      ) +
      scale_y_continuous(
        limits = c(0, input$pressure_ymax),
        breaks = seq(0, input$pressure_ymax, by = 10),
        name   = "Pressure (mmHg)"
      ) +
      labs(
        x = "Renal vascular segment",
        y = "Pressure (mmHg)"
      ) +
      coord_cartesian(xlim = c(-0.05, 1.45)) +
      theme_minimal(base_size = 16) +
      theme(
        axis.title.x  = element_text(size = 16, margin = margin(t = 10)),
        axis.text.x   = element_blank(),     # hide labels
        axis.ticks.x  = element_blank(),     # hide ticks
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.margin = margin(5.5, 20, 5.5, 5.5)
      )
  }) #diameterPlot
  
  # Bar plot showing absolute RBF and GFR (mL/min)
  output$flowHist <- renderPlot({
    req(input$show_flow)   # only draw if checkbox is TRUE
    
    df <- abs_values()
    
    ymax <- max(1500, max(df$Value) * 1.1)  # headroom
    
    ggplot(df, aes(x = Variable, y = Value, fill = Variable)) +
      geom_col(width = 0.6) +
      geom_text(
        aes(label = sprintf("%.0f\nmL/min", Value)),
        vjust = -0.5,
        size = 5
      ) +
      scale_fill_manual(values = c(
        "Renal blood flow" = "red",
        "GFR"              = "yellow"
      )) +
      scale_y_continuous(
        limits = c(0, ymax),
        name   = "Flow / filtration (mL/min)"
      ) +
      labs(x = NULL) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",   # remove legend (optional)
        axis.text.x = element_text(vjust = 0.5, angle = 75),
        plot.margin = margin(10, 10, 10, 10)
      )
  }) #flowHist
  

  # Bar plot showing Ra and Re (same color scheme as arteriole circles)
  output$resHist <- renderPlot({
    df <- resistances()
    
    ymax <- input$res_ymax
    
    ggplot(df, aes(x = vessel, y = Resistance, fill = vessel)) +
      geom_col(width = 0.6) +
      geom_text(
        aes(label = sprintf("%.1f", Resistance)),
        vjust = -0.5,
        size  = 5
      ) +
      scale_y_continuous(
        limits = c(0, ymax),
        name   = "Hybrid Resistance Units\n(mmHg * min/ml)"
      ) +
      labs(x = NULL) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        axis.text.x     = element_text(vjust = 0.5, angle = 75),
        plot.margin     = margin(10, 10, 10, 10)
      )
  })
  
  
  output$flowText <- renderPrint({
    h  <- hemo()
    bl <- base()
    
    rel_Q   <- h$Q   / bl$Q0
    rel_NFP <- h$NFP / bl$NFP0
    
    cat(
      "Net Filtration Pressure (NFP) is a simplified proxy for filtration:\n",
      "  NFP ≈ P_gc - P_Bowman's space - π_gc\n",
      "  (Bowman's space and oncotic pressures held constant in this model.)\n"
    )
  })
  
}