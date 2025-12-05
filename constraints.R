### ---------------------------------------------------------
###  Constraint Logic for Afferent / Efferent Diameter Control
### Modes: "free", "ratio", "sum"
### ---------------------------------------------------------
### Assumes the following in the global env:
###   d_aff_baseline, d_eff_baseline, Ra0, Re0
### And in server(): reactiveValues sync_state with fields:
###   syncing, ratio, Rtot_target
### ---------------------------------------------------------

# Clamp helper (keeps within slider range)
clamp_diam <- function(x) {
  pmax(10, pmin(30, x))  # assuming sliders are 10–30 µm
}

install_constraint_observers <- function(input, session,
                                         d_aff_baseline, d_eff_baseline,
                                         Ra0, Re0,
                                         sync_state) {
  
  ### -----------------------------
  ### Mode change: freeze targets
  ### -----------------------------
  observeEvent(input$res_mode, {
    if (isTRUE(sync_state$syncing)) return(NULL)
    
    if (input$res_mode == "ratio") {
      
      # Freeze whatever the **current** diameter ratio is
      if (input$d_aff > 0) {
        sync_state$ratio <- input$d_eff / input$d_aff
      }
      
    } else if (input$res_mode == "sum") {
      
      # Freeze whatever the **current** Ra + Re is
      Ra <- Ra0 * (d_aff_baseline / input$d_aff)^4
      Re <- Re0 * (d_eff_baseline / input$d_eff)^4
      sync_state$Rtot_target <- Ra + Re
    }
    # In "free" mode we don't need to do anything
  })
  
  ### -----------------------------
  ### When afferent changes
  ### -----------------------------
  observeEvent(input$d_aff, {
    if (isTRUE(sync_state$syncing)) return(NULL)
    
    if (input$res_mode == "ratio") {
      
      # Use stored ratio if available, otherwise default to baseline ratio
      ratio <- sync_state$ratio
      if (is.null(ratio)) {
        ratio <- d_eff_baseline / d_aff_baseline
        sync_state$ratio <- ratio
      }
      
      target_eff <- clamp_diam(ratio * input$d_aff)
      
      sync_state$syncing <- TRUE
      updateSliderInput(session, "d_eff", value = round(target_eff))
      sync_state$syncing <- FALSE
      
    } else if (input$res_mode == "sum") {
      
      # Use stored total if available, otherwise default to baseline Ra+Re
      Rtot_target <- sync_state$Rtot_target
      if (is.null(Rtot_target)) {
        Rtot_target <- Ra0 + Re0
        sync_state$Rtot_target <- Rtot_target
      }
      
      Ra <- Ra0 * (d_aff_baseline / input$d_aff)^4
      Re_desired <- Rtot_target - Ra
      
      if (Re_desired <= 0) {
        target_eff <- 30  # needs "infinite" efferent -> max diameter
      } else {
        # Re_desired = Re0 * (d_eff_baseline / d_eff)^4
        # => d_eff = d_eff_baseline / (Re_desired / Re0)^(1/4)
        target_eff <- d_eff_baseline / (Re_desired / Re0)^(1/4)
      }
      
      target_eff <- clamp_diam(target_eff)
      
      sync_state$syncing <- TRUE
      updateSliderInput(session, "d_eff", value = round(target_eff))
      sync_state$syncing <- FALSE
    }
    
    # "free" mode: do nothing
  })
  
  ### -----------------------------
  ### When efferent changes
  ### -----------------------------
  observeEvent(input$d_eff, {
    if (isTRUE(sync_state$syncing)) return(NULL)
    
    if (input$res_mode == "ratio") {
      
      # Use stored ratio if available, otherwise default to baseline
      ratio <- sync_state$ratio
      if (is.null(ratio)) {
        ratio <- d_eff_baseline / d_aff_baseline
        sync_state$ratio <- ratio
      }
      if (ratio == 0) return(NULL)
      
      target_aff <- clamp_diam(input$d_eff / ratio)
      
      sync_state$syncing <- TRUE
      updateSliderInput(session, "d_aff", value = round(target_aff))
      sync_state$syncing <- FALSE
      
    } else if (input$res_mode == "sum") {
      
      # Use stored total if available, otherwise default to baseline
      Rtot_target <- sync_state$Rtot_target
      if (is.null(Rtot_target)) {
        Rtot_target <- Ra0 + Re0
        sync_state$Rtot_target <- Rtot_target
      }
      
      Re <- Re0 * (d_eff_baseline / input$d_eff)^4
      Ra_desired <- Rtot_target - Re
      
      if (Ra_desired <= 0) {
        target_aff <- 30
      } else {
        # Ra_desired = Ra0 * (d_aff_baseline / d_aff)^4
        # => d_aff = d_aff_baseline / (Ra_desired / Ra0)^(1/4)
        target_aff <- d_aff_baseline / (Ra_desired / Ra0)^(1/4)
      }
      
      target_aff <- clamp_diam(target_aff)
      
      sync_state$syncing <- TRUE
      updateSliderInput(session, "d_aff", value = round(target_aff))
      sync_state$syncing <- FALSE
    }
    
    # "free" mode: do nothing
  })
}