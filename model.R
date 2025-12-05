# model.R

# Baseline constants for renal hemodynamics
Pv   <- 6         # renal venous pressure
dref <- 20        # reference diameter
Ra0  <- 3.9       # baseline afferent resistance
Re0  <- 4.4       # baseline efferent resistance
Rp   <- 1.0       # post-glomerular resistance
P_bs <- 15        # Bowman's space pressure
Pi_gc <- 25       # glomerular oncotic pressure

baselinePa <- 90  # for relative calculations

# Compute hemodynamics given Pa, d_aff, d_eff
compute_hemo <- function(Pa, d_aff, d_eff) {
  
  Ra <- Ra0 * (dref / d_aff)^4
  Re <- Re0 * (dref / d_eff)^4
  
  R_total <- Ra + Re + Rp
  Q       <- (Pa - Pv) / R_total
  
  P_gc <- Pa - Q * Ra
  P_pc <- Pv + Q * Rp
  
  NFP  <- P_gc - P_bs - Pi_gc
  
  list(
    Pa   = Pa,
    d_aff = d_aff,
    d_eff = d_eff,
    Ra    = Ra,
    Re    = Re,
    Q     = Q,
    P_gc  = P_gc,
    P_pc  = P_pc,
    NFP   = NFP
  )
}

# Baseline used for relative RBF & NFP calculations
baseline_hemo <- function() {
  Ra <- Ra0
  Re <- Re0
  R_total0 <- Ra + Re + Rp
  Q0       <- (baselinePa - Pv) / R_total0
  P_gc0    <- baselinePa - Q0 * Ra
  P_pc0    <- Pv + Q0 * Rp
  NFP0     <- P_gc0 - P_bs - Pi_gc
  
  list(Q0 = Q0, P_gc0 = P_gc0, P_pc0 = P_pc0, NFP0 = NFP0)
}