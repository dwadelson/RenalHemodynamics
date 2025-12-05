# model.R

# Baseline constants for renal hemodynamics
Pv  <- 6          # renal venous pressure (mmHg)
baselinePa <- 90  # for relative calculations

# Baseline diameters (µm) – this is the "normal" state
d_aff_baseline <- 20  # afferent arteriole diameter
d_eff_baseline <- 18  # efferent arteriole diameter

# Baseline resistances at those diameters (mmHg · L^-1 · min)
# From your source: Ra/Re ≈ 0.5–0.6, RBF ≈ 1.1–1.2 L/min
Ra0 <- 27.3       # afferent resistance at d_aff_baseline
Re0 <- 51.0       # efferent resistance at d_eff_baseline

Rp   <- 1.0       # post-glomerular resistance (lumped peritubular/venous)
P_bs <- 15        # Bowman's space pressure (mmHg)
Pi_gc <- 25       # glomerular oncotic pressure (mmHg)

RBF0_phys <- 1200  # mL/min (renal blood flow)
GFR0_phys <- 120   # mL/min (glomerular filtration rate)



# Compute hemodynamics given Pa, d_aff, d_eff
compute_hemo <- function(Pa, d_aff, d_eff) {
  
  # Poiseuille scaling: R ∝ 1 / d^4
  # Ra0 and Re0 are the resistances at the baseline diameters
  Ra <- Ra0 * (d_aff_baseline / d_aff)^4
  Re <- Re0 * (d_eff_baseline / d_eff)^4
  
  R_total <- Ra + Re + Rp
  Q       <- (Pa - Pv) / R_total   # renal blood flow proxy
  
  P_gc <- Pa - Q * Ra             # glomerular capillary pressure
  P_pc <- Pv + Q * Rp             # peritubular capillary pressure
  
  NFP  <- P_gc - P_bs - Pi_gc
  
  list(
    Pa    = Pa,
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
  h0 <- compute_hemo(baselinePa, d_aff_baseline, d_eff_baseline)
  list(
    Q0    = h0$Q,
    P_gc0 = h0$P_gc,
    P_pc0 = h0$P_pc,
    NFP0  = h0$NFP
  )
}

baseline_vals <- baseline_hemo()

## ---- NEW: baseline values + helper for % changes ----

baseline_vals <- baseline_hemo()

# Helper: map model Q and NFP onto physiological RBF and GFR
compute_absolute_rbf_gfr <- function(hemo,
                                     baseline = baseline_vals,
                                     RBF0 = RBF0_phys,
                                     GFR0 = GFR0_phys) {
  # Scale flow so that Q = baseline$Q0 -> RBF0_phys mL/min
  RBF <- hemo$Q * (RBF0 / baseline$Q0)
  
  # Very simple GFR ~ Kf * NFP, clipped at 0, scaled so that
  # NFP = baseline$NFP0 -> GFR0_phys mL/min
  if (baseline$NFP0 > 0) {
    Kf <- GFR0 / baseline$NFP0
  } else {
    Kf <- 0
  }
  GFR <- Kf * max(hemo$NFP, 0)
  
  data.frame(
    Variable = factor(c("Renal blood flow", "GFR"),
                      levels = c("Renal blood flow", "GFR")),
    Value    = c(RBF, GFR)
  )
}

# Returns a data frame with % change in RBF and GFR relative to baseline
compute_relative_changes <- function(hemo, baseline = baseline_vals, Kf = 1) {
  # RBF proxy is Q (flow)
  RBF  <- hemo$Q
  RBF0 <- baseline$Q0
  
  # Very simple GFR ~ Kf * NFP, clipped at 0
  GFR  <- Kf * max(hemo$NFP,  0)
  GFR0 <- Kf * max(baseline$NFP0, 0)
  
  rbf_change <- 100 * (RBF - RBF0) / RBF0
  gfr_change <- 100 * (GFR - GFR0) / GFR0
  
  data.frame(
    Variable = factor(c("Renal blood flow", "GFR"),
                      levels = c("Renal blood flow", "GFR")),
    Change   = c(rbf_change, gfr_change)
  )
}