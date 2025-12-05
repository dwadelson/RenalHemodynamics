### diagnostics.R  
### This file should be sourced from inside server(), NOT global.R.

if (DIAGNOSTICS) {
  
  diag_text <- reactiveVal("Diagnostics not yet collected.")
  
  observeEvent(TRUE, {
    txt <- capture.output({
      cat("=== Shiny diagnostics ===\n")
      cat("Time: ", as.character(Sys.time()), "\n\n", sep = "")
      
      cat("R.home():\n"); print(R.home())
      cat("R.version:\n"); print(R.version)
      cat("\n.libPaths():\n"); print(.libPaths())
      
      cat("\nSystem shiny path:\n"); print(system.file(package = "shiny"))
      cat("\nSys.which('R'):\n"); print(Sys.which("R"))
      
      cat("\nLoaded packages:\n")
      print(sessionInfo())
      
      cat("\nCapabilities (png / cairo / X11):\n")
      print(capabilities()[c("png", "cairo", "X11")])
    })
    
    diag_text(paste(txt, collapse = "\n"))
    
    # Print once to stderr() so it's visible in Shiny logs
    if (!exists(".diag_printed", envir = .GlobalEnv)) {
      assign(".diag_printed", TRUE, envir = .GlobalEnv)
      cat(paste(txt, collapse = "\n"), "\n", file = stderr())
    }
    
  }, once = TRUE)
  
  output$diagnostics <- renderText({
    diag_text()
  })
}