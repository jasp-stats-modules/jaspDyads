# Helper function to get the libPaths location for default data
addLibPathLocation <- function(jaspResults) {
  libPathDir <- .libPaths()
  jaspResults[["libPathDir"]] <- createJaspQmlSource("libPathDir", libPathDir)
  return()
}

# Helper function to sanitize matrices (Numeric conversion + NA handling)
sanitizeMatrix <- function(mat, name="Data") {
    if (is.null(mat)) return(NULL)
    # Ensure matrix structure
    mat <- as.matrix(mat)
    # Ensure numeric (Excel sometimes reads as character)
    if (!is.numeric(mat)) {
        storage.mode(mat) <- "numeric"
    }
    # Replace NAs with 0 to prevent J2 MCMC crash
    if (any(is.na(mat))) {
        mat[is.na(mat)] <- 0
    }
    return(mat)
}

# Generic wrapper to run dyads functions with a JASP progress bar
runDyadsWithProgress <- function(dyadsFunction, args, options, label) {
    # 1. Determine target ESS (Effective Sample Size)
    # Most functions in dyads use 'adapt' as the convergence target.
    target_ess <- options[["adapt"]]
    if (is.null(target_ess)) target_ess <- 300 # Default fallback based on dyads source

    # 2. State tracking
    last_reported_progress <- 0

    # 3. Define the Dynamic Callback
    # This matches the signature callback2(neff_min_obs, neff_min) used in dyads
    jasp_ess_callback <- function(current_ess, target) {
        current_val <- floor(max(0, current_ess))

        if (current_val > last_reported_progress) {
            ticks_to_send <- current_val - last_reported_progress

            # Send the delta to JASP
            for (t in seq_len(ticks_to_send)) {
                jaspBase::progressbarTick()
            }

            # Use super-assignment to update variable in .runDyadsWithProgress scope
            last_reported_progress <<- current_val
        }
    }

    # 4. Inject the JASP callback into the dyads package
    jaspBase::assignFunctionInPackage(
        fun     = jasp_ess_callback,
        name    = "callback2",
        package = "dyads"
    )

    # 5. Setup Cleanup on Exit
    # This runs regardless of whether the function succeeds, fails, or is cancelled
    on.exit({
        # Fill the bar to 100% to avoid it hanging if converged early
        remaining <- target_ess - last_reported_progress
        if (remaining > 0) {
            for (k in seq_len(remaining)) jaspBase::progressbarTick()
        }

        # Reset dyads::callback2 to its original dummy state
        jaspBase::assignFunctionInPackage(
            fun     = function(n1, n2) {},
            name    = "callback2",
            package = "dyads"
        )
    })

    # 6. Start the JASP Progress Bar
    jaspBase::startProgressbar(expectedTicks = target_ess, label = label)

    # 7. Execute the actual function (p2ML, b2ML, j2ML, etc.)
    return(do.call(dyadsFunction, args))
}