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

    # The multilevel functions take a list of networks (`nets`); p2/j2 take a single `net`.
    essBased <- "nets" %in% names(args)

    totalTicks <- 1000L
    samplesPerAdaptiveSequence <- 125L  # Sadapt in dyads
    maxAdaptiveSequences <- 1000L       # Nadapt in dyads::p2ML / j2ML / b2ML (loop runs i = 2..Nadapt)

    ticksSent <- 0L
    fraction <- 0                       # running maximum, so the bar never moves backwards

    reportFraction <- function(newFraction) {
        if (!is.finite(newFraction)) return(invisible(NULL))
        fraction <<- max(fraction, min(1, max(0, newFraction)))
        tick <- as.integer(floor(totalTicks * fraction))
        if (tick > ticksSent) {
            for (k in seq_len(tick - ticksSent)) jaspBase::progressbarTick()
            ticksSent <<- tick
        }
        invisible(NULL)
    }

    if (essBased) {
        essHistory <- numeric(0)

        # Projected share of the adaptive loop that is complete, given the min-ESS history
        projectEssProgress <- function(ess, target) {
            k <- length(ess)
            current <- ess[k]
            if (!is.finite(target) || target <= 0) return(0)
            if (current >= target) return(1)

            # Too few points (or unusable values) to extrapolate: be conservative, ESS is a noisy
            # and optimistic guide in the first sequences
            naive <- 0.5 * max(0, current) / target
            minPoints <- 8L
            if (k < minPoints || current <= 0) return(naive)

            recent <- seq.int(max(1L, k %/% 2L), k)
            if (any(ess[recent] <= 0)) return(naive)

            logIndex <- log(recent)
            logEss <- log(ess[recent])
            growthExponent <- sum((logIndex - mean(logIndex)) * (logEss - mean(logEss))) / sum((logIndex - mean(logIndex))^2)
            if (!is.finite(growthExponent)) growthExponent <- 1
            growthExponent <- min(1, max(0.3, growthExponent))

            projectedSequences <- k * (target / current)^(1 / growthExponent)
            k / projectedSequences
        }

        # callback2(neff_min_obs, neff_min): dyads has already replaced NaN by 0
        progressCallback <- function(neff_min_obs, neff_min) {
            essHistory[length(essHistory) + 1L] <<- if (is.na(neff_min_obs)) 0 else neff_min_obs

            projected <- projectEssProgress(essHistory, neff_min)
            # Hard upper limit on the loop length: it also ends after (maxAdaptiveSequences - 1) sequences
            sequenceLimit <- length(essHistory) / (maxAdaptiveSequences - 1L)

            reportFraction(max(projected, sequenceLimit))
        }
    } else {
        # p2/j2 need to know how many adaptive sequences there will be; dyads defaults to 100
        nAdapt <- args[["adapt"]]
        if (is.null(nAdapt) || !is.finite(nAdapt) || nAdapt < 0) nAdapt <- 100
        adaptWork <- samplesPerAdaptiveSequence * nAdapt

        # callback2(iteration, totalIterations)
        progressCallback <- function(iteration, total) {
            if (is.na(iteration) || is.na(total) || total < nAdapt || total <= 0) return(invisible(NULL))

            work <- if (iteration <= nAdapt) samplesPerAdaptiveSequence * iteration else adaptWork + (iteration - nAdapt)
            totalWork <- adaptWork + (total - nAdapt)
            reportFraction(work / totalWork)
        }
    }

    # Remember the original hook so it can be restored exactly
    originalCallback <- get("callback2", envir = getNamespace("dyads"))

    jaspBase::startProgressbar(expectedTicks = totalTicks, label = label)

    jaspBase::assignFunctionInPackage(fun = progressCallback, name = "callback2", package = "dyads")

    # Runs whether the analysis succeeds, fails or is cancelled
    on.exit({
        # Complete the bar (also covers early convergence and runs that end without a final callback)
        reportFraction(1)
        jaspBase::assignFunctionInPackage(fun = originalCallback, name = "callback2", package = "dyads")
    }, add = TRUE)

    # Execute the actual function (p2, j2, p2ML, j2ML, b2ML)
    return(do.call(dyadsFunction, args))
}