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