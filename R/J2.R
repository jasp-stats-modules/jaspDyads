#
# Copyright (C) 2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

##----------------------------------------------------------------
##                    Analyze J2 networks.                    --
##----------------------------------------------------------------
#' @param jaspResults {object} Object that will contain all results from the analysis and connect it to the output.
#' @param dataset {object} (optional) tabular data, if available for the analysis.
#' @param options {list} A named list of interface options selected by the user.
##----------------------------------------------------------------
J2 <- function(jaspResults, dataset = NULL, options, ...) {
    addLibPathLocation(jaspResults)
    previousCompute <- jaspResults[["previousCompute"]]
    if (!is.null(previousCompute) && options[["compute"]] == previousCompute$object) {
        return()
    }
    jaspResults[["previousCompute"]] <- createJaspState(options[["compute"]])
    dependVarsJ2 <- c("compute")

    # Check if the container already exists. Create it if it doesn't.
    if (is.null(jaspResults[["j2Container"]]) || jaspResults[["j2Container"]]$getError()) {
        j2Container <- createJaspContainer(title = "", position = 1)
        j2Container$dependOn(dependVarsJ2)
        jaspResults[["j2Container"]] <- j2Container
    } else {
        j2Container <- jaspResults[["j2Container"]]
    }

    # Initialize the J2 variables
    netMatrix <- NULL
    senderMatrix <- NULL
    receiverMatrix <- NULL
    densityVars <- list()
    reciprocityVars <- list()
    burnin <- NULL
    sample <- NULL
    adapt <- NULL
    seed <- NULL

    # 1. Parse Network
    if (options[["net"]] != "") {
        filepath <- options[["net"]]
        if (file.exists(filepath)) {
            sheetNames <- readxl::excel_sheets(filepath)
            # J2 analyzes a single network, so we take the first sheet
            firstSheet <- as.matrix(readxl::read_excel(path = filepath, sheet = sheetNames[1], col_names = TRUE))
            # Convert to matrix and sanitize
            netMatrix <- sanitizeMatrix(firstSheet, "Network")
        }
    }

    # 2. Parse Sender
    if (nchar(options[["sender"]]) != 0) {
        senderFiles <- unlist(strsplit(options[["sender"]], ";"))
        senderCovariatesList <- lapply(senderFiles, function(filepath) {
            if (file.exists(filepath)) {
                sheetNames <- readxl::excel_sheets(filepath)
                as.matrix(readxl::read_excel(path = filepath, sheet = sheetNames[1], col_names = TRUE))
            } else {
                NULL
            }
        })
        senderCovariatesList <- senderCovariatesList[!sapply(senderCovariatesList, is.null)]

        if (length(senderCovariatesList) > 0) {
            senderMatrix <- do.call(cbind, senderCovariatesList)
            senderMatrix <- sanitizeMatrix(senderMatrix, "Sender")
            colnames(senderMatrix) <- paste0("sender_cov_", seq_len(ncol(senderMatrix)))
        }
    }

    # 3. Parse Receiver
    if (nchar(options[["receiver"]]) != 0) {
        receiverFiles <- unlist(strsplit(options[["receiver"]], ";"))
        receiverCovariatesList <- lapply(receiverFiles, function(filepath) {
            if (file.exists(filepath)) {
                sheetNames <- readxl::excel_sheets(filepath)
                as.matrix(readxl::read_excel(path = filepath, sheet = sheetNames[1], col_names = TRUE))
            } else {
                NULL
            }
        })
        receiverCovariatesList <- receiverCovariatesList[!sapply(receiverCovariatesList, is.null)]

        if (length(receiverCovariatesList) > 0) {
            receiverMatrix <- do.call(cbind, receiverCovariatesList)
            receiverMatrix <- sanitizeMatrix(receiverMatrix, "Receiver")
            colnames(receiverMatrix) <- paste0("receiver_cov_", seq_len(ncol(receiverMatrix)))
        }
    }

    # 4. Parse Density
    if (options[["density"]] != "") {
        densityFiles <- unlist(strsplit(options[["density"]], ";"))
        validFiles <- densityFiles[file.exists(densityFiles)]

        for (i in seq_along(validFiles)) {
            filepath <- validFiles[i]
            sheetNames <- readxl::excel_sheets(filepath)
            mat <- as.matrix(readxl::read_excel(path = filepath, sheet = sheetNames[1], col_names = TRUE))

            # Sanitize
            mat <- sanitizeMatrix(mat, "Density")
            varName <- paste0("density_cov_", i)
            densityVars[[varName]] <- mat
        }
    }

    # 5. Parse Reciprocity
    if (options[["reciprocity"]] != "") {
        reciprocityFiles <- unlist(strsplit(options[["reciprocity"]], ";"))
        validFiles <- reciprocityFiles[file.exists(reciprocityFiles)]

        for (i in seq_along(validFiles)) {
            filepath <- validFiles[i]
            sheetNames <- readxl::excel_sheets(filepath)
            mat <- as.matrix(readxl::read_excel(path = filepath, sheet = sheetNames[1], col_names = TRUE))

            # Sanitize
            mat <- sanitizeMatrix(mat, "Reciprocity")

            if (nrow(mat) != ncol(mat) || !isSymmetric.matrix(unname(mat))) {
                j2Container[["error"]] <- createJaspHtml(text = gettextf("Reciprocity matrix in file '%s' is not symmetric.", basename(filepath)), class = "error")
                return()
            }

            varName <- paste0("reciprocity_cov_", i)
            reciprocityVars[[varName]] <- mat
        }
    }

    # 6. Parse MCMC parameters
    burnin <- options[["burnin"]]
    sample <- options[["sample"]]
    adapt <- options[["adapt"]]
    center <- options[["center"]]
    seed <- options[["seed"]]

    # Ensure netMatrix is available before proceeding
    if (is.null(netMatrix)) {
        j2Container[["info"]] <- createJaspHtml(text = gettext("Please press Compute to see the estimation results."))
        return()
    }

     # Prepare arguments for the J2 call
    j2Args <- list(
        net = netMatrix,
        burnin = burnin,
        sample = sample,
        adapt = adapt,
        center = center,
        seed = seed
    )

    # The formula interface of dyads::J2 requires variables to be in the environment.
    currentEnv <- environment()

    # Handle sender covariates
    if (!is.null(senderMatrix)) {
        list2env(as.data.frame(senderMatrix), envir = currentEnv)
        j2Args$sender <- as.formula(paste("~", paste(colnames(senderMatrix), collapse = " + ")))
    }

    # Handle receiver covariates
    if (!is.null(receiverMatrix)) {
        list2env(as.data.frame(receiverMatrix), envir = currentEnv)
        j2Args$receiver <- as.formula(paste("~", paste(colnames(receiverMatrix), collapse = " + ")))
    }

    # Handle density covariates
    if (length(densityVars) > 0) {
        list2env(densityVars, envir = currentEnv)
        j2Args$density <- as.formula(paste("~", paste(names(densityVars), collapse = " + ")))
    }

    # Handle reciprocity covariates
    if (length(reciprocityVars) > 0) {
        list2env(reciprocityVars, envir = currentEnv)
        j2Args$reciprocity <- as.formula(paste("~", paste(names(reciprocityVars), collapse = " + ")))
    }

    # Run the J2 model with all specified arguments
    resultsJ2 <- tryCatch({
        startProgressbar(length(j2Args), gettext("Estimating network parameters for J2"))
        progressbarTick()
        do.call(dyads::j2, j2Args)
    }, error = function(e) {
        j2Container[["error"]] <- createJaspHtml(text = gettextf("An error occurred during model estimation: %s", e$message), class = "error")
        return(NULL)
    })

    # If the model ran successfully, display the summary
    if (!is.null(resultsJ2)) {
        # resultsJ2 <- summary(resultsJ2)
        resultsJ2 <- cbind(Parameter=rownames(resultsJ2), as.data.frame(resultsJ2))
        # Create table for the J2 results
        tableJ2 <- createJaspTable(title = gettextf("J2 Results"))
        tableJ2$dependOn(dependVarsJ2)
        tableJ2$setData(resultsJ2)
        tableJ2$position <- 1
        j2Container[["tableJ2"]] <- tableJ2
    }
}