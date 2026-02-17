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
##                    Analyze P2 networks.                    --
##----------------------------------------------------------------
#' @param jaspResults {object} Object that will contain all results from the analysis and connect it to the output.
#' @param dataset {object} (optional) tabular data, if available for the analysis.
#' @param options {list} A named list of interface options selected by the user.
##----------------------------------------------------------------
J2ML <- function(jaspResults, dataset = NULL, options, ...) {
    addLibPathLocation(jaspResults)
    previousCompute <- jaspResults[["previousCompute"]]
    if (!is.null(previousCompute) && options[["compute"]] == previousCompute$object) {
        return()
    }
    jaspResults[["previousCompute"]] <- createJaspState(options[["compute"]])
    dependVarsJ2ML <- c("compute")

    # Check if the container already exists. Create it if it doesn't.
    if (is.null(jaspResults[["j2mlContainer"]]) || jaspResults[["j2mlContainer"]]$getError()) {
        j2mlContainer <- createJaspContainer(title = "")
        j2mlContainer$dependOn(dependVarsJ2ML)
        jaspResults[["j2mlContainer"]] <- j2mlContainer
    } else {
        j2mlContainer <- jaspResults[["j2mlContainer"]]
    }

    # Initialize the J2ML variables
    netList <- NULL
    senderMatrix <- NULL
    receiverMatrix <- NULL
    densityMatrix <- NULL
    reciprocityMatrix <- NULL

    # 1. Parse network
    # net => each excel file can have multiple sheets. Need to concatenate.
    if (options[["net"]] != "") {
        filepath <- options[["net"]]
        if (file.exists(filepath)) {
            sheetNames <- readxl::excel_sheets(filepath)
            netList <- lapply(sheetNames, function(sheet) {
                as.matrix(readxl::read_excel(path = filepath, sheet = sheet, col_names = TRUE))
            })
            netList <- lapply(netList, function(x) {
                matrix(x, ncol = dim(x)[1])
            })
        }
    }

    # 2. Parse sender
    if (nchar(options[["sender"]]) != 0) {
        senderFiles <- unlist(strsplit(options[["sender"]], ";"))
        senderCovariatesList <- lapply(senderFiles, function(filepath) {
            if (file.exists(filepath)) {
                sheetNames <- readxl::excel_sheets(filepath)
                sheetsData <- lapply(sheetNames, function(sheet) {
                    as.matrix(readxl::read_excel(path = filepath, sheet = sheet, col_names = TRUE))
                })
                do.call(rbind, sheetsData)
            } else {
                NULL
            }
        })

        # Remove any NULLs from files that didn't exist
        senderCovariatesList <- senderCovariatesList[!sapply(senderCovariatesList, is.null)]

        if (length(senderCovariatesList) > 0) {
            # Combine all covariates into a single matrix
            senderMatrix <- do.call(cbind, senderCovariatesList)
            colnames(senderMatrix) <- tools::file_path_sans_ext(basename(senderFiles))
            # Add prefix to avoid ambiguity in results
            colnames(senderMatrix) <- paste0("sender_", colnames(senderMatrix))
        }
    }

    # 3. Parse receiver
    if (nchar(options[["receiver"]]) != 0) {
        receiverFiles <- unlist(strsplit(options[["receiver"]], ";"))
        receiverCovariatesList <- lapply(receiverFiles, function(filepath) {
            if (file.exists(filepath)) {
                sheetNames <- readxl::excel_sheets(filepath)
                sheetsData <- lapply(sheetNames, function(sheet) {
                    as.matrix(readxl::read_excel(path = filepath, sheet = sheet, col_names = TRUE))
                })
                do.call(rbind, sheetsData)
            } else {
                NULL
            }
        })

        # Remove any NULLs from files that didn't exist
        receiverCovariatesList <- receiverCovariatesList[!sapply(receiverCovariatesList, is.null)]

        if (length(receiverCovariatesList) > 0) {
            # Combine all covariates into a single matrix
            receiverMatrix <- do.call(cbind, receiverCovariatesList)
            colnames(receiverMatrix) <- tools::file_path_sans_ext(basename(receiverFiles))
            # Add prefix to avoid ambiguity in results
            colnames(receiverMatrix) <- paste0("receiver_", colnames(receiverMatrix))
        }
    }

    # 4. Parse density
    if (options[["density"]] != "") {
        densityFiles <- unlist(strsplit(options[["density"]], ";"))
        densityCovariatesList <- lapply(densityFiles, function(filepath) {
            if (file.exists(filepath)) {
                sheetNames <- readxl::excel_sheets(filepath)
                sheetsData <- lapply(sheetNames, function(sheet) {
                    as.matrix(readxl::read_excel(path = filepath, sheet = sheet, col_names = TRUE))
                })
                # Stack matrices from all sheets; handles networks of different sizes.
                do.call(plyr::rbind.fill.matrix, sheetsData)
            } else {
                NULL
            }
        })

        # Filter out NULLs for files that didn't exist
        validFiles <- densityFiles[!sapply(densityCovariatesList, is.null)]
        densityCovariatesList <- densityCovariatesList[!sapply(densityCovariatesList, is.null)]

        if (length(densityCovariatesList) > 0) {
            # Name the list of stacked matrices for use in the model formula
            names(densityCovariatesList) <- tools::file_path_sans_ext(basename(validFiles))
            # Add prefix to avoid ambiguity in results
            names(densityCovariatesList) <- paste0("density_", names(densityCovariatesList))
            densityMatrix <- densityCovariatesList
        }
    }

    # 5. Parse reciprocity
    if (options[["reciprocity"]] != "") {
        reciprocityFiles <- unlist(strsplit(options[["reciprocity"]], ";"))
        reciprocityCovariatesList <- lapply(reciprocityFiles, function(filepath) {
            if (file.exists(filepath)) {
                sheetNames <- readxl::excel_sheets(filepath)
                sheetsData <- lapply(sheetNames, function(sheet) {
                    mat <- as.matrix(readxl::read_excel(path = filepath, sheet = sheet, col_names = TRUE))
                    # Reciprocity matrices must be square and symmetric
                    if (nrow(mat) != ncol(mat) || !isSymmetric.matrix(unname(mat))) {
                        stop(gettextf("Matrix in sheet '%s' of file '%s' is not symmetric. All matrices for reciprocity covariates must be symmetric.", sheet, basename(filepath)))
                    }
                    mat
                })
                # Stack matrices from all sheets; handles networks of different sizes.
                do.call(plyr::rbind.fill.matrix, sheetsData)
            } else {
                NULL
            }
        })

        # Filter out NULLs for files that didn't exist
        validFiles <- reciprocityFiles[!sapply(reciprocityCovariatesList, is.null)]
        reciprocityCovariatesList <- reciprocityCovariatesList[!sapply(reciprocityCovariatesList, is.null)]

        if (length(reciprocityCovariatesList) > 0) {
            # Name the list of stacked matrices for use in the model formula
            names(reciprocityCovariatesList) <- tools::file_path_sans_ext(basename(validFiles))
            # Add prefix to avoid ambiguity in results
            names(reciprocityCovariatesList) <- paste0("reciprocity_", names(reciprocityCovariatesList))
            reciprocityMatrix <- reciprocityCovariatesList
        }
    }

    # 6. Parse MCMC parameters
    burnin <- options[["burnin"]]
    adapt <- options[["adapt"]]
    seed <- options[["seed"]]
    center <- options[["center"]]
    separateSigma <- options[["separateSigma"]]
    densVar <- options[["densVar"]]
    recVar <- options[["recVar"]]

    # Ensure netList is available before proceeding
    if (is.null(netList)) {
        j2mlContainer[["info"]] <- createJaspHtml(text = gettext("Please press Compute to see the estimation results."))
        return()
    }

    # Prepare arguments for the j2ML call
    j2mlArgs <- list(
        nets = netList,
        burnin = burnin,
        adapt = adapt,
        seed = seed,
        center = center,
        separateSigma = separateSigma,
        densVar = densVar,
        recVar = recVar
    )

    # The formula interface of dyads::j2ML requires variables to be in the environment.
    currentEnv <- environment()

    # Handle sender covariates
    if (!is.null(senderMatrix)) {
        list2env(as.data.frame(senderMatrix), envir = currentEnv)
        j2mlArgs$sender <- as.formula(paste("~", paste(colnames(senderMatrix), collapse = " + ")))
    }

    # Handle receiver covariates
    if (!is.null(receiverMatrix)) {
        list2env(as.data.frame(receiverMatrix), envir = currentEnv)
        j2mlArgs$receiver <- as.formula(paste("~", paste(colnames(receiverMatrix), collapse = " + ")))
    }

    # Handle density covariates
    if (!is.null(densityMatrix)) {
        list2env(densityMatrix, envir = currentEnv)
        j2mlArgs$density <- as.formula(paste("~", paste(names(densityMatrix), collapse = " + ")))
    }

    # Handle reciprocity covariates
    if (!is.null(reciprocityMatrix)) {
        list2env(reciprocityMatrix, envir = currentEnv)
        j2mlArgs$reciprocity <- as.formula(paste("~", paste(names(reciprocityMatrix), collapse = " + ")))
    }

    # Run the j2ML model with all specified arguments
    resultsJ2ML <- tryCatch({
        startProgressbar(length(j2mlArgs), gettext("Estimating network parameters for J2ML"))
        progressbarTick()
        do.call(dyads::j2ML, j2mlArgs)
    }, error = function(e) {
        j2mlContainer[["error"]] <- createJaspHtml(text = gettextf("An error occurred during model estimation: %s", e$message), class = "error")
        return(NULL)
    })

    # If the model ran successfully, display the summary
    if (!is.null(resultsJ2ML)) {
        # Create table for the J2ML results
        resultsJ2ML <- summary(resultsJ2ML)
        resultsJ2ML <- cbind(Parameter=rownames(resultsJ2ML), as.data.frame(resultsJ2ML))
        tableJ2ML <- createJaspTable(title = gettextf("J2ML Results"))
        tableJ2ML$dependOn(dependVarsJ2ML)
        tableJ2ML$setData(resultsJ2ML)
        tableJ2ML$position <- 1
        j2mlContainer[["tableJ2ML"]] <- tableJ2ML
    }
}