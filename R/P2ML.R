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
P2ML <- function(jaspResults, dataset = NULL, options, ...) {
    dependVarsP2ML <- c("net", "sender", "receiver", "density", "reciprocity", "burnin", "adapt", "seed", "center", "separate")
    # Check if the container already exists. Create it if it doesn't.
    if (is.null(jaspResults[["p2mlContainer"]]) || jaspResults[["p2mlContainer"]]$getError()) {
        p2mlContainer <- createJaspContainer(title = "")
        p2mlContainer$dependOn(dependVarsP2ML)
        jaspResults[["p2mlContainer"]] <- p2mlContainer
    } else {
        p2mlContainer <- jaspResults[["p2mlContainer"]]
    }

    # Initialize the P2ML variables
    netList <- NULL
    senderMatrix <- NULL
    receiverMatrix <- NULL
    densityMatrix <- NULL
    reciprocityMatrix <- NULL

    # Parse the option values and store them in the variables
    # net => each excel file can have multiple sheets. Need to concatenate.
    if (options[["net"]] != "") {
        filepath <- options[["net"]]
        # code here
        if (file.exists(filepath)) {
            sheetNames <- readxl::excel_sheets(filepath)
            netList <- lapply(sheetNames, function(sheet) {
                as.matrix(readxl::read_excel(path = filepath, sheet = sheet, col_names = TRUE))
            })
            netList <- lapply(netList, function(x) {
                matrix(x, ncol = dim(x)[1])
            })
        }
        # netop <- createJaspHtml(text = gettextf("Raw value of netList is: %s\n", toString(netList)))
        # p2mlContainer[["netop"]] <- netop
    }

    # m3 <-  dyads::p2ML(netList, adapt = 20, burnin = 100)
    # p2mlop <- createJaspHtml(text = gettextf("P2ML: %s\n", toString(summary(m3))))
    # p2mlContainer[["p2mlop"]] <- p2mlop

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
            senderMatrix <- do.call(cbind, senderCovariatesList) # Has one column per file
            # Use ncol(senderMatrix) to ensure the number of names matches the data
            safeColNames <- paste0("sender_cov_net_", seq_len(ncol(senderMatrix)))
            colnames(senderMatrix) <- safeColNames
        }
        # senderMatrixop <- createJaspHtml(text = gettextf("Raw value of senderMatrix is: %s\n", toString(senderMatrix)))
        # p2mlContainer[["senderMatrixop"]] <- senderMatrixop
    }

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
            receiverMatrix <- do.call(cbind, receiverCovariatesList) # Has one column per file
            # Use ncol(receiverMatrix) to ensure the number of names matches the data
            safeColNames <- paste0("receiver_cov_net_", seq_len(ncol(receiverMatrix)))
            colnames(receiverMatrix) <- safeColNames
        }
        # receiverMatrixop <- createJaspHtml(text = gettextf("Raw value of receiverMatrix is: %s\n", toString(receiverMatrix)))
        # p2mlContainer[["receiverMatrixop"]] <- receiverMatrixop
    }

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
            # Create safe, unique names to avoid clashes, e.g., density_cov_1, density_cov_2
            names(densityCovariatesList) <- paste0("density_cov_net_", seq_along(validFiles))
            densityMatrix <- densityCovariatesList

            # For debugging/display purposes
            # densityMatrixOp <- createJaspHtml(text = gettextf("Raw value of densityMatrix is: %s\n", toString(densityMatrix)))
            # p2mlContainer[["densityMatrixOp"]] <- densityMatrixOp
        }
    }

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
            # Create safe, unique names to avoid clashes, e.g., reciprocity_cov_1, reciprocity_cov_2
            names(reciprocityCovariatesList) <- paste0("reciprocity_cov_net_", seq_along(validFiles))
            reciprocityMatrix <- reciprocityCovariatesList

            # For debugging/display purposes
            # reciprocityMatrixOp <- createJaspHtml(text = gettextf("Raw value of reciprocityMatrix is: %s\n", toString(reciprocityMatrix)))
            # p2mlContainer[["reciprocityMatrixOp"]] <- reciprocityMatrixOp
        }
    }

    # Parse MCMC parameters
    burnin <- options[["burnin"]]
    adapt <- options[["adapt"]]
    seed <- options[["seed"]]
    center <- options[["center"]]
    separate <- options[["separate"]]

    # mcmcParamsText <- gettextf("MCMC params -> burnin: %s, adapt: %s, seed: %s, center: %s, separate: %s", burnin, adapt, seed, center, separate)
    # p2mlContainer[["mcmcParamsOp"]] <- createJaspHtml(text = mcmcParamsText)

    # Ensure netList is available before proceeding
    if (is.null(netList)) {
        p2mlContainer[["error"]] <- createJaspHtml(text = gettext("Network data could not be loaded. Please check the input file."), class = "error")
        return()
    }

    # Prepare arguments for the p2ML call
    p2mlArgs <- list(
        nets = netList,
        burnin = burnin,
        adapt = adapt,
        seed = seed,
        center = center,
        separate = separate
    )

    # The formula interface of dyads::p2ML requires variables to be in the environment.
    # We add them to the current function's environment, which is safe and temporary.
    currentEnv <- environment()

    # Handle sender covariates
    if (!is.null(senderMatrix)) {
        list2env(as.data.frame(senderMatrix), envir = currentEnv)
        p2mlArgs$sender <- as.formula(paste("~", paste(colnames(senderMatrix), collapse = " + ")))
    }

    # Handle receiver covariates
    if (!is.null(receiverMatrix)) {
        list2env(as.data.frame(receiverMatrix), envir = currentEnv)
        p2mlArgs$receiver <- as.formula(paste("~", paste(colnames(receiverMatrix), collapse = " + ")))
    }

    # Handle density covariates
    if (!is.null(densityMatrix)) {
        list2env(densityMatrix, envir = currentEnv)
        p2mlArgs$density <- as.formula(paste("~", paste(names(densityMatrix), collapse = " + ")))
    }

    # Handle reciprocity covariates
    if (!is.null(reciprocityMatrix)) {
        list2env(reciprocityMatrix, envir = currentEnv)
        p2mlArgs$reciprocity <- as.formula(paste("~", paste(names(reciprocityMatrix), collapse = " + ")))
    }

    # Run the p2ML model with all specified arguments
    resultsP2ML <- tryCatch({
        do.call(dyads::p2ML, p2mlArgs)
    }, error = function(e) {
        p2mlContainer[["error"]] <- createJaspHtml(text = gettextf("An error occurred during model estimation: %s", e$message), class = "error")
        return(NULL)
    })

    # If the model ran successfully, display the summary
    if (!is.null(resultsP2ML)) {
        # summaryText <- paste(capture.output(summary(finalModel)), collapse = "\n")
        # summaryOp <- createJaspHtml(text = gettextf("<pre>%s</pre>", summaryText))
        # p2mlContainer[["finalModelSummary"]] <- summaryOp
        # Create table for the P2ML results
        resultsP2ML <- summary(resultsP2ML)
        resultsP2ML <- cbind(Parameter=rownames(resultsP2ML), as.data.frame(resultsP2ML))
        tableP2ML <- createJaspTable(title = gettextf("P2ML Results"))
        tableP2ML$dependOn(dependVarsP2ML)
        tableP2ML$setData(resultsP2ML)
        # tableP2$addRows(resultsP2, rowNames = unique(rownames(resultsP2)))
        tableP2ML$position <- 1
        p2mlContainer[["tableP2ML"]] <- tableP2ML
    }
}