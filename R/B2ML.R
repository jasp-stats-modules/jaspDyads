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
B2ML <- function(jaspResults, dataset = NULL, options, ...) {
    dependVarsB2ML <- c("net", "actor", "density", "burnin", "adapt", "seed", "center", "separate", "densVar")
    # Check if the container already exists. Create it if it doesn't.
    if (is.null(jaspResults[["b2mlContainer"]]) || jaspResults[["b2mlContainer"]]$getError()) {
        b2mlContainer <- createJaspContainer(title = "")
        b2mlContainer$dependOn(dependVarsB2ML)
        jaspResults[["b2mlContainer"]] <- b2mlContainer
    } else {
        b2mlContainer <- jaspResults[["b2mlContainer"]]
    }

    # Initialize the B2ML variables
    netList <- NULL
    actorMatrix <- NULL
    densityMatrix <- NULL

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
        # b2mlContainer[["netop"]] <- netop
    }

    # m3 <-  dyads::b2ML(netList, adapt = 20, burnin = 100)
    # b2mlop <- createJaspHtml(text = gettextf("B2ML: %s\n", toString(summary(m3))))
    # b2mlContainer[["b2mlop"]] <- b2mlop

    if (nchar(options[["actor"]]) != 0) {
        actorFiles <- unlist(strsplit(options[["actor"]], ";"))
        actorCovariatesList <- lapply(actorFiles, function(filepath) {
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
        actorCovariatesList <- actorCovariatesList[!sapply(actorCovariatesList, is.null)]

        if (length(actorCovariatesList) > 0) {
            # Combine all covariates into a single matrix
            actorMatrix <- do.call(cbind, actorCovariatesList)
            colnames(actorMatrix) <- tools::file_path_sans_ext(basename(actorFiles))
            # Add prefix to avoid ambiguity in results
            colnames(actorMatrix) <- paste0("actor_", colnames(actorMatrix))
        }
        # actorMatrixop <- createJaspHtml(text = gettextf("Raw value of actorMatrix is: %s\n", toString(actorMatrix)))
        # b2mlContainer[["actorMatrixop"]] <- actorMatrixop
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
            names(densityCovariatesList) <- tools::file_path_sans_ext(basename(validFiles))
            # Add prefix to avoid ambiguity in results
            names(densityCovariatesList) <- paste0("density_", names(densityCovariatesList))
            densityMatrix <- densityCovariatesList

            # For debugging/display purposes
            # densityMatrixOp <- createJaspHtml(text = gettextf("Raw value of densityMatrix is: %s\n", toString(densityMatrix)))
            # b2mlContainer[["densityMatrixOp"]] <- densityMatrixOp
        }
    }

    # Parse MCMC parameters
    burnin <- options[["burnin"]]
    adapt <- options[["adapt"]]
    seed <- options[["seed"]]
    center <- options[["center"]]
    separate <- options[["separate"]]
    densVar <- options[["densVar"]]

    # mcmcParamsText <- gettextf("MCMC params -> burnin: %s, adapt: %s, seed: %s, center: %s, separate: %s", burnin, adapt, seed, center, separate)
    # b2mlContainer[["mcmcParamsOp"]] <- createJaspHtml(text = mcmcParamsText)

    # Ensure netList is available before proceeding
    if (is.null(netList)) {
        b2mlContainer[["error"]] <- createJaspHtml(text = gettext("Network data could not be loaded. Please check the input file."), class = "error")
        return()
    }

    # Prepare arguments for the b2ML call
    b2mlArgs <- list(
        nets = netList,
        burnin = burnin,
        adapt = adapt,
        seed = seed,
        center = center,
        separate = separate,
        densVar = densVar
    )

    # The formula interface of dyads::b2ML requires variables to be in the environment.
    # We add them to the current function's environment, which is safe and temporary.
    currentEnv <- environment()

    # Handle actor covariates
    if (!is.null(actorMatrix)) {
        list2env(as.data.frame(actorMatrix), envir = currentEnv)
        b2mlArgs$actor <- as.formula(paste("~", paste(colnames(actorMatrix), collapse = " + ")))
    }

    # Handle density covariates
    if (!is.null(densityMatrix)) {
        list2env(densityMatrix, envir = currentEnv)
        b2mlArgs$density <- as.formula(paste("~", paste(names(densityMatrix), collapse = " + ")))
    }

    # Run the b2ML model with all specified arguments
    resultsB2ML <- tryCatch({
        do.call(dyads::b2ML, b2mlArgs)
    }, error = function(e) {
        b2mlContainer[["error"]] <- createJaspHtml(text = gettextf("An error occurred during model estimation: %s", e$message), class = "error")
        return(NULL)
    })

    # If the model ran successfully, display the summary
    if (!is.null(resultsB2ML)) {
        # summaryText <- paste(capture.output(summary(finalModel)), collapse = "\n")
        # summaryOp <- createJaspHtml(text = gettextf("<pre>%s</pre>", summaryText))
        # b2mlContainer[["finalModelSummary"]] <- summaryOp
        # Create table for the B2ML results
        resultsB2ML <- summary(resultsB2ML)
        resultsB2ML <- cbind(Parameter=rownames(resultsB2ML), as.data.frame(resultsB2ML))
        tableB2ML <- createJaspTable(title = gettextf("B2ML Results"))
        tableB2ML$dependOn(dependVarsB2ML)
        tableB2ML$setData(resultsB2ML)
        # tableP2$addRows(resultsP2, rowNames = unique(rownames(resultsP2)))
        tableB2ML$position <- 1
        b2mlContainer[["tableB2ML"]] <- tableB2ML
    }
}