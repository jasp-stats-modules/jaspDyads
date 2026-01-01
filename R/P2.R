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
P2 <- function(jaspResults, dataset = NULL, options, ...) {
    dependVarsP2 <- c("net", "sender", "receiver", "density", "reciprocity", "burnin", "sample", "adapt", "seed")
    # Check if the container already exists. Create it if it doesn't.
    if (is.null(jaspResults[["p2Container"]]) || jaspResults[["p2Container"]]$getError()) {
        p2Container <- createJaspContainer(title = "", position = 1)
        p2Container$dependOn(dependVarsP2)
        jaspResults[["p2Container"]] <- p2Container
    } else {
        p2Container <- jaspResults[["p2Container"]]
    }

    # Initialize the P2 variables
    netMatrix <- NULL
    senderVector <- NULL
    receiverVector <- NULL
    densityMatrix <- NULL
    reciprocityMatrix <- NULL
    burnin <- NULL
    sample <- NULL
    adapt <- NULL
    seed <- NULL

    # Parse the option values and store them in the variables
    if (options[["net"]] != "") {
        df_net <- na.omit(read.csv(options[["net"]], header=FALSE))
        netMatrix <- as.matrix(df_net)
    }

    if (nchar(options[["sender"]]) != 0) {
        seq_sender <- cleanSequence(options[["sender"]])
        senderVector <- as.numeric(seq_sender)
    }

    if (nchar(options[["receiver"]]) != 0) {
        seq_receiver <- cleanSequence(options[["receiver"]])
        receiverVector <- as.numeric(seq_receiver)
    }

    if (options[["density"]] != "") {
        df_density <- na.omit(read.csv(options[["density"]], header=FALSE))
        densityMatrix <- as.matrix(df_density)
    }

    if (options[["reciprocity"]] != "") {
        df_reciprocity <- na.omit(read.csv(options[["reciprocity"]], header=FALSE))
        reciprocityMatrix <- as.matrix(df_reciprocity)
    }

    # Parse MCMC parameters
    burnin <- options[["burnin"]]
    sample <- options[["sample"]]
    adapt <- options[["adapt"]]
    seed <- options[["seed"]]

    if (!is.null(netMatrix) && !is.null(densityMatrix) && !is.null(reciprocityMatrix) && !is.null(senderVector) && !is.null(receiverVector)) {
        # resultsP2 <- dyads::p2(netMatrix, density = ~ densityMatrix, reciprocity= ~ reciprocityMatrix, burnin = burnin, sample = sample, adapt = adapt, seed = seed)
        resultsP2 <- dyads::p2(netMatrix, sender = ~ senderVector, receiver = ~ receiverVector, density = ~ densityMatrix, reciprocity= ~ reciprocityMatrix, burnin = burnin, sample = sample, adapt = adapt, seed = seed)
        resultsP2 <- cbind(Parameter=rownames(resultsP2), as.data.frame(resultsP2))

        # Create table for the P2 results
        tableP2 <- createJaspTable(title = gettextf("P2 Results"))
        tableP2$dependOn(dependVarsP2)

        # Add column information
        # colNamesP2 <- c("Parameter", colnames(resultsP2))
        # tableP2$addColumnInfo(name = colNamesP2[1], title = "", type = "string")
        # for (columnName in colNamesP2[2:length(colNamesP2)]) {
        #     tableP2$addColumnInfo(name = columnName, title = gettext(columnName), type = "number")
        # }

        # Add rows
        # rowNamesP2 <- rownames(resultsP2)
        # for (i in 1:length(rowNamesP2)) {
        #     rowP2 <- c(rowNamesP2[i], resultsP2[i,])
        #     names(rowP2) <- colNamesP2
        #     tableP2$addRows(rowP2)
        # }
        # tableP2$showSpecifiedColumnsOnly <- TRUE

        tableP2$setData(resultsP2)
        # tableP2$addRows(resultsP2, rowNames = unique(rownames(resultsP2)))
        tableP2$position <- 1
        p2Container[["tableP2"]] <- tableP2

        # p2DebugOp <- createJaspHtml(text = gettextf("Dimensions for matrix: %s\nP2 results (raw string):\n%s\n", toString(dim(resultsP2)), toString(resultsP2)), position = 2)
        # p2Container[["p2DebugOp"]] <- p2DebugOp
    }
}