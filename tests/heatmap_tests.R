#!/usr/bin/env Rscript

## Package
##--------

library(RUnit)

## Constants
##----------

tesOutDirC <- "output"

## Functions
##-----------

## Reading tables (matrix or data frame)
reaTabF <- function(filC, typC = c("matrix", "dataframe")[1]) {

    	file.exists(filC) || stop(paste0("No output file \"", filC ,"\"."))

        switch(typC,
               matrix = return(t(as.matrix(read.table(file = filC,
                   header = TRUE,
                   row.names = 1,
                   sep = "\t",
                   stringsAsFactors = FALSE)))),
               dataframe = return(read.table(file = filC,
                   header = TRUE,
                   row.names = 1,
                   sep = "\t",
                   stringsAsFactors = FALSE)))

}

## Call wrapper
wrpCalF <- function(parLs) {

	## Set program path
    	wrpPatC <- file.path("..", "heatmap_wrapper.R")

	## Set arguments
	argLs <- NULL
	for (parC in names(parLs))
		argLs <- c(argLs, parC, parLs[[parC]])

	## Call
	wrpCalC <- paste(c(wrpPatC, argLs), collapse = " ")

        if(.Platform$OS.type == "windows")
            wrpCalC <- paste("Rscript", wrpCalC)

	wrpCodN <- system(wrpCalC)

	if (wrpCodN != 0)
		stop("Error when running heatmap_wrapper.R.")

	## Get output
	outLs <- list()
	if ("dataMatrix_out" %in% names(parLs))
            outLs[["datMN"]] <- reaTabF(parLs[["dataMatrix_out"]], "matrix")
	if ("sampleMetadata_out" %in% names(parLs))
            outLs[["samDF"]] <- reaTabF(parLs[["sampleMetadata_out"]], "dataframe")
	if ("variableMetadata_out" %in% names(parLs))
            outLs[["varDF"]] <- reaTabF(parLs[["variableMetadata_out"]], "dataframe")
        if("information" %in% names(parLs))
            outLs[["infVc"]] <- readLines(parLs[["information"]])

	return(outLs)
}

## Setting default parameters
argDefF <- function(tesC) {

    tesInpDirC <- unlist(strsplit(tesC, "_"))[1]

    argDefLs <- list()
    if(file.exists(file.path(tesInpDirC, "dataMatrix.tsv")))
        argDefLs[["dataMatrix_in"]] <- file.path(tesInpDirC, "dataMatrix.tsv")
    if(file.exists(file.path(tesInpDirC, "sampleMetadata.tsv")))
        argDefLs[["sampleMetadata_in"]] <- file.path(tesInpDirC, "sampleMetadata.tsv")
    if(file.exists(file.path(tesInpDirC, "variableMetadata.tsv")))
        argDefLs[["variableMetadata_in"]] <- file.path(tesInpDirC, "variableMetadata.tsv")

    argDefLs[["dataMatrix_out"]] <- file.path(tesOutDirC, "dataMatrix.tsv")
    argDefLs[["sampleMetadata_out"]] <- file.path(tesOutDirC, "sampleMetadata.tsv")
    argDefLs[["variableMetadata_out"]] <- file.path(tesOutDirC, "variableMetadata.tsv")
    argDefLs[["figure"]] <- file.path(tesOutDirC, "figure.pdf")
    argDefLs[["information"]] <- file.path(tesOutDirC, "information.txt")

    argDefLs

}

## Main
##-----

## Create output folder

file.exists(tesOutDirC) || dir.create(tesOutDirC)

test_input_cut4 <- function() {

    tesC <- "input_cut1"
    argLs <- list(disC = "1-cor",
                  cutSamN = "4",
                  cutVarN = "3",
                  corMetC = "spearman",
                  scaL = "TRUE",
                  cexN = "0.8")
    ## chkC <- "checkEqualsNumeric(outLs[['samDF']][13, 'heat_clust'], 4)"

    argLs <- c(argDefF(tesC = tesC), argLs)
    outLs <- wrpCalF(argLs)
    ## stopifnot(eval(parse(text = chkC)))

}
test_input_cut4()


test_exa1_cut3 <- function() {

    tesC <- "exa1_cut3"
    argLs <- list(disC = "1-cor",
                  cutSamN = "3",
                  cutVarN = "4",
                  corMetC = "spearman",
                  scaL = "TRUE",
                  cexN = "1")
    chkC <- "checkEqualsNumeric(outLs[['varDF']]['V24', 'heat_clust'], 4)"

    argLs <- c(argDefF(tesC = tesC), argLs)
    outLs <- wrpCalF(argLs)
    stopifnot(eval(parse(text = chkC)))

}
test_exa1_cut3()


test_exa2_cut4 <- function() {

    tesC <- "exa2_cut1"
    argLs <- list(disC = "1-cor",
                  cutSamN = "1",
                  cutVarN = "1",
                  corMetC = "spearman",
                  scaL = "TRUE",
                  cexN = "1")
    chkC <- "checkEquals(outLs[['varDF']]['V31', 'meta2'], 'AM')"

    argLs <- c(argDefF(tesC = tesC), argLs)
    outLs <- wrpCalF(argLs)
    stopifnot(eval(parse(text = chkC)))

}
test_exa2_cut4()
