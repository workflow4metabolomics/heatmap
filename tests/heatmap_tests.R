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
    argLs <- list(cutSamN = "4",
                  cutVarN = "3",
                  scaL = "TRUE",
                  cexN = "0.8")
    chkC <- "checkEqualsNumeric(outLs[['samDF']][13, 'heat_clust'], 4)"

    argLs <- c(argDefF(tesC = tesC), argLs)
    outLs <- wrpCalF(argLs)
    stopifnot(eval(parse(text = chkC)))

}
test_input_cut4()


test_exa1_cut3 <- function() {

    tesC <- "exa1_cut3"
    argLs <- list(cutSamN = "3",
                  cutVarN = "4",
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
    argLs <- list(cutSamN = "1",
                  cutVarN = "1",
                  scaL = "TRUE",
                  cexN = "1")
    chkC <- "checkEquals(outLs[['varDF']]['V31', 'meta2'], 'AM')"

    argLs <- c(argDefF(tesC = tesC), argLs)
    outLs <- wrpCalF(argLs)
    stopifnot(eval(parse(text = chkC)))

}
test_exa2_cut4()

## tesArgLs <- list(input_cut1= c(cutSamN = "4",
##                      cutVarN = "3",
##                      scaL = "TRUE",
##                      cexN = "0.8"),
##                  mpetera = c(cutSamN = "3",
##                      cutVarN = "4",
##                      scaL = "TRUE",
##                      cexN = "1"),
##                  friols = c(cutSamN = "1",
##                      cutVarN = "1",
##                      scaL = "TRUE",
##                      cexN = "1"))


## wrapperF <- function(argVc) {

##     source("../heatmap_script.R")

##     ##------------------------------
##     ## Initializing
##     ##------------------------------

##     ## options
##     ##--------

##     strAsFacL <- options()[["stringsAsFactors"]]
##     options(stringsAsFactors=FALSE)

##     ## constants
##     ##----------

##     modNamC <- "Heatmap" ## module name

##     ## log file
##     ##---------

##     sink(argVc["information"])

##     cat("\nStart of the '", modNamC, "' module: ",
##         format(Sys.time(), "%a %d %b %Y %X"), "\n", sep="")

##     ## loading
##     ##--------

##     proMN <- t(as.matrix(read.table(argVc["dataMatrix_in"],
##                                     check.names = FALSE,
##                                     header = TRUE,
##                                     row.names = 1,
##                                     sep = "\t")))

##     obsDF <- read.table(argVc["sampleMetadata_in"],
##                         check.names = FALSE,
##                         header = TRUE,
##                         row.names = 1,
##                         sep = "\t")

##     feaDF <- read.table(argVc["variableMetadata_in"],
##                         check.names = FALSE,
##                         header = TRUE,
##                         row.names = 1,
##                         sep = "\t")

##     cutSamN <- as.numeric(argVc["cutSamN"])
##     cutVarN <- as.numeric(argVc["cutVarN"])

##     ## checking
##     ##---------

##     if(cutSamN > nrow(proMN))
##         stop("Number of sample clusters must be inferior to the number of samples")
##     if(cutVarN > ncol(proMN))
##         stop("Number of variable clusters must be inferior to the number of variables")


##     ##------------------------------
##     ## Computation
##     ##------------------------------


##     heaLs <- heatmapF(proMN = proMN,
##                       obsDF = obsDF,
##                       feaDF = feaDF,
##                       cutSamN = cutSamN,
##                       cutVarN = cutVarN,
##                       fig.pdfC = argVc["figure"],
##                       scaL = ifelse("scaL" %in% names(argVc),
##                           as.logical(argVc["scaL"]),
##                           TRUE),
##                       cexN = ifelse("cexN" %in% names(argVc),
##                           as.numeric(argVc["cexN"]),
##                           0.8))


##     ##------------------------------
##     ## Ending
##     ##------------------------------


##     ## saving
##     ##-------

##     proDF <- cbind.data.frame(dataMatrix = colnames(heaLs[["proMN"]]),
##                               as.data.frame(t(heaLs[["proMN"]])))
##     write.table(proDF,
##                 file = argVc["dataMatrix_out"],
##                 quote = FALSE,
##                 row.names = FALSE,
##                 sep = "\t")

##     obsDF <- cbind.data.frame(sampleMetadata = rownames(heaLs[["obsDF"]]),
##                               heaLs[["obsDF"]])
##     write.table(obsDF,
##                 file = argVc["sampleMetadata_out"],
##                 quote = FALSE,
##                 row.names = FALSE,
##                 sep = "\t")

##     feaDF <- cbind.data.frame(variableMetadata = rownames(heaLs[["feaDF"]]),
##                               heaLs[["feaDF"]])
##     write.table(feaDF,
##                 file = argVc["variableMetadata_out"],
##                 quote = FALSE,
##                 row.names = FALSE,
##                 sep = "\t")

##     ## Ending
##     ##-------

##     cat("\nEnd of the '", modNamC, "' Galaxy module call: ",
##         format(Sys.time(), "%a %d %b %Y %X"), "\n", sep = "")

##     sink()

##     options(stringsAsFactors = strAsFacL)

##     rm(list = ls())


## }

## exaDirOutC <- "output"
## stopifnot(file.exists(exaDirOutC))

## tesArgLs <- list(input_cut1= c(cutSamN = "4",
##                      cutVarN = "3",
##                      scaL = "TRUE",
##                      cexN = "0.8"),
##                  mpetera = c(cutSamN = "3",
##                      cutVarN = "4",
##                      scaL = "TRUE",
##                      cexN = "1"),
##                  friols = c(cutSamN = "1",
##                      cutVarN = "1",
##                      scaL = "TRUE",
##                      cexN = "1"))

## for(tesC in names(tesArgLs))
##     tesArgLs[[tesC]] <- c(tesArgLs[[tesC]],
##                           dataMatrix_in = file.path(unlist(strsplit(tesC, "_"))[1], "dataMatrix.tsv"),
##                           sampleMetadata_in = file.path(unlist(strsplit(tesC, "_"))[1], "sampleMetadata.tsv"),
##                           variableMetadata_in = file.path(unlist(strsplit(tesC, "_"))[1], "variableMetadata.tsv"),
##                           dataMatrix_out = file.path(exaDirOutC, "dataMatrix.tsv"),
##                           sampleMetadata_out = file.path(exaDirOutC, "sampleMetadata.tsv"),
##                           variableMetadata_out = file.path(exaDirOutC, "variableMetadata.tsv"),
##                           figure = file.path(exaDirOutC, "figure.pdf"),
##                           information = file.path(exaDirOutC, "information.txt"))

## for(tesC in names(tesArgLs)) {
##     print(tesC)
##     heaLs <- wrapperF(tesArgLs[[tesC]])
##     if(".chkC" %in% names(tesArgLs[[tesC]]))
##         stopifnot(eval(parse(text = tesArgLs[[tesC]][[".chkC"]])))
## }

## message("Checks successfully completed")
