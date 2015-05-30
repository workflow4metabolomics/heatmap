#!/usr/bin/Rscript --vanilla --slave --no-site-file


library(batch) ## parseCommandArgs

source_local <- function(fname){
    argv <- commandArgs(trailingOnly = FALSE)
    base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
    source(paste(base_dir, fname, sep="/"))
}

source_local("heatmap_script.R")



rssVsGalL <- FALSE

if(rssVsGalL) { ## for running with R outside the Galaxy environment during development of the script

    ## 'example' input dir
    exaDirInpC <- "example/input"

    argLs <- list(dataMatrix_in = file.path(exaDirInpC, "dataMatrix.tsv"),
                  sampleMetadata_in = file.path(exaDirInpC, "sampleMetadata.tsv"),
                  variableMetadata_in = file.path(exaDirInpC, "variableMetadata.tsv"),
                  cutSamN = "1",
                  cutVarN = "1",
                  scaL = "TRUE",
                  cexN = "0.8")

    ## 'example' output dir
    exaDirOutC <- gsub("input", "output", exaDirInpC)

    argLs <- c(argLs,
               list(dataMatrix_out = file.path(exaDirOutC, "dataMatrix.tsv"),
                    sampleMetadata_out = file.path(exaDirOutC, "sampleMetadata.tsv"),
                    variableMetadata_out = file.path(exaDirOutC, "variableMetadata.tsv"),
                    figure = file.path(exaDirOutC, "figure.pdf")))

    stopifnot(file.exists(exaDirOutC))

} else
    argLs <- parseCommandArgs(evaluate=FALSE)


##------------------------------
## Initializing
##------------------------------

## options
##--------

strAsFacL <- options()[["stringsAsFactors"]]
options(stringsAsFactors=FALSE)

## constants
##----------

modNamC <- "Heatmap" ## module name

## log file
##---------

cat("\nStart of the '", modNamC, "' module: ",
    format(Sys.time(), "%a %d %b %Y %X"), "\n", sep="")

## loading
##--------

proMN <- t(as.matrix(read.table(argLs[["dataMatrix_in"]],
                                check.names = FALSE,
                                header = TRUE,
                                row.names = 1,
                                sep = "\t")))

obsDF <- read.table(argLs[["sampleMetadata_in"]],
                    check.names = FALSE,
                    header = TRUE,
                    row.names = 1,
                    sep = "\t")

feaDF <- read.table(argLs[["variableMetadata_in"]],
                    check.names = FALSE,
                    header = TRUE,
                    row.names = 1,
                    sep = "\t")

cutSamN <- as.numeric(argLs[["cutSamN"]])
cutVarN <- as.numeric(argLs[["cutVarN"]])

## checking
##---------

if(cutSamN > nrow(proMN))
    stop("Number of sample clusters must be inferior to the number of samples")
if(cutVarN > ncol(proMN))
    stop("Number of variable clusters must be inferior to the number of variables")


##------------------------------
## Computation
##------------------------------


heaLs <- heatmapF(proMN = proMN,
                  obsDF = obsDF,
                  feaDF = feaDF,
                  cutObsN = cutSamN,
                  cutFeaN = cutVarN,
                  fig.pdfC = argLs[["figure"]],
                  scaL = ifelse("scaL" %in% names(argLs),
                      as.logical(argLs[["scaL"]]),
                      TRUE),
                  cexN = ifelse("cexN" %in% names(argLs),
                      as.numeric(argLs[["cexN"]]),
                      0.8))


##------------------------------
## Ending
##------------------------------


## saving
##-------

proDF <- cbind.data.frame(dataMatrix = colnames(heaLs[["proMN"]]),
                          as.data.frame(t(heaLs[["proMN"]])))
write.table(proDF,
            file = argLs[["dataMatrix_out"]],
            quote = FALSE,
            row.names = FALSE,
            sep = "\t")

obsDF <- cbind.data.frame(sampleMetadata = rownames(heaLs[["obsDF"]]),
                          heaLs[["obsDF"]])
write.table(obsDF,
            file = argLs[["sampleMetadata_out"]],
            quote = FALSE,
            row.names = FALSE,
            sep = "\t")

feaDF <- cbind.data.frame(variableMetadata = rownames(heaLs[["feaDF"]]),
                          heaLs[["feaDF"]])
write.table(feaDF,
            file = argLs[["variableMetadata_out"]],
            quote = FALSE,
            row.names = FALSE,
            sep = "\t")

## Ending
##-------

cat("\nEnd of the '", modNamC, "' Galaxy module call: ",
    format(Sys.time(), "%a %d %b %Y %X"), "\n", sep = "")

## sink()

options(stringsAsFactors = strAsFacL)

rm(list = ls())
