#!/usr/bin/Rscript --vanilla --slave --no-site-file


library(batch) ## parseCommandArgs

source_local <- function(fname){
    argv <- commandArgs(trailingOnly = FALSE)
    base_dir <- dirname(substring(argv[grep("--file=", argv)], 8))
    source(paste(base_dir, fname, sep="/"))
}

source_local("heatmap_script.R")

argVc <- unlist(parseCommandArgs(evaluate=FALSE))


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

sink(argVc["information"])

cat("\nStart of the '", modNamC, "' module: ",
    format(Sys.time(), "%a %d %b %Y %X"), "\n", sep="")

## loading
##--------

proMN <- t(as.matrix(read.table(argVc["dataMatrix_in"],
                                check.names = FALSE,
                                header = TRUE,
                                row.names = 1,
                                sep = "\t")))

obsDF <- read.table(argVc["sampleMetadata_in"],
                    check.names = FALSE,
                    header = TRUE,
                    row.names = 1,
                    sep = "\t")

feaDF <- read.table(argVc["variableMetadata_in"],
                    check.names = FALSE,
                    header = TRUE,
                    row.names = 1,
                    sep = "\t")

cutSamN <- as.numeric(argVc["cutSamN"])
cutVarN <- as.numeric(argVc["cutVarN"])
disC <- as.character(argVc["disC"])

## checking
##---------

if(cutSamN > nrow(proMN))
    stop("Number of sample clusters must be inferior to the number of samples")
if(cutVarN > ncol(proMN))
    stop("Number of variable clusters must be inferior to the number of variables")
if(!(disC %in% c("1-cor", "1-abs(cor)")))
   stop("Distance must be either '1-cor', '1-abs(cor)'")


##------------------------------
## Computation
##------------------------------


heaLs <- heatmapF(proMN = proMN,
                  obsDF = obsDF,
                  feaDF = feaDF,
                  disC = disC,
                  cutSamN = cutSamN,
                  cutVarN = cutVarN,
                  fig.pdfC = argVc["figure"],
                  corMetC = ifelse("corMetC" %in% names(argVc),
                      as.character(argVc["corMetC"]),
                      "pearson"),
                  scaL = ifelse("scaL" %in% names(argVc),
                      as.logical(argVc["scaL"]),
                      TRUE),
                  cexN = ifelse("cexN" %in% names(argVc),
                      as.numeric(argVc["cexN"]),
                      0.8))


##------------------------------
## Ending
##------------------------------


## saving
##-------

proDF <- cbind.data.frame(dataMatrix = colnames(heaLs[["proMN"]]),
                          as.data.frame(t(heaLs[["proMN"]])))
write.table(proDF,
            file = argVc["dataMatrix_out"],
            quote = FALSE,
            row.names = FALSE,
            sep = "\t")

obsDF <- cbind.data.frame(sampleMetadata = rownames(heaLs[["obsDF"]]),
                          heaLs[["obsDF"]])
write.table(obsDF,
            file = argVc["sampleMetadata_out"],
            quote = FALSE,
            row.names = FALSE,
            sep = "\t")

feaDF <- cbind.data.frame(variableMetadata = rownames(heaLs[["feaDF"]]),
                          heaLs[["feaDF"]])
write.table(feaDF,
            file = argVc["variableMetadata_out"],
            quote = FALSE,
            row.names = FALSE,
            sep = "\t")

## Ending
##-------

cat("\nEnd of the '", modNamC, "' Galaxy module call: ",
    format(Sys.time(), "%a %d %b %Y %X"), "\n", sep = "")

sink()

options(stringsAsFactors = strAsFacL)

rm(list = ls())
