library(RUnit)

wrapperF <- function(argVc) {

    source("../heatmap_script.R")

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
                      cutSamN = cutSamN,
                      cutVarN = cutVarN,
                      fig.pdfC = argVc["figure"],
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


}

exaDirOutC <- "output"
stopifnot(file.exists(exaDirOutC))

tesArgLs <- list(input_cut1= c(cutSamN = "4",
                     cutVarN = "3",
                     scaL = "TRUE",
                     cexN = "0.8"),
                 mpetera = c(cutSamN = "3",
                     cutVarN = "4",
                     scaL = "TRUE",
                     cexN = "1"),
                 friols = c(cutSamN = "1",
                     cutVarN = "1",
                     scaL = "TRUE",
                     cexN = "1"))

for(tesC in names(tesArgLs))
    tesArgLs[[tesC]] <- c(tesArgLs[[tesC]],
                          dataMatrix_in = file.path(unlist(strsplit(tesC, "_"))[1], "dataMatrix.tsv"),
                          sampleMetadata_in = file.path(unlist(strsplit(tesC, "_"))[1], "sampleMetadata.tsv"),
                          variableMetadata_in = file.path(unlist(strsplit(tesC, "_"))[1], "variableMetadata.tsv"),
                          dataMatrix_out = file.path(exaDirOutC, "dataMatrix.tsv"),
                          sampleMetadata_out = file.path(exaDirOutC, "sampleMetadata.tsv"),
                          variableMetadata_out = file.path(exaDirOutC, "variableMetadata.tsv"),
                          figure = file.path(exaDirOutC, "figure.pdf"),
                          information = file.path(exaDirOutC, "information.txt"))

for(tesC in names(tesArgLs)) {
    print(tesC)
    heaLs <- wrapperF(tesArgLs[[tesC]])
    if(".chkC" %in% names(tesArgLs[[tesC]]))
        stopifnot(eval(parse(text = tesArgLs[[tesC]][[".chkC"]])))
}

message("Checks successfully completed")
