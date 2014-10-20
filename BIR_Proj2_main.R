####################
# NCKU 2014 BIR Project #2
# Author: OEG
# 

rm(list=ls())   # clear all data
cat("\014")     # clear all console

# Source all files under "./function" folder
functions <- list.files(path = "./function/",
                        full.names = T)
sapply(functions, FUN = source)

# Source all files under "./class" folder
classes <- list.files(path = "./class/",
                      full.names = T)
sapply(functions, FUN = source)

# Get list of input XML files
inputFolder <- "./input/#2"
filesPath <- list.files(path = inputFolder, 
                        pattern = "*.xml", 
                        recursive = T, 
                        full.names = T)

# Vector for storing index of encoding error
readXmlErrIndex <- numeric(0)

# Read XML files and deal with encoding error
for(i in 1:1477){
    tryCatch(
        getXmlContent(filesPath[i], "AbstractText"),
        error = function(e){
            cat(paste0("Index: ", i, "\n"))
            readXmlErrIndex <<- c(readXmlErrIndex, i)
        }
    )
}
