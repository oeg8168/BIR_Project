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
sapply(classes, FUN = source)

# Get list of input XML files
inputFolder <- "./input/#2"
filesPath <- list.files(path = inputFolder, 
                        pattern = "*.xml", 
                        recursive = T, 
                        full.names = T)

# Vector for storing raw contents of XML files
rawContents <- character(0)

# Vector for storing index of encoding error
readXmlErrIndex <- numeric(0)

# Read XML files and deal with encoding error
for(i in 1:length(filesPath)){
    #tempDoc <- new("Document", path = filesPath[i])
    tryCatch(
        rawContents <- c(rawContents, getXmlContent(filesPath[i], "AbstractText")),
        error = function(e){
            cat(paste0("Index: ", i, "\n"))
            readXmlErrIndex <<- c(readXmlErrIndex, i)
        }
    )
}

# have problem
v <- new("Document")
lapply(1:length(rawContents), function(i){
        tempDoc <- new("Document", content = rawContents[i])
        tempDoc <- parseXmlData(tempDoc)
        v <<- c(v, tempDoc)
    }
)
