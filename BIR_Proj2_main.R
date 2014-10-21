####################
# NCKU 2014 BIR Project #2
# Author: OEG
# 

rm(list=ls())   # clear all data
cat("\014")     # clear all console

library(graphics)

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
    tryCatch(
        
        # Read XML files
        rawContents <- c(rawContents, getXmlContent(filesPath[i], "AbstractText")),
        
        # If encoding error occured, record index and then ignore it
        error = function(e){
            cat(paste0("Index: ", i, "\n"))
            readXmlErrIndex <<- c(readXmlErrIndex, i)
        }
    )
}

# List to store each documents
docList <- NULL

# Vector to store all words in files
allWords <- NULL

# For each document, split it and collect all words
for(i in 1:length(rawContents)){
    
    # Parse each document into words
    tempDoc <- new("Document", content = rawContents[i])
    tempDoc <- parseXmlData(tempDoc)
    
    # All documents combine into a list
    docList <- c(docList, tempDoc)
    
    # All words in all documents combine into a list
    allWords <- c(allWords, tolower(tempDoc@words))
}

# Count frequency and sort it by decreasing order
allWords <- table(allWords)
allWords <- sort(allWords, decreasing = T)

# Plot the frequency figure
plot(allWords)
