####################
# NCKU 2014 BIR Project #2
# Author: OEG
# 

rm(list=ls())   # clear all data
cat("\014")     # clear all console

library(Rstem)
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

# Parse all documents and combine into a list
docList <- sapply(X = rawContents, 
                  FUN = function(raw) 
                      parseXmlData(new("Document", content = raw)), 
                  USE.NAMES = F
                  )

# For each document, split it and collect all words
allWords <- sapply(X = docList, 
                   FUN = function(doc) 
                       wordStem(tolower(doc@words))
                   )

# Count frequency and sort it by decreasing order
allWords <- table(unlist(allWords))
allWords <- sort(allWords, decreasing = T)

# Convert all encoding to UTF-8, so these "strange" encoding will become "NA"
row.names(allWords) <- iconv(row.names(allWords), to="UTF-8")

# Create data frame base on previous work
DF <- data.frame(word = row.names(allWords), freq = allWords)

# Remove rows with "NA"
DF <- na.omit(DF)

# Index rearrangement
row.names(DF) <- 1:nrow(DF)

# Plot the frequency figure
plot(x = sort(DF$freq, decreasing = T), xlab = "ranking", ylab = "frequency")

# Show frequency chart
View(DF)

