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
rawContents <- vector(mode = "character")

# Vector for storing index of encoding error
readXmlErrIndex <- vector(mode = "character")

# Read XML files and deal with encoding error
for(i in 1:length(filesPath)){
    tryCatch(
        
        # Read XML files
        rawContents <- rbind(rawContents, getXmlContent(filesPath[i], "AbstractText")),
        
        # If encoding error occured, record index and then ignore it
        error = function(e){
            cat(paste0("Index: ", i, "\n"))
            readXmlErrIndex <<- rbind(readXmlErrIndex, c(i, filesPath[i]))
        }
    )
}

# Set column names of vector "readXmlErrIndex"
colnames(readXmlErrIndex) <- c("file index", "path")

# List to store each documents
docList <- list()

# Vector to store all words in files
allWords <- vector(mode = "character")

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

