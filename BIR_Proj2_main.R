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
        rawContents <- rbind(rawContents, 
                             c(filesPath[i], getXmlContent(filesPath[i], "AbstractText"))
                             ),
        
        # If encoding error occured, record index and then ignore it
        error = function(e){
            cat(paste0("Index: ", i, "\n"))
            readXmlErrIndex <<- rbind(readXmlErrIndex, c(i, filesPath[i]))
        }
    )
}

# Convert "rawContents" into data frame and set column name
rawContents <- as.data.frame(rawContents)
colnames(rawContents) <- c("path", "content")

# Convert "readXmlErrIndex" into data frame and set column name
readXmlErrIndex <- as.data.frame(readXmlErrIndex)
colnames(readXmlErrIndex) <- c("index", "path")

# Show list of "readXmlErrIndex"
View(readXmlErrIndex)

# List to store each documents
docList <- list()

# Vector to store all words in files
allStemWords <- vector(mode = "character")

# Parse all documents and combine into a list
docList <- apply(X = rawContents, 
                 MARGIN = 1,
                 FUN = function(raw)
                     parseXmlData(new("Document", 
                                      path = raw["path"], 
                                      content = raw["content"]
                                      ))
                 )

# Collect all stem words
allStemWords <- sapply(X = docList, 
                   FUN = function(doc) 
                       c(allStemWords, doc@stemWords)
                   )

# Count frequency and sort it by decreasing order
allStemWords <- table(unlist(allStemWords))
allStemWords <- sort(allStemWords, decreasing = T)

# Convert all encoding to UTF-8, so these "strange" encoding will become "NA"
row.names(allStemWords) <- iconv(row.names(allStemWords), to="UTF-8")

##which(is.na(iconv(row.names(allStemWords), to="UTF-8"))))

# Create data frame base on previous work
freqDF <- data.frame(word = names(allStemWords), freq = allStemWords)

# Remove rows with "NA"
freqDF <- na.omit(freqDF)

# Index rearrangement
row.names(freqDF) <- 1:nrow(freqDF)

# Plot the frequency chart
plot(x = sort(freqDF$freq, decreasing = T), xlab = "ranking", ylab = "frequency")

# Get all words category by documents
docWordList <- lapply(X = docList, 
                      FUN = function(input) 
                          c(input@stemWords)
)

# Compute all word-document relations
#system.time(
#wordRelation <- lapply(freqDF$word, FUN = function(word) inWhichDoc(word, docWordList))
#)

# Combine word-document relations into frequency data frame
#freqDF <- cbind(freqDF, cbind(wordRelation))

# Show frequency table
View(freqDF)

