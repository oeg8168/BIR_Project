####################
# NCKU 2014 BIR Project #1
# Author: OEG
# 

rm(list=ls())   # clear all data
cat("\014")     # clear all console

# Import class and functions
source("./class/Document.R")
source("./function/getXmlContent.R")
source("./function/isIdentical.R")
#source("./function/parseSentences.R")

# Construct objects and read in stop words
doc1 <- new("Document", path = "./input/#1/text1.xml")
doc2 <- new("Document", path = "./input/#1/text3.xml")
stopWords <- scan("./input/stopwords.txt", what = character())

# Parse XML data(Tag: AbstractText) from documents
doc1 <- parseXmlData(doc1)
doc2 <- parseXmlData(doc2)

# Count frequency of each word and store into data frame
df1 <- as.data.frame(table(tolower(doc1@words)))
df2 <- as.data.frame(table(tolower(doc2@words)))

# Merge two dataframe
result <- merge(df1, df2, by = "Var1", all = T)

# See if words is a stop word
result <- cbind(result, lapply(result["Var1"], is.element, stopWords))

# Rename column names
colnames(result) <- c("Words", "doc1.freq", "doc2.freq", "is.stopwords")

# Count number of stopwords in two documents
doc1@stopwordSum <- sum(subset(result, is.stopwords & !is.na(doc1.freq))$doc1.freq)
doc2@stopwordSum <- sum(subset(result, is.stopwords & !is.na(doc2.freq))$doc2.freq)

# Show results
View(result)
show(doc1@sentences)
show(doc2@sentences)
showDocumentStat(doc1)
showDocumentStat(doc2)
isIdentical(doc1, doc2)
