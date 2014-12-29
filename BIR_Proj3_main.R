####################
# NCKU 2014 BIR Project #3
# Author: OEG
# 

rm(list=ls())   # clear all data
cat("\014")     # clear all console

# Source all files under "./function" folder
functions <- list.files(path = "./function/",
                        full.names = T,
                        recursive = T)
sapply(functions, FUN = source)

docSet_1 <- parseXmlDocSet("./input/#3/pubmed_hemagglutinin.xml")
docSet_2 <- parseXmlDocSet("./input/#3/pubmed_neuraminidase.xml")

wordFreq_1 <- getWordFreq(docSet_1, atLeast = 0)
wordFreq_2 <- getWordFreq(docSet_2, atLeast = 0)

docSet_1$TFIDF <- apply(docSet_1, MARGIN = 1, FUN = function(doc) getTFIDF(doc, wordFreq_1))
docSet_2$TFIDF <- apply(docSet_2, MARGIN = 1, FUN = function(doc) getTFIDF(doc, wordFreq_2))

index_1_BSBI <- BSBI(docSet_1)
index_2_BSBI <- BSBI(docSet_2)

index_1_SPIMI <- SPIMI(docSet_1)
index_2_SPIMI <- SPIMI(docSet_2)


