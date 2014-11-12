####################
# NCKU 2014 BIR Project #3
# Author: OEG
# 

rm(list=ls())   # clear all data
cat("\014")     # clear all console

# Source all files under "./function" folder
functions <- list.files(path = "./function/",
                        full.names = T)
sapply(functions, FUN = source)

source("./function/parseXmlDocSet.R")

docSet_1 <- parseXmlDocSet("./input/#3/pubmed_hemagglutinin.xml")
docSet_2 <- parseXmlDocSet("./input/#3/pubmed_neuraminidase.xml")

wordFreq_1 <- getWordFreq(docSet_1, atLeast = 0)
wordFreq_2 <- getWordFreq(docSet_2, atLeast = 0)

TFIDF_1 <-  apply(docSet_1, MARGIN = 1, FUN = function(doc) getTFIDF(doc, wordFreq_1))
TFIDF_2 <-  apply(docSet_2, MARGIN = 1, FUN = function(doc) getTFIDF(doc, wordFreq_2))
