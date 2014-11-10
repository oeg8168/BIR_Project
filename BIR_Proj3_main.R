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

wordFreq_1 <- getWordFreq(docSet_1)
wordFreq_2 <- getWordFreq(docSet_2)



