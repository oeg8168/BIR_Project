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

inputFolder <- "./input/#2"
filesPath <- list.files(path = inputFolder, 
                        pattern = "*.xml", 
                        recursive = T, 
                        full.names = T)


getXmlContent(filesPath[1], "AbstractText")
getXmlContent(filesPath[11], "AbstractText")
getXmlContent(filesPath[113], "AbstractText")

lapply(filesPath[1:113],FUN = getXmlContent,"AbstractText")




