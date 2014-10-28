####################
# 
#   Find a single word are in which documents
# 

inWhichDoc <- function(word, docWordList){
    
    indexList <- which(sapply(X = docWordList,
                              FUN = function(singleDocWords)
                                  word %in% singleDocWords
                              )
                       )
    return(indexList)
}
