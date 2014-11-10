####################
# 
#   Find a single word are in which documents and return PMID
# 

inWhichDocPMID <- function(word, docSet, processed = T){
    
    if(processed)
        docWordList <- docSet$wordProcessed
    else
        docWordList <- docSet$word
    
    indexList <- which(sapply(X = docWordList,
                              FUN = function(singleDocWords)
                                  word %in% singleDocWords
                              )
                       )
    return(unlist(docSet$PMID[indexList]))
}
