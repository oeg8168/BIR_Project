####################
# 
#   Find a single word are in which documents and return PMID
# 

inWhichDocPMID <- function(word, docSet, processed = T){
    
    if(processed)
        docWordSet <- docSet$wordProcessed
    else
        docWordSet <- docSet$word
    
    indexList <- which(sapply(X = docWordSet,
                              FUN = function(singleDocWords)
                                  word %in% singleDocWords
                              )
                       )
    return(unlist(docSet$PMID[indexList]))
}
