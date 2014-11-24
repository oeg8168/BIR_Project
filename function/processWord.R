processWord <- function(word,
                        stopWordFile = "./input/stopwords.txt",
                        stemming = TRUE,
                        removeStopWord = TRUE,
                        removeNumber = TRUE
                        )
{
    # Get stop words
    stopWords <- scan(stopWordFile, what = character())
    
    # Start process words
    wordProcessed <- word
    
    # (optional) Word stemming
    if(stemming)
        wordProcessed <- lapply(wordProcessed, 
                                wordStem)
    
    # (optional) Remove stop words
    if(removeStopWord)
        wordProcessed <- lapply(wordProcessed, 
                                function(input) input[!input%in%stopWords])
    
    # (optional) Remove number, term begin with number, unwanted encoding convertion
    if(removeNumber)
        wordProcessed <- lapply(wordProcessed, 
                                function(input) input[!grepl("^[[:digit:]]+$|^[[:digit:]]+|<.*>", input)])
    
    return(wordProcessed)
}
