####################
# 
#   Parse XML document collection and process words
#   (Packaging previous works)
#

parseXmlDocSet <- function(inputXmlDocSet,
                           stopWordFile = "./input/stopwords.txt",
                           stemming = TRUE,
                           removeStopWord = TRUE,
                           removeNumber = TRUE
)
{
    require(XML)
    require(Rstem)
    
    # Parse XML file
    xmlFile <- xmlParse(inputXmlDocSet, encoding = "UTF-8")
    
    # Get PubMed ID of article
    PMID <- xpathSApply(xmlFile, "//*/MedlineCitation/PMID", xmlValue)
    
    # Get abstract content of article
    content <- xpathSApply(xmlFile, "//*/Abstract", xmlValue)
    
    # Split content into words
    word <- strsplit(tolower(content), "[[:space:]]+|[[:punct:]]+")
    
    # Remove empty words
    word <- lapply(word, function(input) input[input!=""])
    
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
    
    # (optional) Remove number
    if(removeNumber)
        wordProcessed <- lapply(wordProcessed, 
                                function(input) input[!grepl("^[[:digit:]]+$", input)])
    
    # Combine all informations
    raw <- cbind(PMID, content, word, wordProcessed)
    
    # Convert result as data frame
    raw <- as.data.frame(raw, stringsAsFactors = FALSE)
    
    return(raw)
}

