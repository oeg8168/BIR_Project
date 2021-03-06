####################
# 
#   Class definition of "Document"
# 

# Class definition
setClass(
    Class = "Document", 

    slots = list(
        # Path of this document file
        path = "character",
        
        # AbstractText of this document
        content = "character",
        
        # Vector of splitted sentences
        sentences = "vector",
        
        # Vector of splitted words
        words = "vector",
        
        # Vector of words after stemming
        stemWords = "vector",
        
        # Number of stopwords
        stopwordSum = "numeric"
    )
)

# To parse data from XML document by its file path
setGeneric("parseXmlData", function(self) {standardGeneric("parseXmlData")})
setMethod("parseXmlData", signature(self = "Document"),
          function(self) {
              require(Rstem)
              
              # Regular expression to split content
              regExp <- "[[:space:]]+"
              
              if(length(self@content) == 0){
                  self@content <- getXmlContent(self@path, "AbstractText")
              }
              #self@sentences <- parseSentences(self)
              #self@sentences <- unlist(strsplit(self@content, split="[.][[:space:]]+[[:upper:]]"))
              
              # Split words using defined regular expression
              self@words <- unlist(strsplit(self@content, split=regExp))          
              
              # Remove punctations
              self@words <- gsub("[[:punct:]]", "", self@words)
              
              # Remove empty strings
              self@words <- self@words[self@words != ""]
              
              # Stem the words using Porter's algorithm
              self@stemWords <- wordStem(tolower(self@words))      
              
              return(self)
          }
)

# Split content into sentences
setGeneric("parseSentences", function(self) {standardGeneric("parseSentences")})
setMethod("parseSentences", signature(self = "Document"),
          function(self) {
              require("NLP")
              require("openNLP")
              
              text <- as.String(self@content)
              
              # Set token for parse sentence
              sent_token_annotator <- Maxent_Sent_Token_Annotator()
              
              # Parse sentence
              sentenceIndex <- annotate(text, sent_token_annotator)
              
              return(text[sentenceIndex])
          }
)

# Show statistical information of document
setGeneric("showDocumentStat", function(self) {standardGeneric("showDocumentStat")})
setMethod("showDocumentStat", signature(self = "Document"),
          function(self) {
              cat(paste0("file path: ", self@path, "\n"))
              cat(paste0("number of characters: ", nchar(self@content), "\n"))
              cat(paste0("number of sentences: ", length(self@sentences), "\n"))
              cat(paste0("number of words: ", length(self@words), "\n"))
              cat(paste0("number of stopwords: ", self@stopwordSum, "\n"))
          }
)
