####################
# 
#   Split content into sentences
# 

parseSentences <- function(content){
    require("NLP")
    require("openNLP")
    
    content <- as.String(content)
    
    sent_token_annotator <- Maxent_Sent_Token_Annotator()
    
    sentenceIndex <- annotate(content, sent_token_annotator)
    
    return(content[sentenceIndex])
}