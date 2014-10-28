####################
# 
#   Get index of matched word
# 

wordMatch <- function(word, DF, option = "IDENTICAL"){
    require(Rstem)
    require(stringdist)
    
    switch(toupper(option), 
           IDENTICAL = {
               temp <- which(DF$word %in% word)
           },
           
           ED = {
               editDist <- stringdist(word, DF$word)
               minDist <-  min(editDist)
               temp <- which(editDist == minDist)
           },
           
           STEM = {
               temp <- which(DF$word %in% wordStem(tolower(word)))
           },
           
           # Default
           {
               print("Please enter correct option! (identical, ed, stem)")
           }
           
           )    # End of switch
    
    return(temp)
}
