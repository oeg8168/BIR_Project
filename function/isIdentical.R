####################
# 
#   Return if two content in documents are identical
# 

isIdentical <- function(doc1, doc2){
    cat(paste0("Is identical: ", doc1@content %in% doc2@content), "\n")
}