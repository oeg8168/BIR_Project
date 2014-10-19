####################
# 
#   Get content of specific tag name in XML
# 

getXmlContent <- function(xmlFilePath, tag){
    require(XML)
    
    xmlFile <- xmlParse(xmlFilePath, encoding = "UTF-8")
    tagPath <- paste("//*/", tag)
    content <- xpathSApply(xmlFile, tagPath, xmlValue)
    
    # If there is more than one tag, combine it
    content <- apply(as.matrix(content), 2, paste, collapse="")
    
    return(content)
}
