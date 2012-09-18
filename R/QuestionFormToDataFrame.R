QuestionFormToDataFrame <-
function (xml = NULL, xml.parsed = NULL) 
{
    if (!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    removeXMLNamespaces(xml.parsed, all = TRUE)
    return(NULL)
}
