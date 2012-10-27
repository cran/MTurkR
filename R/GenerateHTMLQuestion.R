GenerateHTMLQuestion <-
function (character = NULL, file = NULL, frame.height = 450) 
{
    if (!is.null(character)) 
        html <- character
    if (!is.null(file)) {
        filedata <- readLines(file, warn = FALSE)
        html <- ""
        for (i in 1:length(filedata)) {
            html <- paste(html, filedata[i], sep = "")
        }
        html <- gsub("\t", "", html)
    }
    htmlquestion <- newXMLNode("HTMLQuestion", namespaceDefinitions = "http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2011-11-11/HTMLQuestion.xsd")
    content.frame <- newXMLNode("HTMLContent", paste("<![CDATA[", 
        html, "]]>", sep = ""), parent = htmlquestion)
    height <- newXMLNode("FrameHeight", frame.height, parent = htmlquestion)
    addChildren(htmlquestion, c(content.frame, height))
    string <- toString.XMLNode(htmlquestion)
    encoded <- curlEscape(string)
    invisible(list(xml.parsed = htmlquestion, string = string, 
        url.encoded = encoded))
}
