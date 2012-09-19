GenerateHTMLQuestion <-
function (character = NULL, file = NULL) 
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
    htmlquestion <- paste("<![CDATA[", html, "]]>", sep = "")
    invisible(htmlquestion)
}
