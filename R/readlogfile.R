readlogfile <-
function (filename = NULL, shell = FALSE) 
{
    if (is.null(filename)) 
        filename <- "MTurkRlog.tsv"
    if (!filename %in% list.files()) 
        cat("No Log File Found\n")
    else {
        if (shell == FALSE) {
            invisible(read.delim(filename, header = TRUE, sep = "\t", 
                quote = "", stringsAsFactors = FALSE))
        }
        else if (shell == TRUE) {
            shell.exec(filename)
        }
    }
}
