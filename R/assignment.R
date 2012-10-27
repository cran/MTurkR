assignment <-
function (assignment = NULL, hit = NULL, hit.type = NULL, status = NULL, 
    return.all = FALSE, pagenumber = "1", pagesize = "10", sortproperty = "SubmitTime", 
    sortdirection = "Ascending", keypair = credentials(), print = TRUE, 
    browser = FALSE, log.requests = TRUE, sandbox = FALSE, return.assignment.dataframe = TRUE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    if (!sortproperty %in% c("AcceptTime", "SubmitTime", "AssignmentStatus")) 
        stop("'sortproperty' must be 'AcceptTime' | 'SubmitTime' | 'AssignmentStatus'")
    if (!sortdirection %in% c("Ascending", "Descending")) 
        stop("'sortdirection' must be 'Ascending' | 'Descending'")
    if (as.numeric(pagesize) < 1 || as.numeric(pagesize) > 100) 
        stop("'pagesize' must be in range (1,100)")
    if (as.numeric(pagenumber) < 1) 
        stop("'pagenumber' must be > 1")
    if (!is.null(assignment)) {
        operation <- "GetAssignment"
        Assignments <- NA
        HITs <- NA
        for (i in 1:length(assignment)) {
            GETparameters = paste("&AssignmentId=", assignment[i], 
                sep = "")
            auth <- authenticate(operation, secret)
            if (browser == TRUE) {
                request <- request(keyid, auth$operation, auth$signature, 
                  auth$timestamp, GETparameters, browser = browser, 
                  sandbox = sandbox, log.requests = log.requests)
            }
            else {
                request <- request(keyid, auth$operation, auth$signature, 
                  auth$timestamp, GETparameters, log.requests = log.requests, 
                  sandbox = sandbox)
                QualificationRequirements <- list()
                if (request$valid == TRUE) {
                  a <- AssignmentsToDataFrame(xml = request$xml)$assignments
                  h <- HITsToDataFrame(xml = request$xml)
                  a$Answer <- NULL
                  if (i == 1) {
                    Assignments <- a
                    HITs <- h$HITs
                    QualificationRequirements <- h$QualificationRequirements
                  }
                  else {
                    Assignments <- rbind(Assignments, a)
                    HITs <- rbind(HITs, h$HITs)
                    QualificationRequirements <- c(QualificationRequirements, 
                      h$QualificationRequirements)
                  }
                  if (print == TRUE) 
                    cat("Assignment ", assignment[i], " Retrieved\n", 
                      sep = "")
                }
            }
        }
        if (print == TRUE) 
            print(Assignments)
        invisible(list(Assignments = Assignments, HITs = HITs, 
            QualificationRequirements = QualificationRequirements))
    }
    else {
        operation <- "GetAssignmentsForHIT"
        if (return.all == TRUE) {
            sortproperty <- "SubmitTime"
            sortdirection <- "Ascending"
            pagesize <- "100"
            pagenumber <- "1"
        }
        if ((is.null(hit) & is.null(hit.type)) | (!is.null(hit) & 
            !is.null(hit.type))) 
            stop("Must provide 'assignment' xor 'hit' xor 'hit.type'")
        else if (!is.null(hit)) {
            hitlist <- hit
        }
        else if (!is.null(hit.type)) {
            hitsearch <- SearchHITs(keypair = keypair, print = FALSE, 
                log.requests = log.requests, sandbox = sandbox, 
                return.qual.dataframe = FALSE)
            hitlist <- hitsearch$HITs[hitsearch$HITs$HITTypeId == 
                hit.type, ]$HITId
            if (length(hitlist) == 0) 
                stop("No HITs found for HITType")
        }
        batch <- function(batchhit, pagenumber) {
            GETiteration <- ""
            if (!is.null(status)) {
                if (status %in% c("Approved", "Rejected", "Submitted")) 
                  GETiteration <- paste(GETiteration, "&AssignmentStatus=", 
                    status, sep = "")
                else status = NULL
            }
            GETiteration <- paste("&HITId=", batchhit, "&PageNumber=", 
                pagenumber, "&PageSize=", pagesize, "&SortProperty=", 
                sortproperty, "&SortDirection=", sortdirection, 
                sep = "")
            auth <- authenticate(operation, secret)
            batch <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETiteration, log.requests = log.requests, 
                sandbox = sandbox)
            batch$total <- as.numeric(strsplit(strsplit(batch$xml, 
                "<TotalNumResults>")[[1]][2], "</TotalNumResults>")[[1]][1])
            batch$batch.total <- length(xpathApply(xmlParse(batch$xml), 
                "//Assignment"))
            if (batch$batch.total > 0 & return.assignment.dataframe == 
                TRUE) {
                batch$assignments <- AssignmentsToDataFrame(xml = batch$xml)$assignments
                batch$assignments$Answer <- NULL
            }
            else if (batch$batch.total > 0 & return.assignment.dataframe == 
                FALSE) 
                batch$assignments <- NULL
            else batch$assignments <- NA
            return(batch)
        }
        for (i in 1:length(hitlist)) {
            request <- batch(hitlist[i], pagenumber)
            runningtotal <- request$batch.total
            pagenumber = 2
            if (return.all == TRUE) {
                while (request$total > runningtotal) {
                  nextbatch <- batch(hitlist[i], pagenumber)
                  request$request.id <- c(request$request.id, 
                    nextbatch$request.id)
                  request$request.url <- c(request$request.url, 
                    nextbatch$request.url)
                  request$valid <- c(request$valid, nextbatch$valid)
                  request$xml <- c(request$xml, nextbatch$xml)
                  if (return.assignment.dataframe == TRUE) 
                    request$assignments <- rbind(request$assignments, 
                      nextbatch$assignments)
                  request$pages.returned <- pagenumber
                  runningtotal <- runningtotal + nextbatch$batch.total
                  pagenumber <- pagenumber + 1
                }
            }
            request$batch.total <- NULL
        }
        if (!is.null(hittype)) 
            request$assignments$HITTypeId <- hit.type
        if (print == TRUE) 
            cat(runningtotal, " of ", request$total, " Assignments Retrieved\n", 
                sep = "")
        invisible(request$assignments)
    }
}
