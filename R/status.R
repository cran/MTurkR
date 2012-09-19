status <-
function (hit = NULL, hit.type = NULL, keypair = credentials(), 
    print = TRUE, log.requests = TRUE, sandbox = FALSE) 
{
    if ((is.null(hit) & is.null(hit.type)) | (!is.null(hit) & 
        !is.null(hit.type))) 
        stop("Must provide 'hit' xor 'hit.type'")
    else if (!is.null(hit)) {
        hitlist <- hit
    }
    else if (!is.null(hit.type)) {
        hitsearch <- SearchHITs(keypair = keypair, print = FALSE, 
            log.requests = log.requests, sandbox = sandbox, return.qual.dataframe = FALSE)
        hitlist <- hitsearch$HITs[hitsearch$HITs$HITTypeId == 
            hit.type, ]$HITId
        if (length(hitlist) == 0) 
            stop("No HITs found for HITType")
    }
    HITs <- NA
    for (i in 1:length(hitlist)) {
        x <- GetHIT(hitlist[i], response.group = "HITAssignmentSummary", 
            keypair = keypair, print = FALSE, browser = FALSE, 
            log.requests = log.requests, sandbox = FALSE, return.hit.dataframe = TRUE, 
            return.qual.dataframe = FALSE)
        if (!is.null(x$HITs)) {
            if (i == 1) 
                HITs <- x$HITs
            else HITs <- rbind(HITs, x$HITs)
        }
    }
    if (length(hitlist) > 1) {
        HITs$HITId[i + 1] <- "------------------------------"
        HITs$NumberofAssignmentsPending[i + 1] <- "--------------------"
        HITs$NumberofAssignmentsAvailable[i + 1] <- "------------------"
        HITs$NumberofAssignmentsCompleted[i + 1] <- "--------------------"
        HITs$HITId[i + 2] <- "Totals"
        HITs$NumberofAssignmentsPending[i + 2] <- sum(as.numeric(HITs$NumberofAssignmentsAvailable[1:i]))
        HITs$NumberofAssignmentsAvailable[i + 2] <- sum(as.numeric(HITs$NumberofAssignmentsPending[1:i]))
        HITs$NumberofAssignmentsCompleted[i + 2] <- sum(as.numeric(HITs$NumberofAssignmentsCompleted[1:i]))
    }
    if (print == TRUE) 
        print(HITs[, c("HITId", "NumberofAssignmentsPending", 
            "NumberofAssignmentsAvailable", "NumberofAssignmentsCompleted")], 
            row.names = FALSE)
    invisible(HITs)
}
