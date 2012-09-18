RejectAssignment <-
function (assignments, feedback = NULL, keypair = credentials(), 
    print = TRUE, browser = FALSE, log.requests = TRUE, sandbox = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "RejectAssignment"
    if (!is.null(feedback)) {
        for (i in 1:length(feedback)) {
            if (!is.null(feedback[i]) && nchar(curlEscape(feedback[i])) > 
                1024) 
                warning("Feedback ", i, " is too long (1024 char max)")
        }
        if (length(feedback) == 1) 
            feedback <- rep(feedback[1], length(assignments))
        else if (!length(feedback) == length(assignments)) 
            stop("Number of feedback is not 1 nor length(assignmetns)")
    }
    Assignments <- data.frame(matrix(nrow = length(workers), 
        ncol = 3))
    names(Assignments) <- c("AssignmentId", "Feedback", "Valid")
    for (i in 1:length(assignments)) {
        GETparameters <- paste("&AssignmentId=", assignments[i], 
            sep = "")
        if (!is.null(feedback[i])) 
            GETparameters <- paste(GETparameters, "&RequesterFeedback=", 
                curlEscape(feedback[i]), sep = "")
        auth <- authenticate(operation, secret)
        if (browser == TRUE) {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, browser = browser, 
                sandbox = sandbox)
        }
        else {
            request <- request(keyid, auth$operation, auth$signature, 
                auth$timestamp, GETparameters, log.requests = log.requests, 
                sandbox = sandbox)
            Assignments[i, ] <- c(assignments[i], feedback[i], 
                request$valid)
            if (request$valid == TRUE) {
                if (print == TRUE) 
                  cat("Assignment (", assignments[i], ") Rejected\n", 
                    sep = "")
            }
            if (request$valid == FALSE) {
                if (print == TRUE) 
                  cat("Invalid request for assignment ", assignments[i], 
                    "\n")
            }
        }
    }
    if (print == TRUE) {
        cat(sum(Assignments$Valid), " of ", i, " Assignments Rejected\n", 
            sep = "")
        return(Assignments)
    }
    else invisible(Assignments)
}
