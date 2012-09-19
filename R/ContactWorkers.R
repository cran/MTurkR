ContactWorkers <-
function (subjects, msgs, workers, keypair = credentials(), print = FALSE, 
    browser = FALSE, log.requests = TRUE, sandbox = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "NotifyWorkers"
    for (i in 1:length(subjects)) {
        if (nchar(curlEscape(subjects[i])) > 200) 
            stop(paste("Subject ", i, " Too Long (200 char max)", 
                sep = ""))
    }
    for (i in 1:length(msgs)) {
        if (nchar(curlEscape(msgs[i])) > 4096) 
            stop(paste("Message ", i, "Text Too Long (4096 char max)", 
                sep = ""))
    }
    if (length(subjects) == 1) 
        subjects <- rep(subjects[1], length(workers))
    else if (!length(subjects) == length(workers)) 
        stop("Number of subjects is not 1 nor length(workers)")
    if (length(msgs) == 1) 
        msgs <- rep(msgs[1], length(workers))
    else if (!length(msgs) == length(workers)) 
        stop("Number of messages is not 1 nor length(workers)")
    Notifications <- data.frame(matrix(nrow = length(workers), 
        ncol = 4))
    names(Notifications) <- c("WorkerId", "Subject", "Message", 
        "Valid")
    for (i in 1:length(workers)) {
        GETparameters <- paste("&Subject=", curlEscape(subjects[i]), 
            "&MessageText=", curlEscape(msgs[i]), "&WorkerId.1=", 
            workers[i], sep = "")
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
            Notifications[i, ] <- c(workers[i], subjects[i], 
                msgs[i], request$valid)
            if (request$valid == TRUE) {
                if (print == TRUE) 
                  cat("Worker (", workers[i], ") Notified\n", 
                    sep = "")
            }
            else if (request$valid == FALSE) {
                if (print == TRUE) 
                  cat("Invalid Request for worker ", workers[i], 
                    "\n")
            }
        }
    }
    if (print == TRUE) 
        return(Notifications)
    else invisible(Notifications)
}
