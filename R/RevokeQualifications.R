RevokeQualifications <-
function (quals, workers, reasons = NULL, keypair = credentials(), 
    print = TRUE, browser = FALSE, log.requests = TRUE, sandbox = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "RevokeQualification"
    if (!is.null(reasons) && length(reasons) > 1) 
        stop("Reason must be NULL or length==1; other configurations not currently supported")
    batch <- function(qual, worker, reason) {
        GETparameters <- paste("&QualificationTypeId=", qual, 
            "&WorkerId=", worker, sep = "")
        if (!is.null(reason)) 
            GETparameters <- paste(GETparameters, "&SendNotification=", 
                curlEscape(reason), sep = "")
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
            if (request$valid == TRUE) {
                if (print == TRUE) 
                  cat("Qualification (", qual, ") for worker ", 
                    worker, " Revoked\n", sep = "")
            }
            else if (request$valid == FALSE) {
                if (print == TRUE) 
                  cat("Invalid Request for worker ", worker, 
                    "\n", sep = "")
            }
            invisible(request)
        }
    }
    Qualifications <- data.frame(matrix(ncol = 4))
    names(Qualifications) <- c("WorkerId", "QualificationTypeId", 
        "Reason", "Valid")
    if (length(quals) == 1 & length(workers) == 1) {
        x <- batch(quals[1], workers[1], reasons)
        Qualifications[1, ] = c(workers[1], quals[1], reasons, 
            x$valid)
    }
    else if (length(quals) > 1 & length(workers) == 1) {
        for (i in 1:length(quals)) {
            x <- batch(quals[i], workers[1], reasons)
            Qualifications[i, ] = c(workers[1], quals[i], reasons, 
                x$valid)
        }
    }
    else if (length(quals) == 1 & length(workers) > 1) {
        for (i in 1:length(workers)) {
            x <- batch(quals[1], workers[i], reasons)
            Qualifications[i, ] = c(workers[i], quals[1], reasons, 
                x$valid)
        }
    }
    else if (length(quals) > 1 & length(workers) > 1) {
        for (i in 1:length(workers)) {
            for (j in 1:length(quals)) {
                x <- batch(quals[j], workers[i], reasons)
                Qualifications[i, ] = c(workers[i], quals[j], 
                  reasons, x$valid)
            }
        }
    }
    if (print == TRUE) 
        return(Qualifications)
    else invisible(Qualifications)
}
