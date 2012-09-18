CreateQualificationType <-
function (name, description, status, keywords = NULL, retry.delay = NULL, 
    test = NULL, answerkey = NULL, test.duration = NULL, auto = NULL, 
    auto.value = NULL, keypair = credentials(), print = TRUE, 
    browser = FALSE, log.requests = TRUE, sandbox = FALSE) 
{
    if (!is.null(keypair)) {
        keyid <- keypair[1]
        secret <- keypair[2]
    }
    else stop("No keypair provided or 'credentials' object not stored")
    operation <- "CreateQualificationType"
    if (!status %in% c("Active", "Inactive")) 
        stop("QualificationTypeStatus must be Active or Inactive")
    GETparameters <- paste("&Name=", name, "&Description=", curlEscape(description), 
        "&QualificationTypeStatus=", status, sep = "")
    if (!is.null(keywords)) 
        GETparameters <- paste(GETparameters, "&Keywords=", curlEscape(keywords), 
            sep = "")
    if (!is.null(test)) 
        GETparameters <- paste(GETparameters, "&Test=", test, 
            "&TestDurationInSeconds=", test.duration, sep = "")
    if (!is.null(retry.delay)) 
        GETparameters <- paste(GETparameters, "&RetryDelayInSeconds=", 
            retry.delay, sep = "")
    if (!is.null(answerkey)) 
        GETparameters <- paste(GETparameters, "&AnswerKey=", 
            answerkey, , , sep = "")
    if (!is.null(auto) && auto == TRUE & is.null(test) & !is.null(auto.value)) 
        GETparameters <- paste(GETparameters, "&AutoGranted=", 
            "true", "&AutoGrantedValue=", auto.value, sep = "")
    else if (!is.null(auto) && auto == FALSE & is.null(test) & 
        !is.null(auto.value)) 
        GETparameters <- paste(GETparameters, "&AutoGranted=", 
            "false", "&AutoGrantedValue=", auto.value, sep = "")
    else if (!is.null(auto) && auto == TRUE & is.null(test) & 
        is.null(auto.value)) 
        GETparameters <- paste(GETparameters, "&AutoGranted=", 
            "true", sep = "")
    else if (!is.null(auto) && auto == FALSE & is.null(test) & 
        is.null(auto.value)) 
        GETparameters <- paste(GETparameters, "&AutoGranted=", 
            "false", sep = "")
    else if (!is.null(auto) && !is.null(test)) 
        warning("AutoGranted Ignored! Test and AutoGranted cannot be declared together")
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
            QualificationType <- QualificationTypesToDataFrame(xml = request$xml)
            if (print == TRUE) {
                cat("QualificationType Created: ", QualificationType$QualificationTypeId[1], 
                  "\n", sep = "")
            }
            invisible(QualificationType)
        }
        else if (request$valid == FALSE) {
            if (print == TRUE) 
                cat("Invalid request\n")
            invisible(NULL)
        }
    }
}
