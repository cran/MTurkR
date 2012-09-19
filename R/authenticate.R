authenticate <-
function (operation, secret, service = "AWSMechanicalTurkRequester", 
    version = NULL) 
{
    version <- "2011-10-01"
    timestamp <- format(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ", 
        tz = "UTC")
    signature <- base64Encode(hmac(secret, paste(service, operation, 
        timestamp, sep = ""), algo = "sha1", serialize = FALSE, 
        raw = TRUE))[1]
    return(list(operation = operation, signature = signature, 
        timestamp = timestamp))
}
