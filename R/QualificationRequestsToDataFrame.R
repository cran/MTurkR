QualificationRequestsToDataFrame <-
function (xml = NULL, xml.parsed = NULL) 
{
    if (!is.null(xml)) 
        xml.parsed <- xmlParse(xml)
    requests.xml <- xpathApply(xml.parsed, "//QualificationRequest")
    if (length(requests.xml) > 0) {
        requests <- data.frame(matrix(nrow = length(requests.xml), 
            ncol = 6))
        names(requests) <- c("QualificationRequestId", "QualificationTypeId", 
            "SubjectId", "SubmitTime", "Test", "Answer")
        for (i in 1:length(requests.xml)) {
            value.type <- xmlName(xpathApply(requests.xml[[i]], 
                "node()[3]")[[1]])
            qreq <- xpathApply(xml.parsed, paste("//QualificationRequest[", 
                i, "]/QualificationRequestId", sep = ""))
            if (length(qreq) == 1) 
                requests[i, 1] <- xmlValue(qreq[[1]])
            qual <- xpathApply(xml.parsed, paste("//QualificationRequest[", 
                i, "]/", value.type, sep = ""))
            if (length(qual) == 1) 
                requests[i, 4] <- xmlValue(qual[[1]])
            subj <- xpathApply(xml.parsed, paste("//QualificationRequest[", 
                i, "]/SubjectId", sep = ""))
            if (length(subj) == 1) 
                requests[i, 2] <- xmlValue(subj[[1]])
            time <- xpathApply(xml.parsed, paste("//QualificationRequest[", 
                i, "]/SubmitTime", sep = ""))
            if (length(time) == 1) 
                requests[i, 3] <- xmlValue(time[[1]])
            test <- xpathApply(xml.parsed, paste("//QualificationRequest[", 
                i, "]/Test", sep = ""))
            if (length(test) == 1) 
                requests[i, 3] <- xmlValue(test[[1]])
            answ <- xpathApply(xml.parsed, paste("//QualificationRequest[", 
                i, "]/Answer", sep = ""))
            if (length(answ) == 1) 
                requests[i, 3] <- xmlValue(answ[[1]])
        }
        return(QualificationRequests = requests)
    }
    else return(NULL)
}
