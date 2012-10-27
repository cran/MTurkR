GenerateAnswerKey <-
function (questions, scoring) 
{
    answerkey <- newXMLNode("AnswerKey", namespaceDefinitions = "http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2005-10-01/AnswerKey.xsd")
    for (i in 1:length(questions)) {
        question <- newXMLNode("Question", parent = answerkey)
        id <- newXMLNode("QuestionIdentifier", questions[[i]]$QuestionIdentifier, 
            parent = question)
        addChildren(question, id)
        opts <- subset(q[[i]], names(q[[i]]) == "AnswerOption")
        for (j in 1:length(opts)) {
            opt <- newXMLNode("AnswerOption", parent = question)
            for (k in 1:(length(opts[[j]]) - 1)) {
                selid <- newXMLNode("SelectionIdentifier", opts[[j]][[k]], 
                  parent = opt)
                addChildren(opt, selid)
            }
            score <- newXMLNode("AnswerScore", opts[[j]]$AnswerScore, 
                parent = opt)
            addChildren(opt, score)
        }
        if (!is.null(questions[[i]]$DefaultScore)) {
            default <- newXMLNode("DefaultScore", questions[[i]]$DefaultScore, 
                parent = question)
            addChildren(question, default)
        }
        addChildren(answerkey, question)
    }
    if (!is.null(scoring)) {
        if (!names(scoring)[1] %in% c("PercentageMapping", "ScaleMapping", 
            "RangeMapping")) 
            stop("'scoring' must be PercentageMapping | ScaleMapping | RangeMapping | NULL")
        else {
            qualnode <- newXMLNode("QualificationValueMapping", 
                parent = answerkey)
            scorenode <- newXMLNode(names(scoring), parent = qualnode)
            if (names(scoring)[1] == "RangeMapping") {
                for (i in 1:length(scoring$RangeMapping)) {
                  scorerange <- newXMLNode("SummedScoreRange", 
                    parent = scorenode)
                  lower <- newXMLNode("InclusiveLowerBound", 
                    scoring$RangeMapping[[i]]$InclusiveLowerBound, 
                    parent = scorerange)
                  upper <- newXMLNode("InclusiveUpperBound", 
                    scoring$RangeMapping[[i]]$InclusiveUpperBound, 
                    parent = scorerange)
                  value <- newXMLNode("QualificationValue", scoring$RangeMapping[[i]]$QualificationValue, 
                    parent = scorerange)
                  addChildren(scorerange, c(lower, upper, value))
                  addChildren(scorenode, scorerange)
                }
                rangenode <- newXMLNode("OutOfRangeQualificationValue", 
                  scoring[[1]]$OutOfRangeQualificationValue, 
                  parent = scorenode)
                addChildren(scorenode, rangenode)
            }
            else if (names(scoring)[1] == "PercentageMapping") {
                valuenode <- newXMLNode("MaximumSummedScore", 
                  scoring[[1]], parent = scorenode)
                addChildren(scorenode, valuenode)
            }
            else if (names(scoring)[1] == "ScaleMapping") {
                valuenode <- newXMLNode("ScaleMapping", scoring[[1]], 
                  parent = scorenode)
                addChildren(scorenode, valuenode)
            }
            addChildren(qualnode, scorenode)
            addChildren(answerkey, qualnode)
        }
    }
    string <- toString.XMLNode(answerkey)
    encoded <- curlEscape(string)
    invisible(list(xml.parsed = answerkey, string = string, url.encoded = encoded))
}
