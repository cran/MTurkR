GenerateAnswerKey <-
function (questions, scoring = NULL) 
{
    answerkey <- newXMLNode("AnswerKey", namespaceDefinitions = "http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2005-10-01/AnswerKey.xsd")
    for (i in 1:dim(questions)[1]) {
        question <- newXMLNode("Question", parent = answerkey)
        id <- newXMLNode("QuestionIdentifier", questions$QuestionIdentifier[i], 
            parent = question)
        addChildren(question, id)
        opt <- newXMLNode("AnswerOption", parent = question)
        selid <- newXMLNode("SelectionIdentifier", questions$SelectionIdentifier[i], 
            parent = opt)
        score <- newXMLNode("AnswerScore", questions$AnswerScore[i], 
            parent = opt)
        addChildren(opt, c(selid, score))
        if (!is.null(questions$DefaultScore[i])) {
            default <- newXMLNode("DefaultScore", questions$DefaultScore[i], 
                parent = question)
            addChildren(question, default)
        }
        addChildren(answerkey, question)
    }
    if (!is.null(scoring)) {
        if (!is.null(scoring$Type) && !scoring$Type %in% c("PercentageMapping", 
            "ScaleMapping", "RangeMapping")) 
            stop("'scoring$Type' must be PercentageMapping | ScaleMapping | RangeMapping | NULL")
        else if (!is.null(scoring$Type)) {
            qualnode <- newXMLNode("QualificationValueMapping", 
                parent = answerkey)
            scorenode <- newXMLNode(scoring$Type, parent = qualnode)
            if (scoring$Type == "RangeMapping") {
                for (i in 1:dim(scoring$RangeMapping)[1]) {
                  scorerange <- newXMLNode("SummedScoreRange", 
                    parent = scorenode)
                  lower <- newXMLNode("InclusiveLowerBound", 
                    scoring$RangeMapping$InclusiveLowerBound[i], 
                    parent = scorerange)
                  upper <- newXMLNode("InclusiveUpperBound", 
                    scoring$RangeMapping$InclusiveUpperBound[i], 
                    parent = scorerange)
                  value <- newXMLNode("QualificationValue", scoring$RangeMapping$QualificationValue[i], 
                    parent = scorerange)
                  addChildren(scorerange, c(lower, upper, value))
                  addChildren(scorenode, scorerange)
                }
                rangenode <- newXMLNode("OutOfRangeQualificationValue", 
                  scoring$OutOfRangeQualificationValue, parent = scorenode)
                addChildren(scorenode, rangenode)
            }
            else if (scoring$Type == "PercentageMapping") {
                valuenode <- newXMLNode("MaximumSummedScore", 
                  scoring$MaximumSummedScore, parent = scorenode)
                addChildren(scorenode, valuenode)
            }
            else if (scoring$Type == "ScaleMapping") {
                valuenode <- newXMLNode("ScaleMapping", scoring$SummedScoreMultiplier, 
                  parent = scorenode)
                addChildren(scorenode, valuenode)
            }
            addChildren(qualnode, scorenode)
            addChildren(answerkey, qualnode)
        }
    }
    string <- toString.XMLNode(answerkey)
    encoded <- curlEscape(string)
    return(list(xml.parsed = answerkey, string = string, url.encoded = encoded))
}
