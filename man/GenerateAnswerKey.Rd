\name{GenerateAnswerKey}
\alias{GenerateAnswerKey}
\title{Generate AnswerKey}
\description{Generate an AnswerKey data structure for a Qualification test.}
\usage{
GenerateAnswerKey(questions, scoring)
}
\arguments{
  \item{questions}{A list containing a QuestionIdentifiers, AnswerOptions, AnswerScores, and DefaultScores. See MTurk API Documentation.}
  \item{scoring}{A list containing a QualificationValueMapping. See MTurk API Documentation.}
}
\details{This generates an AnswerKey data structure (from a list), which serves to automatically score a Qualification test, as specified in the \code{test} parameter of \code{\link{CreateQualificationType}}. An AnswerKey data structure is also returned by \code{\link{GetQualificationType}}.}
\value{A list containing an AnswerKey data structure as an parsed XML tree, character string containing that tree, and a url-encoded character string.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_AnswerKeyDataStructureArticle.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateQualificationType}}
}
\examples{
\dontrun{
qs <- list(Question=list(	QuestionIdentifier="Question1",
							AnswerOption=list(SelectionIdentifier="A", SelectionIdentifier="B", AnswerScore=15),
							AnswerOption=list(SelectionIdentifier="A", AnswerScore=10),
							AnswerOption=list(SelectionIdentifier="B", AnswerScore=10),
							DefaultScore=5),
			Question=list(	QuestionIdentifier="Question2",
							AnswerOption=list(SelectionIdentifier="D", AnswerScore=10) ) )

scoring1 <- list(	PercentageMapping=5 )

scoring2 <- list(	RangeMapping=list(	list(InclusiveLowerBound=0, InclusiveUpperBound=20, QualificationValue=5),
										list(InclusiveLowerBound=21, InclusiveUpperBound=100, QualificationValue=10)),
					OutOfRangeQualificationValue=0 )

ak1 <- GenerateAnswerKey(qs, scoring1)
ak2 <- GenerateAnswerKey(qs, scoring2)
}
}
\keyword{Qualifications}