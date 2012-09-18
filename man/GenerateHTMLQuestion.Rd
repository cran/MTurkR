\name{GenerateHTMLQuestion}
\alias{GenerateHTMLQuestion}
\title{Generate HTMLQuestion}
\description{Generate an HTMLQuestion data structure for use in the \sQuote{Question} parameter of \code{\link{CreateHIT}}.}
\usage{
GenerateHTMLQuestion(character = NULL, file = NULL)
}
\arguments{
  \item{character}{An optional character string from which to construct the HTMLQuestion data structure.}
  \item{file}{An optional file handle from which to construct the HTMLQuestion data structure.}
}
\details{Must specify either \code{character} or \code{file}.}
\value{A character string containing a URL query parameter-formatted HTMLQuestion data structure for use in the \code{question} parameter of \code{\link{CreateHIT}}.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HTMLQuestionArticle.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{CreateHIT}}

\code{\link{GenerateExternalQuestion}}

\code{\link{GenerateHITLayoutParameter}}
}
\examples{
\dontrun{
GenerateHTMLQuestion(file="mynewhit.html")
}
}
\keyword{HITs}