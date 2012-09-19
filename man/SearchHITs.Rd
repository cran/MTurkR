\name{SearchHITs}
\alias{SearchHITs}
\alias{searchhits}
\title{Search your HITs}
\description{Search for your HITs and return those HITs as R objects.}
\usage{
SearchHITs(	response.group = NULL, return.all = TRUE, 
			pagenumber = "1", pagesize = "10", 
			sortproperty = "Name", sortdirection = "Ascending", 
			keypair = credentials(), print = TRUE, log.requests = TRUE, sandbox = FALSE, 
			return.hit.dataframe = TRUE, return.qual.dataframe = TRUE)
}
\arguments{
  \item{response.group}{An optional character string specifying what details of each HIT to return of: \dQuote{Minimal}, \dQuote{HITQuestion}, \dQuote{HITDetail}, \dQuote{HITAssignmentSummary}. For more information, see \url{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_HITDataStructureArticle.html}.}
  \item{return.all}{A logical indicating whether all HITs (as opposed to a specified page of the search results) should be returned. Default is \code{TRUE}.}
  \item{pagenumber}{An optional character string indicating which page of search results should be returned. Most users can ignore this.}
  \item{pagesize}{An optional character string indicating how many search results should be returned by each request. Most users can ignore this.}
  \item{sortproperty}{Most users can ignore this.}
  \item{sortdirection}{Most users can ignore this.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server. Default is \code{FALSE}.}
  \item{return.hit.dataframe}{A logical indicating whether the dataframe of HITs should be returned. Default is \code{TRUE}.}
  \item{return.qual.dataframe}{A logical indicating whether the list of each HIT's QualificationRequirements (stored as dataframes in that list) should be returned. Default is \code{TRUE}.}
}
\details{
Retrieve your current HITs (and, optionally, characteristics thereof). To view HITs on the MTurk requester website, see \code{\link{OpenManageHITPage}}. To view HITs on the MTurk worker website, use \code{\link{ViewAvailableHITs}}.

\code{searchhits()} is an alias.
}
\value{Optionally a dataframe containing the HITs and a list of each HIT's QualificationRequirements (stored as dataframes in that list).}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_SearchHITsOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
\seealso{
\code{\link{GetHIT}}

\code{\link{GetReviewableHITs}}

\code{\link{SearchQualificationTypes}}

\code{\link{ViewAvailableHITs}}
}
\examples{
\dontrun{
SearchHITs()
}
}
\keyword{HITs}