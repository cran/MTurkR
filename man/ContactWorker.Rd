\name{ContactWorker}
\alias{ContactWorker}
\alias{ContactWorkers}
\alias{contact}
\title{Contact Worker(s)}
\description{Contact one or more workers. This sends an email with specified subject line and body text to one or more workers. This can be used to recontact workers in panel/longitudinal research or to send follow-up work. Most likely will need to be used in tandem with \code{\link{GrantBonus}} to implement panels.}
\usage{
ContactWorker(	subjects, msgs, workers, keypair = credentials(), 
				print = FALSE, browser = FALSE, log.requests = TRUE, sandbox = FALSE)
}
\arguments{
  \item{subjects}{A character string containing subject line of an email, or a vector of character strings of of length equal to the number of workers to be contacted containing the subject line of the email for each worker. Maximum of 200 characters.}
  \item{msgs}{A character string containing body text of an email, or a vector of character strings of of length equal to the number of workers to be contacted containing the body text of the email for each worker. Maximum of 4096 characters.}
  \item{workers}{A character string containing a WorkerId, or a vector of character strings containing multiple WorkerIds.}
  \item{keypair}{A two-item character vector containing an AWS Access Key ID in the first position and the corresponding Secret Access Key in the second position. Set default with \code{\link{credentials}}.}
  \item{print}{Optionally print the results of the API request to the standard output. Default is \code{TRUE}.}
  \item{browser}{Optionally open the request in the default web browser, rather than opening in R. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute the request in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
}
\details{
Send an email to one or more workers, either with a common subject and body text or subject and body customized for each worker.

\code{ContactWorkers()} and \code{contact()} are aliases.
}
\value{A dataframe containing the list of workers, subjects, and messages, and whether the request to contact each of them was valid.}
\references{
\href{http://docs.amazonwebservices.com/AWSMechTurk/latest/AWSMturkAPI/ApiReference_NotifyWorkersOperation.html}{API Reference}
}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
a <- "Complete a follow-up survey for $.50"
b <- "Thanks for completing my HIT!
I will pay a $.50 bonus if you complete a follow-up survey by Friday at 5:00pm.
The survey can be completed at
http://www.surveymonkey.com/s/pssurvey?c=A1RO9UJNWXMU65."
c1 <- "A1RO9UEXAMPLE"
d <- ContactWorker(subjects=a,msgs=b,workers=c)

c2 <- c("A1RO9EXAMPLE1","A1RO9EXAMPLE2","A1RO9EXAMPLE3")
3 <- ContactWorker(subjects=a,msgs=b,workers=c2)
}
}
\keyword{Workers}