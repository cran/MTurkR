\name{MTurkR.Wizard}
\alias{MTurkR.Wizard}
\alias{mturkr.wizard}
\title{Interactive MTurkR Mode}
\description{An interactive, menu-based wizard to perform slimmed down versions of most MTurkR functions. Designed for beginners and those with aversion to the programming required elsewhere in the package.}
\usage{
MTurkR.Wizard(graphics = FALSE, log.requests = TRUE, sandbox = NULL)
}
\arguments{
  \item{graphics}{Optionally use graphical menus, if available, via Tcl/tk. See \code{\link{menu}}. Default is \code{FALSE}.}
  \item{log.requests}{A logical specifying whether API requests should be logged. Default is \code{TRUE}. See \code{\link{readlogfile}} for details.}
  \item{sandbox}{Optionally execute all requests in the MTurk sandbox rather than the live server.  Default is \code{FALSE}.}
}
\details{
An interactive, menu-based wizard (with optionally graphical menus) to perform most basic MTurkR operations. It is intended as a way for MTurk (and MTurkR) beginners to quickly create and monitor HITs; approve and reject assignments; notify, bonus, and block/unblock workers; manage Qualifications; monitor MTurk statistics; and interact with the MTurk Requester User Interface (RUI). All functionality accepts basic inputs interactively and executes requests without programming individual commands.

\code{mturkr.wizard()} is an alias.
}
\value{Currently returns nothing.}
%\references{}
\author{Thomas J. Leeper}
%\note{}
%\seealso{}
\examples{
\dontrun{
MTurkR.Wizard()
}
}
