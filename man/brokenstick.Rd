\name{brokenstick}
\alias{brokenstick}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Generates a brokenstick community }
\description{
  Generates a brokenstick community.
}
\usage{
brokenstick(n)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{n}{Number of species. }
}
\details{
  Brokenstick has been a popular null-model for abundance relations
  since the days of MacArthur.  Brokenstick community is generated so
  that species simultaneously divide the available community (or resources) in
  random pieces.  When these pieces are arranged in rank
  abundance order, the result looks very similar to log-Normal.
}
\value{
  Returns the ranked abundance proportions (sum = 1).
}
\references{
  Tokeshi, M. (1993). Species abundance patterns and community
  structure. Adv. Ecol. Res. 24, 111-186.

  Wilson, J.B. (1993). Would we recognize a Broken-Stick community if we
  found one? Oikos 67: 181-183.
}
\author{Jari Oksanen }

\seealso{ \code{\link{rad.lines}} for displaying results,
  \code{\link{hubbell.build}} for an alternative. }

\examples{
## Generate and plot a broken-stick community
comm <- brokenstick(30)
rad.lines(comm) 
}

\keyword{ distribution }% at least one, from doc/KEYWORDS




