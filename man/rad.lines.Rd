\name{rad.lines}
\alias{rad.lines}
\alias{logn.fun}
\alias{logn.ml}
\title{ Ranked Abundance -- Dominance plots and fits RAD models  }
\description{
  The function produces Ranked Abundance -- Dominance plots and fits
  some popular models to these plots
}
\usage{
rad.lines(x, sticks=10, main=NULL, legend.add=TRUE, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{Vector of species abundance values. }
  \item{sticks}{Number of brokenstick simulations added to the plot. }
  \item{main}{Main title for the plot }
  \item{legend.add}{Add a legend to line types. }
  \item{\dots}{Other graphical parameters to \code{\link{plot}}. }
}
\details{
  Ranked Abundance -- Diversity (RAD) graphs plot logarithmic abundance on
  vertical axis and corresponding rank on horizontal axis.  It is
  possible to identify some species--abundance models in these plots.
  If species have a simple pre-empting hierarchy, plots should be
  linear.  In most other models, such as \code{\link{brokenstick}},
  Preston's log-Normal or in Hubbell's ``zero-sum multinomial'' model
  the pattern of points should be more or less sigmoid.

  The function fits two alternative models following Wilson (1991):
  pre-emption model and log-Normal model.  Motomura's pre-emption model
  is simply a linear regression in RAD plot.  Log-Normal model is fitted
  in two alternative ways.  Moment estimate uses mean and standard
  deviation of log-transformed abundances for plotting a normal quantile
  curve.  Maximum Likelihood estimate fits normal quantiles directly to
  log-transformed abundances.
}
\value{
  The function returns invisibly the fit results and plot data.
}
\references{
  Hubbell, S.P. (2001). The Unified Neutral Theory of Biodiversity and
  Biogeography.  Princeton Univ. Press.
  
  Wilson, J. B. (1991). Methods for fitting dominance/diversity
  curves. J. Veg. Sci. 2: 35-46.
}
\author{ Jari Oksanen }
\note{ Still preliminary. }

 
\seealso{ \code{\link{brokenstick}} for generating brokenstick community,
  \code{\link{hubbell.build}} for Hubbell's RAD model, \code{\link{qnorm}}
    for Normal quantiles.}

  \examples{
## Take a random site from the Carabid data
data(carabid)
take <- sample(1:nrow(carabid), 1)
rad.lines(carabid[take,])
## Would we recognize a brokenstick community if we found one?
comm <- brokenstick(30)
rad.lines(comm) 
}
}
\keyword{ hplot }% at least one, from doc/KEYWORDS
\keyword{ models }

