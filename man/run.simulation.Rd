\name{run.simulation}
\alias{run.simulation}
\title{Runs a simulation experiment}
\usage{
run.simulation(params, experiment, preparer)
}
\arguments{
  \item{params}{A named list of parameter vectors.}

  \item{experiment}{A function.}

  \item{preparer}{A named list of functions, defaults to
  \code{list()}.}
}
\value{
An object of class \code{grex}.
}
\description{
This function runs an experiment for all combinations of
  parameters from a list of parameter vectors.  It can be viewed as a
  variant of \code{\link{expand.grid}} that allows running code at each
  expansion
}
\examples{
run.simulation(list(a=1:3, b=2:5), function(x, y) list(z=x-y),
               list(a=function(a) list(x=a*a),
                    b=function(a, b) if (b < 4) list(y=b-3) else list(y=a)))
}

