\name{run.simulation}
\alias{run.simulation}
\title{Runs a simulation experiment}
\usage{
run.simulation(params, preparers = list(), ...)
}
\arguments{
  \item{params}{A named list of parameter vectors.}

  \item{preparers}{A named list of functions, defaults to
  \code{list()}.}

  \item{...}{Further parameters passed down to the main
  \code{\link[plyr]{llply}} call}
}
\value{
An object of class \code{grex}.
}
\description{
This function runs an experiment for all combinations of
  parameters from a list of parameter vectors.  It can be viewed as a
  variant of \code{\link{expand.grid}} that allows running code at each
  expansion.  The main contribution of this function is a breadth-first
  expansion of the parameter space, as opposed to a depth-first expansion
  that would occur, e.g., if implementing such an expansion as plain
  nested loops.  This allows for fair parallelization (workload can be
  distributed almost uniformly over the available processors) which is
  difficult to achieve with a depth-first approach.
}
\examples{
run.simulation(list(a=1:2, b=2:4, experiment=TRUE),
               list(a=function(a) list(x=a*a),
                    b=function(a, b) if (b < 4) list(y=b-3) else list(y=a),
                    experiment=function(x, y) list(z=x-y)))
}
\seealso{
\code{\link[plyr]{llply}}, \code{\link{expand.grid}}
}

