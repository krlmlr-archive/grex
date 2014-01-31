#' @title Runs a simulation experiment
#' @description This function runs an experiment for all combinations of
#'   parameters from a list of parameter vectors.  It can be viewed as a
#'   variant of \code{\link{expand.grid}} that allows running code at each
#'   expansion
#' @param params A named list of parameter vectors.
#' @param experiment A function.
#' @param preparer A named list of functions, defaults to \code{list()}.
#' @return An object of class \code{grex}.
#' @examples
#' run.simulation(list(a=1:3, b=2:5), function(x, y) list(z=x-y),
#'                list(a=function(a) list(x=a*a),
#'                     b=function(a, b) if (b < 4) list(y=b-3) else list(y=a)))
#' @export
run.simulation <- function(params, experiment, preparer) {
}
