#' @title Runs a simulation experiment
#' @description This function runs an experiment for all combinations of
#'   parameters from a list of parameter vectors.  It can be viewed as a
#'   variant of \code{\link{expand.grid}} that allows running code at each
#'   expansion
#' @param params A named list of parameter vectors.
#' @param preparers A named list of functions, defaults to \code{list()}.
#' @return An object of class \code{grex}.
#' @examples
#' run.simulation(list(a=1:2, b=2:4, experiment=TRUE),
#'                list(a=function(a) list(x=a*a),
#'                     b=function(a, b) if (b < 4) list(y=b-3) else list(y=a),
#'                     experiment=function(x, y) list(z=x-y)))
#' @export
#' @importFrom plyr llply
run.simulation <- function(params, preparers=list()) {
  stopifnot(!is.null(names(params)))
  stopifnot(names(params) == unique(names(params)))
  stopifnot(length(preparers) == 0L || !is.null(names(preparers)))
  stopifnot(length(setdiff(names(preparers), names(params))) == 0L)

  params.len <- length(params)
  param.frame <- data.frame(row.names=1)
  envs <- list(list())

  for (param.index in seq_along(params)) {
    param <- params[[param.index]]
    param.name <- names(params)[[param.index]]
    param.len <- length(param)
    stopifnot(param.len > 0L)

    new.param.frame <- data.frame(param)
    colnames(new.param.frame) <- param.name
    new.param.frame <- merge(param.frame, new.param.frame, by=c())

    new.envs <- llply(
      seq_len(nrow(new.param.frame)),
      function(row) {
        env.pos <- (row - 1L) %/% param.len + 1
        env <- envs[[env.pos]]
        if (!is.null(preparer <- preparers[[param.name]])) {
          call.env <- c(env,
                        as.list(new.param.frame[row, , drop=F]),
                        params[seq(from=param.index + 1, length.out=params.len - param.index)])
          preparer.args <- formals(preparer)
          preparer.args.names <- names(preparer.args)
          stopifnot(preparer.args.names %in% names(call.env))

          return.env <- do.call(preparer, call.env[preparer.args.names])
          stopifnot(!any(names(return.env) %in% colnames(new.param.frame)))
          env[names(return.env)] <- return.env
        }
        env
      }
    )

    param.frame <- new.param.frame
    envs <- new.envs
  }

  structure(
    list(
      grid=param.frame,
      results=envs
    ),
    class="grex"
  )
}
