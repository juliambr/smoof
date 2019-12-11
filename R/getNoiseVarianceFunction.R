#' @title
#' Get noise variance function .
#'
#' @description
#'
#' @param [\code{smoof_noisy_single_objective_function}]
#' @return [\code{\link[ParamHelpers]{ParamSet}}]
#' @examples
#' fn = makeSphereFunction(3L)
#' ps = getNoiseVarianceFunction(fn)
#' print(ps)
#' @name getNoiseVarianceFunction
#' @rdname getNoiseVarianceFunction
NULL

#' @export
getNoiseVarianceFunction = function(fn) {
  UseMethod("getNoiseVarianceFunction")
}

getNoiseVarianceFunction.smoof_noisy_single_objective_function = function(x) {
  return(attr(x, "fn.noise.var"))
}
