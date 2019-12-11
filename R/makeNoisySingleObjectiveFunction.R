#' Generator for noisy single-objective target functions.
#'
#' @template arg_name
#' @template arg_id
#' @template arg_description
#' @template arg_has_simple_signature
#' @template arg_par_set
#' @template arg_fn_mean
#' @template arg_minimize
#' @template arg_vectorized
#' @param tags [\code{character}]\cr
#'   Optional character vector of tags or keywords which characterize the function,
#'   e.~g. \dQuote{unimodal}, \dQuote{separable}. See \code{\link{getAvailableTags}} for
#'   a character vector of allowed tags.
#' @param fn.mean [\code{function}] \cr Mean function. Function that should be optimized.
#' @param noise.fn [\code{function}] \cr Noise variance function. Normal distributed noise is added on top 
#'   of the mean function \code{fn.mean}. This function is scaled to a range between \code{min.noise.var}\% and 
#'   \code{max.noise.var}\% of the range of the mean function.  
#' @param noise.type [\code{character}]\cr Either "x" if the noise should work on x, or y if the noise should be on y. 
#' @param min.noise.var [\code{numeric}] \cr Value between [0, 1]. Minimum noise variance in percentage of the range 
#'   of the range of the mean function. 
#' @param max.noise.var [\code{numeric}] \cr Value between [0, 1], \code{max.noise.var} >= \code{min.noise.var}. 
#'   Minimum noise variance in percentage of the range of the range of the mean function. 
#' @param global.opt.params [\code{list} | \code{numeric} | \code{data.frame} | \code{matrix} | \code{NULL}]\cr
#'   Default is \code{NULL} which means unknown. Passing a \code{numeric} vector will
#'   be the most frequent case (numeric only functions). In this case there is only a
#'   single global optimum. If there are multiple global optima, passing a numeric
#'   \code{matrix} is the best choice. Passing a \code{list} or a \code{data.frame}
#'   is necessary if your function is mixed, e.g., it expects both numeric and discrete
#'   parameters. Internally, however, each representation is casted to a \code{data.frame}
#'   for reasons of consistency.
#' @param global.opt.value [\code{numeric(1)} | \code{NULL}]\cr
#'   Global optimum value if known. Default is \code{NULL}, which means unknown. If
#'   only the \code{global.opt.params} are passed, the value is computed automatically.
#' @param local.opt.params [\code{list} | \code{numeric} | \code{data.frame} | \code{matrix} | \code{NULL}]\cr
#'   Default is \code{NULL}, which means the function has no local optima or they are
#'   unknown. For details see the description of \code{global.opt.params}.
#' @param local.opt.values [\code{numeric} | \code{NULL}]\cr
#'   Value(s) of local optima. Default is \code{NULL}, which means unknown. If
#'   only the \code{local.opt.params} are passed, the values are computed automatically.
#' @return [\code{function}] Objective function with additional stuff attached as attributes.
#' @examples
#' library(ggplot2)
#'
#' fn = makeSingleObjectiveFunction(
#'   name = "Sphere Function",
#'   fn = function(x) sum(x^2),
#'   par.set = makeNumericParamSet("x", len = 1L, lower = -5L, upper = 5L),
#'   global.opt.params = list(x = 0)
#' )
#' print(fn)
#' print(autoplot(fn))
#'
#' fn.num2 = makeSingleObjectiveFunction(
#'   name = "Numeric 2D",
#'   fn = function(x) sum(x^2),
#'   par.set = makeParamSet(
#'     makeNumericParam("x1", lower = -5, upper = 5),
#'     makeNumericParam("x2", lower = -10, upper = 20)
#'   )
#' )
#' print(fn.num2)
#' print(autoplot(fn.num2))
#'
#' fn.mixed = makeSingleObjectiveFunction(
#'   name = "Mixed 2D",
#'   fn = function(x) x$num1^2 + as.integer(as.character(x$disc1) == "a"),
#'   has.simple.signature = FALSE,
#'   par.set = makeParamSet(
#'     makeNumericParam("num1", lower = -5, upper = 5),
#'     makeDiscreteParam("disc1", values = c("a", "b"))
#'   ),
#'   global.opt.params = list(num1 = 0, disc1 = "b")
#' )
#' print(fn.mixed)
#' print(autoplot(fn.mixed))
#' @export
makeNoisySingleObjectiveFunction = function(
  name = NULL,
  id = NULL,
  description = NULL,
  fn.mean, fn.noise, noise.type = "x",
  min.noise.var, max.noise.var,
  has.simple.signature = TRUE,
  vectorized = FALSE,
  par.set,
  minimize = TRUE,
  tags = character(0),
  global.opt.params = NULL,
  global.opt.value = NULL,
  local.opt.params = NULL,
  local.opt.values = NULL) {

  # check if functions return a single objective value 
  x = generateDesign(n = 1, par.set)
  assertNumeric(fn.mean(x[1, ]), len = 1, finite = TRUE, any.missing = FALSE, all.missing = FALSE)
  assertNumeric(fn.noise(x[1, ]), len = 1, finite = TRUE, any.missing = FALSE, all.missing = FALSE)

  # check if the function is constant
  if (is.numeric(functionBody(fn.noise)) | min.noise.var == max.noise.var) {
    noise.type = "constant"
  }

  # scale the noise function to the [0, 1]
  # distinguish if noise is applied to y- or x-values
  if (noise.type == "x") {
    nrg = computeRange(fn.noise, par.set) 
    nfns = function(x) as.numeric((fn.noise(x) - nrg[1]) / (nrg[2] - nrg[1]))
  } else if (noise.type == "y") {
    nrg = computeRange(function(x) fn.noise(fn.mean(x)), par.set) 
    nfns = function(x) as.numeric((fn.noise(fn.mean(x)) - nrg[1]) / (nrg[2] - nrg[1]))
  } else if (noise.type == "constant") {
    nfns = function(x) 0 # is set to zero, corrected later
  } else {
    stop("Type of noise must either be x, y, or constant.")
  }
  
  frg = computeRange(fn.mean, par.set)
  frgl = frg[2] - frg[1]

  # scale the noise variance function to [min.noise.var, max.noise.var]
  nfns2 = function(x) as.numeric((nfns(x) * (max.noise.var - min.noise.var) + min.noise.var) * frgl)

  # scale noise fun such that it is scaled between zero and one
  fn = function(x) as.numeric(fn.mean(x) + nfns2(x) * rnorm(1, 0, 1))

  smoof.fn = makeObjectiveFunction(
    name, id, description, fn,
    has.simple.signature, par.set, 1L,
    TRUE, fn.mean, minimize, vectorized
  )

  #FIXME: currently we offer this only for single objective functions
  assertSubset(tags, choices = getAvailableTags(), empty.ok = TRUE)

  global.opt.params = preprocessOptima(global.opt.params, smoof.fn, par.set, "global")
  local.opt.params = preprocessOptima(local.opt.params, smoof.fn, par.set, "local")

  if (is.null(global.opt.value) && !is.null(global.opt.params)) {
    global.opt.value = fn.mean(global.opt.params[1, ]) # mean 
  }

  if (!is.null(global.opt.params) && !is.null(global.opt.value)) {
    assertNumber(global.opt.value, finite = TRUE) # mean 
  }

  if (is.null(local.opt.values) && !is.null(local.opt.params)) {
    # print(local.opt.params)
    # print(par.set)
    local.opt.values = apply(local.opt.params, 1, fn.mean)
  }

  if (!is.null(local.opt.params) && !is.null(local.opt.values)) {
    assertNumeric(local.opt.values, len = nrow(local.opt.params), finite = TRUE, any.missing = FALSE, all.missing = FALSE)
  }

  smoof.fn = setAttribute(smoof.fn, "global.opt.params", global.opt.params)
  smoof.fn = setAttribute(smoof.fn, "global.opt.value.mean", global.opt.value)
  smoof.fn = setAttribute(smoof.fn, "local.opt.params", local.opt.params)
  smoof.fn = setAttribute(smoof.fn, "local.opt.value.mean", local.opt.values)
  smoof.fn = setAttribute(smoof.fn, "tags", tags)
  smoof.fn = setAttribute(smoof.fn, "fn.noise.var", nfns2)
  smoof.fn = setAttribute(smoof.fn, "noise.type", noise.type)

  class(smoof.fn) = c("smoof_noisy_single_objective_function", "smoof_single_objective_function", class(smoof.fn))

  return(smoof.fn)
}


computeRange = function(fn, par.set = NULL) {

  if(isSmoofFunction(fn) & is.null(par.set)) {
    par.set = getParamSet(fn)
  }

  if(isSmoofFunction(fn) & !is.null(par.set)) {
    warning("A paramset was given, but is not used. The Paramset of the fn was used.")
  }

  if(!isSmoofFunction(fn) & is.null(par.set)) {
    stop("Please provide a paramset of the function. ")
  }

  des = generateDesign(10000, par.set)
  des$y = apply(des, 1, fn)

  range = data.frame(ymin = min(des$y), ymax = max(des$y))

  return(range)
}
