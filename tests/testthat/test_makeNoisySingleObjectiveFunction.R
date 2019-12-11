context("makeNoisySingleObjectiveFunction")

test_that("makeNoisySingleObjectiveFunction", {
	name = "Noisy test function"
	description = "Noisy test function description"
	tags = c("unimodal", "noisy")
	par.set = makeParamSet(
		makeNumericParam("x1", lower = -1, upper = 1),
		makeNumericParam("x2", lower = -1, upper = 1)
	)
	fn.mean = function(x) sum(x^2)
	fn.noise  = function(x) sum(x) # linear noise 

	fn = makeNoisySingleObjectiveFunction(
		name = name,
    	id = "id",
		description = description,
		tags = tags,
		fn.mean = fn.mean, fn.noise = fn.noise, noise.type = "x",
		min.noise.var = 0.1, max.noise.var = 0.5,
		par.set = par.set
	)

	expect_true(isSmoofFunction(fn))
	expect_true(isNoisy(fn))
	expect_true(sd(replicate(n = 10, fn(1))) > 0)


	# corner cases: constant noise function
	fn.noise  = function(x) 1 
	fn = makeNoisySingleObjectiveFunction(
		name = name,
    	id = "id",
		description = description,
		tags = tags,
		fn.mean = fn.mean, fn.noise = fn.noise, noise.type = "x",
		min.noise.var = 0.1, max.noise.var = 0.5,
		par.set = par.set,
		global.opt.params = list(x1 = 0, x2 = 0)
	)

	expect_true(attr(fn, "noise.type") == "constant")
	fn.noise.var = attr(fn, "fn.noise.var")
	# constant noise function 
	expect_true(sd(replicate(n = 100, fn.noise.var(c(1, 1)))) == 0)
	# noise.var.fun should be constant and equal the min.noise.var
	# expect_true(fn.noise.var(c(1, 1)) == 0.1)

	# corner cases: min.noise.var = max.noise.var
	fn.noise  = function(x) sum(x)
	fn = makeNoisySingleObjectiveFunction(
		name = name,
    	id = "id",
		description = description,
		tags = tags,
		fn.mean = fn.mean, fn.noise = fn.noise, noise.type = "x",
		min.noise.var = 0.5, max.noise.var = 0.5,
		par.set = par.set,
		global.opt.params = list(x1 = 0, x2 = 0)
	)

	expect_true(attr(fn, "noise.type") == "constant")
	fn.noise.var = attr(fn, "fn.noise.var")
	# constant noise function 
	expect_true(sd(replicate(n = 100, fn.noise.var(c(1, 1)))) == 0)
	# noise.var.fun should be constant and equal the min.noise.var
	# expect_true(fn.noise.var(c(1, 1)) == 0.5)

})

