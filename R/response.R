# Author: Robert Hijmans
# January 2010
# License GPL3

varImportance <- function(mod, dat, vars, n=10) {
	rmse <- matrix(nrow=n, ncol=length(vars))
	colnames(rmse) <- vars
	for (i in 1:length(vars)) {
	rd <- dat
	v <- vars[i]
		for (j in 1:n) {
			rd[[v]] <- sample(rd[[v]])
			p <- stats::predict(mod, rd)
			rmse[j,i] <- RMSE(rd$SOC, p)
		}
	}
	return(rmse)
}


partialResponse <- function(model, data, variable, rng=NULL, nsteps=25) {
	if (is.factor(data[[variable]])) {
		steps <- levels(data[[variable]])
	} else {
		if (is.null(rng)) { 
			rng <- range(data[[variable]]) 
		}
		increment <- (rng[2] - rng[1])/(nsteps-2)
		steps <- seq(rng[1]-increment, rng[2]+increment, increment)
	}
	res <- rep(NA, length(steps))
	for (i in 1:length(steps)) {
		data[[variable]] <- steps[i]
		p <- stats::predict(model, data)
		res[i] <- mean(p)
	}
	data.frame(variable=steps, p=res)
}


partialResponse2 <- function(model, data, var1, var2, var2levels, rng=NULL, nsteps=25) {
	if (is.factor(data[[var1]])) {
		steps <- levels(data[[var1]])
	} else {
		if (is.null(rng)) { 
			rng <- range(data[[var2]]) 
		}
		increment <- (rng[2] - rng[1])/(nsteps-2)
		steps <- seq(rng[1]-increment, rng[2]+increment, increment)
	}
	res <- rep(NA, length(steps))
	out <- data.frame(var=steps)	
	for (v in var2levels) {
		data[[var1]] <- v
		for (i in 1:length(steps)) {
			data[[var1]] <- steps[i]
			p <- stats::predict(model, data)
			res[i] <- mean(p)
		}
		out[[paste(var2, v, sep="_")]] <- res
	}
	out
}


