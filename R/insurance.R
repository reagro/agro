
utility <- function(income, rho, scale=FALSE) {

	income <- pmax(1, income)
	rho[rho==1] <- 0.99
	u <- (income ^ (1 - rho)) / (1 - rho)
	if (scale) {
		u <- u - min(u)
		u <- u / max(u)
	}
	u
}

ce_utility <- function(utility, rho){
	u <- mean(utility)
	if (rho == 1) { 
		exp(u)
	} else {
		((1-rho) * u) ^ (1/(1-rho))
	}
}



ce_income <- function(income, rho){
	u <- utility(income, rho, scale=FALSE)
	ce_utility(u, rho)
}


mqs <- function(ce_insured, ce_not_insured) {
   m <- sign(ce_insured - ce_not_insured) + 2
   c("negative", "neutral", "postive")[m]
}



