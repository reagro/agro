

plant_harvest <- function(x, min_prec=30, max_prec=90, max_len=5) {

# must have 12 values (months)
	stopifnot(length(x) == 12)
# cannot have any NAs	
	if (any(is.na(x))) return(matrix(NA, nrow=12, ncol=2))

# max_len is the number of months
	stopifnot(max_len > 0 & max_len <= 12)
# min precipitation threshold below max threshold
	stopifnot(min_prec < max_prec)

# compute actual threshold
	med <- stats::median(x)
# clamp it between min_prec and max_prec	
	prec_threshold <- min(max_prec, max(min_prec, med))

# make 12 months, from July to July, to make it easier 
# to compute across year boundaries (from Dec to Jan)	
	x24 <- c(x[7:12], x, x[1:6])
	
# which months are above the threshold?	
	above_th <- x24 >= prec_threshold

# cumulate successive months above the threshold
	y <- cumsum(above_th)

# remove months below the threshold and reset 
# the beginning of a sequence to 1
	wet <- (y - cummax(y * !above_th))

# go back to 12 months (Jan to Dec)
	wet <- wet[7:18]

# set up output	
	planting <- harvest <- rep(0, 12)	

# find the length of the growing season
	m <- min(max_len, max(wet))
	
# growing season must be at least 3 months	
	if (m > 2) {
	# harvest months	
		harvest[wet >= m] <- 1
	# planting months
		p <- which(wet >= m) - m + 1
	# 1 month before January -> December
		p[p < 1] <- 12 + p[p < 1]
		planting[p] <- 1
	}
	cbind(planting=planting, harvest=harvest)
}

