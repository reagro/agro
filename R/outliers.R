
mean.nol.2sd <- function(x) {
    x <- na.omit(x)  
    m <- median(x)
    r <- 2 * sd(x)
    i <- abs(x - m) < r
    mean(x[i])
}


mean.nol.2iq <- function(x) {
    x <- na.omit(x)  
    m <- median(x)
    r <- quantile(x, c(0.25, 0.75))
    i <- abs(x - m) < (2 * diff(r))
    mean(x[i])
}

