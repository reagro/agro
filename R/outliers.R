
mean_nol_2sd <- function(x) {
    x <- stats::na.omit(x)  
    m <- stats::median(x)
    r <- 2 * stats::sd(x)
    i <- abs(x - m) < r
    mean(x[i])
}


mean_nol_2iq <- function(x) {
    x <- stats::na.omit(x)  
    m <- stats::median(x)
    r <- stats::quantile(x, c(0.25, 0.75))
    i <- abs(x - m) < (2 * diff(r))
    mean(x[i])
}

