# Author: Robert J. Hijmans
# Sept 2019
# version 1
# license GPL3

.getColnames <- function(filename, format) {
	if (format == "csv") {
		d <- read.csv(filename)
		return(colnames(d))
	} else {
		return(NULL)
	}
}


.getVars <- function(filename, format) {
	d <- try(.getColnames(filename, format))
	if (is.null(d)) return(d)
	if (class(d) == "try-error") return(NULL)
	
	v <- list()
	for (i in 1:length(d)) {
		v[[d[i]]][[1]] <- list(
			rename = d[i]
		)
	}	
	list(filename = filename, names=v)  
}


make_recipe <- function(uri, files, outfile=NULL) {
	x <- list()
	x$uri <- uri
	x$name
	ff <- vector(length(files), mode="list")
	vv <- ff
	hasV <- FALSE
	for (i in 1:length(files)) {
		f <- list(
			name=files[i],
			type = "data",
			format = tolower(tools::file_ext(files[i]))
		)
		ff[[i]] <- f
		if (f$format == "csv") {
			v <- .getVars(files[i], f$format)
			if (!is.null(v)) {
				vv[[i]] = v
				hasV <- TRUE
			}
		}
	}
	x$files = ff
	if (hasV) {
		x$variables = vv
	}
	if (!is.null(outfile)) {
		try( yaml::write_yaml(x, outfile) )
		invisible(x)
	} else {
		x
	}
}




read_from_recipe <- function(recipe) {


}


#uri <- "https://doi.org/10.7910/DVN/YDQDJH"
#ff <- get_data_from_uri(uri, ".", quiet=TRUE)

