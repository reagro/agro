# Author: Robert J. Hijmans
# Sept 2019
# version 1
# license GPL3


.get_repos <- function() {
	harv=cbind("dataverse.harvard.edu", c("AfricaRice", "IFPRI", "RiceResearch", "CIAT", "WorldFish", "ICRAF", "Bioversity", "cardresearch", "horticulture", "SIIL", "gfc"))
	
	harv <- cbind(harv, harv[,2])
	harv[harv[,2]=="RiceResearch",2] <- "IRRI"
	harv[harv[,2]=="cardresearch",2] <- "CARD"
	harv[harv[,2]=="horticulture",2] <- "FTF_HORT"
	harv[harv[,2]=="SIIL",2] <- "FTF_SIIL"
	harv[harv[,2]=="gfc",2] <- "FTF_GFC"
		
	self <- matrix(c("data.cipotato.org", "CIP", "data.mel.cgiar.org", "ICARDA", "data.cifor.org", "CIFOR", "data.cimmyt.org", "CIMMYT", "dataverse.icrisat.org", "ICRISAT", "dataverse.cirad.fr", "CIRAD", "dataverse.ird.fr", "IRD", "data.inra.fr", "INRA"), ncol=2, byrow=TRUE)
	self <- cbind(self, ":root")
	
	sch <- cbind("dataverse.scholarsportal.info", "GUELPH-AG", "ugardr")
	
	repos <- rbind(harv, self, sch)
	repos <- cbind(repos, "dataverse")


	ckan <- cbind(c("data.iita.org", "data.ilri.org"), c("IITA", "ILRI"), c("", "portal"), rep("CKAN", 2))
	repos <- rbind(repos, ckan)
	colnames(repos) <- c("server", "label", "root", "type")
	repos <- data.frame(repos, stringsAsFactors=FALSE)
	repos$api <- ifelse(repos$label %in% c("CIRAD", "INRA"), FALSE, TRUE)
	repos$protocol <- ifelse(repos$type == "CKAN", "http://", "https://")
	repos$protocol[repos$label == "ICRISAT"] <- "http://"
	repos
}

.getdomain <- function(x) strsplit(gsub("http://|https://|www\\.", "", x), "/")[[c(1, 1)]]
.getprotocol <- function(x) paste0(strsplit(x, "/")[[c(1, 1)]], "//")
.removeprotocol <- function(x) gsub("http://|https://|www\\.", "", x)

get_simple_URI <- function(uri) {
	ur <- .removeprotocol(uri)
	if (isTRUE(grep("dx.doi.org/", ur)==1)) {
		u <- gsub("dx.doi.org/", "", ur)
		u <- paste0("doi_", u)
	} else if (isTRUE(grep("doi.org/", ur)==1)) {
		u <- gsub("doi.org/", "", ur)
		u <- paste0("doi_", u)
	} else if (isTRUE(grep("persistentId=doi:", ur)==1)) {
		u <- unlist(strsplit(ur, "persistentId=doi:"))[2]
		u <- paste0("doi_", u)
	} else if (isTRUE(grep("^doi:", ur)==1)) {
		u <- gsub("^doi:", "doi_", ur)		
	} else if (isTRUE(grep("persistentId=hdl:", ur)==1)) {
		u <- unlist(strsplit(ur, "persistentId=hdl:"))[2]
		u <- paste0("hdl_", u)
	} else if (isTRUE(grep("^hdl:", ur)==1)) {
		u <- gsub("^hdl:", "hdl_", ur)		
	} else if (isTRUE(grep("hdl.handle.net/", ur)==1)) {
		u <- gsub("hdl.handle.net/", "", ur)
		u <- paste0("hdl_", u)
	} else {
		stop("???")
	}
	gsub("/", "_", u)
}

## only for dataverse, ckan tbd
get_data_from_uri <- function(uri, path, uripath=TRUE, unzip=TRUE) {

	uname <- get_simple_URI(uri)
	if (uripath) path <- file.path(path, uname)
	zipf0 <- file.path(path, paste0(uname, ".zip"))
	zipf1 <- file.path(path, paste0(uname, "_1.zip"))

	if ((file.exists(zipf0) || file.exists(zipf1))) {
		zipf <- list.files(path, paste0(uname, ".*zip$"), full.names=TRUE)		
	} else {
		if (isTRUE(grep("^doi:", uri)==1)) {
			uri <- gsub("^doi:", "https://dx.doi.org/", uri)
		} else if (isTRUE(grep("^hdl:", uri)==1)) {
			uri <- gsub("^hdl:", "https://hdl.handle.net/", uri)
		}
		zipf <- zipf1
		dir.create(path, FALSE, TRUE)
		if (!file.exists(path)) {
			stop(paste("cannot create path:", path))
		}
		x <- httr::GET(uri)
		stopifnot(x$status == 200)
		u <- x$url
		domain <- .getdomain(u)
		protocol <- .getprotocol(u)
		baseu <- paste0(protocol, domain)
		pid <- unlist(strsplit(u, "\\?"))[2]
		uu <- paste0(baseu, "/api/datasets/:persistentId?", pid)

		# the nice way
		#r <- httr::GET(uu)
		#httr::stop_for_status(r)
		#js <- httr::content(r, as = "text", encoding = "UTF-8")
		# but for cimmyt...
		tmpf <- tempfile()
		download.file(uu, tmpf, quiet=TRUE)
		js <- readLines(tmpf, encoding = "UTF-8", warn=FALSE)
		js <- jsonlite::fromJSON(js)
		fjs <- js$data$latestVersion$files
		jsp <- jsonlite::toJSON(js, pretty=TRUE)
		writeLines(jsp, file.path(path, paste0(uname, ".json")))
		f <- if(is.null(fjs$dataFile)) {fjs$datafile} else {fjs$dataFile}
		f$checksum <- NULL
		f$tabularTags <- NULL
		fn <- file.path(path, paste0(uname, "_files.txt"))
		try(write.csv(f, fn))
		
		rest <- f$restricted
		if (!is.null(rest)) {
			f <- f[!rest, ]
			if (nrow(f) == 0) {
				stop("access to the files is restricted")
			}
			warning("access to some files is restricted")
		}
		if (nrow(f) == 0) {
			stop("no files!")
		}
	
		if (sum(f$originalFileSize, na.rm=TRUE) < 10000000) {
			files <- paste0(f$id, collapse = ",")
			fu <- paste0(protocol, domain, "/api/access/datafiles/", files, "?format=original")
			download.file(fu, zipf, mode="wb", quiet=TRUE)
		} else {
			f$originalFileSize[is.na(f$originalFileSize)] <- 0
			i <- 1
			zipf <- NULL
			while(TRUE) {
				print(paste("part", i)); flush.console()
				cs <- cumsum(f$originalFileSize)
				k <- which (cs < 9000000)
				if (length(k) == 0) k <- 1
				files <- paste0(f$id[k], collapse = ",")
				fu <- paste0(protocol, domain, "/api/access/datafiles/", files, "?format=original")
				zipi <- file.path(path, paste0(uname, "_", i, ".zip"))
				download.file(fu, zipi, mode="wb", quiet=TRUE)
				f <- f[-k,]
				zipf <- c(zipf, zipi)
				if (nrow(f) == 0) break
				i <- i + 1
			}
		}		
	}
	
	allzf <- NULL
	for (z in zipf) {
		zf <- unzip(z, list=TRUE)
		zf <- zf$Name[zf$Name != "MANIFEST.TXT"]
		allzf <- c(allzf, zf)
		if (unzip) {
			ff <- list.files(path)
			there <- (zf %in% ff)
			if (!all(there)) {
				unzip(z, zf[!there], exdir = path)
			}	
		}
	}
	zf <- grep("\\.pdf$", allzf, value=TRUE, invert=TRUE)
	zf <- file.path(path, zf)
	try(make_recipe(uri, zf, file.path(path, paste0(uname, ".yaml"))))
	return(zf)
}



#uri <- "https://doi.org/10.7910/DVN/YDQDJH"
#ff <- get_data_from_uri(uri, ".", quiet=TRUE)

