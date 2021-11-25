# ----------
# Package Functions
# ----------

#' @title smart_pack_versions
#' @description Return all associated package versions
#' @param pack A string for the package name
#' @param repo A string that takes values
#'	"CRAN", "aCRAN", and "cCRAN" for combining
#'	options "aCRAN" and "cCRAN". "cCRAN" refers
#'	to contributed packages. "aCRAN" refers
#'	to archived packages.
#' @export
smart_pack_versions = function(pack,repo = "CRAN"){
	
	if( !(repo %in% c("CRAN","aCRAN","cCRAN")) )
		stop(sprintf("repo = %s isn't an option",repo))
	
	if( repo == "cCRAN" ){ # To get newest pack version
		pack_html = sprintf("https://cran.r-project.org/web/packages/%s/index.html",pack)
		aa0 = getURLContent(pack_html)
		aa = strsplit(aa0,"\n")[[1]]
		aa = aa[grepl(".tar.gz",aa)]
		aa = strsplit(aa," ")[[1]]
		aa = aa[grepl(pack,aa)]
		aa = aa[!grepl("href=",aa)]
		aa = gsub(sprintf("%s_",pack),"",aa)
		aa = gsub(".tar.gz","",aa)
		return(aa)
	}
	if( repo == "aCRAN" ){ # To get list of package versions
		pack_html = sprintf("https://cran.r-project.org/src/contrib/Archive/%s/",pack)
		aa0 = RCurl::getURLContent(pack_html)
		aa = strsplit(aa0,"\n")[[1]]
		aa = aa[grepl("href=",aa)]
		aa = aa[grepl(pack,aa)]
		aa = gsub("\"","",aa)
		aa = t(sapply(aa,function(xx) strsplit(xx,"><")[[1]],USE.NAMES = FALSE))
		aa = aa[,c(6,8)] # package version and time
		aa[,1] = sapply(aa[,1],function(xx) strsplit(xx,"[><]")[[1]][2],USE.NAMES = FALSE)
		aa[,2] = sapply(aa[,2],function(xx) strsplit(xx,"[><]")[[1]][2],USE.NAMES = FALSE)
		aa[,2] = trimws(aa[,2])
		aa = smart_df(aa); names(aa) = c("filename","date")
		aa$version = sapply(aa$filename,function(xx) gsub(".tar.gz","",
			strsplit(xx,"_")[[1]][2]),USE.NAMES = FALSE)
		aa$year = as.integer(sapply(aa$date,function(xx) 
			strsplit(xx,"[ -]")[[1]][1],USE.NAMES = FALSE))
		aa$month = as.integer(sapply(aa$date,function(xx) 
			strsplit(xx,"[ -]")[[1]][2],USE.NAMES = FALSE))
		aa = aa[order(aa$year,aa$month),]
		aa$REPO = "aCRAN"
		return(aa)
	}
	if( repo == "CRAN" ){ # Combine cCRAN and aCRAN info
		aa = smart_pack_versions(pack = pack,repo = "aCRAN")
		bb = smart_pack_versions(pack = pack,repo = "cCRAN")
		bb = smart_df(filename = sprintf("%s_%s.tar.gz",pack,bb),
			date = "",version = bb,year = NA,month = NA,REPO = "cCRAN")
		aa = rbind(aa,bb)
		return(aa)
	}
	
}

#' @title smart_pack
#' @description Check if package installed or 
#'	install package
#' @param pack A string for the package name
#' @param repo A string that takes values
#'	"CRAN" for CRAN, "BIOC" for Bioconductor,
#'	"source" for a package installed from a 
#'	compressed file, "cCRAN" for contributed packages,
#'	"aCRAN" for archived packages, "github" for 
#'	installing packages directly from github, or
#'	"local" for installing a package from a local directory.
#' @param src_fn If repo = "source", specify 
#'	the full path to the source file
#' @param uname If \code{repo = "github"}, specify
#'	the github username or organization containing the repository
#' @param vers If repo = "cCRAN" or "aCRAN",
#'	specify the specific package version to install
#' @param pack_dir If repo = "local", specify
#'	the full path to the local package directory
#' @param all_packs Defaults to \code{NULL}. Otherwise
#'	a character vector of installed packages
#' @export
smart_pack = function(pack,repo = "CRAN",src_fn = "",uname = "",
	vers = "",pack_dir = "",all_packs = NULL){
	
	if( is.null(all_packs) ){
		all_packs = as.character(installed.packages()[,"Package"])
	}
	
	cran_url = "https://cran.r-project.org/src/contrib"
	
	if( !(pack %in% all_packs) ){
		aa = getCRANmirrors()
		aa = aa[which(aa[,"Country"] == "0-Cloud"),]
		if( repo == "CRAN" ){
			# aa; aa$URL
			install.packages(pkgs = pack,repos = aa$URL[1])
		} else if( repo == "BIOC" ){
			# source("https://bioconductor.org/biocLite.R")
			# biocLite(pack)
			if( !requireNamespace("BiocManager",quietly = TRUE) )
				install.packages(pkgs = "BiocManager",repos = aa$URL[1])
			BiocManager::install(pkg = pack,force = TRUE)
		} else if( repo == "source" ){
			if( !file.exists(src_fn) ) stop(sprintf("%s missing",src_fn))
			install.packages(pkgs = src_fn,repos = NULL,type = "source")
		} else if( repo == "cCRAN" ){
			if( vers == "" ) stop("Input version number!")
			pack_url = sprintf("%s/%s_%s.tar.gz",cran_url,pack,vers)
			install.packages(pkgs = pack_url,repos = NULL,type = "source")
		} else if( repo == "aCRAN" ){
			if( vers == "" ) stop("Input version number!")
			pack_url = sprintf("%s/Archive/%s/%s_%s.tar.gz",cran_url,pack,pack,vers)
			install.packages(pkgs = pack_url,repos = NULL,type = "source")
		} else if( repo == "github" ){
			if( uname == "" ) stop("Include uname!")
			install_github(sprintf("%s/%s",uname,pack))
		} else if( repo == "local" ){
			if( pack_dir == "" ) stop("Specify pack_dir!")
			if( !dir.exists(paths = pack_dir) )
				stop(sprintf("%s doesn't exist!",pack_dir))
			devtools::install(pack_dir)
		} else {
			stop(sprintf("repo = %s is not an option",repo))
		}
	}
	
}

###

