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
		if( length(aa) == 0 ) return(NULL)
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

#' @title smart_prepPack
#' @description Test, check, install, and maybe build a package
#' @param pack_dir Defaults to \code{NULL}, assumes \code{getwd()} is the package directory.
#'	Otherwise, a character string for the local package directory.
#' @param pandoc Defaults to \code{NULL}. If specified, it is a character string
#'	for the full path of pandoc.
#' @param make_vign Boolean set to \code{FALSE} by default. If \code{TRUE},
#'	attempts to compile available vignettes when checking the package.
#' @param cran Boolean set to \code{TRUE} by default. If \code{FALSE},
#'	don't check CRAN specifications.
#' @param build_dir Defaults to \code{NULL}. Otherwise specify a
#'	character string for the path to build the package tar.gz file under.
#' @param verbose Boolean set to \code{TRUE} by default. Provides
#'	verbose output of package preparation.
#' @export
smart_prepPack = function(pack_dir = NULL,pandoc = NULL,
	make_vign = FALSE,cran = TRUE,build_dir = NULL,verbose = TRUE){
	
	if(FALSE){
		rm(list=ls())
		pack_dir = NULL
		pack_dir = "C:/Users/Admin/Desktop/github/ROKET"
		# pack_dir = "C:/Users/Admin/Desktop/github/smarter"
		pandoc = NULL
		make_vign = FALSE
		verbose = TRUE
		
	}
	
	if( is.null(pack_dir) ){
		bb = strsplit(getwd(),"/")[[1]]
		pack_dir = paste(bb[-length(bb)],collapse = "/")
	} else {
		if( !dir.exists(pack_dir) ) stop(sprintf("%s doesn't exist",pack_dir))
		setwd(pack_dir)
	}
	pack_dir
	
	if( verbose ) cat("Check if getwd() a package directory ...\n")
	pack_fns = list.files(pack_dir); pack_fns
	nms = c("DESCRIPTION","NAMESPACE","R")
	for(nm in nms){
		if( !any(grepl(sprintf("^%s$",nm),pack_fns)) )
			stop(sprintf("Add %s",nm))
	}
	
	pack = strsplit(pack_dir,"/")[[1]]
	pack = pack[length(pack)]
	pack
	
	if( pack %in% installed.packages()[,1] ){
		if( verbose ) cat(sprintf("%s already installed, removing ...\n",pack))
		remove.packages(pack)
		q("no")
	}
	
	if( any(grepl("src",pack_fns)) && length(list.files("src",pattern = ".cpp$")) > 0 ){
		if( verbose ) cat(sprintf("%s: Compiling %s's files ...\n",date(),pack))
		tryCatch(compileAttributes(pkgdir = pack_dir),
			error = function(ee){stop("Rcpp::compileAttributes() failed")})
	}
	
	if( verbose ) cat(sprintf("%s: Documenting %s ...\n",date(),pack))
	tryCatch(document(pkg = pack_dir),
		error = function(ee){stop("devtools::document() failed")})
	
	if( verbose ) cat(sprintf("%s: Licensing %s ...\n",date(),pack))
	tryCatch(use_gpl3_license(),
		error = function(ee){stop("usethis::use_gpl3_license() failed")})
	
	if( !is.null(pandoc) && is.character(pandoc) && file.exists(pandoc) 
		&& Sys.getenv("RSTUDIO_PANDOC") == "" )
		Sys.setenv("RSTUDIO_PANDOC" = pandoc)
	check_pandoc = pandoc_available()
	
	chk_vign = length(list.files("vignettes",pattern = ".Rmd$")) > 0
	make_vign = check_pandoc && chk_vign && make_vign
	
	if( verbose ) cat(sprintf("%s: Checking %s ...\n",date(),pack))
	tryCatch(check(pkg = pack_dir,manual = TRUE,cran = cran,
		error_on = "note",vignettes = make_vign),
		error = function(ee){stop("devtools::check() failed")})
	
	if( verbose ) cat(sprintf("%s: Installing %s ...\n",date(),pack))
	tryCatch(install(pkgdir = pack_dir,build_vignettes = make_vign),
		error = function(ee){stop("devtools::install() failed")})
	
	bb = readLines(file.path(pack_dir,"DESCRIPTION"))
	vers = strsplit(bb[grepl("Version",bb)]," ")[[1]][2]
	vers
	
	if( !is.null(build_dir) ){
		if( !dir.exists(build_dir) ) dir.create(build_dir)
		if( verbose ) cat(sprintf("%s: Building %s ...\n",date(),pack))
		tryCatch(build(pkg = pack_dir,path = file.path(build_dir,
			sprintf("%s_%s.tar.gz",pack,vers))),
			error = function(ee){stop("devtools::build() failed")})
	}
	
	return(NULL)
}

###

