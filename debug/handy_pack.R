

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
#' @export
smart_pack = function(pack,repo = "CRAN",src_fn = "",uname = "",
	vers = "",pack_dir = ""){
	
	chk_pack = tryCatch(find.package(pack),
		error = function(ee){NULL})
	if( !is.null(chk_pack) ) return(NULL)
	
	cran_url = "https://cran.r-project.org/src/contrib"
	
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
		pack_url = sprintf("%s/Archive/%s/%s_%s.tar.gz",
			cran_url,pack,pack,vers)
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

#' @title smart_prepPack
#' @description Test, check, install, and maybe build a package
#' @param pack_dir Character string for the local package directory.
#' @param pandoc Defaults to \code{NULL}. If specified, it is a character string
#'	for the full path of pandoc.
#' @param make_vign Boolean set to \code{FALSE} by default. If \code{TRUE},
#'	attempts to compile available vignettes when checking the package.
#' @param cran Boolean set to \code{TRUE} by default. If \code{FALSE},
#'	don't check CRAN specifications.
#' @param build_dir Defaults to \code{NULL}. Otherwise specify a
#'	character string for the path to build the package tar.gz file under.
#' @param quick Boolean for how to install package.
#' @param verbose Boolean set to \code{TRUE} by default. Provides
#'	verbose output of package preparation.
#' @export
smart_prepPack = function(pack_dir,pandoc = NULL,
	make_vign = FALSE,cran = TRUE,build_dir = NULL,
	quick = FALSE,verbose = TRUE){
	
	if( !dir.exists(pack_dir) ) 
		stop(sprintf("%s doesn't exist",pack_dir))
	
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
	
	# Check if package already installed
	chk_pack = tryCatch(find.package(pack),
		error = function(ee){NULL})
	if( !is.null(chk_pack) ){
		if( verbose ) 
			cat(sprintf("%s already installed, removing ...\n",pack))
		remove.packages(pack)
		q("no")
	}
	
	if( any(grepl("src",pack_fns)) 
		&& length(list.files("src",pattern = ".cpp$")) > 0 ){
		
		if( verbose ) cat(sprintf("%s: Compiling %s's files ...\n",date(),pack))
		
		tryCatch(Rcpp::compileAttributes(pkgdir = pack_dir),
			error = function(ee){stop("Rcpp::compileAttributes() failed")})
		
	}
	
	if( verbose ) cat(sprintf("%s: Documenting %s ...\n",date(),pack))
	tryCatch(devtools::document(pkg = pack_dir),
		error = function(ee){stop("devtools::document() failed")})
	
	if( verbose ) cat(sprintf("%s: Licensing %s ...\n",date(),pack))
	tryCatch(usethis::use_gpl3_license(),
		error = function(ee){stop("usethis::use_gpl3_license() failed")})
	
	old_PANDOC = Sys.getenv("RSTUDIO_PANDOC")
	if( !is.null(pandoc) && is.character(pandoc) 
		&& file.exists(pandoc) 
		&& Sys.getenv("RSTUDIO_PANDOC") == "" ){
		
		Sys.setenv("RSTUDIO_PANDOC" = pandoc)
		on.exit(Sys.setenv("RSTUDIO_PANDOC" = old_PANDOC))
		
	}
	check_pandoc = pandoc_available()
	
	chk_vign = length(list.files("vignettes",pattern = ".Rmd$")) > 0
	make_vign = check_pandoc && chk_vign && make_vign
	
	if( verbose ) cat(sprintf("%s: Checking %s ...\n",date(),pack))
	tryCatch(devtools::check(pkg = pack_dir,manual = TRUE,
		cran = cran,error_on = "note",vignettes = make_vign),
		error = function(ee){stop("devtools::check() failed")})
	
	if( verbose ) cat(sprintf("%s: Installing %s ...\n",date(),pack))
	tryCatch(devtools::install(pkgdir = pack_dir,
		build_vignettes = make_vign,
		quick = quick),
		error = function(ee){stop("devtools::install() failed")})
	
	desc_fn = file.path(pack_dir,"DESCRIPTION")
	bb = readLines(desc_fn)
	vers = strsplit(bb[grepl("Version",bb)]," ")[[1]][2]
	vers
	
	if( !is.null(build_dir) ){
		if( !dir.exists(build_dir) ) dir.create(build_dir)
		if( verbose ) cat(sprintf("%s: Building %s ...\n",date(),pack))
		tryCatch(devtools::build(pkg = pack_dir,
			path = file.path(build_dir,sprintf("%s_%s.tar.gz",pack,vers)),
			vignettes = make_vign),
			error = function(ee){stop("devtools::build() failed")})
	}
	
	return(NULL)
	
}

#' @title smart_packDeps
#' @description Install package with accompandied dependencies and load
#' @param cran_packs A character string of CRAN package names.
#' @param bioc_packs A character string of Bioconductor package names.
#' @param github_packs A character string of GitHub repositories of the form
#'	organization/repo or username/repo.
#' @param local_packs A character string of local full path names R packages
#' @inheritParams smart_prepPack
#' @param build_vign Boolean for building R package vignettes if available.
#' @export
smart_packDeps = function(cran_packs = NULL,bioc_packs = NULL,
	github_packs = NULL,local_packs = NULL,pandoc = NULL,build_vign = FALSE){
	
	# Dependencies
	cran_packs = c("BiocManager","devtools","rmarkdown",cran_packs)
	req_packs = c(cran_packs,bioc_packs,github_packs,local_packs)
	req_packs = unique(req_packs)
	if( is.null(req_packs) ) stop("Specify a package")
	if( length(req_packs) == 0 ) stop("No package specified")
	all_packs = as.character(installed.packages()[,1])
	rerun = 0
	
	# Check Pandoc
	if( !is.null(pandoc) && is.character(pandoc) && file.exists(pandoc) 
		&& Sys.getenv("RSTUDIO_PANDOC") == "" )
		Sys.setenv("RSTUDIO_PANDOC" = pandoc)
	check_pandoc = pandoc_available()
	build_vign = ( check_pandoc 
		&& Sys.getenv("RSTUDIO_PANDOC") != ""
		&& file.exists(Sys.getenv("RSTUDIO_PANDOC"))
		&& build_vign )
	
	for(pack in req_packs){
		pack2 = strsplit(pack,"/")[[1]]
		pack2 = pack2[length(pack2)]
		
		if( pack2 %in% all_packs ){
			library(package = pack2,character.only = TRUE)
			next
		}
		
		bb = NULL
		if( !is.null(github_packs) && pack %in% github_packs ){
			bb = tryCatch(install_github(pack,dependencies = TRUE,
				build_vignettes = build_vign),
				error = function(ee){"error"})
		} else if( !is.null(cran_packs) && pack %in% cran_packs ){
			bb = tryCatch(install.packages(pkgs = pack,dependencies = TRUE,
				build_vignettes = build_vign),
				error = function(ee){"error"})
		} else if( !is.null(bioc_packs) && pack %in% bioc_packs ){
			bb = tryCatch(BiocManager::install(pkg = pack,dependencies = TRUE,
				build_vignettes = build_vign),
				error = function(ee){"error"})
		} else if( !is.null(local_packs) && pack %in% local_packs ){
			if( !dir.exists(pack) ) stop(sprintf("%s doesn't exist",pack))
			bb = tryCatch(devtools::install(pack,dependencies = TRUE,
				build_vignettes = build_vign),
				error = function(ee){"error"})
		}
		
		if( !is.null(bb) && bb == "error" )
			stop(sprintf("Error for package = %s",pack2))
		rerun = 1

	}

	if( rerun == 1 ) stop("Re-run smart_packDeps()")
	
	return(NULL)
	
}

###
