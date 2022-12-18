# Steps to create/check/install package from directory

rm(list = ls())
pack_dir = getwd()
pack_dir = strsplit(pack_dir,"/")[[1]]
pack_dir = pack_dir[-length(pack_dir)]
pack_dir = paste(pack_dir,collapse = "/")
pack_dir

pack = strsplit(pack_dir,"/")[[1]]
pack = pack[length(pack)]
pack

chk_pack = tryCatch(find.package(pack),
	error = function(ee){NULL}); chk_pack
if( !is.null(chk_pack) ){
	remove.packages(pack)
	q("no")
}

req_packs = c("usethis","Rtools",
	"Rcpp","RcppArmadillo","devtools")
for(tmp_pack in req_packs){
	# tmp_pack = req_packs[8]; tmp_pack
	
	chk_pack2 = tryCatch(find.package(tmp_pack),
		error = function(ee){NULL})
	chk_pack2

	if( !is.null(chk_pack2) ){
		suppressPackageStartupMessages(library(tmp_pack,
			character.only = TRUE))
		next
	}
	
	if( tmp_pack == "Rtools" ){
		
		source(file.path(pack_dir,"R/inst_Rtools.R"))
		cat("Rtools is installed!\n")
		
	} else {
		bb = tryCatch(install.packages(tmp_pack),
			error = function(ee){NA})
		if( !is.null(bb) && is.na(bb) ) 
			stop(sprintf("Error with %s",tmp_pack))
		
	}
	
}

Sys.getenv("_R_CHECK_SYSTEM_CLOCK_")
Sys.setenv("_R_CHECK_SYSTEM_CLOCK_" = 0)

compileAttributes(pkgdir = pack_dir)
document(pkg = pack_dir)
use_gpl3_license()

devtools::check(pkg = pack_dir,
	manual = !TRUE,cran = TRUE,
	vignettes = FALSE,
	error_on = "note")

# Install locally
install(pack_dir)

# Build package for CRAN
bb = readLines(file.path(pack_dir,"DESCRIPTION"))
vers = strsplit(bb[grepl("Version",bb)]," ")[[1]][2]; vers
user_dir = gsub("\\\\","/",Sys.getenv("USERPROFILE"))
gz_fn = file.path(user_dir,sprintf("Desktop/%s_%s.tar.gz",
	pack,vers)); gz_fn
build(pkg = pack_dir,path = gz_fn)

# Delete built package
if( file.exists(gz_fn) )
	unlink(gz_fn)


###
