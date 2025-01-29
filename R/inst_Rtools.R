# ----------
# Check/Install Rtools
# ----------
req_packs = c("pkgbuild","installr","Rtools")

for(pack in req_packs){
	# pack = req_packs[1]; pack
	
	chk_pack = tryCatch(find.package(pack),
		error = function(ee){NULL})
	chk_pack

	if( !is.null(chk_pack) ){
		suppressPackageStartupMessages(library(pack,
			character.only = TRUE))
		next
	}
	
	if( pack == "Rtools" && .Platform$OS.type == "windows" ){
		
		chk_rtools = pkgbuild::find_rtools()
		chk_rtools = chk_rtools && pkgbuild::check_rtools()
		chk_rtools = chk_rtools && pkgbuild::has_rtools()
		rtools_dir = tryCatch(pkgbuild::rtools_path(),
			error = function(ee){""})
		rtools_dir = gsub("\\\\","/",rtools_dir); rtools_dir
		chk_rtools = chk_rtools && all(dir.exists(rtools_dir))
		chk_rtools = chk_rtools && grepl("rtools",Sys.getenv("PATH"))
		gpp_dir = Sys.which("g++")
		chk_rtools = chk_rtools && all(file.exists(gpp_dir))
		chk_rtools = chk_rtools && all(grepl("rtools",gpp_dir))
		chk_rtools
		
		if( chk_rtools ) next
		
		bb = tryCatch(installr::install.Rtools(check = FALSE,
			check_r_update = FALSE),error = function(ee){NULL})
		if( !is.null(bb) ) next
		stop("Some error in Rtools")
		
	} else {
		bb = tryCatch(install.packages(pack),
			error = function(ee){NA})
		if( !is.null(bb) && is.na(bb) ) 
			stop(sprintf("Error with %s",pack))
		
	}
	
}

###

