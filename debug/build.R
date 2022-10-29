# Steps to create/check/install package from directory
rm(list = ls())
bb = strsplit(getwd(),"/")[[1]]
pack_dir = paste(bb[-length(bb)],collapse = "/")
pack = strsplit(pack_dir,"/")[[1]]
pack = pack[length(pack)]
pack

chk_pack = tryCatch(find.package(pack),
	error = function(ee){NULL})
chk_pack

if( !is.null(chk_pack) ){
	remove.packages(pack)
	q("no")
}

req_packs = c("Rcpp","devtools","usethis")
for(tmp_pack in req_packs){
	
	chk_pack2 = tryCatch(find.package(tmp_pack),
		error = function(ee){NULL})
	chk_pack2

	if( !is.null(chk_pack2) ){
		library(tmp_pack,character.only = TRUE)
		next
	}
	
	stop(sprintf("Install package = %s",tmp_pack))
	
}

compileAttributes(pkgdir = pack_dir)
document(pkg = pack_dir)
use_gpl3_license()
check(pkg = pack_dir,
	manual = TRUE,cran = TRUE,
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
