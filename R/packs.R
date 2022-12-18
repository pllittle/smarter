# ----------
# Package Functions
# ----------

#' @title chkInst_PACK
#' @description Check package is installed
#' @param PACK A character string for a package name
#' @return Boolean for \code{TRUE} if package installed
#'	and \code{FALSE} if package is not installed or located
#' @export
chkInst_PACK = function(PACK){
	
	chk_pack = tryCatch(find.package(PACK),
		error = function(ee){NULL})
	chk_pack
	
	output = ifelse(is.null(chk_pack),FALSE,TRUE)
	return(output)
	
}

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
		aa0 = getURLContent(pack_html)
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


###

