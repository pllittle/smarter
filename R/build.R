#' @importFrom RCurl getURLContent
#' @importFrom utils read.table write.table
#'	getCRANmirrors install.packages installed.packages
#'	remove.packages
#' @importFrom devtools install_github install
#'	document check build
#' @importFrom usethis use_gpl3_license
#' @importFrom rmarkdown pandoc_available
#' @importFrom grDevices hcl rgb
#' @importFrom graphics abline axis barplot boxplot
#'	hist lines mtext par text layout image
#' @importFrom stats cor density var quantile qnorm
#'	hclust dist as.dist as.dendrogram
#' @importFrom gplots colorpanel 
#' @importFrom Rcpp sourceCpp compileAttributes
#' @useDynLib smarter
NULL

# Steps to create/check/install package from directory
# bb = strsplit(getwd(),"/")[[1]]; pack_dir = paste(bb[-length(bb)],collapse = "/")
# pack = strsplit(pack_dir,"/")[[1]]; pack = pack[length(pack)]
# if( pack %in% installed.packages()[,1] ){ remove.packages(pack); q("no") }
# Rcpp::compileAttributes(pkgdir = pack_dir)
# devtools::document(pkg = pack_dir); usethis::use_gpl3_license()
# devtools::check(pkg = pack_dir,manual = TRUE,cran = TRUE,error_on = "note")
# devtools::install(pack_dir)
# bb = readLines(file.path(pack_dir,"DESCRIPTION")); vers = strsplit(bb[grepl("Version",bb)]," ")[[1]][2]; vers
# devtools::build(pkg = pack_dir,path = sprintf("C:/Users/Admin/Desktop/%s_%s.tar.gz",pack,vers))



####
