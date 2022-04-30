# smarter

<!-- badges: start -->
<img src="https://img.shields.io/badge/c++-%2300599C.svg?style=for-the-badge&logo=c%2B%2B&logoColor=gold" alt="Girl in a jacket" width="100" height="40">
![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![CRAN status](https://www.r-pkg.org/badges/version/smarter)
<!-- badges: end -->

A R package with modified R functions for data exploration and other features.

## Installation

Copy/paste the following code to install R package dependencies and **smarter**.

```R
# Dependencies
req_packs = c("Rcpp","RcppArmadillo","devtools",
	"BiocManager","smarter")
all_packs = as.character(installed.packages()[,1])
rerun = 0

for(pack in req_packs){
	if( pack %in% all_packs ){
		library(package = pack,character.only = TRUE)
		next
	}
	
	bb = NULL
	
	if( pack %in% "smarter" ){
		bb = tryCatch(devtools::install_github("pllittle/smarter",
			dependencies = TRUE),
			error = function(ee){"error"})
	} else {
		bb = tryCatch(devtools::install.packages(pkgs = pack,
			dependencies = TRUE),
			error = function(ee){"error"})
	}
	
	if( !is.null(bb) && bb == "error" )
		stop(sprintf("Error for package = %s",pack))
	rerun = 1

}

if( rerun == 1 ) stop("Re-run above code")

```

## Future ideas
