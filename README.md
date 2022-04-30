# smartr

A R package with modified R functions for data exploration and other features.

## Installation

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
