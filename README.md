# smartr

A R package with modified R functions for data exploration and other features.

## Installation

```R
# Dependencies
req_packs = c("Rcpp","RcppArmadillo","devtools",
	"BiocManager")
all_packs = as.character(installed.packages()[,1])

for(pack in req_packs){
	if( pack %in% all_packs ) next
	stop(sprintf("Install R package = %s",pack))

}

# Install package
if( !("smartr" %in% installed.packages()[,1]) )
	devtools::install_github("pllittle/smartr")
```

