# smartr
A R package with modified R functions for data exploration and other features.

## Installation

```
# Install dependencies
req_packs = c("Rcpp","RcppArmadillo","devtools",
	"BiocManager")

if( !all(req_packs %in% installed.packages()[,1]) ){
	miss_packs = req_packs[!(req_packs %in% all_packs)]
	stop(sprintf("Missing package(s): %s",paste(miss_packs,collapse = ",")))
}

# Install package
if( !("smartr" %in% installed.packages()[,1]) )
	devtools::install_github("pllittle/smartr")
```
