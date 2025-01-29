<div align="left">
<a href=""><img src="https://img.shields.io/badge/R-%23276DC3.svg?style=square&logo=r&logoColor=pink&label=smarter" height="80" /></a>
</div>

<!-- badges: start -->
![C++](https://img.shields.io/badge/C++-%2300599C.svg?style=square&logo=c%2B%2B&logoColor=gold)
![R](https://img.shields.io/badge/R-%23276DC3.svg?style=square&logo=r&logoColor=pink)
![CRAN status](https://www.r-pkg.org/badges/version/smarter)
[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![](https://img.shields.io/github/languages/code-size/pllittle/smarter.svg)](https://github.com/pllittle/smarter)
[![](https://img.shields.io/github/last-commit/pllittle/smarter.svg)](https://github.com/pllittle/smarter/commits/master)
<!-- badges: end -->

A R package with modified and recycled R functions for data exploration and other features.

## Table of Contents

* [News](#news)
* [Future Ideas](#future-ideas)
* [Installation](#installation)

## News

* 2025-01: smarter v1.0.0 published to [CRAN](https://cran.r-project.org/package=smarter)

## Future ideas

* Vignettes/Demos

## Installation

<details>
<summary>Install from CRAN</summary>

Copy/paste the following code to install R package from CRAN.

```R
install.packages("smarter")
```

</details>

<details>
<summary>Install development version</summary>

Copy/paste the following code to install R package dependencies and **smarter**.

```R
# Dependencies
req_packs = c("usethis","rmarkdown","Rcpp",
	"RcppArmadillo","devtools","BiocManager",
	"smarter")

for(pack in req_packs){
	
	chk_pack = tryCatch(find.package(pack),
		error = function(ee){NULL})
	
	if( !is.null(chk_pack) ){
		library(package = pack,character.only = TRUE)
		next
	}
	
	bb = NULL
	
	if( pack %in% "smarter" ){
		bb = tryCatch(install_github("pllittle/smarter",
			dependencies = TRUE),
			error = function(ee){"error"})
	} else {
		bb = tryCatch(install.packages(pkgs = pack,
			dependencies = TRUE),
			error = function(ee){"error"})
	}
	
	if( !is.null(bb) && bb == "error" )
		stop(sprintf("Error for package = %s",pack))
	
}

```

</details>

