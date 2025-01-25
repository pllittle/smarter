# ----------
# Optimization Related Functions
# ----------

#' @title logSumExp
#' @description Calculates the log(sum(exp(x)))
#'	in Rcpp
#' @param x A numeric vector
#' @return A numeric vector
#' @export
logSumExp = function(x){
	Rcpp_logSumExp(log_x = x)
}

#' @title smart_solve
#' @param mm A square numeric matrix
#' @return A square numeric matrix.
#' @export
smart_solve = function(mm){
	
	mat_rcond = rcond(mm)
	
	if( mat_rcond == 0 )
		NA
	else
		solve(mm,tol = 0.1 * mat_rcond)
	
}

#' @title chk_threads
#' @param NN A positive integer for total iterations
#'	to loop over
#' @param ncores A positive integer for number of threads
#' @return An integer for number of threads.
#' @export
chk_threads = function(NN,ncores){
	Rcpp_chk_threads(NN = NN,ncores = ncores)
}


###

