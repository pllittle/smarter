
# ----------
# Statistical Functions
# ----------

#' @title calc_JK
#' @param EST A numeric vector of parameter estimates
#' @param LOO_EST A numeric matrix of parameter estimates
#'	where columns correspond to each parameter and
#'	rows correspond to each leave one out estimate
#' @param alpha A numeric value for constructing
#'	(1 - alpha) * 100\% confidence intervals
#' @export
calc_JK = function(EST,LOO_EST,alpha = 0.05){
	
	LOO_EST = as.matrix(LOO_EST)
	if( length(EST) != ncol(LOO_EST) )
		stop("Dimension mismatch")
	
	# Calculate pseudovalues
	nn = nrow(LOO_EST); nn
	PSDO = apply(as.matrix(LOO_EST),1,function(xx){
		EST + (nn - 1) * (EST - xx)
	})
	PSDO = as.matrix(PSDO)
	range(PSDO)
	# hist(PSDO,breaks = 30)
	dim(PSDO)
	
	# Calculate mean
	JK_mean = apply(PSDO,2,mean); JK_mean
	
	# Calculate covariance
	JK_var = var(PSDO); JK_var
	
	# Calculate CI
	JK_CI = JK_mean + c(-1,1) * 
		qnorm(1 - alpha/2) * sqrt(diag(JK_var) / nn)
	
	out = list(JK_mean = JK_mean,
		JK_var = JK_var,JK_CI = JK_CI)
	# out
	
	return(out)
	
}


###

