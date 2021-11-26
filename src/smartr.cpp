// [[Rcpp::depends(RcppArmadillo)]]

#include <RcppArmadillo.h>

// --------------------
// smartr Functions
// --------------------

// [[Rcpp::interfaces(r,cpp)]]

// [[Rcpp::export(Rcpp_logSumExp)]]
double Rcpp_logSumExp(const arma::vec& log_x){
	if( log_x.n_elem == 1 ){
		return log_x.at(0);
	} else {
		double max_val = arma::max(log_x);
		arma::vec log_x_2 = log_x - max_val;
		return std::log(arma::sum(arma::exp(log_x_2))) + max_val;
	}
}

// [[Rcpp::export(Rcpp_round)]]
void Rcpp_round(arma::vec& vv,const arma::uword& digits){
	double scale = std::pow(10,digits);
	vv = arma::round(vv * scale) / scale;
}


