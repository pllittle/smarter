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

// [[Rcpp::export(Rcpp_chk_threads)]]
void Rcpp_chk_threads(const arma::uword& NN,
	const int& ncores = 1){
	
	arma::uvec vec_threads = arma::zeros<arma::uvec>(NN);
	
	vec_threads.zeros();
	omp_set_num_threads(ncores);
	# pragma omp parallel for schedule(static) \
		shared(NN,vec_threads)
	for(arma::uword ii = 0; ii < NN; ii++){
		vec_threads.at(ii) = omp_get_thread_num();
	}
	vec_threads.t().print("Static: vec_threads = ");
	
	vec_threads.zeros();
	omp_set_num_threads(ncores);
	# pragma omp parallel for schedule(dynamic) \
		shared(NN,vec_threads)
	for(arma::uword ii = 0; ii < NN; ii++){
		vec_threads.at(ii) = omp_get_thread_num();
	}
	vec_threads.t().print("Dynamic: vec_threads = ");
	
}
