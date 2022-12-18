// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

#ifdef _OPENMP
#include <omp.h>
#endif

// --------------------
// smartr Functions
// --------------------

// [[Rcpp::export]]
double Rcpp_logSumExp(const arma::vec& log_x){
	if( log_x.n_elem == 1 ){
		return log_x.at(0);
	} else {
		double max_val = arma::max(log_x);
		arma::vec log_x_2 = log_x - max_val;
		return std::log(arma::sum(arma::exp(log_x_2))) + max_val;
	}
}

// [[Rcpp::export]]
void Rcpp_round(arma::vec& vv,const arma::uword& digits){
	double scale = std::pow(10,digits);
	vv = arma::round(vv * scale) / scale;
}

// [[Rcpp::export]]
void Rcpp_chk_threads(const arma::uword& NN,
	const int& ncores = 1){
	
	arma::uvec vec_threads = arma::zeros<arma::uvec>(NN);
	
	vec_threads.zeros();
	#ifdef _OPENMP
	# pragma omp parallel for schedule(static) \
		num_threads(ncores) \
		shared(NN,vec_threads)
	#endif
	for(arma::uword ii = 0; ii < NN; ii++){
		#ifdef _OPENMP
		vec_threads.at(ii) = omp_get_thread_num();
		#else
		vec_threads.at(ii) = 0;
		#endif
	}
	vec_threads.t().print("Static: vec_threads = ");
	
	vec_threads.zeros();
	#ifdef _OPENMP
	# pragma omp parallel for schedule(dynamic) \
		num_threads(ncores) \
		shared(NN,vec_threads)
	#endif
	for(arma::uword ii = 0; ii < NN; ii++){
		#ifdef _OPENMP
		vec_threads.at(ii) = omp_get_thread_num();
		#else
		vec_threads.at(ii) = 0;
		#endif
	}
	vec_threads.t().print("Dynamic: vec_threads = ");
	
}
