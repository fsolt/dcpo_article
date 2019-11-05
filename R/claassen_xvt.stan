// bayesian latent trait model 5 for "Estimating Smooth Panels"
// item intercepts / biases, no slopes / factor loadings
// item-country intercepts
// single autoregressive error
// responses are modelled as beta-binomial count

data{
  int<lower=1> N;               		// number of national survey opinions, training set
  int<lower=1> J;               		// number of countries
  int<lower=1> K;  	        			// number of items
  int<lower=1> P;  	        			// number of items-country combinations
  int<lower=1> T;  	        			// number of years
  int<lower=1,upper=J> jj[N];   		// country j for opinion n
  int<lower=1,upper=K> kk[N];   		// item k for opinion n
  int<lower=1,upper=P> pp[N];   		// item-country p for opinion n
  int<lower=1,upper=T> tt[N];   		// year t for opinion n
  int<lower=1> x[N];   			    	// vector of survey responses, count
  int<lower=1> samp[N];					// vector of sample sizes
  int<lower=1> N_test;          // number of national survey opinions, test set
  int<lower=1,upper=J> jj_test[N_test];   // country j for opinion n, test set
  int<lower=1,upper=K> kk_test[N_test];   // item k for opinion n, test set
  int<lower=1,upper=P> pp_test[N_test];   // item-country p for opinion n, test set
  int<lower=1,upper=T> tt_test[N_test];   // year t for opinion n, test set
  int<lower=1> samp_test[N_test];					// vector of sample sizes, test set
}

parameters{
  real<lower=0> sigma_theta;	        // opinion evolution error variance
  real<lower=0> sigma_lambda;	        // item intercepts error variance
  real<lower=0> sigma_delta;	        // item-country intercept error variance
  vector[K-1] lambda_raw;           		// raw item intercepts
  matrix[T,J] theta_raw; 	        	// raw matrix of T by J latent traits
  vector[P] delta_raw;					// raw item-country intercepts
  row_vector[J] theta_init;				// initial latent traits for first year
  real mu_lambda;			 			// expectation of item intercepts
  real<lower=0> phi;					// dispersion parameter
}

transformed parameters{
  matrix[T,J] theta; 	                // matrix of T by J latent traits
  vector[N] theta_tt_jj;				// N-vector for expanded theta vales
  vector[N_test] theta_tt_jj_test;  // N-vector for expanded theta values, test set
  vector<lower=0,upper=1>[N] eta;      	// fitted values, on logit scale
  vector<lower=0>[N] alpha;					// beta alpha parameter
  vector<lower=0>[N] beta;					// beta beta parameter
  vector<lower=0,upper=1>[N_test] eta_test;      	// fitted values, on logit scale, test set
  vector<lower=0>[N_test] alpha_test;					// beta alpha parameter, test set
  vector<lower=0>[N_test] beta_test;					// beta beta parameter, test set
  vector[K] lambda;           			// item intercepts
  vector[P] delta;						// item-country intercepts
  theta[1] = theta_init;
  lambda[1] = 1;							// constrain initial intercept
  lambda[2:K] = mu_lambda + sigma_lambda * lambda_raw[1:K-1];
  delta = sigma_delta * delta_raw;
  for (t in 2:T)                        // parameter expansion for theta
	theta[t] = theta[t-1] + sigma_theta * theta_raw[t-1];
  for (i in 1:N)
	theta_tt_jj[i] = theta[tt[i], jj[i]];// expand theta to N-vector
  eta = inv_logit(lambda[kk] + theta_tt_jj + delta[pp]); // fitted values model
  alpha = phi * eta; 						// reparamaterise beta-binom lambda par
  beta = phi * (1 - eta); 					// reparamaterise beta-binom beta par

  for (n in 1:N_test) {
    theta_tt_jj_test[n] = theta[tt_test[n], jj_test[n]];
  }
  eta_test = inv_logit(lambda[kk_test] + theta_tt_jj_test + delta[pp_test]); // fitted values model
  alpha_test = phi * eta_test; 						// reparamaterise beta-binom lambda par
  beta_test = phi * (1 - eta_test); 					// reparamaterise beta-binom beta par
}

model{
  x ~ beta_binomial(samp, alpha, beta); 			// response model
  phi ~ gamma(4, 0.1);
  sigma_theta ~ cauchy(0, 2);
  sigma_lambda ~ cauchy(0, 2);
  sigma_delta ~ cauchy(0, 2);
  mu_lambda ~ normal(1, 2);
  theta_init ~ normal(0, 1);
  lambda_raw ~ normal(0, 1);
  delta_raw ~ normal(0, 1);
  for(t in 1:T)
	theta_raw[t] ~ normal(0, 1);
}

generated quantities {
  vector[N_test] x_test;						// fitted data to check model
  for (n in 1:N_test) {
    x_test[n] = beta_binomial_rng(samp_test[n], alpha_test[n], beta_test[n]);
  }
}
