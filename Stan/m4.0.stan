data {                                                                                      
  	int<lower=0> N;                                                                           
  	array[N] vector[2] y;                                                                     
  	vector[2] m0;                                                                             
  	matrix[2, 2] P0;                                                                          
  	int h;                                                                                    
  	// seasonal                                                                               
 	array[N+h] int month;                                                                     
 	int period;                                                                               
 	int dft; // only 1 works, otherwise it conflicts with ar                                  
 	                                                                                          
 	// covariates                                                                             
 	/*                                                                                        
 	int J;                                                                                    
 	matrix[N, J] X;  */    
 	// garch
 	int garch;
}                                                                                           
transformed data {                                                                          
 	int D = 2;                                                                                
 	matrix[D, D] I;                                                                           
 	matrix[dft*2, N+h] c;                                                                     
 	I = diag_matrix(rep_vector(1, D));                                                        
 	// Build up the matrix of seasonality vectors constants (see prophet's paper, section 3.2)
 	for(t in 1:N+h) {                                                                         
 	  for(i in 1:(dft*2)){                                                                    
 	    if(i % 2 == 0)                                                                        
 	      continue;                                                                           
 	    c[i,t] = cos(2 * pi() * i * month[t] / period);                                       
 	    c[(i+1),t] =  sin(2 * pi() * i * month[t] / period);                                  
 	  }                                                                                       
 	}                                                                                         
}                                                                                           
parameters {                                                                                
 	vector<lower=0>[D] sigma_procs;                                                           
 	vector<lower=0>[D] sigma_obs;                                                             
 	corr_matrix[D] Rho_x;                                                                     
 	vector<lower=0>[D] q0;                                                                    
 	// cov_matrix[D] Q1;                                                                         
 	// cov_matrix[D] Q2;      
 	vector<lower=0, upper=1>[D] q1;    
 	vector<lower=0, upper=1>[D] q2;    
 	matrix[D, dft*2] C;                                                                       
 	vector<lower=0, upper=1>[D] ar;                                                           
 	vector[D] level;                                                                          
}                                                                                           
transformed parameters {                                                                    
 	array[N+h] vector[D] m;                                                                   
 	array[N+h] matrix[D, D] P;                                                                
 	array[N+h] vector[D] m_pred;                                                              
 	array[N+h] matrix[D, D] P_pred;                                                           
 	array[N+h] matrix[D, D] S;                                                                
 	array[N+h] vector[D] mu;                                                                  
 	array[N+h] matrix[D, D] Q;                                                                
 	matrix[D, D] R;                                                                           
 	array[N+h] vector[D] v;                                                                   
 	vector[N] Dz;                                                                             
	{                                                                                           
 	  matrix[D, D] Z;                                                                         
 	  row_vector[D] H;                                                                        
 	  vector[D] Ht;                                                                           
 	  array[N+h] matrix[D, D] K;                                                              
 	  array[N+h] vector[D] xi;                                                                
 	                                                                                          
 	  Z = diag_matrix(ar);                                                                    
 	  //Q = diag_matrix(sigma_procs) * Rho_x * diag_matrix(sigma_procs);                      
 	  R = diag_matrix(sigma_obs) /* Rho_y */* diag_matrix(sigma_obs);                         
 	  v = rep_array([0,0]', N+h);                                                             
	                                                                                            
 	  for(t in 1:2) {                                                                         
 	    m_pred[t] = m0;                                                                       
 	    P_pred[t] = P0;                                                                       
 	    Q[t] = diag_matrix([1,1]');                                                           
 	    xi[t] = [1,1]';                                                                       
 	  }                                                                                       
 	  for (t in 1 : N+h) {                                                                    
 	    if (t > 2) {     
 	      if(garch)
 	        xi[t] = q0                                                                          
 	                  + q1 .* (y[t-1] - y[t-2])^2                                              
 	                  + q2 .* xi[t-1];                                                         
 	      else
 	        xi[t] = q0;     
 	      Q[t] = quad_form_diag(Rho_x, xi[t]);                                                
 	      m_pred[t] = Z * m[t - 1] + level + C * c[,t];                                       
 	      P_pred[t] = Z * P[t - 1] * Z' + Q[t];                                               
 	    }                                                                                     
 	    v[t] = y[t] - m_pred[t];                                                              
 	    S[t] = P_pred[t] + R;                                                                 
 	    K[t] = P_pred[t] / S[t];                                                              
 	    m[t] = m_pred[t] + K[t] * v[t];                                                       
 	    P[t] = (I - K[t]) * P_pred[t];                                                        
 	    mu[t] = m_pred[t];                                                                    
 	  }                                                                                       
 	                                                                                          
 	}                                                                                         
}                                                                                           
model {                                                                                     
 	sigma_procs ~ exponential(1);                                                             
 	sigma_obs ~ exponential(1);                                                               
 	Rho_x ~ lkj_corr(D);                                                                      
 	//Rho_y ~ lkj_corr(D);                                                                    
 	q0 ~ exponential(1);                                                                      
//  	Q1 ~ inv_wishart(D, diag_matrix(rep_vector(1, D)));                                       
// 	Q2 ~ inv_wishart(D, diag_matrix(rep_vector(1, D)));                                       
	for(i in 1:D) C[i] ~ normal(0, 1);                                                        
	level ~ cauchy(0, 1);                                                                     
	ar ~ beta(10, 2);                                                                         
	for (i in 1 : N+h)                                                                        
	  y[i] ~ multi_normal(mu[i], S[i]);                                                       
 }                                                                                           
 generated quantities {                                                                      
	array[N] real log_lik;                                                                    
	array[N+h] vector[D] y_hat;                                                               
	for (i in 1 : N)                                                                          
	    log_lik[i] = multi_normal_lpdf(y[i] | mu[i], S[i]);                                   
	for (i in 1 : N+h)                                                                        
	    y_hat[i] = multi_normal_rng(mu[i], S[i]);                                             
 }                                                                                           
