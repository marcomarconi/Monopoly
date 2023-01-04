functions{
matrix cov_GPL2(matrix x, real sq_alpha, real sq_rho, vector delta) {
        int N = dims(x)[1];
        matrix[N, N] K;
        for (i in 1:(N-1)) {
          K[i, i] = sq_alpha + delta[i];
          for (j in (i + 1):N) {
            K[i, j] = sq_alpha * exp(-sq_rho * square(x[i,j]) );
            K[j, i] = K[i, j];
          }
        }
        K[N, N] = sq_alpha + delta[N];
        return K;
    }
}

data {                                                                                      
	int<lower=0> N;   
	int<lower=0> D;   
	array[N] vector[D] y;                                                                     
	vector[D] m0;                                                                             
	matrix[D, D] P0;                                                                          
	int h;            // NOT IMPLEMENTED                                                                           
  // seasonal                                                                               
 	array[N+h] int month;                                                                     
 	int period;                                                                               
 	int dft;                
 	// moving average
  int ma;   
  // with level
  int level;
  // Use a kernel for the correlation matrix, must provide a distance matrix for the series
  int gp;
  matrix[D,D] Dmat;
 	// covariates                                                                             
 	int J;                                                                                    
 	matrix[N, J] X; 
 	// garch
 	int garch;
  // Training set
  int trainset;
}                                                                                           
transformed data {                                                                          
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
 	vector<lower=0>[D] q0;                                                                    
  vector<lower=0>[D] r0;                                                             
 	array[garch==1] vector<lower=0, upper=1>[D] q1;    
 	array[garch==1] vector<lower=0, upper=1>[D] q2;  
 	//array[gp==0] corr_matrix[D] q_corr;
 	array[gp==0] cholesky_factor_corr[D] q_L_corr;   
 	array[gp==0] cholesky_factor_corr[D] r_L_corr;   
  array[gp==1] real<lower=0> q_alpha;
  array[gp==1] real<lower=0> q_rho;
  array[gp==1] real<lower=0> r_alpha;
  array[gp==1] real<lower=0> r_rho;
  matrix[D, ma] theta; 
 	matrix[D, dft*2] C;                                                                       
 	array[level==1]vector<lower=0, upper=1>[D] ar;                                                           
 	array[level==1]vector[D] pl;        
 	matrix[J, D] Bx;
  matrix[J, D] By;
}                                                                                           
transformed parameters {                             
  array[N+h] vector[D] mu;                                                                  
 	array[N+h] matrix[D, D] S;                                                                 
  array[N+h] vector[D] v;   
  matrix[D, N] Dx;
  matrix[D, N] Dy;
	{                                                                                           
 	  array[N+h] vector[D] m;                                                                   
  	array[N+h] vector[D] m_pred;                                                              
 	  array[N+h] matrix[D, D] P_pred;      
 	  array[N+h] matrix[D, D] P;                                                                
   	array[N+h] matrix[D, D] Q;                                                                
   	matrix[D, D] R;    
   	int start;
 	  array[N+h] matrix[D, D] K;                                                              
 	  array[N+h] vector[D] xi;   
 	  
 	  // Build up the covariates*coefficient matrices for later use
    Dx = Bx' * X';
    Dy = By' * X';
 	  v = rep_array(rep_vector(0, D), N+h);                                                             
    if(gp)
      R = cov_GPL2(Dmat, r_alpha[1], r_rho[1], r0^2);
    else  
      R = quad_form_diag(r_L_corr[1] * r_L_corr[1]', r0);    
      
    start = max(garch+1, ma+1);
    for(t in 1:start) {                                                         
 	    m_pred[t] = m0;                                                                       
 	    P_pred[t] = P0;                                                                       
 	    Q[t] = diag_matrix(rep_vector(1, D));                                                           
 	    xi[t] = rep_vector(1, D);                                                                       
 	  }                                                                                       
 	  for (t in 1 : N+h) {                                                                    
 	    if (t > start) { 
 	      // garch
 	      xi[t] = q0; 
 	      if(garch)
 	        xi[t] +=    q1[1] .* (y[t-1] - y[t-2])^2                                              
 	                  + q2[1] .* xi[t-1];    
 	      // kernel            
        if(gp) {
          Q[t] = cov_GPL2(Dmat, q_alpha[1], q_rho[1], xi[t]^2);
        }
        else {        
 	        Q[t] = quad_form_diag(q_L_corr[1] * q_L_corr[1]', xi[t]);
        }
 	      // variance prediction
 	      P_pred[t] = P[t - 1] + Q[t];        
        // state prediction
        m_pred[t] = m[t - 1];
 	      if(level)
 	        m_pred[t] = ar[1] .* m_pred[t] + pl[1];   
 	      // seasonality
 	      m_pred[t] += C * c[,t];      
 	      // MA
 	      if(ma > 0)
 	        for(i in 1:ma)
 	          m_pred[t] += theta[,i] .* (m[t-i] - m[t-i-1]);
 	    }                                                                                     
 	    if(t > start && t <= N) {
 	        // covariates
          m_pred[t] += Dx[,t-1];
          v[t] = y[t] - (m_pred[t] + Dy[,t-1]);
      }                                                   
 	    S[t] = P_pred[t] + R;                                                                 
 	    K[t] = P_pred[t] / S[t];                                    
 	    m[t] = m_pred[t] + K[t] * v[t];                                                       
 	    P[t] = (I - K[t]) * P_pred[t];                                                        
 	    mu[t] = m_pred[t];                                                                    
 	  }                                                                                       
 	                                                                                          
 	}                                                                                         
}   

model {                                                                                     
 	q0 ~ exponential(1);            
 	r0 ~ exponential(1);   
 	if(gp) {
 	  q_alpha[1] ~ exponential(1);
    q_rho[1] ~ exponential(1);
    r_alpha[1] ~ exponential(1);
    r_rho[1] ~ exponential(1);
 	}
 	else {
 	  q_L_corr[1] ~ lkj_corr_cholesky(D);
 	  r_L_corr[1] ~ lkj_corr_cholesky(D);
 	}
	for(i in 1:D) {
	  C[i] ~ normal(0, 1);  
	  theta[i,] ~ normal(0, 1);
	  Bx[,i] ~ normal(0, 1);
    By[,i] ~ normal(0, 1);
	}
	if(level) {
	  pl[1] ~ cauchy(0, 1);                                                                     
	  ar[1] ~ beta(10, 2);
	}
  for (t in 2 : trainset)                                                                        
	    y[t] ~ multi_normal(mu[t] + Dy[,t-1], S[t]);                                                       
 }         
 
 generated quantities {                                                                      
	array[N] real log_lik;                                                                    
	array[N+h] vector[D] y_hat;      
	array[gp==0] corr_matrix[D] q_corr;
	array[gp==0] corr_matrix[D] r_corr;
	if(gp==0) {
    q_corr[1] = multiply_lower_tri_self_transpose(q_L_corr[1]);
	  r_corr[1] = multiply_lower_tri_self_transpose(r_L_corr[1]);
	}
	log_lik[1] = multi_normal_lpdf(y[1] | mu[1], S[1]);                                   
  y_hat[1] = multi_normal_rng(mu[1], S[1]);     
	for (t in 2 : N)                                                                          
	    log_lik[t] = multi_normal_lpdf(y[t] | mu[t] + Dy[,t-1], S[t]);                                   
	for (t in 2 : N+h)                                                                        
	    y_hat[t] = multi_normal_rng(mu[t] + Dy[,t-1], S[t]);                                             
 }                                                                                           
