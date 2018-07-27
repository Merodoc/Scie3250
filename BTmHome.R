library('Matrix')


BTHome <- function(wh, lh, a, prec = exp(-8), a_th = 1, b_th = 0) {
  # Inputs:
  # - wh: K*K matrix of integers
  #       wh(i,j) is the number of times i beats j at home
  # - lh: K*K matrix of integers
  #       lh(i, j) is the number of times i loses to j at home
  # - a: Scalar. shape parameter for the gamma prior (default = 1)
  #      If a < 0, then it is estimated with a vague prior
  # - prec: Precision of the EM (default = 1e-8)
  # - a_th: Positive scalar. Shape parameter for the gamme prior on the tie
  #         parameter (default = 1)
  # - b_th: Positive scalar. Scale parameter for the gamma prior on the tie
  #         parameter (default = 0)
  # 
  # Outputs - [pi, theta, pi_st, theta_st, ell]
  # - pi is the MAP estimate of the normalized skill parameters
  # - theta is the MAP estimate of the scalar parameter for ties
  # - pi_st gives the values of the normalized skills at each iteration
  # - theta_st gives the values of the home ground advantage at each iteration
  # - ell is the log posterior at each iteration
  K = length(wh[,1])
  b = K*a - 1
  lambda =array(1, K)
  iter_max = 5000
  
  pi_st = Matrix(array(0, iter_max * K), nrow= iter_max, ncol = K)
  theta_st = Matrix(array(0, iter_max), nrow = iter_max, ncol = 1)
  ell = Matrix(array(0, iter_max), nrow = iter_max, ncol = 1)
  
  
  N = wh + lh # number of games between i and j where i is at home
  ak = a - 1 + sum(wh + lh, 2)
  H = sum(sum(wh)) # total number of home field wins
  change = .Machine$double.xmax

  dims = which(N !=0, arr.ind = TRUE)
  ind_i = dims[,1]
  ind_j = dims[,2]
  N_sparse = N[dims]
  
  theta = 1.5
  iteration = 1
  
  #log-posterior (not necessary)
  #Z2 = sparseMatrix(ind_i, ind_j, x = Matrix(N_sparse*log(theta*lambda[ind_i] + lambda[ind_j]))) 
  #ell[iteration] = (a_th - 1 + H)*log(theta) - b_th * theta + t(log(lambda))%*%(a-1 + rowSums(wh + t(lh)) - b * sum(lambda) - sum(Z2))
  pi_st[iteration, ] = (lambda/sum(lambda))
  theta_st[iteration] = theta
  
  # actually calc
  while (abs(change)> prec && iteration < iter_max) {
    
    iteration = iteration + 1
    #  E Step
    Z = sparseMatrix(ind_i, ind_j, x = Matrix(N_sparse/(theta*lambda[ind_i]+lambda[ind_j])))
    
    # Maximize w.r.t lambda
    
    sumZ = rowSums(Z)
    bk = b + theta*sumZ + t(colSums(Z))
    lambda_new = ak/bk
    
    # Maximize w.r.t theta
    Z = sparseMatrix(ind_i, ind_j, x = Matrix(N_sparse/(theta*lambda_new[ind_i]+lambda_new[ind_j])))
    sumZ = rowSums(Z)
    theta = (a_th - 1 + H) / (b_th + sum(lambda_new * sumZ))
    
    change = lambda_new/sum(lambda_new) - lambda/sum(lambda)
    lambda = lambda_new
    
    pi_st[iteration, ] = (lambda/sum(lambda))
    theta_st[iteration] = theta
    
    # Log-post
    #Z2 = sparseMatrix(ind_i, ind_j, x = Matrix(N_sparse*log(theta*lambda[ind_i] + lambda[ind_j]))) 
    #ell[iteration] = (a_th - 1 + H)*log(theta) - b_th * theta + t(log(lambda))%*%(a-1 + rowSums(wh + t(lh))) - b * sum(lambda) - sum(Z2)
    
  }
  
  pi_st = pi_st[1:iteration, ]
  theta_st = theta_st[1:iteration]
  ell = ell[1:iteration]
  pi = lambda/sum(lambda)
  
}