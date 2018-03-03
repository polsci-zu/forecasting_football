data {
  int nteams;
  int ngames;
  int team1[ngames];
  int team2[ngames];
  int score1[ngames];
  int score2[ngames];
  vector[nteams] prior_score;  
}

parameters {
  real baseline;
  real home;
//  real skill[nteams]
  real <lower=0> group_tau;
  vector[nteams] skill;
  real b_prior;
  cov_matrix[2] Sigma;
  matrix[ngames, 2] lambda_cor;
}

transformed parameters {
// // Lambda 
//  vector[ngames] lambda1;
//  vector[ngames] lambda2;
  matrix[ngames, 2] lambda;
  for(ii in 1:ngames){
    lambda[ii,1] = exp(baseline + home + skill[team1[ii]] - skill[team2[ii]]);
    lambda[ii,2] = exp(baseline + skill[team2[ii]] - skill[team1[ii]]);
  
  }
}
// 
model {
//constrain values for lambda > 0
  for (i in 1:ngames){
    lambda_cor[i,] ~ multi_normal(lambda[i,],Sigma);
    score1[i] ~ poisson(lambda_cor[i,1]);
    score2[i] ~ poisson(lambda_cor[i,2]);
  }

//
//skill[1] = 0
  for(j in 1:nteams){
    skill[j] ~ normal(b_prior*prior_score[j], group_tau);
  }

}
