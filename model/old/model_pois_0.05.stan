data {
  int nteams;
  int ngames;
  int team1[ngames];
  int team2[ngames];
  int score1[ngames];
  int score2[ngames];
  int st1[ngames];
  int st2[ngames];
  vector[nteams] prior_score;  
}

parameters {
  real baseline;
  real home;
//  real skill[nteams]
  real <lower=0> group_tau;
  vector[nteams] skill;
  real b_prior;
  real b_skill1;
  real b_skill2;
  
}

transformed parameters {
// // Lambda 
  vector[ngames] lambda1;
  vector[ngames] lambda2;
  for(ii in 1:ngames){
    lambda1[ii] = exp(baseline + home + b_skill1*skill[team1[ii]] - b_skill2*skill[team2[ii]]);
    lambda2[ii] = exp(baseline +  b_skill2*skill[team2[ii]] - b_skill1*skill[team1[ii]]);
  }
}
// 
model {

  for (i in 1:ngames){
    score1[i] ~ poisson(lambda1[i]);
    score2[i] ~ poisson(lambda2[i]);
  }

//
//skill[1] = 0
  for(j in 1:nteams){
    skill[j] ~ normal(b_prior*prior_score[j], group_tau);
  }

}


// priors
//group_skill ~ dnorm(0, 0.0625)
//group_tau = 1 / pow(group_sigma, 2)
//group_sigma ~ dunif(0, 3)
//baseline ~ dnorm(0, 0.0625)    



