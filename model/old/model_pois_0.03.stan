data {
  int nteams;
  int ngames;
//  vector[nteams] prior_score;
  int team1[ngames];
  int team2[ngames];
  int score1[ngames];
  int score2[ngames];
}

parameters {
  real baseline;
  real home;
//  real skill[nteams]
  real group_tau;
  vector[nteams] skill;
}

transformed parameters {
// // Lambda 
  vector[ngames] lambda1;
  vector[ngames] lambda2;
  for(ii in 1:ngames){
    lambda1[ii] = exp(baseline + home + skill[team1[ii]] - skill[team2[ii]]);
    lambda2[ii] = exp(baseline + skill[team2[ii]] - skill[team1[ii]]);
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
  skill[j] ~ normal(0, group_tau);
}

}
// priors
//group_skill ~ dnorm(0, 0.0625)
//group_tau = 1 / pow(group_sigma, 2)
//group_sigma ~ dunif(0, 3)
//baseline ~ dnorm(0, 0.0625)    



