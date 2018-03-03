data {
  int nteams;
  int ngames;
//  vector[nteams] prior_score;
  int team1[ngames];
  int team2[ngames];
  vector[ngames] score1;
  vector[ngames] score2;
}

parameters {
  real baseline_1;
  real baseline_2;
  vector[nteams] skill;
}

transformed parameters {
// Lambda 
  vector[ngames] lambda1;
  vector[ngames] lambda2;
  for(ii in 1:ngames)
    lambda1[ii] = exp(baseline_1 + skill[team1[ii]] - skill[team2[ii]]);
    lambda2[ii] = exp(baseline_2 + skill[team2[ii]] - skill[team1[ii]]);   
}

model {
  
  for (ii in 1:ngames)
    score1[ii] ~ dpois(lambda1[ii]);
    score2[ii] ~ dpois(lambda1[ii]);
    


skill[1] = 0
  for(j in 2:nteams)
    skill[j] ~ dnorm(group_skill, group_tau);


// priors
group_skill ~ dnorm(0, 0.0625)
group_tau = 1 / pow(group_sigma, 2)
group_sigma ~ dunif(0, 3)
baseline ~ dnorm(0, 0.0625)    

}

