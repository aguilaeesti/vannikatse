#JAGS model template

# OPTIONAL (Data transformations)
# data {
#   for (i in 1:N) {
#     z[i] <- sqrt(y[i])
#   }
# }


# MODEL -------------------------------------------------------------------

# model{
#
# # model 1: likelihood only multinomial model 
# for (s in 1:3) {
#     counts[s, 1:3] ~ dmulti(P[s, 1:3], N[s])
#     counts.sim[s, 1:3] ~ dmulti(P[s, 1:3], N[s])
#     P[s, 1:3] ~ ddirch(prior[1:3])
# 
#     # compare sum of squares of residuals step 1
#     for (k in 1:3) {
#       expected[s, k] <- N[s] * P[s, k]
#       
#       # Pearson discrepancy for observed and replicated counts
#       sq[s, k] <- pow(counts[s, k] - expected[s, k], 2) /
#         (expected[s, k] + 0.000001)
#       
#       sq.sim[s, k] <- pow(counts.sim[s, k] - expected[s, k], 2) /
#         (expected[s, k] + 0.000001)
#     }
  



#CHATGPT: Among plants currently in the same stage, do some consistently have a greater tendency to reach particular destination stages?
# model 2: multinomial model with random effect for ID, 3 categories only
  # model {
  #   # Baseline transition matrix
  #   for (s in 1:3) {
  #     P0[s, 1:3] ~ ddirch(prior[1:3])
  #   }
  #   
  #   # Individual effects on destination log odds
  #   for (h in 1:nID) {
  #     for (k in 1:2) {
  #       z[h, k] ~ dnorm(0, 1)
  #       u[h, k] <- sigma_ID[k] * z[h, k]
  #     }
  #     u[h, 3] <- 0
  #   }
  #   
  #   # Regularizing priors on individual variation
  #   for (k in 1:2) {
  #     sigma_ID[k] ~ dnorm(0, 1) T(0,)
  #   }
  #   
  #   # Individual transition records
  #   for (i in 1:nobs) {
  #     for (k in 1:3) {
  #       eta[i, k] <- log(P0[from[i], k]) + u[ID[i], k]
  #     }
  #     
  #     # Stable conversion to probabilities
  #     max_eta[i] <- max(eta[i, 1:3])
  #     
  #     for (k in 1:3) {
  #       weight[i, k] <- exp(eta[i, k] - max_eta[i])
  #       prob[i, k] <- weight[i, k] / sum(weight[i, 1:3])
  #     }
  #     
  #     Y[i, 1:3] ~ dmulti(prob[i, 1:3], 1)
  #     Ysim[i, 1:3] ~ dmulti(prob[i, 1:3], 1)
  #     
  #     # Observed transition log probability for WAIC
  #     log_lik[i] <- inprod(Y[i, 1:3], log(prob[i, 1:3]))
  #   }
# CHATGPT: The parameters mean:
#   Parameter	Interpretation
# P0[s,k]	Transition probability for starting stage s, for a plant whose individual effects are zero
# u[h,1]	Plant h’s tendency toward 1-leaf relative to flowering
# u[h,2]	Plant h’s tendency toward vegetative relative to flowering
# sigma_ID[k]	Magnitude of persistent differences among plants
# prob[i,k]	Destination probability for transition record i, including its individual effect
#
# This model assumes individual tendencies apply across starting stages, and that the two individual effects are independent. 
# It avoids estimating nine separate transition probabilities for every plant. P0 is not the population-average transition matrix; 
# averaging over individual variation requires an additional prediction step.



# model 3: multinomial model with random effect for ID, 4 categories
model {
  # Baseline transition matrix
  for (s in 1:4) {
    P0[s, 1:4] ~ ddirch(prior[1:4])
  }

  # Individual effects on destination log odds
  for (h in 1:nID) {
    for (k in 1:3) {
      z[h, k] ~ dnorm(0, 1)
      u[h, k] <- sigma_ID[k] * z[h, k]
    }
    u[h, 4] <- 0
  }

  # Regularizing priors on individual variation
  for (k in 1:4) {
    sigma_ID[k] ~ dnorm(0, 1) T(0,)
  }

  # Individual transition records
  for (i in 1:nobs) {
    for (k in 1:4) {
      eta[i, k] <- log(P0[from[i], k]) + u[ID[i], k]
    }

    # Stable conversion to probabilities
    max_eta[i] <- max(eta[i, 1:4])

    for (k in 1:4) {
      weight[i, k] <- exp(eta[i, k] - max_eta[i])
      prob[i, k] <- weight[i, k] / sum(weight[i, 1:4])
    }

    Y[i, 1:4] ~ dmulti(prob[i, 1:4], 1)
    Ysim[i, 1:4] ~ dmulti(prob[i, 1:4], 1)

    # Observed transition log probability for WAIC
    log_lik[i] <- inprod(Y[i, 1:4], log(prob[i, 1:4]))
  }
  
  
# Model 4: multinomial model with random effect for year, 4 categories
  
  
  

  # moment matching (see options below)
    # Multinomial
    # a[i] = mu[i] - sigma * sqrt(3) (lower limit)
    # b[i] = mu[i] + sigma * sqrt(3) (upper limit)

#}

#priors

  # alpha/beta ~ dnorm(0,0.00001)
  # sigma ~ dunif(0,10)
  # tau <- 1/sigma^2

  
  # The prior for the δ vector is a Dirichlet distribution.
  # The Dirichlet distribution is the multivariate extension of the beta distribution. We met
  # the beta distribution earlier in this chapter. Like the beta, the Dirichlet is a distribution for
  # probabilities, values between zero and one that all sum to one. The beta is a distribution
  # for two probabilities. The Dirichlet is a distribution for any number. And just like the beta,
  # the Dirichlet is parameterized by pseudo-counts of observations. In the beta, these were the
  # parameters α and β, the prior counts of success and failures, respectively. In the Dirichlet,
  # there is a just a long vector α with pseudo-counts for each possibility. If we assign the same
  # value to each, it is a uniform prior. The larger the α values, the more prior information that
  # the probabilities are all the same.  
  
  
#predictions (ignore this until we get to multiple predictions)

#for(k in 1:length(x.pred)){
	
#	 } #only needed if doing multiple predictions along an x-gradient

  
  
  
# model validation (optional) --------------------------------------------------------
  # # compare mean ('Bayesian p-value')
  # mean.y <- mean(counts[1:3,1:3])                         # replace y and y.sim with the name of the response variable
  # mean.y.sim <- mean(counts.sim[1:3,1:3])
  # p.mean <- mean.y.sim - mean.y
  # 
  # # compare coefficient of variation cv
  # cv.y <- sd(counts[1:3,1:3])/mean(counts[1:3,1:3])
  # cv.y.sim <- sd(counts.sim[1:3,1:3])/mean(counts.sim[1:3,1:3])
  # p.cv <- cv.y.sim - cv.y
  # 
  # # compare sum of squares of residuals step 2
  # fit <- sum(sq[1:3, 1:3])
  # fit.sim <- sum(sq.sim[1:3, 1:3])
  # 
  # p.fit <- step(fit.sim - fit)
  
}


