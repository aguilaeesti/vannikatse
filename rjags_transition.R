#R JAGS template for Bayes course - Matt Low & Malin Aronsson April 2022

#this is the R side of the analysis where you run everything
#the JAGS model code is saved into a file, you run it from in here

rm(list=ls()) #clear the workspace (if wanted)

#1. load the libraries
library(boot)
library(tidyverse)
library(rjags)
library(coda)
library(scales)

load.module("glm") # this loads the glm module in JAPS, to provide efficient samplers for generalized linear mixed modules
# load.module("dic") # this provides novel monitors for assessing model fit using deviance statistics


#2. get the data

#file.choose()

#need to set working directory
df <- read.csv("trans.csv", header=T) #obviously use your data name here

names(df) #and if you called your data something other than 'df', then use that name here
# str(df)
# summary(df) 


# Base matrix ------------------------------------------------------------------
### Calculating transition probabilities based on data (3 categories)
stages <- c("1-leaf", "vegetative", "flowering")

trans.mat <- df %>%
  count(state_t, state_t1) %>%
  filter(state_t1 != "unknown" & state_t1 != "dead" & state_t1 !="dormant" & state_t !="dormant")%>%
  group_by(state_t) %>%
  mutate(probability = n / sum(n)) %>%
  ungroup() %>%
  mutate(state_t1 = factor(state_t1, levels = stages),
         state_t = factor(state_t, levels = stages)) %>%
  arrange(state_t, state_t1)
trans.mat  

# transition matrix for only 3 stages
mat <- round(matrix(data = trans.mat$probability, nrow = 3, ncol = 3, byrow = TRUE),2)
rownames(mat) = stages
colnames(mat) = stages
mat


### Calculating transition probabilities based on data (4 categories)
stages <- c("1-leaf", "vegetative", "flowering", "dormant")

trans.mat <- df %>%
  count(state_t, state_t1) %>%
  filter(state_t1 != "unknown" & state_t1 != "dead")%>%
  group_by(state_t) %>%
  mutate(probability = n / sum(n)) %>%
  ungroup() %>%
  mutate(state_t1 = factor(state_t1, levels = stages),
         state_t = factor(state_t, levels = stages)) %>%
  arrange(state_t, state_t1)
trans.mat  

# transition matrix for 4 stages
mat <- round(matrix(
  xtabs(
    probability ~ state_t + state_t1,
    data = trans.mat,
    drop.unused.levels = FALSE
  ), nrow=4,ncol=4
),2)
rownames(mat) = stages
colnames(mat) = stages
mat

# transmute(
#     from = state_t,
#     to = state_t1,
#     probability = probability,
#     percent = probability * 100,
#     n = n
#   ) %>%
#   filter(to != "unknown" & to != "dead" & to !="dormant" & from !="dormant")

# shorted df
df.trans <- data.frame(to = df$state_t1, from = df$state_t, yr = df$year_num, id = df$ID) %>%
  filter(to != "unknown" & to != "dead") #& to !="dormant" & from !="dormant") # stop excluding dormancy. This is important!

#3. create the data statement for JAGS

data=NULL
data=list(
  to = df.trans$to,                  # stage of id in the next year
  nobs = nrow(df.trans),
  from = df.trans$from,              # stage of id in the current year
  yr = df.trans$yr,                  # year
  nyr = length(unique(df.trans$yr)), # 4 (2022,23,24,25)
  id = df.trans$id,                  # sidebulbs are excluded!
  nid = length(unique(df.trans$id))  # 86 (only motherplants)
) 
data

# Model 1: (only likelihood) create count matrix (nr of counts per transition) ####
# stages <- c("1-leaf", "vegetative", "flowering")
# 
# 
# counts <- as.matrix(table(
#   from = factor(data$from, levels = stages),
#   to   = factor(data$to, levels = stages)
# ))
# 
# jags_data <- list(
#   counts = counts,
#   N = as.integer(rowSums(counts)), # sum of the 'from' column per stage (N1 = 98, N2 = 70, N3 = 108)
#   prior = rep(1, 3)
# )
# jags_data

# Model 2: random ID ####
stages <- c("1-leaf", "vegetative", "flowering","dormant")

# Example: destination flowering becomes c(0, 0, 1)
Y <- diag(4)[match(df.trans$to, stages), , drop = FALSE] # every row gets an index in the correct column (1 = 1leaf, 2 = veg, 3 = flowering, 4 = dormant)
storage.mode(Y) <- "integer"           # numeric matrix to integer matrix

data <- list(
  Y = Y,                                        # based on Y, the multinomial decides which row transitioned to which stage 1:3. Right?
  from = match(df.trans$from, stages),          # integer length of df, 1:3, depending on stage
  ID = match(df.trans$id, unique(df.trans$id)), # integer for each ID, 86 in total. 1 to 4 repetitions of each ID
  nobs = length(match(df.trans$to, stages)),    # 276 values
  nID = length(unique(df.trans$id)),            # 86 individual IDs
  prior = c(1, 1, 1, 1)
)
data

#4. create the intital values for the MCMC chain

inits=list(
list(

))

#5. specify which model to run
model = "JAGS_transition.R" #obviously use your model file name here

#6.compile the model in JAGS
jm = jags.model(model,
data=data, 
n.adapt=1000, 
#inits=inits, 
n.chains=1) 

#7. run the MCMC chains to create stability
burn.in=10000

update(jm, n.iter=burn.in) 

#8. sample the MCMC chains to create the posterior distributions
samples=10000
n.thin=5

#CODA object MCMC sampling
zc = coda.samples(jm,
variable.names=c("P0", "sigma_ID"), #include your parameter names here 
n.iter=samples, 
thin=n.thin)

summary(zc)
plot(zc) 

#JAGS object MCMC sampling
zj = jags.samples(jm, 
variable.names=c("P0"), #include your parameter names here
n.iter=samples, 
thin=n.thin)

#summarise a JAGS object to create a 95% CI for a single or multiple points

# model 1 predictions ####
# pred<-summary(zj$P, quantile, c(.025,.5,.975))$stat 
# 
# pred[2,2,3] # [row,column,list] = [50%, from, to] → change from veg to flow
# stages <- c("1-leaf", "vegetative", "flowering")
# pred.matrix <- round(as.matrix(pred[2,,]),2)
# dimnames(pred.matrix) <- list(stages, stages)



# probability matrix based on data, only incl 3 stages the transition can go to
#             1-leaf   vegetative  flowering
# 1-leaf       0.19       0.26      0.55
# vegetative   0.04       0.09      0.87
# flowering    0.37       0.17      0.46


# model 2 predictions (random ID, 3 stages) ####
pred<-summary(zj$P0, quantile, c(.025,.5,.975))$stat 

pred[2,2,3] # [row,column,list] = [50%, from, to] → change from veg to flow
stages <- c("1-leaf", "vegetative", "flowering")
pred.matrix <- round(as.matrix(pred[2,,]),2)
dimnames(pred.matrix) <- list(stages, stages)
pred.matrix 

# probability matrix based on ID random model, only incl 3 stages the transition can go to
#             1-leaf   vegetative  flowering
# 1-leaf       0.16       0.26      0.58
# vegetative   0.04       0.08      0.87
# flowering    0.39       0.16      0.45


# probability matrix based on data, incl 4 stages the transition can go to
#             1-leaf   vegetative  flowering dormant
# 1-leaf       0.18       0.23      0.50    0.08
# vegetative   0.04       0.09      0.87    0.00
# flowering    0.36       0.16      0.45    0.04
# dormant      0.31       0.19      0.38    0.12

# model 3 predictions (random ID, 4 stages) ####
pred<-summary(zj$P0, quantile, c(.025,.5,.975))$stat 

pred[2,2,3] # [row,column,list] = [50%, from, to] → change from veg to flow
stages <- c("1-leaf", "vegetative", "flowering","dormant")
pred.matrix <- round(as.matrix(pred[2,,]),2)
dimnames(pred.matrix) <- list(stages, stages)
pred.matrix 
# probability matrix based on ID random model, incl 4 stages the transition can go to
# 
#             1-leaf vegetative flowering dormant
# 1-leaf       0.15       0.23      0.52    0.09
# vegetative   0.04       0.08      0.86    0.01
# flowering    0.37       0.15      0.43    0.04
# dormant      0.28       0.20      0.36    0.14

# probability matrix based on ID random model, incl 4 stages the transition can go to, and fancy stuff by AI
# to
# from        1-leaf vegetative  flowering dormant
# 1-leaf      0.164      0.239     0.504   0.090
# vegetative  0.054      0.083     0.843   0.011
# flowering   0.380      0.160     0.413   0.041
# dormant     0.302      0.197     0.339   0.131

# predictions taking into consideration the different probabilities over all simulated individuals - seperately for each posterior draw
zc <- coda.samples(
  jm,
  variable.names = c("P0", "sigma_ID"),
  n.iter = 10000,
  thin = 5
)

draws <- as.matrix(zc)
stages <- c("1-leaf", "vegetative", "flowering", "dormant")

K <- length(stages)
D <- nrow(draws)
M <- 2000L  # Number of individuals used for numerical integration

set.seed(123)

# Reuse standard-normal values to reduce numerical noise.
# Paired positive/negative draws improve symmetry.
Zhalf <- matrix(rnorm((M / 2) * (K - 1)), ncol = K - 1)
Z <- rbind(Zhalf, -Zhalf)

# Dimensions: posterior draw × starting stage × destination
P_average <- array(
  NA_real_,
  dim = c(D, K, K),
  dimnames = list(NULL, from = stages, to = stages)
)

for (d in seq_len(D)) {
  
  sigma <- draws[d, paste0("sigma_ID[", 1:3, "]")]
  
  # Individual effects; fourth destination is the reference
  U <- cbind(
    sweep(Z, 2, sigma, "*"),
    0
  )
  
  for (s in seq_len(K)) {
    
    baseline <- draws[d, paste0("P0[", s, ",", 1:K, "]")]
    
    # One row per simulated individual
    eta <- sweep(U, 2, log(baseline), "+")
    
    # Stable conversion to probabilities
    eta <- sweep(eta, 1, apply(eta, 1, max), "-")
    weights <- exp(eta)
    probabilities <- sweep(weights, 1, rowSums(weights), "/")
    
    P_average[d, s, ] <- colMeans(probabilities)
  }
}


# Posterior mean matrix
mean_matrix <- apply(P_average, c(2, 3), mean)
round(mean_matrix, 3)

# Posterior median and 95% credible intervals
pred <- apply(
  P_average,
  c(2, 3),
  quantile,
  probs = c(0.025, 0.5, 0.975)
)

median_matrix <- pred[2, , ]
round(median_matrix, 3)

# Vegetative → flowering:
pred[, "vegetative", "flowering"]

#####--------------------------------------------########

# Diagnostics -------------------------------------------------------------


#9. other useful code you might use (will need to be adjusted for your models)
##we will get to these in turn during the lectures and exercises
##please don't panic if you don't know what they are used for yet...leave them until we get to them


#diagnostics (see the JAGS Primer p.38-40) - tells if trace plots are 'good' (check book)
raftery.diag(zc)
heidel.diag(zc)
gelman.diag(zc) #needs at least 2 chains


#useful functions for doing predictions
ecdf(zj$output)(0) #obviously substitute the name of your parameter for 'output'
HPDinterval(zc, prob=0.95)
pred<-summary(zj$output, quantile, c(.025,.5,.975))$stat

#extract mcmc chain data from coda object
coda.matrix<-as.matrix(zc[[1]])
head(coda.matrix)

#plotting prediction & 95%CIs using polygon
pred<-summary(zj$output, quantile, c(.025,.5,.975))$stat
x=data$x.pred #if you created an 'x.pred' object in your data statement
y=pred
plot(x,y[2,], col="blue", xlab="Number of trials", ylab="Probability of correct choice", cex=1.4, typ="l", tck=0.03, bty="l") 
polygon(c(x,rev(x)), c(y[1,], rev(y[3,])), col=alpha("blue", alpha=0.4), border=alpha("blue", alpha=0.4))
lines(x,y[1,], lty="dashed", col="blue")
lines(x,y[3,], lty="dashed", col="blue")

abline(h=0.5, lty=2, col="grey80")




# model validation / fit code ---------------------------------------------

#we will get to this in the 'Model Fit' topic
#extracting and plotting the mean, coeff var & sum squares

zj = jags.samples(jm, 
variable.names=c("p.mean" ,"mean.y","mean.y.sim", "cv.y", "cv.y.sim", "p.cv", "fit", "fit.sim", "p.fit"),  
n.iter=samples, 
thin=n.thin)

#plot model fit  (you may need to adjust some of the plotting parameters)
#HINT if you can't see your points in the plot, adjust your xlim=c() and ylim=c() plotting parameters
#means
plot(zj$mean.y,zj$mean.y.sim, xlab="mean data", ylab="mean simulated",cex=.05, pch=20)
abline(0,1) #want at least 10% on each side of the line (the closer to 50% the better)
p<-summary(zj$p.mean, mean)
text(12,17,paste("P =",round(as.numeric(p[1]),4)), cex=1.0) #adjust the x y coords here as appropriate

#coeff var
plot(zj$cv.y,zj$cv.y.sim, xlab="cv data", ylab="cv simulated",cex=.05, pch=20)
abline(0,1) #want at least 10% on each side of the line (the closer to 50% the better)
p<-summary(zj$p.cv, mean)
text(0.3,0.45,paste("P =",round(as.numeric(p[1]),4)), cex=1.0) #adjust the x y coords here as appropriate

#sum squares
par(mfrow = c(1,1))
plot(zj$fit,zj$fit.sim, xlab="fit data", ylab="fit simulated",cex=.05, pch=20)
abline(0,1) #want at least 10% on each side of the line (the closer to 50% the better)
p<-summary(zj$p.fit, mean)
text(30,20,paste("P =",round(as.numeric(p[1]),4)), cex=1.0) #adjust the x y coords here as appropriate




## Model Selection WAIC ####
# ChatGPT for multinomial WAIC:
stages <- c("1-leaf", "vegetative", "flowering")
#stages <- c("1-leaf", "vegetative", "flowering", "dormant")

from_index <- match(data$from, stages)
to_index   <- match(data$to, stages)

stopifnot(
  length(from_index) == length(to_index),
  !anyNA(from_index),
  !anyNA(to_index)
)

zc <- coda.samples(
  jm,
  variable.names = c("P", "counts.sim"),
  n.iter = 5000
)
# Identify the probability associated with each observed transition
nodes <- paste0("P[", from_index, ",", to_index, "]")

# Rows = posterior draws; columns = individual transitions
draws <- as.matrix(zc)
log_lik <- log(draws[, nodes, drop = FALSE])

waic_result <- loo::waic(log_lik)
print(waic_result)

waic_result$estimates["waic", "Estimate"]
# likelihood model: waic 493.0 (Estimate) 20.1 (SE)
