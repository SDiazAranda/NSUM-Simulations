############################################################################
#######################
# Libraries used #    #
library(igraph)       #
library(stringr)      #
library(ggplot2)      #
library(sampler)      #
library(dplyr)        #
library(truncnorm)    #
library(gridExtra)    #
library(cowplot)      #
library(matrixStats)  #
library(rjags)        #
#library(asbio)       #
library(NSUM)         #
#######################
###################
# Basic estimator #
###################

getNh_basic_sum = function(survey,N) {
  #NSUM Basic estimator  
  #survey: survey
  #N: Population size
  
  Nh_f =  N*sum(survey$hp_survey)/sum(survey$reach_memory)
  
  return(Nh_f)
}

getNh_basic_mean = function(survey,N) {
  #NSUM Basic estimator  
  #survey: survey
  #N: Population size
  
  Nh_f =  N*mean(survey$hp_survey/survey$reach_memory)
  
  return(Nh_f)
}

getNh_basicvis_sum = function(survey,N,vis) {
  #NSUM Basic estimator  
  #survey: survey
  #N: Population size
  #vis: estimation of the visibility factor
  
  Nh_f =  N*sum(survey$hp_survey)/sum(survey$reach_memory) * (1/vis)
  
  return(Nh_f)
}


getNh_basicvis_mean = function(survey,N,vis) {
  #NSUM Basic estimator  
  #survey: survey
  #N: Population size
  #vis: estimation of the visibility factor
  
  Nh_f =  N*mean(survey$hp_survey/survey$reach_memory) * (1/vis)
  
  return(Nh_f)
}

##########################
# Modified MLE estimator #
##########################

getNh_MLE_mod = function(enc, v_pob, N){
  #NSUM Mean of Sums(MoS) (Formula from "Thirty Years of the NSUM method")
  #enc: survey
  #v_pob: vector with the number of people in each Subpopulation
  #N: population's size
  
  # \hat{d_i} = N/L \sum_k (y_{ik}/N_k)
  
  d_i_est = rep(NA, nrow(enc))
  for (i in 1:nrow(enc)) {
    d_i_est[i] = (sum( dplyr::select(enc, starts_with("kp_reach_"))[i,] /v_pob))/length(v_pob) * N
  }
  
  Nh_f = N * sum(enc$hp_survey) / sum(d_i_est)
  Nh_f
}

getNh_MLE_modvis = function(enc, v_pob, N, vis){
  #NSUM Mean of Sums(MoS) (Formula from "Thirty Years of the NSUM method")
  #enc: survey
  #v_pob: vector with the number of people in each Subpopulation
  #N: population's size
  
  # \hat{d_i} = N/L \sum_k (y_{ik}/N_k)
  
  d_i_est = rep(NA, nrow(enc))
  for (i in 1:nrow(enc)) {
    d_i_est[i] = (sum( dplyr::select(enc, starts_with("kp_reach_"))[i,] /v_pob))/length(v_pob) * N
  }
  
  Nh_f = N * sum(enc$hp_survey) / sum(d_i_est) * (1/vis)
  Nh_f
}



#################
# MLE estimator #
#################

getNh_MLE = function(enc,v_pob) {
  #NSUM maximum likelihood estimator(MLE) (Formula from "Thirty Years of the NSUM method")
  #enc: survey
  #v_pob: vector with the number of people in each Subpopulation
  
  suma_KP = sum( dplyr::select(enc, starts_with("kp_reach_")) ) # Known Population sum
  # (\sum y_{iu})/(\frac{\sum N_k}{\sum \sum y_{ik}} )
  Nh_f = sum(enc$hp_survey)*(sum(v_pob)/suma_KP)
  
  return(Nh_f)
}


getNh_MLEvis = function(enc,v_pob,vis) {
  #NSUM maximum likelihood estimator(MLE) (Formula from "Thirty Years of the NSUM method")
  #enc: survey
  #v_pob: vector with the number of people in each Subpopulation
  #vis: estimation of the visibility factor  
  
  suma_KP = sum( dplyr::select(enc, starts_with("kp_reach_")) )
  Nh_f = (sum(enc$hp_survey))*(sum(v_pob)/suma_KP)*(1/vis)
  Nh_f
}



###################
# PIMLE estimator #
###################


getNh_PIMLE = function(enc,v_pob,N) {
  #NSUM Plug-in Maximum Likelihood Estimator(MLE) (Formula from "Thirty Years of the NSUM method")
  #enc: survey
  #v_pob: vector with the number of people in each Subpopulation
  #N: population's size
  #vis: estimation of the visibility factor
  
  #reach estimate
  d_iest = c()
  for (i in 1:nrow(enc)) {
    d_iest[i] = N * sum( dplyr::select(enc, starts_with("kp_reach_"))[i,] )/sum(v_pob)
  }
  Nh_f = N * mean(enc$hp_survey/d_iest)
  Nh_f
}


getNh_PIMLEvis = function(enc,v_pob,N,vis) {
  #NSUM Plug-in Maximum Likelihood Estimator(MLE) (Formula from "Thirty Years of the NSUM method")
  #enc: survey
  #v_pob: vector with the number of people in each Subpopulation
  #N: population's size
  #vis: estimation of the visibility factor
  
  #reach estimate
  d_iest = c()
  for (i in 1:nrow(enc)) {
    d_iest[i] = N * sum( dplyr::select(enc, starts_with("kp_reach_"))[i,] )/sum(v_pob)
  }
  Nh_f = N * mean(enc$hp_survey/d_iest) * (1/vis) # \frac{y_{iu}}{\hat{d_i}}
  Nh_f
}


#################
# MoS estimator #
#################


getNh_MoS = function(enc, v_pob, N){
  #NSUM Mean of Sums(MoS) (Formula from "Thirty Years of the NSUM method")
  #enc: survey
  #v_pob: vector with the number of people in each Subpopulation
  #N: population's size
  
  # \hat{d_i} = N/L \sum_k (y_{ik}/N_k)
  
  d_i_est = rep(NA, nrow(enc))
  for (i in 1:nrow(enc)) {
    d_i_est[i] = (sum( dplyr::select(enc, starts_with("kp_reach_"))[i,] /v_pob))/length(v_pob) * N
  }
  
  Nh_f = N * mean(enc$hp_survey/d_i_est)
  Nh_f
}

# Using dplyr in this case increases a lot the complexity of the function

getNh_MoSvis = function(enc, v_pob, N, vis){
  #NSUM Mean of Sums(MoS) (Formula from "Thirty Years of the NSUM method")
  #enc: survey
  #v_pob: vector with the number of people in each Subpopulation
  #N: population's size
  #vis: estimation of the visibility factor 
  
  # \hat{d_i} = N/L \sum_k (y_{ik}/N_k)
  
  # reach estimate
  d_i_est = rep(NA, nrow(enc))
  for (i in 1:nrow(enc)) {
    d_i_est[i] = (sum(dplyr::select(enc, starts_with("kp_reach_"))[i,]/v_pob))/length(v_pob) * N
  }
  
  Nh_f = N * mean(enc$hp_survey/d_i_est) * (1/vis)
  Nh_f
} 


#########
# GNSUM #
#########


getNh_GNSUM  = function(enc, enc_hp, v_pob, N){
  #General NSUM (GNSUM) (Formula from "GENERALIZING THE NETWORK SCALE-UP METHOD")
  #enc:     survey
  #enc_hp:  hidden population's survey
  #v_pob:   vector with the number of people in each Subpopulation
  #N:       population's size
  
  #Numerator estimate
  n_enc = nrow(enc)
  prob_inc = n_enc/N  #Same inclusion probability for all samples
  numerador = (1/prob_inc) * sum(enc$hp_survey) #Numerator estimate
  
  #Denominator estimate
  ind1 = as.numeric(rownames(enc_hp))
  suma = sum(dplyr::select( enc_hp, starts_with("kp_alters_") ))
  denominador = N/sum(v_pob) * suma/nrow(enc_hp)      #Denominator estimate
  
  Nh = numerador/denominador
  return(Nh)
}


####################
# Direct estimator #
####################

getNh_Direct = function(survey,N){
  #Direct estimation
  #survey: survey
  #N: Population size
  
  Nh = sum(survey$hidden_population)/nrow(survey) * N
  return(Nh)
}


##################################
# Teo et al. (2019) bayesian model
##################################

model1 = 'model {
for(i in 1:N)
{
  
  for(k in 1:Ku)
  
  {
  
  nu[i,k] ~ dpois(lambda*alpha[i]*Su[k])
  
  }
  
  for(k in 1:Kk)
  
  {
  
  nk[i,k] ~ dpois(lambda*alpha[i]*Sk[k])
  
  }
  
  alpha[i]~dlnorm(0,tau)
  
}
for(k in 1:Ku)
{
  
  Su[k]~dunif(0,2500000)
  
}
for(k in 1:Kk)
{
  
  Sk[k]~dunif(0,2500000)
  
}
lambda ~ dunif(0,10)
tau ~ dunif(0,10)
}
'

runModel = function(indexu,indexk,NITERATION,y,v_pop_prob)
{
  dataset=list(
    N=dim(y)[1],
    Kk=length(indexk),
    nk=y[,indexk],
    Ku=length(indexu),
    nu=as.matrix(y[,indexu], ncol = length(indexu)),
    #Sk=Curitiba$known[indexk],
    Sk = N* v_pop_prob, 
    Su=rep(NA,length(indexu)))
  
  initialisation=list(lambda=0.1)
  jagmod=jags.model(textConnection(model1),data=dataset,inits=initialisation,n.chains=2)
  update(jagmod, n.iter=5000, progress.bar="text")
  posterior = coda.samples(jagmod, c("alpha","lambda","tau","Su"),n.iter=NITERATION,progress.bar="text",thin=10)
  results = list(indexk=indexk,indexu=indexu,dataset = dataset,posterior=posterior)
  return(results)
}

getNh_TEO = function(survey,v_pop_prob,N,iter){
  data = survey %>% dplyr::select(starts_with("kp_r") | hp_survey)
  y = matrix(data = NA,nrow = nrow(data),ncol = ncol(data))
  for (i in 1:ncol(data)) {
    y[,i]=data[,i]
  }
  #known = N*v_pop_prob
  indexu = length(v_pop_prob)+1
  indexk = 1:length(v_pop_prob)
  teo.basic.res = runModel(indexu, indexk, iter,y,v_pop_prob)
  teo.basic.post = teo.basic.res$posterior
  teo.basic.su = c(teo.basic.post[[1]][,1], teo.basic.post[[2]][,1])
  
  return(mean(teo.basic.su))
}


getNh_TEOvis = function(survey,v_pop_prob,N,iter, vf_est){
  data = survey %>% dplyr::select(starts_with("kp_r") | hp_survey)
  y = matrix(data = NA,nrow = nrow(data),ncol = ncol(data))
  for (i in 1:ncol(data)) {
    y[,i]=data[,i]
  }
  #known = N*v_pop_prob
  indexu = length(v_pop_prob)+1
  indexk = 1:length(v_pop_prob)
  teo.basic.res = runModel(indexu, indexk, iter,y,v_pop_prob)
  teo.basic.post = teo.basic.res$posterior
  teo.basic.su = c(teo.basic.post[[1]][,1], teo.basic.post[[2]][,1])
  
  return(mean(teo.basic.su)* 1/vf_est)
}

#########
# Zheng #
#########

getNh_Zheng = function(survey, v_pop_prob,N, iterations = 5000,burnins =1000){
  data = survey %>% dplyr::select(starts_with("kp_r") | hp_survey)
  y = matrix(data = NA,nrow = nrow(data),ncol = ncol(data))
  for (i in 1:ncol(data)) {
    y[,i]=data[,i]
  }
  known = N*v_pop_prob
  
  N.mc = iterations
  N.i = nrow(y)
  N.k = ncol(y)
  
  
  
  prevalences = v_pop_prob
  pg1.ind = length(v_pop_prob)
  Pg1 = sum(prevalences[pg1.ind])
  
  ## Declare parameters
  alphas = matrix(NA, nrow = N.mc, ncol = N.i)
  betas = matrix(NA, nrow = N.mc, ncol = N.k)
  omegas = matrix(NA, nrow = N.mc, ncol = N.k)
  mu.alpha = mu.beta = sigma.sq.alpha = sigma.sq.beta = rep(NA, N.mc)
  C1 = C2 = C = NA
  
  alphas[1,] = rnorm(N.i, sd = 2)
  betas[1,] = rnorm(N.k, sd = 2)
  omegas[1,] = 20
  mu.alpha[1] = mu.beta[1] = sigma.sq.alpha[1] = sigma.sq.beta[1] = 5
  
  
  for(ind in 2:N.mc){
    ## Step 1
    for(i in 1:N.i){
      alpha.prop = alphas[ind - 1,i] + rnorm(1, 0, 0.4)
      zeta.prop = exp(alpha.prop + betas[ind - 1,]) / (omegas[ind - 1,] - 1)
      zeta.old = exp(alphas[ind - 1, i] + betas[ind - 1,]) / (omegas[ind - 1,] - 1)
      sum1 = sum(lgamma(y[i,] + zeta.prop) - lgamma(zeta.prop) - zeta.prop * log(omegas[ind - 1,])) +
        dnorm(alpha.prop, mu.alpha[ind - 1], sqrt(sigma.sq.alpha[ind - 1]), log = T)
      sum2 = sum(lgamma(y[i,] + zeta.old) - lgamma(zeta.old) - zeta.old * log(omegas[ind - 1,])) +
        dnorm(alphas[ind - 1,i], mu.alpha[ind - 1], sqrt(sigma.sq.alpha[ind - 1]), log = T)
      prob.acc = exp(sum1 - sum2)
      
      if(prob.acc > runif(1)){
        alphas[ind, i] = alpha.prop
      }else{
        alphas[ind, i] = alphas[ind - 1, i]
      }
    }
    
    ## Step 2
    for(k in 1:N.k){
      beta.prop = betas[ind - 1,k] + rnorm(1, 0, 0.2)
      zeta.prop = exp(alphas[ind, ] + beta.prop) / (omegas[ind - 1,k] - 1)
      zeta.old = exp(alphas[ind, ] + betas[ind - 1,k]) / (omegas[ind - 1,k] - 1)
      sum1 = sum(lgamma(y[,k] + zeta.prop) - lgamma(zeta.prop) - zeta.prop * log(omegas[ind - 1,k])) +
        dnorm(beta.prop, mu.beta[ind - 1], sqrt(sigma.sq.beta[ind - 1]), log = T)
      sum2 = sum(lgamma(y[,k] + zeta.old) - lgamma(zeta.old) - zeta.old * log(omegas[ind - 1,k])) +
        dnorm(betas[ind - 1,k], mu.beta[ind - 1], sqrt(sigma.sq.beta[ind - 1]), log = T)
      prob.acc = exp(sum1 - sum2)
      
      if(prob.acc > runif(1)){
        betas[ind, k] = beta.prop
      }else{
        betas[ind, k] = betas[ind - 1, k]
      }
    }
    
    ## Step 3
    mu.alpha.hat = mean(alphas[ind,])
    mu.alpha[ind] = rnorm(1, mu.alpha.hat, sqrt(sigma.sq.alpha[ind - 1] / 2))
    
    ## Step 4
    sigma.alpha.hat = mean((alphas[ind,] - mu.alpha[ind])^2)
    sigma.sq.alpha[ind] = rinvchisq(1, N.i - 1, sigma.alpha.hat)
    
    ## Step 5
    mu.beta.hat = mean(betas[ind,])
    mu.beta[ind] = rnorm(1, mu.beta.hat, sqrt(sigma.sq.beta[ind - 1] / 2))
    
    ## Step 6
    sigma.beta.hat = mean((betas[ind,] - mu.beta[ind])^2)
    sigma.sq.beta[ind] = rinvchisq(1, N.k - 1, sigma.beta.hat)
    
    
    ## Step 7
    for(k in 1:N.k){
      omega.prop = omegas[ind - 1,k] + rnorm(1, 0, 0.2)
      if(omega.prop > 1){
        zeta.prop = exp(alphas[ind, ] + betas[ind,k]) / (omega.prop - 1)
        zeta.old = exp(alphas[ind, ] + betas[ind,k]) / (omegas[ind - 1,k] - 1)
        sum1 = sum(lgamma(y[,k] + zeta.prop) - lgamma(zeta.prop) - zeta.prop * log(omega.prop) +
                     y[,k] * log((omega.prop - 1)/omega.prop))
        sum2 = sum(lgamma(y[,k] + zeta.old) - lgamma(zeta.old) - zeta.old * log(omegas[ind - 1,k]) +
                     y[,k] * log((omegas[ind - 1, k] - 1)/omegas[ind - 1,k]))
        prob.acc = exp(sum1 - sum2)
        
        if(prob.acc > runif(1)){
          omegas[ind, k] = omega.prop
        }else{
          omegas[ind, k] = omegas[ind - 1, k]
        }
      }else{
        omegas[ind,k] = omegas[ind - 1, k]
      }
    }
    
    ## Step 8
    C1 = log(sum(exp(betas[ind, pg1.ind]) / Pg1))
    C = C1
    alphas[ind,] = alphas[ind,] + C
    mu.alpha[ind] = mu.alpha[ind] + C
    betas[ind,] = betas[ind,] - C
    mu.beta[ind] = mu.beta[ind] - C
    
  }
  
  ## Burn-in and thin
  alphas = alphas[-c(1:burnins),]
  betas = betas[-c(1:burnins),]
  alphas = alphas[seq(1, nrow(alphas), by = 10),]
  betas = betas[seq(1, nrow(betas), by = 10),]
  
  return(mean(exp(betas[,length(prevalences)+1]) * N))
}


getNh_Zhengvis = function(survey, v_pop_prob,N, vf_est, iterations = 5000,burnins =1000){
  data = survey %>% dplyr::select(starts_with("kp_r") | hp_survey)
  y = matrix(data = NA,nrow = nrow(data),ncol = ncol(data))
  for (i in 1:ncol(data)) {
    y[,i]=data[,i]
  }
  known = N*v_pop_prob
  
  N.mc = iterations
  N.i = nrow(y)
  N.k = ncol(y)
  
  
  
  prevalences = v_pop_prob
  pg1.ind = length(v_pop_prob)
  Pg1 = sum(prevalences[pg1.ind])
  
  ## Declare parameters
  alphas = matrix(NA, nrow = N.mc, ncol = N.i)
  betas = matrix(NA, nrow = N.mc, ncol = N.k)
  omegas = matrix(NA, nrow = N.mc, ncol = N.k)
  mu.alpha = mu.beta = sigma.sq.alpha = sigma.sq.beta = rep(NA, N.mc)
  C1 = C2 = C = NA
  
  alphas[1,] = rnorm(N.i, sd = 2)
  betas[1,] = rnorm(N.k, sd = 2)
  omegas[1,] = 20
  mu.alpha[1] = mu.beta[1] = sigma.sq.alpha[1] = sigma.sq.beta[1] = 5
  
  
  for(ind in 2:N.mc){
    ## Step 1
    for(i in 1:N.i){
      alpha.prop = alphas[ind - 1,i] + rnorm(1, 0, 0.4)
      zeta.prop = exp(alpha.prop + betas[ind - 1,]) / (omegas[ind - 1,] - 1)
      zeta.old = exp(alphas[ind - 1, i] + betas[ind - 1,]) / (omegas[ind - 1,] - 1)
      sum1 = sum(lgamma(y[i,] + zeta.prop) - lgamma(zeta.prop) - zeta.prop * log(omegas[ind - 1,])) +
        dnorm(alpha.prop, mu.alpha[ind - 1], sqrt(sigma.sq.alpha[ind - 1]), log = T)
      sum2 = sum(lgamma(y[i,] + zeta.old) - lgamma(zeta.old) - zeta.old * log(omegas[ind - 1,])) +
        dnorm(alphas[ind - 1,i], mu.alpha[ind - 1], sqrt(sigma.sq.alpha[ind - 1]), log = T)
      prob.acc = exp(sum1 - sum2)
      
      if(prob.acc > runif(1)){
        alphas[ind, i] = alpha.prop
      }else{
        alphas[ind, i] = alphas[ind - 1, i]
      }
    }
    
    ## Step 2
    for(k in 1:N.k){
      beta.prop = betas[ind - 1,k] + rnorm(1, 0, 0.2)
      zeta.prop = exp(alphas[ind, ] + beta.prop) / (omegas[ind - 1,k] - 1)
      zeta.old = exp(alphas[ind, ] + betas[ind - 1,k]) / (omegas[ind - 1,k] - 1)
      sum1 = sum(lgamma(y[,k] + zeta.prop) - lgamma(zeta.prop) - zeta.prop * log(omegas[ind - 1,k])) +
        dnorm(beta.prop, mu.beta[ind - 1], sqrt(sigma.sq.beta[ind - 1]), log = T)
      sum2 = sum(lgamma(y[,k] + zeta.old) - lgamma(zeta.old) - zeta.old * log(omegas[ind - 1,k])) +
        dnorm(betas[ind - 1,k], mu.beta[ind - 1], sqrt(sigma.sq.beta[ind - 1]), log = T)
      prob.acc = exp(sum1 - sum2)
      
      if(prob.acc > runif(1)){
        betas[ind, k] = beta.prop
      }else{
        betas[ind, k] = betas[ind - 1, k]
      }
    }
    
    ## Step 3
    mu.alpha.hat = mean(alphas[ind,])
    mu.alpha[ind] = rnorm(1, mu.alpha.hat, sqrt(sigma.sq.alpha[ind - 1] / 2))
    
    ## Step 4
    sigma.alpha.hat = mean((alphas[ind,] - mu.alpha[ind])^2)
    sigma.sq.alpha[ind] = rinvchisq(1, N.i - 1, sigma.alpha.hat)
    
    ## Step 5
    mu.beta.hat = mean(betas[ind,])
    mu.beta[ind] = rnorm(1, mu.beta.hat, sqrt(sigma.sq.beta[ind - 1] / 2))
    
    ## Step 6
    sigma.beta.hat = mean((betas[ind,] - mu.beta[ind])^2)
    sigma.sq.beta[ind] = rinvchisq(1, N.k - 1, sigma.beta.hat)
    
    
    ## Step 7
    for(k in 1:N.k){
      omega.prop = omegas[ind - 1,k] + rnorm(1, 0, 0.2)
      if(omega.prop > 1){
        zeta.prop = exp(alphas[ind, ] + betas[ind,k]) / (omega.prop - 1)
        zeta.old = exp(alphas[ind, ] + betas[ind,k]) / (omegas[ind - 1,k] - 1)
        sum1 = sum(lgamma(y[,k] + zeta.prop) - lgamma(zeta.prop) - zeta.prop * log(omega.prop) +
                     y[,k] * log((omega.prop - 1)/omega.prop))
        sum2 = sum(lgamma(y[,k] + zeta.old) - lgamma(zeta.old) - zeta.old * log(omegas[ind - 1,k]) +
                     y[,k] * log((omegas[ind - 1, k] - 1)/omegas[ind - 1,k]))
        prob.acc = exp(sum1 - sum2)
        
        if(prob.acc > runif(1)){
          omegas[ind, k] = omega.prop
        }else{
          omegas[ind, k] = omegas[ind - 1, k]
        }
      }else{
        omegas[ind,k] = omegas[ind - 1, k]
      }
    }
    
    ## Step 8
    C1 = log(sum(exp(betas[ind, pg1.ind]) / Pg1))
    C = C1
    alphas[ind,] = alphas[ind,] + C
    mu.alpha[ind] = mu.alpha[ind] + C
    betas[ind,] = betas[ind,] - C
    mu.beta[ind] = mu.beta[ind] - C
    
  }
  
  ## Burn-in and thin
  alphas = alphas[-c(1:burnins),]
  betas = betas[-c(1:burnins),]
  alphas = alphas[seq(1, nrow(alphas), by = 10),]
  betas = betas[seq(1, nrow(betas), by = 10),]
  
  return(mean(exp(betas[,length(prevalences)+1]) * N * 1/vf_est))
}

###################

getNh_Maltiel = function(survey,v_pop_prob,N,iter=20,burnin=3,size=20){
  data = survey %>% dplyr::select(starts_with("kp_r") | hp_total)
  dat = matrix(data = NA,nrow = nrow(data),ncol = ncol(data))
  for (i in 1:ncol(data)) {
    dat[,i]=data[,i]
  }
  known = N*v_pop_prob
  mcmc = nsum.mcmc(dat,known,N,model = "degree",iterations = 5000,burnin = 2000,size = 2500)
  return(mean(mcmc$NK.values))
}

getNh_Maltiel_barrier = function(survey,v_pop_prob,N,iter=20,burnin=3,size=20){
  data = survey %>% dplyr::select(starts_with("kp_r") | hp_total)
  dat = matrix(data = NA,nrow = nrow(data),ncol = ncol(data))
  for (i in 1:ncol(data)) {
    dat[,i]=data[,i]
  }
  known = N*v_pop_prob
  mcmc = nsum.mcmc(dat,known,N,model = "barrier",iterations = 5000,burnin = 2000,size = 2500)
  return(mean(mcmc$NK.values[1,]))
}

getNh_Maltiel_trans = function(survey,v_pop_prob,N,iter=20,burnin=3,size=20){
  data = survey %>% dplyr::select(starts_with("kp_r") | hp_total)
  dat = matrix(data = NA,nrow = nrow(data),ncol = ncol(data))
  for (i in 1:ncol(data)) {
    dat[,i]=data[,i]
  }
  known = N*v_pop_prob
  mcmc = nsum.mcmc(dat,known,N,model = "transmission",iterations = 5000,burnin = 2000,size = 2500)
  return(mean(mcmc$NK.values[1,]))
}

getNh_Maltiel_general = function(survey,v_pop_prob,N,iter=20,burnin=3,size=20){
  data = survey %>% dplyr::select(starts_with("kp_r") | hp_total)
  dat = matrix(data = NA,nrow = nrow(data),ncol = ncol(data))
  for (i in 1:ncol(data)) {
    dat[,i]=data[,i]
  }
  known = N*v_pop_prob
  mcmc = nsum.mcmc(dat,known,N,model = "combined",iterations = 5000,burnin = 2000,size = 2500)
  return(mean(mcmc$NK.values[1,]))
}