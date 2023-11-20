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
#######################

rinvchisq <- function (n, df, scale = 1/df) 
{
  if ((length(scale) != 1) & (length(scale) != n)) 
    stop("scale should have the same length as n")
  if (df <= 0) 
    stop("df must be greater than zero")
  if (any(scale <= 0)) 
    stop("scale must be greater than zero")
  (df * scale)/rchisq(n, df = df)
}


################################################################################

#############################
# Generation of populations #
#############################

####################################
## Hidden population distribution ##

# This function uniformly assings if the individuals belong to the hidden population
gen_HP <- function(n, prob_hp) {
  # Hidden population generator: returns a dataframe with a single column,  
  # the ones represent belonging to the Hidden Population
  
  # prob_hp: probability of the occurrence of the hidden population (bernoulli parameter)
  # n:    people in the general population
  
  vect = sample(1:n, prob_hp*n, replace = FALSE, p = NULL)
  vect_hp = rep(NA, n)
  for (i in 1:n){
    if (as.logical(sum(i %in% vect))){
      vect_hp[i] = 1
    }
    else{
      vect_hp[i] = 0
    }
  }
  population_buc = data.frame(hidden_population = vect_hp)
  return(population_buc)
}


# This function assings using a SIR structure the individuals who belong to the hidden population
gen_SIRpop = function(n, net ,beta, gamma, chosen_nodes, n_iter){
  # SIR method to determine the distribution of the hidden population
  
  # n: number of individuals in the population
  # beta: infection rate
  # gamma: removal rate
  # n_iter: number of infected people
  # chosen_nodes: nodes from which we start the infection
  
  # Loop variables
  infected_nodes = c()        # Infected nodes
  infected_recovered = c()    # Nodes recovered from the infection
  
  infected_nodes = c(infected_nodes,chosen_nodes)
  for (w in 1:n_iter) {
    
    #Infection stage
    for (node in infected_nodes) {
      for (neighbor in net[[node]][[1]]){
        if (runif(1) < beta & !(neighbor %in% infected_nodes) & !(neighbor %in% infected_recovered )) {
          infected_nodes = c(infected_nodes,neighbor)
          #print(infected_nodes)
        }
      }
    }
    
    # Removal stage
    infected_survivors = c()
    for (node in infected_nodes) {
      if (runif(1) < gamma){
        infected_recovered = c(infected_recovered,node)
      }
      else{
        infected_survivors = c(infected_survivors,node)
      }
    }
    infected_nodes = infected_survivors
  }
  # Final nodes
  final_infected_nodes = infected_survivors
  
  # 0 & 1 vector conversion
  hp_vector = rep(NA, n)
  
  for (i in 1:n){
    if (as.logical(sum(i %in% final_infected_nodes))){
      hp_vector[i] = 1
    }
    else{
      hp_vector[i] = 0
    }
  }
  
  # Dataframe output
  population_buc = data.frame(hidden_population = hp_vector)
  
  return(population_buc) 
}


gen_SIRpop_fix = function(n, net ,beta, gamma, chosen_nodes, n_hp){
  # SIR method to determine the distribution of the hidden population
  
  # n: number of individuals in the population
  # beta: infection rate
  # gamma: removal rate
  # n_iter: number of infected people
  # chosen_nodes: nodes from which we start the infection
  
  # Loop variables
  infected_nodes = c()        # Infected nodes
  infected_recovered = c()    # Nodes recovered from the infection
  
  infected_nodes = c(infected_nodes,chosen_nodes)
  while (length(infected_nodes) < n_hp) {
    
    #Infection stage
    for (node in infected_nodes) {
      for (neighbor in net[[node]][[1]]){
        if (runif(1) < beta & !(neighbor %in% infected_nodes) & !(neighbor %in% infected_recovered )) {
          infected_nodes = c(infected_nodes,neighbor)
          #print(infected_nodes)
        }
      }
    }
    
    # Removal stage
    infected_survivors = c()
    for (node in infected_nodes) {
      if (runif(1) < gamma){
        infected_recovered = c(infected_recovered,node)
      }
      else{
        infected_survivors = c(infected_survivors,node)
      }
    }
    infected_nodes = infected_survivors
  }
  
  # Final nodes
  final_infected_nodes = sample(infected_nodes, n_hp)
  
  # 0 & 1 vector conversion
  hp_vector = rep(NA, n)
  
  for (i in 1:n){
    if (as.logical(sum(i %in% final_infected_nodes))){
      hp_vector[i] = 1
    }
    else{
      hp_vector[i] = 0
    }
  }
  
  # Dataframe output
  population_buc = data.frame(hidden_population = hp_vector)
  
  return(population_buc) 
}
######################################
## Reach & Hidden population number ##

# Gets the Reach value of each individual in the network
gen_Reach = function(net){
  # Reach calculation of each individual from the network
  # net: network to be analysed
  
  # Net size 
  # n = net$size
  n = length(V(net))
  
  # Loop variables
  vect_reach = rep(NA,n)    # the degrees of each individual
  
  for (i in 1:n) {
    # net[[i]], list with one element, the list of the adjacent vertices to i
    vect_reach[i] = length(net[[i]][[1]])
  }
  
  # Dataframe output
  population_buc = data.frame(reach = vect_reach)
  
  return(population_buc)
}

# Calculates the people each individual knows of the hidden population
gen_Reach_hp = function(M_hp){
  # Number of neighbors from the hidden population that each individual knows
  # M_hp: Visibility matrix (before applying the visibility factor)
  
  # Population size
  n = nrow(M_hp)
  
  # Loop variables
  vect_hp = rep(NA,n)      # number of hidden population individuals known by each person
  
  for (i in 1:n) {
    vect_hp[i] = sum(M_hp[i,])
  }
  
  # Dataframe output
  population_buc = data.frame(hp_total = vect_hp)
  
  return(population_buc)
  
}

gen_Reach_hp_memory = function(population_buc, M_vis, mem_factor){
  # Number of neighbors from the hidden population that each individual knows after
  # applying the visibility factor (transmission error) and the memory factor(recall error)
  
  # population_buc: Population dataframe with at least the "hp_total" variable
  # M_vis: Visibility matrix (after applying the visibility factor)
  # mem_factor: memory factor (recall error mesure)
  
  # Population size
  n = nrow(M_vis)
  
  # initializes the vectors
  vect_reach = population_buc$reach      # number of hidden population individuals known for each person
  
  vect_hp_vis = rep(NA,n)                # vect_hp applying visibility
  
  for (i in 1:n) {
    vect_hp_vis[i] = round(rtruncnorm(1, a = max(-0.5,  2 * sum(M_vis[i,]) - vect_reach[i] + 0.5 ) , b = min(2 * sum(M_vis[i,]) + 0.5, vect_reach[i]+0.5), mean = sum(M_vis[i,]), sd = mem_factor*sum(M_vis[i,])))
  }
  
  # Dataframe output
  population_buc = data.frame(hp_survey = vect_hp_vis)
  
  return(population_buc)
  
}


# Calculates the reach applied the memory factor
gen_Reach_memory = function(population_buc, mem_factor){
  # Number of neighbors that each individual knows after applying the memory factor (recall error)
  
  # population_buc: Population dataframe with at least the "hp_survey" & the "reach" variable
  # mem_factor: memory factor (recall error mesure)
  
  # Population size
  n = nrow(population_buc)
  
  # Loop variables
  vect_hp_vis   = population_buc$hp_survey   # vect_hp applying visibility
  vect_reach    = population_buc$reach       # the degrees of each individual
  vect_reach_re = rep(NA,n)                # reach vector applying memory error
  
  for (i in 1:n) {
    vect_reach_re[i] = max(1,round(rtruncnorm(1, a = vect_hp_vis[i] - 0.5, b = 2*vect_reach[i] - vect_hp_vis[i] + 0.5, mean = vect_reach[i], sd = mem_factor*vect_reach[i])))
  }
  
  # Dataframe output
  population_buc = data.frame(reach_memory = vect_reach_re)
  return(population_buc)
}

###############################
## Subpopulations generation ##

# This function generates the subpopulations uniformly (not disjoint)
gen_Subpopulation = function(n, prob_vect){ 
  # Function that generates a dataframe with the subpopulations uniformly distributed 
  # in a not disjoint way, so each individual can belong to several subpopulations.
  
  # n:         number of people in the subpopulation
  # prob_vect: vector with the subpopulations size 
  
  # Loop variables
  subpop_vect = round(n * prob_vect)       # Number of individuals on each subpopulations
  population_buc = data.frame(data = 1:n)  # Dataframe initialisation
  rownames(population_buc) <- c(1:n)       # Dataframe rownames
  
  for (k in 1:length(subpop_vect)) {
    # Index to determine the belonging to each subpopulation (SAMPLE)
    subpop_ind = sample(1:n, subpop_vect[k], replace = FALSE)
    
    # Index transformed into a 0 & 1 vector
    subpop = rep(NA, n)
    for (i in 1:n){
      if (as.logical(sum(i %in% subpop_ind))){
        subpop[i] = 1
      }
      else{
        subpop[i] = 0
      }
      
    }
    
    #Dataframe append
    population_buc = cbind(population_buc, Subpopulation = subpop)
    names(population_buc)[dim(population_buc)[2]] = str_c("subpopulation_",k)
  }
  
  return(dplyr::select(population_buc, -starts_with("data") ))
}



# This function generates the subpopulations uniformly (disjoint)
gen_Subpopulation_disjoint = function(n, prob_vect){
  # Function that generates a dataframe with the subpopulations uniformly distributed 
  # in a disjoint way, so each individual belongs to a unique subpopulations.
  
  # n:         number of people in the subpopulation
  # prob_vect: vector with the subpopulations size 
  
  population_buc = data.frame(data = 1:n)
  
  # Subpopulation size vector
  subpop_vect = round(n*prob_vect)
  
  # Variables for the loop
  sampling_vect = 1:n
  gen_subpop = rep(0, n)
  n_vect = 1:n
  
  for (k in 1:length(subpop_vect)) {
    # Index belonging to the subpopulation k
    subpop_ind = sample(sampling_vect, subpop_vect[k], replace = FALSE)
    
    # Index transformed into a 0 & 1 vector to represent the populations
    subpop = rep(NA, n)
    
    for (i in 1:n){
      if (as.logical(sum(i %in% subpop_ind))){
        subpop[i] = 1
        
        # for k in 1:n appends 1 if a population is assigned
        gen_subpop[i] = 1
      }
      else{
        subpop[i] = 0
      }
      
    }
    
    # Creates a vector with the people who does not have a population 
    sampling_vect = n_vect[gen_subpop == 0]
    
    #Dataframe append population k
    population_buc = cbind(population_buc, Subpopulation = subpop)
    names(population_buc)[dim(population_buc)[2]] = str_c("subpopulation_",k)
  }
  
  return(dplyr::select(population_buc, -starts_with("data") ))
  
  
}  




# Subpopulation memory factor function
gen_Subpopulation_memoryfactor = function(population_buc, M_vis, sub_mem_factor, net){
  # Function that appends to a dataframe the memory factor (recall error) applied
  # to the subpopulation present
  
  # population_buc: dataframe with at least the belonging to the subpopulations 
  # M_vis: Visibility matrix (after applying the visibility factor)
  # sub_mem_factor: memory factor for the subpopulation
  
  # Loop variables
  n     = nrow(population_buc)
  n_pop = ncol(dplyr::select(population_buc, starts_with("subpop")))
  ind1  = 1:n
  population_out = data.frame(data = 1:n)
  
  
  for(j in 1:n_pop){
    v_1  = rep(NA,n)
    # Index of the people that belongs to the subpopulation j
    ind2 = dplyr::select(population_buc, starts_with("subpop") & ends_with( str_c("_", as.character(j) )))[,1] != 0
    for(i in ind1) {
      # Number of people that belongs to the hidden population in the subpopulations
      vis_yij = sum(M_vis[i,ind2]) 
      vis_pob = sum(dplyr::select(population_buc[net[[i]][[1]],],starts_with("subpop") & ends_with( str_c("_", as.character(j) )))) 
      
      # Visibility of population j by i, applying a normal in order to represent the real visibility
      v_1[i] = max(0,round(rtruncnorm(1, a = vis_yij - 0.5 , b = 2*vis_pob - vis_yij + 0.5,  mean = vis_pob, sd = sub_mem_factor*vis_pob)))
    }
    
    population_out = cbind(population_out,Subpoblacion_total = v_1)
    names(population_out)[dim(population_out)[2]] = str_c("kp_reach_",j)
  }
  
  return(dplyr::select(population_out, -starts_with("data") ))
}




# Subpopulation alters memory factor
gen_Subpopulation_alters_memoryfactor = function(population_buc, M_vis, sub_mem_factor){
  # Function that appends to a dataframe the memory factor (recall error) applied
  # to the subpopulation alters
  
  # population_buc: dataframe with at least the belonging to the subpopulations 
  # M_vis: Visibility matrix (after applying the visibility factor)
  # sub_mem_factor: memory factor for the subpopulation
  
  # Loop variables
  n     = nrow(population_buc)
  n_pop = ncol(dplyr::select(population_buc, starts_with("subpop")))
  ind1  = 1:n
  population_out = data.frame(data = 1:n)
  
  
  for (i in 1:n_pop) {
    i_hp_vis = rep(NA,n)
    ind2 = dplyr::select(population_buc, starts_with("subpop") & ends_with( str_c("_", as.character(i) )))[,1] != 0
    for (j in ind1){
      i_hp_vis[j] = round(rtruncnorm(1, a = -0.5, b =  2*sum(M_vis[ind2,j]) + 0.5, mean = sum(M_vis[ind2,j]), sd = sum(M_vis[ind2,j])*sub_mem_factor)) 
    }
    population_out = cbind(population_out, Subpoblacion_total = i_hp_vis)
    names(population_out)[dim(population_out)[2]] = str_c("kp_alters_",i)
  }  
  return(dplyr::select(population_out, -starts_with("data") ))
  
}

# Vector with the subpopulation number
getV_pop = function(n_pop, population_buc){
  # Number of people on each subpopulation
  # n_pop: number of subpopulations
  # Population: Population dataframe
  
  v_pop_total = rep(NA, n_pop)
  for (k in 1:n_pop) {
    v_pop_total[k] = sum(dplyr::select(population_buc, starts_with("subpop") & ends_with( str_c("_", as.character(k) )) ) ) # N_k
  }
  return(v_pop_total)
}

#############
## Surveys ##

# Uniform survey
gen_Survey = function(n_enc, dataframe){
  # This function makes a sample of size n_enc from dataframe
  
  # n_enc = number of individuals interviewed
  # dataframe = general population 
  
  sur = sample(nrow(dataframe), n_enc, replace = FALSE)
  
  # Ordering the dataframe by index
  sur = sur[order(sur)]
  
  return(sur)
}

# Survey for the visibility factor estimate
gen_Survey_VF = function(n_enc, pop, vis_matrix, memory_fact){
  # This function makes a ordered sample of size n_enc from dataframe to make an estimate 
  # of the visibility factor
  
  # n_enc: number of people surveyed
  # pop: Population dataframe
  # vis_matrix: Visibility matrix
  # memory_fact: Memoty factor
  
  # Sample from the hidden population
  enc_hp = pop[sample(nrow(pop[pop$hidden_population==1,]), n_enc, replace = FALSE),]
  
  # Survey index
  ind_survey = as.numeric(rownames(pop[pop$hidden_population==1,]))[as.numeric(rownames(enc_hp))]
  
  # Known variables 
  vect_reach_hp = colSums(vis_matrix[,ind_survey])
  vect_reach = pop$reach[ind_survey]
  
  # New variables
  mem_vect_reach_hp = rep(NA,length(vect_reach_hp))
  mem_vect_reach = rep(NA,length(vect_reach_hp))
  
  # Double truncation + double truncation calculate
  for (i in 1:length(vect_reach_hp)) {
    
    if (vect_reach_hp[i] == vect_reach[i]){
      mem_vect_reach[i] = vect_reach_hp[i]
    } else {
      mem_vect_reach[i] = max(0,round(rtruncnorm(1, a = -0.5 + vect_reach_hp[i] , b = 0.5 + 2 * vect_reach[i] - vect_reach_hp[i], mean = vect_reach[i], sd = memory_factor*vect_reach[i])))
    }
    
    if (vect_reach_hp[i] == mem_vect_reach[i]){
      mem_vect_reach_hp[i] = mem_vect_reach[i]
    } else {
      mem_vect_reach_hp[i] = max(0,round(rtruncnorm(1, a = max( vect_reach_hp[i] - (mem_vect_reach[i]-vect_reach_hp[i]) - 0.5, -0.5), b = min(mem_vect_reach[i] + 0.5, 2 * vect_reach_hp[i] + 0.5),  mean = vect_reach_hp[i] , sd = vect_reach_hp[i]*memory_factor)))   
    }
    
  }
  # Output dataframe construction
  enc_pop = pop[ind_survey,]
  
  # New reach_memory variable
  enc_pop$reach_memory =  mem_vect_reach
  
  # New variables
  enc_pop = cbind(enc_pop, reach_hp = vect_reach_hp)
  enc_pop = cbind(enc_pop, reach_hp_memory = mem_vect_reach_hp)
  
  # Ordering the dataframe by index (future needs)
  enc_pop = enc_pop[order(as.numeric(row.names(enc_pop))), ]
  
  
  return(enc_pop)
}

################################################################################

########################
# Matrix for the GNSUM #
########################

to_matrix = function(x){
  # Converts to matrix the ad. matix
  if(x!=0){
    return(1)
  }
  else {
    return(0)
  }
}

to_matrix_SIR = function(x) {
  ifelse(x %in% c(0,2,3), 0, 1)
}

matrixHP = function(grafo,Pob){
  # Adjacency matrix of the directed graph of connections with the hidden population
  
  # grafo: population graph
  # Pob: Population obtained by getData()
  
  ad = as_adj(grafo) # adjacency matrix
  for (j in 1:ncol(ad)) {
    #if (V(grafo)$label[j] == 0){
    if (Pob$hidden_population[j]==0){
      ad[,j] = 0
    }
  }
  ad = apply(ad, c(1,2), to_matrix)
  return(ad)
}


# Visibility factor calculate for an adjacent matrix (apply method)

berHP = function(x,p){
  # Binomial general function for the visibility matrix (element by element)
  
  # x: matrix 
  # p: binomial probability
  if(x!=0){
    return(x*rbinom(1,1,p))
  }
  else {
    return(0)
  }
}

matrixHP_visibility = function(M_hp, vis_factor){
  M_vis = apply(M_hp,c(1,2), berHP,p = vis_factor)
  return(M_vis)
}

################################################################################

#################################
# General Population generation #
#################################

# General population generation #

# Uniform hidden populatio distribution
gen_Data_uniform = function(n, prob_vect, prob_hp, vis_factor, mem_factor, sub_mem_factor, beta = 0.115, gamma = 0.115/1.5, chosen_nodes = 1, n_iter = 5, net){
  # list, contains the network, the population data and the matrix for the GNSUM
  
  # N:  Population size
  # prob_vect:  Vector with the population's probabilities
  # prob_hp: Hidden Population proportion
  # vis_factor: Visibility factor
  # mem_factor: Memory factor
  # sub_mem_factor: Subpopulation memory factor
  
  # # Seed for homogeneity of the simulations form #
  # set.seed(seed)
  
  # Subpopulation dataframe
  subpop_df = gen_Subpopulation(n, prob_vect)
  
  # # Seed for homogeneity of the simulations form #
  # set.seed(seed)
  
  # Hidden population distribution dataframe
  hp_df = gen_HP(n, prob_hp)
  
  # Matrix representing the directed graph that connects individuals with the people of the Hidden Population they know 
  M_hp     =  matrixHP(net, hp_df)
  M_vis    =  matrixHP_visibility(M_hp, vis_factor)
  
  # Dataframe of the population generation
  population_buc  = hp_df #Hidden population
  population_buc  = cbind(population_buc, subpop_df) #Subpopulations
  population_buc  = cbind(population_buc, gen_Reach(net)) #Reach variable
  population_buc  = cbind(population_buc, gen_Reach_hp(M_hp)) # HP reach variable
  population_buc  = cbind(population_buc, gen_Reach_hp_memory(population_buc, M_vis, mem_factor)) # HP reach recall error variable
  population_buc  = cbind(population_buc, gen_Reach_memory(population_buc, mem_factor)) #Reach recall error variable
  population_buc  = cbind(population_buc, gen_Subpopulation_memoryfactor(population_buc, M_vis, sub_mem_factor, net))
  population_buc  = cbind(population_buc, gen_Subpopulation_alters_memoryfactor(population_buc, M_vis, sub_mem_factor))
  
  # Returns the netwpork, the dataframe with the population data and the visibility matrix
  return(list(net, population_buc, M_vis))
}

# Epidemic data distribution
gen_Data_SIR = function(n, prob_vect, vis_factor, mem_factor, sub_mem_factor, beta = 0.04, gamma = 0.015, chosen_nodes = 1, n_iter = 5, net, hpvector){
  # list, contains the network, the population data and the matrix for the GNSUM
  
  # N:  Population size
  # prob_vect:  Vector with the population's probabilities
  # vis_factor: Visibility factor
  # mem_factor: Memory factor
  # sub_mem_factor: Subpopulation memory factor
  
  # # Seed for homogeneity of the simulations form #
  # set.seed(seed)
  
  # Subpopulation dataframe
  subpop_df = gen_Subpopulation(n, prob_vect)
  
  # # Seed for homogeneity of the simulations form #
  # set.seed(seed)
  
  # Hidden population distribution dataframe
  hp_df = hpvector
  
  # Matrix representing the directed graph that connects individuals with the people of the Hidden Population they know 
  M_hp     =  matrixHP(net, hp_df)
  M_vis    =  matrixHP_visibility(M_hp, vis_factor)
  
  # Dataframe of the population generation
  population_buc  = hp_df #Hidden population
  population_buc  = cbind(population_buc, subpop_df) #Subpopulations
  population_buc  = cbind(population_buc, gen_Reach(net)) #Reach variable
  population_buc  = cbind(population_buc, gen_Reach_hp(M_hp)) # HP reach variable
  population_buc  = cbind(population_buc, gen_Reach_hp_memory(population_buc, M_vis, mem_factor)) # HP reach recall error variable
  population_buc  = cbind(population_buc, gen_Reach_memory(population_buc, mem_factor)) #Reach recall error variable
  population_buc  = cbind(population_buc, gen_Subpopulation_memoryfactor(population_buc, M_vis, sub_mem_factor, net))
  population_buc  = cbind(population_buc, gen_Subpopulation_alters_memoryfactor(population_buc, M_vis, sub_mem_factor))
  
  # Returns the netwpork, the dataframe with the population data and the visibility matrix
  return(list(net, population_buc, M_vis))
}

# This function generates the population with disjoint populations
gen_Population_disjoint <- function(n, net, prob_vect, HP, M_vis, sub_mem_factor, r, r_mem, hp_t, hp_s) {
  # Generates the entire data for the population
  
  # n: the number of individuals
  # prob_vect: vector with the Subpopulations probabilities
  # HP:  Hidden Population vector
  
  population_buc  = data.frame("hidden_population" = HP)
  
  # # Seed for homogeneity of the simulations form #
  # set.seed(seed)
  
  population_buc  = cbind(population_buc, gen_Subpopulation_disjoint(n, prob_vect))
  
  # # Seed for homogeneity of the simulations form #
  # set.seed(seed)
  
  population_buc  = cbind(population_buc, reach = r)
  population_buc  = cbind(population_buc, reach_memory = r_mem)
  population_buc  = cbind(population_buc, hp_total = hp_t)
  population_buc  = cbind(population_buc, hp_survey = hp_s)
  population_buc  = cbind(population_buc, gen_Subpopulation_memoryfactor(population_buc, M_vis, sub_mem_factor, net))
  population_buc  = cbind(population_buc, gen_Subpopulation_alters_memoryfactor(population_buc, M_vis, sub_mem_factor))
  
  return(population_buc)
}


################################################################################
# Visibility factor estimate #

VF_Estimate = function(enc_hp){
  return(sum(enc_hp$reach_hp_memory)/sum(enc_hp$reach_memory))
}
################################################################################


addARD = function(Population,net, memory_factor,sub_memory_factor){
  hp_df = Population$hidden_population
  M_hp     =  matrixHP(net, hp_df)
  M_vis    =  matrixHP_visibility(M_hp, visibility_factor)
  # saveRDS(M_vis,'Robjects/mat_sir_sw.rds')
  
  # Dataframe of the population generation
  # population_buc  = hp_df #Hidden population
  # population_buc  = cbind(population_buc, subpop_df) #Subpopulations
  Population  = cbind(Population, gen_Reach(net)) #Reach variable
  Population  = cbind(Population, gen_Reach_hp(M_hp)) # HP reach variable
  Population  = cbind(Population, gen_Reach_hp_memory(Population, M_vis, memory_factor)) # HP reach recall error variable
  Population  = cbind(Population, gen_Reach_memory(Population, memory_factor)) #Reach recall error variable
  Population  = cbind(Population, gen_Subpopulation_memoryfactor(Population, M_vis, sub_memory_factor, net))
  Population  = cbind(Population, gen_Subpopulation_alters_memoryfactor(Population, M_vis, sub_memory_factor))
  
  return(list(Population,M_vis))
  
}