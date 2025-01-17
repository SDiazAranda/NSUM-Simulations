
# hpdistrib = 'uniform'
hpdistrib = 'sir'
# net = 'sw'
net='pa'
#########################################################
source('~/GitHub/NSUM-Simulations/data/initial data/initial_data.R')
source('~/GitHub/NSUM-Simulations/code/data generation/pop_functions.R')
source('~/GitHub/NSUM-Simulations/code/nsum/nsum_functions.R')


parameters = round(seq(from = 2, to = 10, length.out = 5))


Population = readRDS(paste0('~/GitHub/NSUM-Simulations/data/initial data/pop_disjoint_',hpdistrib,'_',net,'.rds'))
net_sw = readRDS(paste0('~/GitHub/NSUM-Simulations/data/initial data/Net_',net,'.rds'))
Mhp_vis = readRDS(paste0('~/GitHub/NSUM-Simulations/data/initial data/mat_',hpdistrib,'_',net,'.rds'))

v_pop_total = getV_pop(n_pop,Population)



simulaciones          = data.frame(data = parameters)
simulaciones_disjoint = data.frame(data = parameters)

#lista_simulacion          = list() #initial_data
lista_simulacion_disjoint = list()



######################################3
# Simulation
set.seed(seed_sim)
for (w in 1:length(parameters)) {
  ## Parameter implementation ##
  n_pop = parameters[w]
  v_pop_prob = c(rep(0.1, n_pop))
  
  # Not disjoint #
  population_loop = Population %>% select(hidden_population,reach,reach_memory,hp_total,hp_survey)
  population_loop  = cbind(population_loop, gen_Subpopulation_disjoint(N, v_pop_prob))
  population_loop  = cbind(population_loop, gen_Subpopulation_memoryfactor(population_loop, Mhp_vis, sub_memory_factor, net_sw))
  population_loop  = cbind(population_loop, gen_Subpopulation_alters_memoryfactor(population_loop, Mhp_vis, sub_memory_factor))
  
  # population_loop  = data.frame(hidden_population = Population$hidden_population)
  # population_loop  = cbind(population_loop, gen_Subpopulation(N, v_pop_prob))
  # population_loop  = cbind(population_loop, reach = Population$reach)
  # population_loop  = cbind(population_loop, reach_memory = Population$reach_memory)
  # population_loop  = cbind(population_loop, hp_total = Population$hp_total) 
  # population_loop  = cbind(population_loop, hp_survey = Population$hp_survey)
  # population_loop  = cbind(population_loop, gen_Subpopulation_memoryfactor(population_loop, Mhp_vis, sub_memory_factor, net_sw))
  # population_loop  = cbind(population_loop, gen_Subpopulation_alters_memoryfactor(population_loop, Mhp_vis, sub_memory_factor))
  
  Population = population_loop
  
  # Nk
  v_pop_total = getV_pop(n_pop, Population)
  
  # Disjoint
  
  # population_disjoint_loop = Population_disjoint %>% select(hidden_population,reach,reach_memory,hp_total,hp_survey)
  # population_disjoint_loop  = cbind(population_disjoint_loop, gen_Subpopulation_disjoint(N, v_pop_prob))
  # population_disjoint_loop  = cbind(population_disjoint_loop, gen_Subpopulation_memoryfactor(population_disjoint_loop, Mhp_vis, sub_memory_factor, net_sw))
  # population_disjoint_loop  = cbind(population_disjoint_loop, gen_Subpopulation_alters_memoryfactor(population_disjoint_loop, Mhp_vis, sub_memory_factor))
  # 
  # 
  # 
  # Population_disjoint = population_disjoint_loop
  # v_pop_total_disjoint = getV_pop(n_pop, Population_disjoint)
  
  
  lista_sim = list()
  
  
  #Iterations
  for (l in 1:b) {
    #We choose the same survey for each l in order to calculate the bias and variance
    #Surveys
    # survey = Population[list_surveys[[l]],]
    # survey_hp = Population[Population$hidden_population == 1,][list_surveys_hp[[l]],]
    
    survey = Population[gen_Survey(n_survey, Population),]
    survey_hp = Population[Population$hidden_population == 1,][gen_Survey(n_survey_hp, Population[Population$hidden_population == 1,]),]
    
    #Hidden population estimates
    Nh_real = sum(Population$hidden_population) 
    Nh_basic_sum    = getNh_basic_sum(survey,N)
    Nh_basic_mean    = getNh_basic_mean(survey,N)
    Nh_PIMLE    = getNh_PIMLE(survey, v_pop_total, N)
    Nh_MLE     = getNh_MLE(survey, v_pop_total)
    Nh_MoS     = getNh_MoS(survey, v_pop_total, N)
    Nh_MLE_mod  = getNh_MLE_mod(survey, v_pop_total, N)
    Nh_GNSUM     =  getNh_GNSUM(survey, survey_hp, v_pop_total, N)
    Nh_TEO      = getNh_TEO(survey, v_pop_prob, N, iter = 1000)
    Nh_Zheng    = getNh_Zheng(survey, v_pop_prob, N, iterations = 5000, burnins =1000)
    
    
    #Dataframe for saving the estimates
    sim = data.frame(Nh_real = Nh_real)
    names(sim)[dim(sim)[2]] = str_c("Nh_real_",l)
    
    sim = cbind(sim,Nh_basic_sum = Nh_basic_sum)
    names(sim)[dim(sim)[2]] = str_c("Nh_basic_sum_",l)
    
    sim = cbind(sim,Nh_basic_mean = Nh_basic_mean)
    names(sim)[dim(sim)[2]] = str_c("Nh_basic_mean_",l)
    
    sim = cbind(sim,Nh_PIMLE = Nh_PIMLE)
    names(sim)[dim(sim)[2]] = str_c("Nh_PIMLE_",l)
    
    sim = cbind(sim,Nh_MLE = Nh_MLE)
    names(sim)[dim(sim)[2]] = str_c("Nh_MLE_",l)
    
    sim = cbind(sim,Nh_MoS = Nh_MoS)
    names(sim)[dim(sim)[2]] = str_c("Nh_MoS_",l)
    
    sim = cbind(sim,Nh_GNSUM = Nh_GNSUM)
    names(sim)[dim(sim)[2]] = str_c("Nh_GNSUM_",l)
    
    sim = cbind(sim,Nh_MLE_mod = Nh_MLE_mod)
    names(sim)[dim(sim)[2]] = str_c("Nh_MLE_mod_",l)
    
    sim = cbind(sim, Nh_TEO = Nh_TEO)
    names(sim)[dim(sim)[2]] = str_c("Nh_TEO_",l)
    
    sim = cbind(sim, Nh_Zheng = Nh_Zheng)
    names(sim)[dim(sim)[2]] = str_c("Nh_Zheng_",l)
    
    # matrix_vfe[w,l] = vf_estimate
    lista_sim[[l]] = sim
  }
  simulacion = bind_cols(lista_sim)
  lista_simulacion[[w]] = simulacion
  print(w)
  
  
}

simulaciones = bind_rows(lista_simulacion)
# simulaciones_disjoint = bind_rows(lista_simulacion_disjoint)

simulaciones["data"] = parameters
# simulaciones_disjoint["data"] = parameters



################################################################################
file_name = str_c(paste0("Simulation_numbersubpopulations_disjoint_",hpdistrib,
                         "_",net,".csv"))
write.csv(simulaciones,                         # Data frame
          file = file_name,                     # Csv's name
          row.names = TRUE )                    # Row names: TRUE o FALSE 
################################################################################

# ################################################################################
# file_name = str_c(paste0("Simulation_numbersubpopulations_disjoint_",hpdistrib,
#                          "_",net,".csv"))
# write.csv(simulaciones_disjoint,                         # Data frame
#           file = file_name,                     # Csv's name
#           row.names = TRUE )                    # Row names: TRUE o FALSE 
# ################################################################################