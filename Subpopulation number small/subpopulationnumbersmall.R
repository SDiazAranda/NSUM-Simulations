# source('~/data.R')
# source('~/Functions.R')

# hps = c('uniform', 'sir')
# nets = c('sw', 'pa')

# hpdistrib = 'uniform'
hpdistrib = 'sir'
# net = 'sw'
net='pa'
#########################################################
source('initial_data.R')
source('pop_functions.R')
source('nsum_functions.R')




parameters = round(seq(from = 2, to = 20, length.out = 10))


Population = readRDS(paste0('Robjects/pop_',hpdistrib,'_',net,'.rds'))
net_sw = readRDS(paste0('Robjects/Net_',net,'.rds'))
Mhp_vis = readRDS(paste0('Robjects/mat_',hpdistrib,'_',net,'.rds'))

v_pop_total = getV_pop(n_pop,Population)

Population_disjoint = readRDS(paste0('Robjects/pop_disjoint_',hpdistrib,'_',net,'.rds'))
v_pop_total_disjoint = getV_pop(n_pop, Population_disjoint)


simulaciones          = data.frame(data = parameters)
simulaciones_disjoint = data.frame(data = parameters)

#lista_simulacion          = list() #initial_data
lista_simulacion_disjoint = list()

############################################
# list_surveys = list()
# for (h in 1:b) {
#   list_surveys[[h]] = readRDS(paste0('Robjects/survey_',h,'.rds'))
# }
# 
# list_surveys_hp = list()
# for (h in 1:b) {
#   list_surveys_hp[[h]] =  readRDS(paste0('Robjects/surveyhp_',hpdistrib,'_',net,'_',h,'.rds'))
# }



# list_subpopulations = list()
# for (i in 1:length(parameters)){
#   n_pop = parameters[i]
#   v_pop_prob = c(rep(0.75/n_pop, n_pop))
#   
#   list_subpopulations[[i]] = gen_Subpopulation(N, v_pop_prob)
# }


# list_subpopulations_disjoint = list()
# for (i in 1:length(parameters)){
#   n_pop = parameters[i]
#   v_pop_prob = c(rep(0.75/n_pop, n_pop))
#   
#   list_subpopulations_disjoint[[i]] = gen_Subpopulation_disjoint(N, v_pop_prob)
# }



######################################3
# Simulation
set.seed(seed_sim)
for (w in 1:length(parameters)) {
  ## Parameter implementation ##
  n_pop = parameters[w]
  v_pop_prob = c(rep(0.01, n_pop))
  
  # Not disjoint #
  population_loop = Population %>% select(hidden_population,reach,reach_memory,hp_total,hp_survey)
  population_loop  = cbind(population_loop, gen_Subpopulation(N, v_pop_prob))
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
  
  population_disjoint_loop = Population_disjoint %>% select(hidden_population,reach,reach_memory,hp_total,hp_survey)
  population_disjoint_loop  = cbind(population_disjoint_loop, gen_Subpopulation_disjoint(N, v_pop_prob))
  population_disjoint_loop  = cbind(population_disjoint_loop, gen_Subpopulation_memoryfactor(population_disjoint_loop, Mhp_vis, sub_memory_factor, net_sw))
  population_disjoint_loop  = cbind(population_disjoint_loop, gen_Subpopulation_alters_memoryfactor(population_disjoint_loop, Mhp_vis, sub_memory_factor))
  
  # population_disjoint_loop  = data.frame(hidden_population = Population_disjoint$hidden_population)
  # population_disjoint_loop  = cbind(population_disjoint_loop, list_subpopulations_disjoint[[w]])
  # population_disjoint_loop  = cbind(population_disjoint_loop, reach = Population_disjoint$reach)
  # population_disjoint_loop  = cbind(population_disjoint_loop, reach_memory = Population_disjoint$reach_memory)
  # population_disjoint_loop  = cbind(population_disjoint_loop, hp_total = Population_disjoint$hp_total) 
  # population_disjoint_loop  = cbind(population_disjoint_loop, hp_survey = Population_disjoint$hp_survey)
  # population_disjoint_loop  = cbind(population_disjoint_loop, gen_Subpopulation_memoryfactor(population_disjoint_loop, Mhp_vis, sub_memory_factor, net_sw))
  # population_disjoint_loop  = cbind(population_disjoint_loop, gen_Subpopulation_alters_memoryfactor(population_disjoint_loop, Mhp_vis, sub_memory_factor))
  
  Population_disjoint = population_disjoint_loop
  v_pop_total_disjoint = getV_pop(n_pop, Population_disjoint)
  
  # Population number (disjoint)
  # v_pop_total_disjoint = getV_pop(n_pop, Population_disjoint)
  
  lista_sim = list()
  
  # # Population for the VF estimate
  # Population_vf = gen_Survey_VF(sum(Population$hidden_population), Population, Mhp_vis, memory_factor)
  
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
  
  # Disjoint
  lista_sim_disjoint = list()
  for (l in 1:b) {
    
    #We choose the same survey for each l in order to calculate the bias and variance
    #Surveys
    # survey = Population_disjoint[list_surveys[[l]],]
    # survey_hp = Population_disjoint[Population_disjoint$hidden_population == 1,][list_surveys_hp[[l]],]
    
    survey = Population[gen_Survey(n_survey, Population),]
    survey_hp = Population[Population$hidden_population == 1,][gen_Survey(n_survey_hp, Population[Population$hidden_population == 1,]),]
    
    #Hidden population estimates
    Nh_real_disjoint = sum(Population_disjoint$hidden_population) 
    
    Nh_basic_sum_disjoint     = getNh_basic_sum(survey,N)
    Nh_basic_mean_disjoint    = getNh_basic_mean(survey,N)
    Nh_PIMLE_disjoint     = getNh_PIMLE(survey, v_pop_total_disjoint, N)
    Nh_MLE_disjoint      = getNh_MLE(survey, v_pop_total_disjoint)
    Nh_MLE_mod_disjoint      = getNh_MLE_mod(survey, v_pop_total_disjoint, N)
    Nh_MoS_disjoint      = getNh_MoS(survey, v_pop_total_disjoint, N)
    Nh_GNSUM_disjoint    = getNh_GNSUM(survey, survey_hp, v_pop_total_disjoint, N)
    Nh_TEO_disjoint    = getNh_TEO(survey, v_pop_prob, N, iter = 1000)
    Nh_Zheng_disjoint  = getNh_Zheng(survey, v_pop_prob, N, iterations = 5000, burnins =1000)
    
    
    #Dataframe for saving the estimates
    sim_disjoint = data.frame(Nh_real = Nh_real_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_real_",l)
    
    sim_disjoint = cbind(sim_disjoint,Nh_basic_sum = Nh_basic_sum_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_basic_sum_",l)
    
    sim_disjoint = cbind(sim_disjoint,Nh_basic_mean = Nh_basic_mean_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_basic_mean_",l)
    
    sim_disjoint = cbind(sim_disjoint,Nh_PIMLE = Nh_PIMLE_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_PIMLE_",l)
    
    sim_disjoint = cbind(sim_disjoint,Nh_MLE = Nh_MLE_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_MLE_",l)
    
    sim_disjoint = cbind(sim_disjoint,Nh_MoS = Nh_MoS_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_MoS_",l)
    
    sim_disjoint = cbind(sim_disjoint, Nh_GNSUM = Nh_GNSUM_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_GNSUM_",l)
    
    sim_disjoint = cbind(sim_disjoint,Nh_MLE_mod = Nh_MLE_mod_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_MLE_mod_",l)
    
    sim_disjoint = cbind(sim_disjoint, Nh_TEO = Nh_TEO_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_TEO_",l)
    
    sim_disjoint = cbind(sim_disjoint, Nh_Zheng = Nh_Zheng_disjoint)
    names(sim_disjoint)[dim(sim_disjoint)[2]] = str_c("Nh_Zheng_",l)
    
    lista_sim_disjoint[[l]] = sim_disjoint
  }
  simulacion_disjoint = bind_cols(lista_sim_disjoint)
  lista_simulacion_disjoint[[w]] = simulacion_disjoint
  print(w)
}

simulaciones = bind_rows(lista_simulacion)
simulaciones_disjoint = bind_rows(lista_simulacion_disjoint)

simulaciones["data"] = parameters
simulaciones_disjoint["data"] = parameters



################################################################################
file_name = str_c(paste0("Simulation_numbersubpopulationssmall_notdisjoint_",hpdistrib,
                         "_",net,".csv"))
write.csv(simulaciones,                         # Data frame
          file = file_name,                     # Csv's name
          row.names = TRUE )                    # Row names: TRUE o FALSE 
################################################################################

################################################################################
file_name = str_c(paste0("Simulation_numbersubpopulationssmall_disjoint_",hpdistrib,
                         "_",net,".csv"))
write.csv(simulaciones_disjoint,                         # Data frame
          file = file_name,                     # Csv's name
          row.names = TRUE )                    # Row names: TRUE o FALSE 
################################################################################