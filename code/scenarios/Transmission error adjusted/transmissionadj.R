
# hpdistrib = 'uniform'
hpdistrib = 'sir'
# net = 'sw'
net='pa'
#########################################################
source('~/GitHub/NSUM-Simulations/data/initial data/initial_data.R')
source('~/GitHub/NSUM-Simulations/code/data generation/pop_functions.R')
source('~/GitHub/NSUM-Simulations/code/nsum/nsum_functions.R')




parameters = seq(from = 0.1, to = 1, length.out = 10)


Population = readRDS(paste0('~/GitHub/NSUM-Simulations/data/initial data/pop_',hpdistrib,'_',net,'.rds'))
net_sw = readRDS(paste0('~/GitHub/NSUM-Simulations/data/initial data/Net_',net,'.rds'))
Mhp_vis = readRDS(paste0('~/GitHub/NSUM-Simulations/data/initial data/mat_',hpdistrib,'_',net,'.rds'))

v_pop_total = getV_pop(n_pop,Population)

simulaciones          = data.frame(data = parameters)

Mhp_vis_list = list()
for (i in 1:length(parameters)){
  visibility_factor = parameters[i] 
  # Mhp_vis_list[[i]] =  matrixHP_visibility(Mhp_model, visibility_factor)
  # saveRDS(Mhp_vis_list[[i]], paste0('~/GitHub/NSUM-Simulations/data/initial data/vismat_',hpdistrib,'_',net,'_sw.rds'))
  Mhp_vis_list[[i]] = readRDS(paste0('~/GitHub/NSUM-Simulations/data/initial data/vismat_',hpdistrib,'_',net,'_',i,'.rds'))
}



######################################3
# Simulation
set.seed(seed_sim)
for (w in 1:length(parameters)) {
  ## Parameter implementation ##
  visibility_factor = parameters[w] 
  Mhp_vis = Mhp_vis_list[[w]]
  
  # Not disjoint #
  population_buc = dplyr::select(Population, 'hidden_population' | starts_with('subpop') | 'reach' | 'hp_total')  
  
  population_buc  = cbind(population_buc, gen_Reach_hp_memory(population_buc, Mhp_vis, memory_factor)) # HP reach recall error variable
  population_buc  = cbind(population_buc, gen_Reach_memory(population_buc, memory_factor)) #Reach recall error variable
  population_buc  = cbind(population_buc, gen_Subpopulation_memoryfactor(population_buc, Mhp_vis, sub_memory_factor, net_sw) )
  population_buc  = cbind(population_buc, gen_Subpopulation_alters_memoryfactor(population_buc, Mhp_vis, sub_memory_factor) )
  
  Population = population_buc
  
  
  
  lista_sim = list()
  
  # Population for the VF estimate
  Population_vf = gen_Survey_VF(sum(Population$hidden_population), Population, Mhp_vis, memory_factor)
  
  #Iterations
  for (l in 1:b) {
    #We choose the same survey for each l in order to calculate the bias and variance
    #Surveys
    # survey = Population[list_surveys[[l]],]
    # survey_hp = Population[Population$hidden_population == 1,][list_surveys_hp[[l]],]
    # survey_hp_vf = Population_vf[list_surveys_hp[[l]],]
    
    survey = Population[gen_Survey(n_survey, Population),]
    sur.hp = gen_Survey(n_survey_hp, Population[Population$hidden_population == 1,])
    survey_hp = Population[Population$hidden_population == 1,][sur.hp,]
    survey_hp_vf = Population_vf[sur.hp,]
    
    #Visibility factor estimate
    vf_estimate = VF_Estimate(survey_hp_vf)
    
    #Hidden population estimates
    Nh_real = sum(Population$hidden_population) 
    Nh_basic_sum    = getNh_basic_sum(survey,N)/vf_estimate
    Nh_basic_mean    = getNh_basic_mean(survey,N)/vf_estimate
    Nh_PIMLE    = getNh_PIMLE(survey, v_pop_total, N)/vf_estimate
    Nh_MLE     = getNh_MLE(survey, v_pop_total)/vf_estimate
    Nh_MoS     = getNh_MoS(survey, v_pop_total, N)/vf_estimate
    Nh_MLE_mod  = getNh_MLE_mod(survey, v_pop_total, N)/vf_estimate
    Nh_GNSUM     =  getNh_GNSUM(survey, survey_hp, v_pop_total, N)
    Nh_TEO      = getNh_TEO(survey, v_pop_prob, N, iter = 1000)/vf_estimate
    Nh_Zheng    = getNh_Zheng(survey, v_pop_prob, N, iterations = 5000, burnins =1000)/vf_estimate
    
    
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
file_name = str_c(paste0("Simulation_transmissionadj_notdisjoint_",hpdistrib,
                         "_",net,".csv"))
write.csv(simulaciones,                         # Data frame
          file = file_name,                     # Csv's name
          row.names = TRUE )                    # Row names: TRUE o FALSE 
################################################################################