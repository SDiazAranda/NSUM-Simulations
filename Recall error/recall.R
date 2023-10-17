# source('~/data.R')
# source('~/Functions.R')

# hps = c('uniform', 'sir')
# nets = c('sw', 'pa')

# hpdistrib = 'uniform'
hpdistrib = 'sir'
net = 'sw'
net='pa'
#########################################################
source('initial_data.R')
source('pop_functions.R')
source('nsum_functions.R')




parameters = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)



Population = readRDS(paste0('Robjects/pop_',hpdistrib,'_',net,'.rds'))
net_sw = readRDS(paste0('Robjects/Net_',net,'.rds'))
Mhp_vis = readRDS(paste0('Robjects/mat_',hpdistrib,'_',net,'.rds'))

v_pop_total = getV_pop(n_pop,Population)

simulaciones          = data.frame(data = parameters)

#lista_simulacion          = list() #initial_data
# lista_simulacion_disjoint = list()



# list_surveys = list()
# for (h in 1:b) {
#   list_surveys[[h]] = readRDS(paste0('Robjects/survey_',h,'.rds'))
# }
# 
# list_surveys_hp = list()
# for (h in 1:b) {
#   list_surveys_hp[[h]] = readRDS(paste0('Robjects/surveyhp_',hpdistrib,'_',net,'_',h,'.rds'))
# }


######################################3
# Simulation
set.seed(seed_sim)
for (w in 1:length(parameters)) {
  ## Parameter implementation ##
  sub_memory_factor = parameters[w] 
  memory_factor = parameters[w] 
  
  Population  = dplyr::select(Population, -starts_with("kp_reach") & -starts_with("kp_alters"))
  
  vect_hp_vis   = gen_Reach_hp_memory(Population, Mhp_vis, memory_factor)$hp_survey
  Population$hp_survey    = vect_hp_vis
  vect_reach_re = gen_Reach_memory(Population, memory_factor)$reach_memory
  Population$reach_memory = vect_reach_re
  
  
  Population  = cbind(Population, gen_Subpopulation_memoryfactor(Population, Mhp_vis, sub_memory_factor, net_sw))
  Population  = cbind(Population, gen_Subpopulation_alters_memoryfactor(Population, Mhp_vis, sub_memory_factor))
  
  
  lista_sim = list()
  
  # Population for the VF estimate
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
  
}

simulaciones = bind_rows(lista_simulacion)
# simulaciones_disjoint = bind_rows(lista_simulacion_disjoint)

simulaciones["data"] = parameters
# simulaciones_disjoint["data"] = parameters



################################################################################
file_name = str_c(paste0("Simulation_recall_notdisjoint_",hpdistrib,
                         "_",net,".csv"))
write.csv(simulaciones,                         # Data frame
          file = file_name,                     # Csv's name
          row.names = TRUE )                    # Row names: TRUE o FALSE 
################################################################################