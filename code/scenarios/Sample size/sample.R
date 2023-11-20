

# hpdistrib = 'uniform'
hpdistrib = 'sir'
# net = 'sw'
net='pa'
#########################################################
source('~/GitHub/NSUM-Simulations/data/initial data/initial_data.R')
source('~/GitHub/NSUM-Simulations/code/data generation/pop_functions.R')
source('~/GitHub/NSUM-Simulations/code/nsum/nsum_functions.R')


parameters    = c(25,50, 100, 200, 300, 400, 500, 1000,2000,3000)


Population = readRDS(paste0('~/GitHub/NSUM-Simulations/data/initial data/pop_',hpdistrib,'_',net,'.rds'))
net_sw = readRDS(paste0('~/GitHub/NSUM-Simulations/data/initial data/Net_',net,'.rds'))
Mhp_vis = readRDS(paste0('~/GitHub/NSUM-Simulations/data/initial data/mat_',hpdistrib,'_',net,'.rds'))

v_pop_total = getV_pop(n_pop,Population)

simulaciones          = data.frame(data = parameters)
#lista_simulacion          = list() #initial_data

set.seed(seed_sim)
for (w in 1:length(parameters)) {
  n_survey = parameters[w] 
  
  lista_sim = list()
  
  #Iterations
  for (l in 1:b) {
    
    survey = Population[gen_Survey(n_survey, Population),]
    survey_hp = Population[Population$hidden_population == 1,][gen_Survey(n_survey_hp, Population[Population$hidden_population == 1,]),]
    
    Nh_real = sum(Population$hidden_population) 
    Nh_basic_sum    = getNh_basic_sum(survey,N) 
    Nh_basic_mean    = getNh_basic_mean(survey,N) 
    Nh_PIMLE = getNh_PIMLE(survey, v_pop_total, N)
    Nh_MLE = getNh_MLE(survey, v_pop_total)
    Nh_MLE_mod  = getNh_MLE_mod(survey, v_pop_total, N)
    Nh_MoS = getNh_MoS(survey, v_pop_total, N)
    Nh_GNSUM =  getNh_GNSUM(survey, survey_hp, v_pop_total, N)
    Nh_TEO      = getNh_TEO(survey, v_pop_prob, N, iter = 1000)
    Nh_Zheng    = getNh_Zheng(survey, v_pop_prob, N, iterations = 5000, burnins =1000)
    Nh_Direct = getNh_Direct(survey, N)
    
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
    
    
    lista_sim[[l]] = sim
  }
  simulacion = bind_cols(lista_sim)
  lista_simulacion[[w]] = simulacion
  
  print(w)
  
}

simulaciones = bind_rows(lista_simulacion)
simulaciones["data"] = parameters


file_name = str_c(paste0("Simulation_samplesize_notdisjoint_",hpdistrib,
                         "_",net,".csv"))
write.csv(simulaciones,                      # Data frame
          file = file_name,                  # CSV name
          row.names = FALSE )                 # row names: TRUE or FALSE 
