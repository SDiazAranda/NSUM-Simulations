# hpdistrib = 'uniform'
hpdistrib = 'sir'
net = 'sw'
# net='pa'
#########################################################
setwd('NSUM Simulations')
source('initial_data.R')
source('pop_functions.R')
source('nsum_functions.R')

library(NSUM)



parameters = 0 # 

Population = readRDS(paste0('Robjects/pop_',hpdistrib,'_',net,'.rds'))
net_sw = readRDS(paste0('Robjects/Net_',net,'.rds'))
Mhp_vis = readRDS(paste0('Robjects/mat_',hpdistrib,'_',net,'.rds'))

v_pop_total = getV_pop(n_pop,Population)

simulaciones          = data.frame(data = parameters)

set.seed(seed_sim)
  
lista_sim = list()
  t = Sys.time()
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
    Nh_Zheng    = getNh_Zheng(survey, v_pop_prob, N, iterations = 10000, burnins =1000)
    # Nh_Maltiel = getNh_Maltiel(survey,v_pop_prob,N)
    # Nh_Maltiel_barr = getNh_Maltiel_barrier(survey,v_pop_prob,N)
    # Nh_Maltiel_trans = getNh_Maltiel_trans(survey,v_pop_prob,N)
    # Nh_Maltiel_comb = getNh_Maltiel_general(survey,v_pop_prob,N)
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
    
    # sim = cbind(sim, Nh_Maltiel = Nh_Maltiel)
    # names(sim)[dim(sim)[2]] = str_c("Nh_Maltiel_",l)
    # 
    # 
    # sim = cbind(sim, Nh_Maltiel_trans = Nh_Maltiel_trans)
    # names(sim)[dim(sim)[2]] = str_c("Nh_trans_Maltiel_",l)
    # 
    # sim = cbind(sim, Nh_Maltiel_barr = Nh_Maltiel_barr)
    # names(sim)[dim(sim)[2]] = str_c("Nh_barr_Maltiel_",l)
    # 
    # sim = cbind(sim, Nh_Maltiel_comb = Nh_Maltiel_comb)
    # names(sim)[dim(sim)[2]] = str_c("Nh_comb_Maltiel_",l)

    
    
    lista_sim[[l]] = sim
  }
  simulacion = bind_cols(lista_sim)
  lista_simulacion = simulacion
  
 
  simulaciones = lista_simulacion


# simulaciones = bind_rows(lista_simulacion)
simulaciones["data"] = parameters

t = Sys.time()-t

file_name = str_c(paste0("Baselinecomparison_notdisjoint_",hpdistrib,
                         "_",net,".csv"))
write.csv(simulaciones,                      # Data frame
          file = file_name,                  # CSV name
          row.names = FALSE )                 # row names: TRUE or FALSE 
