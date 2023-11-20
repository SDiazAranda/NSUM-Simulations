source('~/GitHub/NSUM-Simulations/data/initial data/initial_data.R')
source('~/GitHub/NSUM-Simulations/code/data generation/pop_functions.R')

hps = c('uniform', 'sir')
nets = c('sw', 'pa')

for (net in nets) {
  for (hpdistrib in hps) {
    parameters = seq(from = 0.1, to = 1, length.out = 10)
    
    Population = readRDS(paste0('~/GitHub/NSUM-Simulations/data/initial data/pop_',hpdistrib,'_',net,'.rds'))
    net_sw = readRDS(paste0('~/GitHub/NSUM-Simulations/data/initial data/Net_',net,'.rds'))
    Mhp_vis = readRDS(paste0('~/GitHub/NSUM-Simulations/data/initial data/mat_',hpdistrib,'_',net,'.rds'))
    
    v_pop_total = getV_pop(n_pop,Population)
    
    
   
    
    Mhp_vis_list = list()
    for (i in 1:length(parameters)){
      visibility_factor = parameters[i] 
      # Mhp_vis_list[[i]] =  matrixHP_visibility(Mhp_vis, visibility_factor)
      M = matrixHP_visibility(Mhp_vis, visibility_factor)
      saveRDS(M, paste0('~/GitHub/NSUM-Simulations/data/initial data/vismat_',hpdistrib,'_',net,'_',i,'.rds'))
      # Mhp_vis_list[[i]] = readRDS(paste0('~/GitHub/NSUM-Simulations/data/initial data/vismat_',hpdistrib,'_',net,'_sw.rds'))
    }
  }
}