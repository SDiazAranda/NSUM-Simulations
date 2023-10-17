source('initial_data.R')
source('pop_functions.R')

hps = c('uniform', 'sir')
nets = c('sw', 'pa')

for (net in nets) {
  for (hpdistrib in hps) {
    parameters = seq(from = 0.1, to = 1, length.out = 10)
    
    Population = readRDS(paste0('Robjects/pop_',hpdistrib,'_',net,'.rds'))
    net_sw = readRDS(paste0('Robjects/Net_',net,'.rds'))
    Mhp_vis = readRDS(paste0('Robjects/mat_',hpdistrib,'_',net,'.rds'))
    
    v_pop_total = getV_pop(n_pop,Population)
    
    
    # list_surveys = list()
    # for (h in 1:b) {
    #   list_surveys[[h]] = readRDS(paste0('Robjects/survey_',h,'.rds'))
    # }
    # 
    # list_surveys_hp = list()
    # for (h in 1:b) {
    #   list_surveys_hp[[h]] = readRDS(paste0('Robjects/surveyhp_',h,'.rds'))
    # }
    
    
    Mhp_vis_list = list()
    for (i in 1:length(parameters)){
      visibility_factor = parameters[i] 
      # Mhp_vis_list[[i]] =  matrixHP_visibility(Mhp_vis, visibility_factor)
      M = matrixHP_visibility(Mhp_vis, visibility_factor)
      saveRDS(M, paste0('Robjects/vismat_',hpdistrib,'_',net,'_',i,'.rds'))
      # Mhp_vis_list[[i]] = readRDS(paste0('Robjects/vismat_',hpdistrib,'_',net,'_sw.rds'))
    }
  }
}