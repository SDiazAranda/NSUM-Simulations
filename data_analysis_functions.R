# Data analysis
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
################################################################################
####################### Functions for the graphs ###############################

data_analysis = function(Nh_df, Nh_ref_df){
  # Estimation dataframe analysis
  
  df_analysis = data.frame( abserror = rowMeans(as.matrix(abs(Nh_df-Nh_ref_df))),
                            mse      = rowMeans(as.matrix((Nh_df-Nh_ref_df)^2)),
                            mean     = rowMeans(as.matrix(Nh_df)),
                            bias = abs(rowMeans(as.matrix(Nh_df))-Nh_ref_df),
                            sd       = rowSds(as.matrix(Nh_df)),
                            median   = rowMedians(as.matrix(Nh_df)),
                            mad      = rowMads(as.matrix(Nh_df)) )
  
  return(df_analysis)
}

gen_graph_df = function(simulation_data, magn){
  # sm_df: dataframe to study
  # magn: choose between abserror, mse, bias, sd and median
  # New magnitudes can be implemented and uploaded
  
  n_row  = nrow(simulation_data)
  df_ref = dplyr::select(simulation_data, starts_with("Nh_real"))
  
  graph_df = data.frame(data = 1:n_row)
  
  # Duplicate names 
  
  df_buc = dplyr::select(simulation_data, starts_with(c('Nh_MLE_mod_')))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLE_mod'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  n.col = ncol(df_buc)
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLE_modvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLEmodvis'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  n.col.vis = ncol(df_buc)
  
  # Duplicate elimination
  simulation_data = dplyr::select(simulation_data,  -starts_with(c('Nh_MLE_mod')))
  
  # Análisis
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_PIMLE_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'PiMLE'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_PIMLEvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_PIMLEvis'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLE_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'MLE'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLEvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLEvis'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MoS_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'MoS'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MoSvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MoSvis'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  
  if ('Nh_MLEmod' %in% colnames(graph_df)) {
    graph_df$KLN = graph_df$Nh_MLE_mod
    
    graph_df = subset(graph_df,select = -c(Nh_MLE_mod))
  }
  
  # graph_df %>% select(-Nh_MLE_mod)
  
  
  if ('Nh_MLEmodvis' %in% colnames(graph_df)) {
    graph_df$KLN_vis = graph_df$Nh_MLEmodvis
    graph_df = subset(graph_df,select = -c(Nh_MLEmodvis))
  }
  # graph_df = subset(graph_df,select = -c(Nh_MLEmodvis))
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_GNSUM_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'GNSUM'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Zheng_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Overd.'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Zhengvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_Zhengvis'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_TEO_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'TPC'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_TEOvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_TEOvis'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basic_sum_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'GAO'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basicvis_sum_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basicvis_sum'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basic_mean_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'GAO,a'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basicvis_mean_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basicvis_mean'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Direct'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_Direct'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_real'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(simulation_data, 'Nh_real_1') 
    colnames(df_an) = 'Real'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  # graph_df = subset(graph_df,select = -c(data,Real))
  return(graph_df)
}


# Disjoint graph dataframe generator #

gen_graph_df_disjoint = function(simulation_data, magn){
  # sm_df: dataframe to study
  # magn: choose between abserror, mse, bias, sd and median
  # New magnitudes can be implemented and uploaded
  
  n_row  = nrow(simulation_data)
  df_ref = dplyr::select(simulation_data, starts_with("Nh_real"))
  
  graph_df = data.frame(data = 1:n_row)
  
  # Duplicate names 
  
  df_buc = dplyr::select(simulation_data, starts_with(c('Nh_MLE_mod_')))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLEmod_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLE_modvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLEmodvis_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  # Duplicate elimination
  simulation_data = dplyr::select(simulation_data,  -starts_with(c('Nh_MLE_mod')))
  
  # Análisis
  
  
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_PIMLE_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_PIMLE_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_PIMLEvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_PIMLEvis_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLE_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLE_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLEvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLEvis_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MoS_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MoS_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MoSvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MoSvis_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  if ('Nh_MLEmod_disjoint' %in% colnames(graph_df)) {
    graph_df$KLN_disjoint = graph_df$Nh_MLEmod_disjoint
    
    graph_df = subset(graph_df,select = -c(Nh_MLE_mod_disjoint))
  }
  
  if ('Nh_MLEmodvis_disjoint' %in% colnames(graph_df)) {
    graph_df$KLN_vis_disjoint = graph_df$Nh_MLEmodvis_disjoint
    graph_df = subset(graph_df,select = -c(Nh_MLEmodvis_disjoint))
  }
  
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_GNSUM_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_GNSUM_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Zheng_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_Zheng_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Zhengvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_Zhengvis_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_TEO_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_TEO_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_TEOvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_TEOvis_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basic_sum_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basic_sum_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basicvis_sum_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basicvis_sum_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basic_mean_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basic_mean_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basicvis_mean_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basicvis_mean_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Direct'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_Direct_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_real'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(simulation_data, 'Nh_real_1') 
    colnames(df_an) = 'Nh_real'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  return(graph_df)
}


gen_df = function(simulation_data, magn){
  # simulation_data: dataframe to study
  # magn: choose between abserror, mse, bias, sd and median
  # New magnitudes can be implemented and uploaded
  
  n_row  = nrow(simulation_data)
  df_ref = dplyr::select(simulation_data, starts_with("Nh_real"))
  
  graph_df = data.frame(data = 1:n_row)
  
  # Duplicate names 
  
  df_buc = dplyr::select(simulation_data, starts_with(c('Nh_MLE_mod_')))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLE_mod'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  n.col = ncol(df_buc)
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLE_modvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLEmodvis'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  n.col.vis = ncol(df_buc)
  
  # Duplicate elimination
  simulation_data = dplyr::select(simulation_data,  -starts_with(c('Nh_MLE_mod')))
  
  # Análisis
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_PIMLE_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'PiMLE'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_PIMLEvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'PiMLE'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLE_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'MLE'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLEvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'MLE'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MoS_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'MoS'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MoSvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'MoS'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  
  # graph_df %>% select(-Nh_MLE_mod)
  
  if ('Nh_MLE_mod' %in% colnames(graph_df)) {
    graph_df$KLN = graph_df$Nh_MLE_mod
    
    graph_df = subset(graph_df,select = -c(Nh_MLE_mod))
  }
  
  if ('Nh_MLEmodvis' %in% colnames(graph_df)) {
    graph_df[['KLN']] = graph_df$Nh_MLEmodvis
    graph_df = subset(graph_df,select = -c(Nh_MLEmodvis))
  }
  # graph_df = subset(graph_df,select = -c(Nh_MLEmodvis))
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_GNSUM_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'GNSUM'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Maltiel_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'MRM'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_barr_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'MRM barr.'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_trans_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'MRM trans.'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_comb_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'MRM comb.'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Zheng_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Overd.'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Zhengvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Overd.'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_TEO_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'TPC'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_TEOvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'TPC'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basic_sum_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'RoA'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basicvis_sum_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'RoA'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basic_mean_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'AoR'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basicvis_mean_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'AoR'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  # df_buc = dplyr::select(simulation_data, starts_with('Nh_Direct'))
  # if (ncol(df_buc) != 0){
  #   df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
  #   colnames(df_an) = 'Nh_Direct'
  #   graph_df        = cbind(graph_df, df_an )
  #   
  # }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_real'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(simulation_data, 'Nh_real_1') 
    colnames(df_an) = 'Real'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  graph_df = subset(graph_df,select = -c(data,Real))
  return(graph_df)
}


gen_df_disjoint = function(simulation_data, magn){
  # sm_df: dataframe to study
  # magn: choose between abserror, mse, bias, sd and median
  # New magnitudes can be implemented and uploaded
  
  n_row  = nrow(simulation_data)
  df_ref = dplyr::select(simulation_data, starts_with("Nh_real"))
  
  graph_df = data.frame(data = 1:n_row)
  
  # Duplicate names 
  
  df_buc = dplyr::select(simulation_data, starts_with(c('Nh_MLE_mod_')))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLEmod_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLE_modvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLEmodvis_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  # Duplicate elimination
  simulation_data = dplyr::select(simulation_data,  -starts_with(c('Nh_MLE_mod')))
  
  # Análisis
  
  
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_PIMLE_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_PIMLE_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_PIMLEvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_PIMLEvis_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLE_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLE_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MLEvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MLEvis_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MoS_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MoS_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_MoSvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_MoSvis_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  if ('Nh_MLEmod_disjoint' %in% colnames(graph_df)) {
    graph_df$KLN_disjoint = graph_df$Nh_MLEmod_disjoint
    
    graph_df = subset(graph_df,select = -c(Nh_MLE_mod_disjoint))
  }
  
  if ('Nh_MLEmodvis_disjoint' %in% colnames(graph_df)) {
    graph_df$KLN_vis_disjoint = graph_df$Nh_MLEmodvis_disjoint
    graph_df = subset(graph_df,select = -c(Nh_MLEmodvis_disjoint))
  }
  
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_GNSUM_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_GNSUM_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Zheng_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_Zheng_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Zhengvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_Zhengvis_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_TEO_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_TEO_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_TEOvis_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_TEOvis_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basic_sum_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basic_sum_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basicvis_sum_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basicvis_sum_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basic_mean_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basic_mean_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_basicvis_mean_'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_basicvis_mean_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_Direct'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(data_analysis(df_buc, df_ref), all_of(magn)) 
    colnames(df_an) = 'Nh_Direct_disjoint'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  
  df_buc = dplyr::select(simulation_data, starts_with('Nh_real'))
  if (ncol(df_buc) != 0){
    df_an           = dplyr::select(simulation_data, 'Nh_real_1') 
    colnames(df_an) = 'Nh_real'
    graph_df        = cbind(graph_df, df_an )
    
  }
  
  graph_df = subset(graph_df,select = -c(data,Real))
  return(graph_df)
}

latex.table = function(df){
  s.df = ' ' 
  for (i in 1:ncol(df)) {
    s.df = paste(s.df,paste0("& ",colnames(df)[i]))
    for (j in 1:nrow(df)) {
      s.df = paste(s.df, paste0(" & ",round(df[j,i],2)))
    }
    s.df = paste(s.df, "\\\\", "\n")
  }
  cat(s.df)
}

latex.table2 = function(df){
  s.df = ' ' 
  best = c()
  for (k in 1:nrow(df)) {
    best[k] = which.min(df[k,])
  }
  for (i in 1:ncol(df)) {
    s.df = paste(s.df,paste0("& ",colnames(df)[i]))
    for (j in 1:nrow(df)) {
      if (i == best[j]) {
        s.df = paste(s.df, paste0(" & \\textbf{",round(df[j,i],2),'}'))
      }
      else{
      s.df = paste(s.df, paste0(" & ",round(df[j,i],2)))
      }
    }
    s.df = paste(s.df, "\\\\", "\n")
  }
  cat(s.df)
}

latex.table.mean = function(df){
  s.df = ' ' 
  best = c()
  for (k in 1:nrow(df)) {
    best[k] = which.min(abs(df[k,]-100))
  }
  for (i in 1:ncol(df)) {
    s.df = paste(s.df,paste0("& ",colnames(df)[i]))
    for (j in 1:nrow(df)) {
      if (i == best[j]) {
        s.df = paste(s.df, paste0(" & \\textbf{",round(df[j,i],2),'}'))
      }
      else{
        s.df = paste(s.df, paste0(" & ",round(df[j,i],2)))
      }
    }
    s.df = paste(s.df, "\\\\", "\n")
  }
  cat(s.df)
}

latex.code = function(df){
  s.df = ' ' 
  best = c()
  for (k in 1:nrow(df)) {
    best[k] = which.min(df[k,])
  }
  for (i in 1:ncol(df)) {
    s.df = paste(s.df,paste0("& ",colnames(df)[i]))
    for (j in 1:nrow(df)) {
      if (i == best[j]) {
        s.df = paste(s.df, paste0(" & \\textbf{",round(df[j,i],2),'}'))
      }
      else{
        s.df = paste(s.df, paste0(" & ",round(df[j,i],2)))
      }
    }
    s.df = paste(s.df, "\\\\", "\n")
  }
  return(s.df)
}

latex.code.mean = function(df){
  s.df = ' ' 
  best = c()
  for (k in 1:nrow(df)) {
    best[k] = which.min(abs(df[k,]-100))
  }
  for (i in 1:ncol(df)) {
    s.df = paste(s.df,paste0("& ",colnames(df)[i]))
    for (j in 1:nrow(df)) {
      if (i == best[j]) {
        s.df = paste(s.df, paste0(" & \\textbf{",round(df[j,i],2),'}'))
      }
      else{
        s.df = paste(s.df, paste0(" & ",round(df[j,i],2)))
      }
    }
    s.df = paste(s.df, "\\\\", "\n")
  }
  return(s.df)
}

################################################################################
################# Functions for the network analysis ###########################

net_degree_distribution = function(net, p, nei){
  # Variables #
  size = net$size 
  degree_vect = c()
  degree_loop = rep(NA, size)
  for (j in 1:size){
    degree_loop[j] = length(net[[j]][[1]])
  }
  
  degree_vect = append(degree_vect, degree_loop)
  degree_df = data.frame(degrees = degree_vect)
  
  # Variables of interest #
  degree_var    = round(var(degree_vect), digits = 2)
  degree_mean   = round(mean(degree_vect), digits = 2)
  degree_median = median(degree_vect)
  degree_max    = max(degree_vect)
  degree_min    = min(degree_vect)
  
  # Graph representation #
  sub_title = str_c("Mean = ", degree_mean, ", median = ", degree_median, ", var = ", degree_var,", min = ", degree_min, ", max = ", degree_max, ". Small World model with p = ", p, " and nei = ", nei, ".")
  
  degree_graph = ggplot(degree_df) +
    geom_histogram( aes(x = degrees, y = ..count../sum(..count..)), binwidth = 1, color = "black", fill = "grey", alpha = 0.4) +
    labs(title = "Network degree distribution",
         subtitle = sub_title,
         x = "Number of neighbors",
         y = "Proportion")
  
  return(degree_graph)
}

net_hplinks_distribution = function(net, pop){
  
  final_infected_nodes = as.integer(row.names(pop)[pop$hidden_population == 1])
  links_hp = rep(NA, N)
  for (j in 1:N){
    count = 0
    for (l in net[[j]][[1]]){
      if (as.logical(sum(l %in% final_infected_nodes))){
        count = count + 1
      }
    }
    links_hp[j] = count
  }
  
  # Variables of interest #
  link_var    = round(var(links_hp), digits = 2)
  link_mean   = round(mean(links_hp), digits = 2)
  link_median = median(links_hp)
  link_max    = max(links_hp)
  link_min    = min(links_hp)
  
  sub_title = str_c("Mean = ", link_mean, ", median = ", link_median, ", var = ", link_var,", min = ", link_min, ", max = ", link_max, ". SIR model with beta = 0.04, gamma = 0.015, 1 hotspot & 5 iterations. HP = ", sum(pop$hidden_population))
  links_graph = ggplot() + 
    geom_line(aes(x = 1:N , y = links_hp)) +
    scale_color_discrete("Legend") + 
    labs(title = "Hidden population distribution",
         subtitle = sub_title,
         x = "People",
         y = "Hidden population links")
  
  return(links_graph)
  
}

net_analysis = function(net, pop, p, nei){
  # Double plot
  plot1 =  net_degree_distribution(net, p, nei)
  plot2 = net_hplinks_distribution(net, pop)
  plt   = grid.arrange(plot1,plot2)
  
  #Variables analysis
  Global_cluster_coefficent = transitivity(net, type = "global")
  Mean_distance = mean_distance(net, weights = NULL, directed = F, unconnected = TRUE, details = FALSE)
  Diameter = diameter(net, directed = F, unconnected = TRUE, weights = NULL)
  Radius = radius(net, mode = "all")
  
  sub_title = str_c("Network parameters: Cluster coefficient = ", round(Global_cluster_coefficent, 2), ", Mean distance = ", round(Mean_distance, 2), ", Diameter = ", Diameter,", Radius = ", Radius) 
  
  title <- ggdraw() + 
    draw_label(sub_title, x = 0.05, fontfamily = "bold", hjust = 0, size = 14)
  
  
  result_graph = plot_grid(title, plt, ncol = 1, rel_heights = c(0.1, 1)
  )
  return(result_graph)
}

net_analysisv2 = function(net, pop, po=1){
  # Double plot
  # plot1 =  net_degree_distribution(net, p, nei)
  degree_vect = degree(net)
  degree_df = data.frame(degrees = degree_vect)
  
  # Variables of interest #
  degree_var    = round(var(degree_vect), digits = 2)
  degree_mean   = round(mean(degree_vect), digits = 2)
  degree_median = median(degree_vect)
  degree_max    = max(degree_vect)
  degree_min    = min(degree_vect)
  
  # Graph representation #
  sub_title = str_c("Mean = ", degree_mean, ", median = ", degree_median, ", var = ", degree_var,", min = ", degree_min, ", max = ", degree_max,
                    ". Preferential attachment model with power = ", po, ".")
  
  plot1 = degree_graph = ggplot(degree_df) +
    geom_histogram( aes(x = degrees, y = ..count../sum(..count..)), binwidth = 1, color = "black", fill = "grey", alpha = 0.4) +
    labs(title = "Network degree distribution",
         subtitle = sub_title,
         x = "Number of neighbors",
         y = "Proportion")
  plot2 = net_hplinks_distribution(net, pop)
  plt   = grid.arrange(plot1,plot2)
  
  #Variables analysis
  Global_cluster_coefficent = transitivity(net, type = "global")
  Mean_distance = mean_distance(net, weights = NULL, directed = F, unconnected = TRUE, details = FALSE)
  Diameter = diameter(net, directed = F, unconnected = TRUE, weights = NULL)
  Radius = radius(net, mode = "all")
  
  sub_title = str_c("Network parameters: Cluster coefficient = ", round(Global_cluster_coefficent, 2), ", Mean distance = ", round(Mean_distance, 2), ", Diameter = ", Diameter,", Radius = ", Radius) 
  
  title <- ggdraw() + 
    draw_label(sub_title, x = 0.05, fontfamily = "bold", hjust = 0, size = 14)
  
  
  result_graph = plot_grid(title, plt, ncol = 1, rel_heights = c(0.1, 1)
  )
  return(result_graph)
}


