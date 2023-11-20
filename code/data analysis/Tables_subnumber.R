library(dplyr)
library(matrixStats)
library(ggplot2)
library(stringr)
library(stats)

library(devtools)
library(ggmagnify)

######################
source('~/GitHub/NSUM-Simulations/code/data analysis/data_analysis_functions.R')

# Data import

scenario = 'Subpopulation number'
sim_path = 'Simulation_numbersubpopulations'

scenario2 = 'Subpopulation number small'
sim_path2 = 'Simulation_numbersubpopulationssmall'

# network = 'sw'
network = 'pa'
hid_distrib = 'uniform'
# hid_distrib = 'sir'

path = paste0('~/GitHub/NSUM-Simulations/data/results/',scenario)
data_path = paste0(path,'/CSV/')

path2 = paste0('~/GitHub/NSUM-Simulations/data/results/',scenario2)
data_path2 = paste0(path2,'/CSV/')

notdisj_path = paste0(data_path2,sim_path2,'_notdisjoint_',hid_distrib,'_',network,'.csv')
notdisj_path2 = paste0(data_path2,sim_path2,'_notdisjoint_',hid_distrib,'_',network,'.csv')


simulation_data = read.csv(notdisj_path)
simulation_data2 = read.csv(notdisj_path2)

tab = gen_df(simulation_data,"mean")
tab = (tab + gen_df(simulation_data2,"mean"))/2
round(colMeans(tab),2)


tab = gen_df(simulation_data,"abserror")
tab = (tab + gen_df(simulation_data2,"abserror"))/2
round(colMeans(tab),2)
round(colMeans(tab),2)[which.min(round(colMeans(tab),2))]

tab = gen_df(simulation_data,"mse")
tab = (tab + gen_df(simulation_data2,"mse"))/2
round(colMeans(tab),2)
round(colMeans(tab),2)[which.min(round(colMeans(tab),2))]

