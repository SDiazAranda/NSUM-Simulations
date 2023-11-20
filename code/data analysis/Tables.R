####################### table script #############################

###### Packages ######

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
scenario = 'Recall error'
sim_path = 'Simulation_recall'

scenario = 'Subpopulation number'
sim_path = 'Simulation_numbersubpopulations'

scenario = 'Subpopulation number small'
sim_path = 'Simulation_numbersubpopulationssmall'

scenario = 'Transmission error'
sim_path = 'Simulation_transmission'

scenario = 'Transmission error adjusted'
sim_path = 'Simulation_transmissionadj'

scenario = 'Subpopulation size'
sim_path = 'Simulation_subpopulationssize'

scenario = 'Sample size'
sim_path = 'Simulation_samplesize'

scenario = 'Baseline_maltiel2015'
sim_path = 'Baselinecomparison'
# sim_path = 'Baselinecomparisondegree'
# sim_path = 'Baselinecomparisonbarrier'
# sim_path = 'Baselinecomparisontrans'
# sim_path = 'Baselinecomparisoncomb'

# network = 'sw'
network = 'pa'
# hid_distrib = 'uniform'
hid_distrib = 'sir'


path = paste0('~/GitHub/NSUM-Simulations/data/results/',scenario)
data_path = paste0(path,'/CSV/')

notdisj_path = paste0(data_path,sim_path,'_notdisjoint_',hid_distrib,'_',network,'.csv')
# disj_path =  paste0(data_path,sim_path,'_disjoint_',hid_distrib,'_',network,'.csv')

simulation_data = read.csv(notdisj_path)
# simulation_data = read.csv(disj_path)



tab = gen_df(simulation_data,"mean")
latex.table.mean(tab)
round(colMeans(tab),2)

tab = gen_df(simulation_data,"sd")
latex.table2(tab)
round(colMeans(tab),2)
round(colMeans(tab),2)[which.min(round(colMeans(tab),2))]

tab = gen_df(simulation_data,"abserror")
latex.table2(tab)
round(colMeans(tab),2)
round(colMeans(tab),2)[which.min(round(colMeans(tab),2))]

 tab = gen_df(simulation_data,"mse")
latex.table(tab)
round(colMeans(tab),2)
round(colMeans(tab),2)[which.min(round(colMeans(tab),2))]

s = ' '
s = paste(s,'\\hline ', '\n')
s = paste(s,'\\multirow{9}{*}{Mean}', '\n')
tab = gen_df(simulation_data,"mean")
s  = paste(s, latex.code.mean(tab))
s = paste(s,'\\hline ', '\n')
s = paste(s,'\\multirow{9}{*}{SD}', '\n')
tab = gen_df(simulation_data,"sd")
s  = paste(s, latex.code(tab))
s = paste(s,'\\hline ', '\n')
s = paste(s,'\\multirow{9}{*}{MAE}', '\n')
tab = gen_df(simulation_data,"abserror")
s  = paste(s, latex.code(tab))
s = paste(s,'\\hline ', '\n')
s = paste(s,'\\multirow{9}{*}{MSE}', '\n')
tab = gen_df(simulation_data,"mse")
s  = paste(s, latex.code(tab))
s = paste(s,'\\hline ', '\n')
cat(s)

