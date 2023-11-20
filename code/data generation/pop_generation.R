source('~/GitHub/NSUM-Simulations/data/initial data/initial_data.R')
source('~/GitHub/NSUM-Simulations/code/data generation/pop_functions.R')

# set.seed(922)
net = readRDS('~/GitHub/NSUM-Simulations/data/initial data/Net_sw.rds')
Graph_population_matrix = gen_Data_uniform(N, v_pop_prob, hp_prob, visibility_factor, memory_factor, sub_memory_factor, net = net)
Population = Graph_population_matrix[[2]]   # Population
Mhp_vis    = Graph_population_matrix[[3]]   # Population's visibility matrix

saveRDS(Population,'~/GitHub/NSUM-Simulations/data/initial data/pop_uniform_sw.rds')
saveRDS(Mhp_vis,'~/GitHub/NSUM-Simulations/data/initial data/mat_uniform_sw.rds')

Population_disjoint =  gen_Population_disjoint(N, net, v_pop_prob, Population$hidden_population, Mhp_vis, sub_memory_factor, Population$reach, Population$reach_memory, Population$hp_total, Population$hp_survey)
saveRDS(Population_disjoint,'~/GitHub/NSUM-Simulations/data/initial data/pop_disjoint_uniform_sw.rds')

Population = Population %>% select(hidden_population,starts_with('subpopulation'))
hp_df = readRDS('~/GitHub/NSUM-Simulations/data/initial data/hpvector_sw.rds')
Population$hidden_population = hp_df

# M_hp     =  matrixHP(net, hp_df)
# M_vis    =  matrixHP_visibility(M_hp, visibility_factor)
# saveRDS(M_vis,'~/GitHub/NSUM-Simulations/data/initial data/mat_sir_sw.rds')
# 
# # Dataframe of the population generation
# # population_buc  = hp_df #Hidden population
# # population_buc  = cbind(population_buc, subpop_df) #Subpopulations
# Population  = cbind(Population, gen_Reach(net)) #Reach variable
# Population  = cbind(Population, gen_Reach_hp(M_hp)) # HP reach variable
# Population  = cbind(Population, gen_Reach_hp_memory(Population, M_vis, memory_factor)) # HP reach recall error variable
# Population  = cbind(Population, gen_Reach_memory(Population, memory_factor)) #Reach recall error variable
# Population  = cbind(Population, gen_Subpopulation_memoryfactor(Population, M_vis, sub_memory_factor, net))
# Population  = cbind(Population, gen_Subpopulation_alters_memoryfactor(Population, M_vis, sub_memory_factor))

# saveRDS(Population,'~/GitHub/NSUM-Simulations/data/initial data/pop_sir_sw.rds')

dat = addARD(Population,net,memory_factor,sub_memory_factor)
saveRDS(dat[[1]],'~/GitHub/NSUM-Simulations/data/initial data/pop_sir_sw.rds')
saveRDS(dat[[2]],'~/GitHub/NSUM-Simulations/data/initial data/mat_sir_sw.rds')

Population = dat[[1]]
Mhp_vis = dat[[2]]

Population_disjoint =  gen_Population_disjoint(N, net, v_pop_prob, Population$hidden_population, Mhp_vis, sub_memory_factor, Population$reach, Population$reach_memory, Population$hp_total, Population$hp_survey)
saveRDS(Population_disjoint,'~/GitHub/NSUM-Simulations/data/initial data/pop_disjoint_sir_sw.rds')

#Preferential attachment

# net = readRDS('~/GitHub/NSUM-Simulations/data/initial data/Net_pa.rds')
# Graph_population_matrix = gen_Data_uniform(N, v_pop_prob, hp_prob, visibility_factor, memory_factor, sub_memory_factor, net = net)
# Population = Graph_population_matrix[[2]]   # Population
# Mhp_vis    = Graph_population_matrix[[3]]   # Population's visibility matrix
# 
# saveRDS(Population,'~/GitHub/NSUM-Simulations/data/initial data/pop_uniform_pa.rds')
# saveRDS(Mhp_vis,'~/GitHub/NSUM-Simulations/data/initial data/mat_uniform_pa.rds')
# 
# Population_disjoint =  gen_Population_disjoint(N, net, v_pop_prob, Population$hidden_population, Mhp_vis, sub_memory_factor, Population$reach, Population$reach_memory, Population$hp_total, Population$hp_survey)
# saveRDS(Population_disjoint,'~/GitHub/NSUM-Simulations/data/initial data/pop_disjoint_uniform_pa.rds')
# 
# Population = Population %>% select(hidden_population,starts_with('subpopulation'))
# hp_df = readRDS('~/GitHub/NSUM-Simulations/data/initial data/hpvector_pa.rds')
# Population$hidden_population = hp_df
# 
# 
# M_hp     =  matrixHP(net, hp_df)
# M_vis    =  matrixHP_visibility(M_hp, visibility_factor)
# saveRDS(M_vis,'~/GitHub/NSUM-Simulations/data/initial data/mat_sir_pa.rds')
# 
# # Dataframe of the population generation
# # population_buc  = hp_df #Hidden population
# # population_buc  = cbind(population_buc, subpop_df) #Subpopulations
# Population  = cbind(Population, gen_Reach(net)) #Reach variable
# Population  = cbind(Population, gen_Reach_hp(M_hp)) # HP reach variable
# Population  = cbind(Population, gen_Reach_hp_memory(Population, M_vis, memory_factor)) # HP reach recall error variable
# Population  = cbind(Population, gen_Reach_memory(Population, memory_factor)) #Reach recall error variable
# Population  = cbind(Population, gen_Subpopulation_memoryfactor(Population, M_vis, sub_memory_factor, net))
# Population  = cbind(Population, gen_Subpopulation_alters_memoryfactor(Population, M_vis, sub_memory_factor))
# 
# saveRDS(Population,'~/GitHub/NSUM-Simulations/data/initial data/pop_sir_pa.rds')

net = readRDS('~/GitHub/NSUM-Simulations/data/initial data/Net_pa.rds')
Population = Population %>% select(hidden_population,starts_with('subpopulation'))
hp_df = gen_HP(N, hp_prob)
Population$hidden_population = hp_df


# M_hp     =  matrixHP(net, hp_df)
# M_vis    =  matrixHP_visibility(M_hp, visibility_factor)
# saveRDS(M_vis,'~/GitHub/NSUM-Simulations/data/initial data/mat_uniform_pa.rds')
# 
# Population  = cbind(Population, gen_Reach(net)) #Reach variable
# Population  = cbind(Population, gen_Reach_hp(M_hp)) # HP reach variable
# Population  = cbind(Population, gen_Reach_hp_memory(Population, M_vis, memory_factor)) # HP reach recall error variable
# Population  = cbind(Population, gen_Reach_memory(Population, memory_factor)) #Reach recall error variable
# Population  = cbind(Population, gen_Subpopulation_memoryfactor(Population, M_vis, sub_memory_factor, net))
# Population  = cbind(Population, gen_Subpopulation_alters_memoryfactor(Population, M_vis, sub_memory_factor))
# 
# saveRDS(Population,'~/GitHub/NSUM-Simulations/data/initial data/pop_uniform_pa.rds')

dat = addARD(Population,net,memory_factor,sub_memory_factor)
saveRDS(dat[[1]],'~/GitHub/NSUM-Simulations/data/initial data/pop_uniform_pa.rds')
saveRDS(dat[[2]],'~/GitHub/NSUM-Simulations/data/initial data/mat_uniform_pa.rds')

Population = dat[[1]]
Mhp_vis = dat[[2]]

Population_disjoint =  gen_Population_disjoint(N, net, v_pop_prob, Population$hidden_population, Mhp_vis, sub_memory_factor, Population$reach, Population$reach_memory, Population$hp_total, Population$hp_survey)
saveRDS(Population_disjoint,'~/GitHub/NSUM-Simulations/data/initial data/pop_disjoint_uniform_pa.rds')

# PA- SIR
Population = Population %>% select(hidden_population,starts_with('subpopulation'))
hp_df = readRDS('~/GitHub/NSUM-Simulations/data/initial data/hpvector_pa.rds')
Population$hidden_population = hp_df


# M_hp     =  matrixHP(net, hp_df)
# M_vis    =  matrixHP_visibility(M_hp, visibility_factor)
# saveRDS(M_vis,'~/GitHub/NSUM-Simulations/data/initial data/mat_sir_pa.rds')
# 
# Population  = cbind(Population, gen_Reach(net)) #Reach variable
# Population  = cbind(Population, gen_Reach_hp(M_hp)) # HP reach variable
# Population  = cbind(Population, gen_Reach_hp_memory(Population, M_vis, memory_factor)) # HP reach recall error variable
# Population  = cbind(Population, gen_Reach_memory(Population, memory_factor)) #Reach recall error variable
# Population  = cbind(Population, gen_Subpopulation_memoryfactor(Population, M_vis, sub_memory_factor, net))
# Population  = cbind(Population, gen_Subpopulation_alters_memoryfactor(Population, M_vis, sub_memory_factor))
# 
# saveRDS(Population,'~/GitHub/NSUM-Simulations/data/initial data/pop_sir_pa.rds')

dat = addARD(Population,net,memory_factor,sub_memory_factor)
saveRDS(dat[[1]],'~/GitHub/NSUM-Simulations/data/initial data/pop_sir_pa.rds')
saveRDS(dat[[2]],'~/GitHub/NSUM-Simulations/data/initial data/mat_sir_pa.rds')

Population = dat[[1]]
Mhp_vis = dat[[2]]

Population_disjoint =  gen_Population_disjoint(N, net, v_pop_prob, Population$hidden_population, Mhp_vis, sub_memory_factor, Population$reach, Population$reach_memory, Population$hp_total, Population$hp_survey)
saveRDS(Population_disjoint,'~/GitHub/NSUM-Simulations/data/initial data/pop_disjoint_sir_pa.rds')


# for (h in 1:20) {
#   saveRDS(gen_Survey(n_survey, Population),paste0('~/GitHub/NSUM-Simulations/data/initial data/survey_',h,'.rds'))
# }
# 
# for (h in 1:b) {
#   saveRDS(gen_Survey(n_survey_hp, Population[Population$hidden_population == 1,]),paste0('~/GitHub/NSUM-Simulations/data/initial data/surveyhp_',h,'.rds'))
# }

