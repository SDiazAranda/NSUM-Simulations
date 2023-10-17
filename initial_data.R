N = 10000

# Probability of each subpopulation
# v_pop_prob = c(0.150, 0.150, 0.125, 0.100,0.075, 0.050, 0.050)
v_pop_prob = c(rep(0.02,10), rep(0.04, 5), 0.08, 0.08,0.08, 0.16,0.016)
n_pop = length(v_pop_prob)

# Number of subpopulations
n_pop = length(v_pop_prob)   

# Number of individuals we draw in the survey
n_survey = 50                

# Number of individuals we draw in the hidden population survey 
n_survey_hp = 15              

# Proportion of individuals in the hidden population
hp_prob = 0.01 

# Subpopulation memory factor (parameter to change variance of the perturbations' normal)
sub_memory_factor = 0   

# Visibility factor (Binomial's probability)
visibility_factor = 1     

#reach memory factor (parameter to change variance of the perturbations' normal)
memory_factor = 0            

################################################################################
# Seed

# Seed to obtain the fixed parameters #
seed = 921  

# Seed to perform the simulation #
seed_sim = 2022

#Number of iterations for the simulation
b = 20

lista_simulacion = list()

