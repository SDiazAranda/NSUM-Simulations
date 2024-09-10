source('~/GitHub/NSUM-Simulations/code/data generation/pop_functions.R')
source('~/GitHub/NSUM-Simulations/code/nsum/nsum_functions.R')

Population = read.csv(paste0('~/GitHub/NSUM-Simulations/data/initial data/music_real_Asian.Music.csv'))
# Population = read.csv(paste0('~/GitHub/NSUM-Simulations/data/initial data/music_real_Blues.csv'))
# Population = read.csv(paste0('~/GitHub/NSUM-Simulations/data/initial data/music_real_Chill.Out.Trip.Hop.Lounge.csv'))

# Population = read.csv(paste0('~/GitHub/NSUM-Simulations/data/initial data/music_unif_Asian.Music.csv'))
# Population = read.csv(paste0('~/GitHub/NSUM-Simulations/data/initial data/music_unif_Blues.csv'))
# Population = read.csv(paste0('~/GitHub/NSUM-Simulations/data/initial data/music_unif_Chill.Out.Trip.Hop.Lounge.csv'))

# Population = read.csv(paste0('~/GitHub/NSUM-Simulations/data/initial data/music_sir_Asian.Music.csv'))
# Population = read.csv(paste0('~/GitHub/NSUM-Simulations/data/initial data/music_sir_Blues.csv'))
# Population = read.csv(paste0('~/GitHub/NSUM-Simulations/data/initial data/music_sir_Chill.Out.Trip.Hop.Lounge.csv'))

########################
n_pop =20
N = nrow(Population)
v_pop_total = getV_pop(n_pop,Population)
v_pop_prob = v_pop_total/N


# Number of individuals we draw in the survey
n_survey = 100                

# Number of individuals we draw in the hidden population survey 
n_survey_hp = 30              

# Seed

# Seed to obtain the fixed parameters #
seed = 921  

# Seed to perform the simulation #
seed_sim = 2022

#Number of iterations for the simulation
b = 50
#####################################


sim = matrix(NA,nrow = b, ncol = 10)
colnames(sim) = c("Real","PiMLE", "MLE", "MoS", "KLN", "GNSUM", "Overd.", "TPC", "RoA", "AoR")

set.seed(seed_sim)
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
  # Nh_Direct = getNh_Direct(survey, N)
  
  sim[l,"Real"] = Nh_real
  sim[l,"PiMLE"] = Nh_PIMLE
  sim[l,"MLE"] = Nh_MLE
  sim[l,"MoS"] = Nh_MoS
  sim[l,"KLN"] = Nh_MLE_mod
  sim[l,"GNSUM"] = Nh_GNSUM
  sim[l,"Overd."] = Nh_Zheng
  sim[l,"TPC"] = Nh_TEO
  sim[l,"RoA"] = Nh_basic_sum
  sim[l,"AoR"] = Nh_basic_mean
  
  print(l)
}

simulations = as.data.frame(sim)

file_name = str_c(paste0("Simulation_music_real_asian_50.csv"))
# file_name = str_c(paste0("Simulation_music_real_blues_50.csv"))
# file_name = str_c(paste0("Simulation_music_real_chillout_50.csv"))

# file_name = str_c(paste0("Simulation_music_unif_asian_50.csv"))
# file_name = str_c(paste0("Simulation_music_unif_blues_50.csv"))
# file_name = str_c(paste0("Simulation_music_unif_chillout_50.csv"))

# file_name = str_c(paste0("Simulation_music_sir_asian_50.csv"))
# file_name = str_c(paste0("Simulation_music_sir_blues_50.csv"))
# file_name = str_c(paste0("Simulation_music_sir_chillout_50.csv"))


setwd("~/GitHub/NSUM-Simulations/data/results/Deezer dataset")

write.csv(simulations,                      # Data frame
          file = file_name,                  # CSV name
          row.names = FALSE )                 # row names: TRUE or FALSE 
