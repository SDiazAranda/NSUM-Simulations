source("pop_functions.R")

N = 10000


## Graph  properties ##

m = 50

seed = 921

net_model = readRDS('~/NSUM Simulations/Robjects/Net_pa.rds')

# gen_SIRpop(n, net ,beta, gamma, chosen_nodes, n_iter)

number.params = 0
ind = c()
betas = c()
gammas = c()

list.betas = seq(0.01,1,0.001)
list.gammas = seq(0.01,1,0.001)
# iterations = seq(30,100)
for (gamma in list.gammas) {
  for (beta in list.betas) {
    it = 1
    infected.number = 0
    while(infected.number<100 & it <100) {
      set.seed(seed)
      infected.number = sum(gen_SIRpop(N, net_model ,beta, gamma, chosen_nodes= 1, it)$hidden_population)
      print(c(it,beta,gamma,infected.number))    
      if (infected.number == 100) {
        number.params = number.params +1
        ind = c(ind,it)
        betas = c(betas,beta)
        gammas = c(gammas,gamma)
        print(('------------------'))
        print('------------------')
      }
      it = it+1
    }
  }
}

# it = 5; beta =0.046; gamma =0.01
it = 2; beta =0.039; gamma =0.01


set.seed(seed)
infected.number = gen_SIRpop(N, net_model ,beta, gamma, chosen_nodes= 1, it)
saveRDS(infected.number,'~/NSUM Simulations/Robjects/hpvector_pa.rds')

# iter =5, beta = 0.046 gamma = 0.01 sw
# 2, 0.039, 0.010