library(igraph)
# sw
dim=1
N=10000
nei=25
p=0.1
# pa
m= 25

set.seed(920)
net_model = sample_smallworld(dim, N, nei, p, loops = FALSE, multiple = FALSE)
saveRDS(net_model,'Net_sw.rds')
net_model = sample_pa(N,1,m=m,directed = FALSE)
saveRDS(net_model,'Net_pa.rds')
