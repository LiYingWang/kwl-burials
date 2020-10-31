# example: https://journal.r-project.org/archive/2018/RJ-2018-056/RJ-2018-056.pdf
library(igraph)
graph_david <- read.graph("http://networkdata.ics.uci.edu/data/adjnoun/adjnoun.gml",
                          format = "gml")
A <- as.matrix(as_adjacency_matrix(graph_david))

B <- 500
set.seed(1)
Astar <- vertboot(A, B)
boot_density <- sapply(1:B, function(x)
  graph.density(graph_from_adjacency_matrix(Astar[[x]])))
CIvertboot <- quantile(boot_density, c(0.025, 0.975))
CIvertboot
bootstrap_standard_error <- sd(boot_density)
bootstrap_standard_error

set.seed(5)
igraph_david <- igraph_to_network(graph_david)
CIpatchwork <- lsmi_cv(igraph_david, n.seeds = c(3:5), n.wave = 1, B = B)
CIpatchwork$bci/(igraph_david$n - 1)

#-------burial example--------
# make igraph object
pre_E_igraph <- graph_from_data_frame(d = edges_for_network_pre,
                                      vertices = nodes_pre,
                                      directed = FALSE)
density_obs <- graph.density(pre_E_igraph )
density_obs
pre_matrix <- as.matrix(as_adjacency_matrix(pre_E_igraph))

# vertex bootstrap, useful for small network
library(snowboot)
number <- 50
set.seed(1)
Astar <- vertboot(pre_matrix, number)

# calculate 95% bootstrap confidence intervals for the density and bootstrap standard error
boot_density <- sapply(1:number, function(x)
 graph.density(graph_from_adjacency_matrix(Astar[[x]])))
CIvertboot <- quantile(boot_density, c(0.025, 0.975))
CIvertboot
bootstrap_standard_error <- sd(boot_density)
bootstrap_standard_error

# patchwork bootstrap
set.seed(5)
pre_E_igraph_to_network <- igraph_to_network(pre_E_igraph)
CIpatchwork <- lsmi_cv(pre_E_igraph_to_network,
                       n.seeds = c(3:5),
                       n.wave = 1, B = number)

# calculate 95% bootstrap confidence intervals for the density
CIpatchwork$bci/(pre_E_igraph_to_network$n - 1)
