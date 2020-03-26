library(readxl)
library(tidyverse)
library(here)

burial <- read_excel(here("index", "data", "burials", "Kiwulan_Burials.xlsx"))

# combine some age column 
burial_age <- 
  burial %>% 
  mutate(Phase = ifelse(Phase == 'euro', 'post', Phase)) %>% 
  filter(!is.na(Phase), !is.na(Age)) %>% 
  mutate(Gold_leaf = ifelse(Gold_leaf == "shatter", "1", Gold_leaf),
         Stoneware = ifelse(Stoneware == "base", "1", Stoneware),
         Stamped_ceramic = ifelse(Stamped_ceramic == "cluster", "1", Stamped_ceramic)) %>% 
  mutate(`Indo-Pacific_bead` = as.numeric(`Indo-Pacific_bead`),
         `Gold_leaf` = as.numeric(`Gold_leaf`),
         Stoneware = as.numeric(Stoneware)) %>% 
  mutate(Age_com = case_when(
    `Age` %in% c("1","2") ~ "0-12",
    `Age` == "3" ~ "12~20",
    `Age` %in% c("4","5","6","7","8") ~ "+20", 
    TRUE ~ "other")) 

# drop all columns that grave goods occur in less than 5 burials for testing
burial_age_tidy <- 
  burial_age[, colSums(is.na(burial_age)) < max(colSums(is.na(burial_age)))-4] %>% 
  select(-Stamped_ceramic)
#---------------------------------------------------------------------------------

# filter the data before European contact
pre17_dat <- 
  burial_age_tidy %>% 
  filter(Phase == "pre")

# create matrix 
burial_mat_pre17 <- matrix(0, nrow = nrow(pre17_dat), ncol = nrow(pre17_dat))

for(i in 1:nrow(pre17_dat)){
  for(j in 19:ncol(pre17_dat)){
    pre17_dat[, j][is.na(pre17_dat[, j])] <- 0
    
    if(pre17_dat[i , j] > 0){
      idx <- which(pre17_dat[, j] > 0)
      burial_mat_pre17[i, idx] <- burial_mat_pre17[i, idx] + 1
    }
  }
}

diag(burial_mat_pre17) <- 0

# network
library(igraph)

burial_G_pre17 <- graph.adjacency(burial_mat_pre17, 
                                  mode = "undirected",
                                  weighted = TRUE)
plot(burial_G_pre17,
     edge.width = E(burial_G_pre17)$weight)

E(burial_G_pre17)$weight

# calculate density
edge_density(burial_G_pre17)

# degree centrality
degrees_pre17 <- rowSums(burial_G_pre17)

# sd in centrality
sd(degrees_pre17)

# test example
#devtools::install_github("acaimo/Bergm", force = TRUE)
#install.packages("statnet", repos = "http://cran.r-project.org")
library(statnet)
library(Bergm)
library(network)

# create network for pre-European
detach("package:igraph") # some conflicts between igraph and statnet
burial_net_pre17 <- network(x = burial_mat_pre17, # the network object
                            directed = FALSE, # specify whether the network is directed
                            loops = FALSE, # do we allow self ties (should not allow them)
                            matrix.type = "adjacency" # the type of input
)

# Add attributes to the network object burial_net_pre17
set.vertex.attribute(burial_net_pre17, "Age", burial_age_tidy$Age_com)

# can't see color?
set.seed(30)
CS <- colorspace::rainbow_hcl(3)
VA <- get.vertex.attribute(burial_net_pre17, "Age")
plot(burial_net_pre17, 
     vertex.col = CS[VA], 
     vertex.cex = 2, 
     main = "Before European Contact")
legend("topleft", 
       pt.bg  = CS[unique(VA)], 
       pt.cex = 1.2,
       pch    = 21, 
       legend = unique(VA), 
       title  = 'Ages')

set.seed(30)
VA_post <- get.vertex.attribute(burial_net_pre17, "Age")
plot(burial_net_pre17, 
     vertex.col = "Age", 
     vertex.cex = 2)
legend("topleft",
       pch = 16, 
       col = c(1, 2, 3, 4), 
       legend = unique(VA_post), 
       title = 'Ages')

# ERGM with three network statistics:
model.1 <- burial_net_pre17 ~ edges + 
  kstar(2) + triangle
summary(model.1)

# ERGM- consider transitivity by taking into account high-order k-triangles.
model.2 <- burial_net_pre17 ~ edges+ # density
  gwesp(1, fixed = TRUE) +  # transitivity
  gwdegree(1, fixed = TRUE)  # popularity
summary(model.2)

# Bayesian inference for ERGMs:
model.3 <- burial_net_pre17 ~ edges +  # density
  nodematch('Age') +    # Age-based homophily
  gwesp(0.1, fixed = TRUE) +    # transitivity, change from 0.5 to 0.1 and it works
  gwdegree(0.1, fixed = TRUE)   # popularity, change from 0.5 to 0.1 and it works
summary(model.3)

# set a prior distribution to null, and work on smaller sampling size, it works
prior.mean <- NULL
prior.sigma <- NULL

parpost <- bergm(model.3,
                 prior.mean  = prior.mean,
                 prior.sigma = prior.sigma,
                 burn.in     = 200,
                 main.iters  = 20,
                 aux.iters   = 100, 
                 nchains     = 8, 
                 gamma       = 0.5)

summary(parpost)
plot(parpost)

# Bayesian parameter inference procedure, doesn't work when dealing with larger samples
post <- bergm(model.3, 
              mean.prior = mean.prior,
              sigma.prior = sigma.prior,
              aux.iters = 2000, # number of iterations for step 1.ii
              burn.in = 300, # number of burnin iterations for each chain
              main.iters = 2500, # number of iterations for each chain (after burnin)
              nchains = 6, # number of chains
              gamma = 0.6, # gamma parameter (tuned to get ~20% acceptance probability)
)

# visualize the results of the MCMC estimation
summary(post)
